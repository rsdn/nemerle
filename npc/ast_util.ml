open Ast
open Util

let decl_name_loc decl =
  match decl with
  | D_type {td_name = n; td_loc = l}
  | D_function {fun_name = n; fun_loc = l}
  | D_method {meth_fun = {fun_name = n; fun_loc = l}}
  | D_value {val_name = n; val_loc = l}
  | D_enum_entry {ee_name = n; ee_loc = l}
  | D_field {fld_name = n; fld_loc = l} 
  | D_iface_method {ifm_loc = l; ifm_name = n} ->
    (n, l)

let rec decl_walk type_f plain_f ctx lst = 
  let self = decl_walk type_f plain_f in
  match lst with
  | x :: xs ->
    let (_, loc) = decl_name_loc x in
    locate loc begin fun () ->
      try
        match x with
        | D_type td ->
          let decls =
            match td.td_raw with
            | T_class decls 
            | T_struct decls 
            | T_enum decls 
            | T_interface decls -> decls
            | T_variant d -> List.map (fun x -> D_type x) d
            | T_external _
            | T_alias _ -> []
          in
          let ctx' = type_f ctx td in
          self ctx' decls
        | _ ->
          plain_f ctx x
      with Recovery -> ()
    end;
    self ctx xs
 | [] -> ()

let rec tydecl_walk f lst =
  let type_f () td = f td in
  let plain_f () _ = () in
  decl_walk type_f plain_f () lst

let rec walk_expr f expr =
  let self = walk_expr f in
  let self_p p = walk_expr f p.parm_expr in
  locate expr.e_loc begin fun () ->
    try
      f expr;   (* This can raise Recovery or Dont_descend *)
      match expr.e_raw with
      | E_ref _
      | E_this
      | E_base
      | E_literal _ -> ()
      | E_fun (f, e) ->
        self e;
        List.iter (fun f -> 
                   match f.fun_body with 
                   | Fb_expr e -> self e
                   | Fb_extern _ -> ()) f
      | E_match (e, mcs) ->
        self e;
        List.iter (fun mc -> self mc.mc_body) mcs
      | E_tymatch (e, tcs) ->
        self e;
        List.iter (fun tc -> self tc.tc_body) tcs
      | E_cons (_, ps) ->
        List.iter self_p ps
      | E_method_call (e, _, ps)
      | E_fun_call (e, ps) ->
        self e; 
        List.iter self_p ps
      | E_ctor_call (_, _, ps) ->
        List.iter self_p ps
      | E_assignment (e1, e2) ->
        self e1; 
        self e2
      | E_array_access (e, es) ->
        List.iter self (e :: es)
      | E_let (v, e) ->
        begin
          match v.val_kind with
          | Val_local e -> self e
          | _ -> assert false
        end;
        self e
      | E_field_ref (e, _)
      | E_raise e -> 
        self e
      | E_try_with (e1, _, e2) ->
        self e1;
        self e2
      | E_try_finally (e1, e2) -> 
        self e1;
        self e2
      | E_type_conversion (e, _)
      | E_type_enforcement (e, _) ->
        self e
      | E_tuple exprs
      | E_sequence exprs -> 
        List.iter self exprs
    with Recovery | Dont_descend -> ()
  end

let copy_binding b = {b with b_value = b.b_value}

let rec copy_ty t = 
  let f = copy_ty in
  let fl = List.map f in
  match t with
  | T_app (b, args) -> T_app (copy_binding b, fl args)
  | T_var b -> T_var (copy_binding b)
  | T_fun (t1, t2) -> T_fun (f t1, f t2)
  | T_prod l -> T_prod (fl l)
  | T_ref t -> T_ref (f t)
  | T_out t -> T_out (f t)
  | T_array (n, t) -> T_array (n, f t)
  | T_void -> T_void

let copy_typarms tp =
  let copy_tyvar tv = 
    {tv with 
      tv_id = next_id (); 
      tv_constraints = List.map copy_ty tv.tv_constraints}
  in
  let copy_constraint (b, t) = (copy_binding b, copy_ty t) in
  {
    tp_tyvars = List.map copy_tyvar tp.tp_tyvars;
    tp_constraints = List.map copy_constraint tp.tp_constraints;
  }

let tv_binding tv =
  {
    b_name = tv.tv_name;
    b_namespace = "";
    b_value = B_bound tv;
  }
  
let td_binding td =
  {
    b_name = td.td_name;
    b_namespace = ""; (* FIXME: ? *)
    b_value = B_bound td;
  }

let generic_td_type td =
  let b = td_binding td in
  let vars = List.map tv_binding td.td_typarms.tp_tyvars in
  T_app (b, List.map (fun v -> T_var v) vars)
