open Ast
open Util
open Ast_util
open Ty_util

(* Types. *)
type ctx =
  {
    c_prefix : string;
    c_tyvars : tyvar Smap.t;
    c_parent_tydecl : type_decl option;
  }

(* Global state. *)
let env = Env.global

let empty_ctx =
  {
    c_prefix = "";
    c_tyvars = Smap.empty;
    c_parent_tydecl = None;
  }

(* Push type variables into context. *)
let add_tyvars allow_shadow ctx tp =
  let add_tv ctx tv =
    if not allow_shadow && Smap.mem tv.tv_name ctx.c_tyvars then
      warning (xf "type variable '%s shadows other type variable" tv.tv_name)
    else
      ();
    {ctx with c_tyvars = Smap.add tv.tv_name tv ctx.c_tyvars}
  in
  List.fold_left add_tv ctx tp.tp_tyvars

let bind_tv ctx t =
  try
    t.b_value <- B_bound (Smap.find t.b_name ctx.c_tyvars)
  with Not_found ->
    error (xf "unbound type variable '%s" t.b_name)
 
let bind_td ctx b =
  match Env.lookup env b.b_name with
  | [D_type td] -> 
    b.b_value <- B_bound td;
    b.b_namespace <- ctx.c_prefix;
    td
  | _ :: _ :: _ -> 
    error (xf "`%s' is ambiguous" b.b_name);
    raise Recovery
  | _ -> 
    error (xf "`%s' was expected to be type name" b.b_name); 
    raise Recovery

let bind_type ctx t =
  let rec bind () t =
    match t with
    | T_app (b, l) ->
      begin
        try
          let td = bind_td ctx b in
          let act = List.length l in
          let formal = List.length td.td_typarms.tp_tyvars in
          if act != formal then
            begin
              error (xf "type constructor `%s' applied to %d argument(s)" 
                        (full_tydecl_name td) act);
              error ~loc:td.td_loc (xf "while it needs %d argument(s)" formal)
            end
          else
            ()
        with Recovery -> ()
      end
    | T_var t -> bind_tv ctx t
    | _ -> ()
  in
  let rec check_refs allow_ref t = 
    match t with
    | T_fun (_, _) -> true
    | T_ref _ when not allow_ref ->
      error "ref type not allowed here"; false
    | T_out _ when not allow_ref -> 
      error "out type not allowed here"; false
    | _ -> false
  in
  iter_type bind () t;
  iter_type check_refs false t

let fix_mods mods =
  if List.mem M_private mods ||
     List.mem M_public mods then mods
  else 
    M_public :: mods
  
(* Pass 1. *)
let scan_globals decls =
  let type_f ctx td = 
    td.td_modifiers <- fix_mods td.td_modifiers;
    Env.add env ctx.c_prefix (D_type td);
    begin
      match ctx.c_parent_tydecl with
      | Some {td_raw = T_variant _} ->
        let chop_last s =
          try
            String.sub s 0 (String.rindex_from s (String.length s - 2) '.' + 1)
          with Not_found -> ""
        in Env.add env (chop_last ctx.c_prefix) (D_type td)
      | _ -> ()
    end;
    let ctx' = 
      {
        ctx with 
          c_prefix = ctx.c_prefix ^ td.td_name ^ ".";
          c_parent_tydecl = Some td;
      }
    in ctx'
  in
  let plain_f ctx decl =
    Env.add env ctx.c_prefix decl;
    match decl with
    | D_enum_entry ee ->
      ee.ee_modifiers <- fix_mods ee.ee_modifiers;
      let td = 
        match ctx.c_parent_tydecl with 
        | Some t -> t 
        | None -> assert false
      in
      Env.add_enum_parent env ee td
    | D_value v ->
      v.val_modifiers <- fix_mods v.val_modifiers
    | D_function f | D_method {meth_fun = f} ->
      f.fun_modifiers <- fix_mods f.fun_modifiers
    | D_field f ->
      f.fld_modifiers <- fix_mods f.fld_modifiers
    | _ -> ()
  in
  decl_walk type_f plain_f empty_ctx decls
  
(* Pass 2. *)
let bind_types decls =
  let bind_typarms ctx tp =
    let allow_shadow = 
      match ctx.c_parent_tydecl with
      | Some {td_raw = T_variant _} -> true
      | _ -> false
    in
    let ctx = add_tyvars allow_shadow ctx tp in
    let bind_constraint (tv, t) =
      bind_tv ctx tv;
      bind_type ctx t;
      match tv.b_value with
      | B_bound tv ->
        tv.tv_constraints <- t :: tv.tv_constraints
      | _ -> 
        ()
    in
    List.iter bind_constraint tp.tp_constraints;
    ctx
  in

  let rec bind_expr ctx expr =
    let self = bind_expr ctx in
    let f expr =
      match expr.e_raw with
      | E_fun (f, e) ->
        List.iter (bind_function ctx) f;
        self e;
        raise Dont_descend
      | E_tymatch (e, tcs) ->
        let bind_tc tc =
          let strip_tc =
            function Tc_subtype (tv, t) 
                   | Tc_type_eq (tv, t) -> (tv, t)
          in
          let fake_tp =
            {
              tp_tyvars = tc.tc_tyvars;
              tp_constraints = List.map strip_tc tc.tc_constraints;
            }
          in
          let ctx' = bind_typarms ctx fake_tp in
          bind_type ctx' tc.tc_type;
          bind_expr ctx' tc.tc_body
        in
        self e;
        List.iter bind_tc tcs;
        raise Dont_descend
      | E_match (_e, mcs) ->
        let rec bind_pat = function
          | P_cons (td, p) -> ignore (bind_td ctx td); bind_pat p
          | P_tuple ps -> List.iter bind_pat ps
          | P_record rs -> List.iter bind_pat (List.map snd rs)
          | P_variable _
          | P_underscore -> ()
        in
        let bind_mc mc =
          locate mc.mc_body.e_loc begin fun () ->
            try bind_pat mc.mc_pattern
            with Recovery -> ()
          end
        in
        List.iter bind_mc mcs;
        ()
          
      | E_let (v, _) ->
        assert (v.val_type = None)
      | E_try_with (_, v, _) ->
        bind_val_type ctx v
      | E_type_conversion (_, t)
      | E_type_enforcement (_, t) ->
        bind_type ctx t
      | _ -> ()
    in walk_expr f expr
    
  and bind_function ctx f =
    let ctx' = bind_typarms ctx f.fun_typarms in
    begin
      match ctx.c_parent_tydecl with
      | Some td -> Ty_info.add_function td f
      | _ -> assert false
    end;
    bind_type ctx' f.fun_type;
    List.iter (bind_val_type ctx') f.fun_parms;
    match f.fun_body with
    | Fb_expr e ->
      bind_expr ctx' e
    | Fb_extern _ ->
      ()
    
  and bind_val_type ctx v =
    match v.val_type with
    | Some t -> bind_type ctx t
    | None -> assert false
  in

  let type_f ctx td =
    let ctx = bind_typarms ctx td.td_typarms in
    List.iter (bind_type ctx) (td_parents td);
    begin
      match td.td_raw with
      | T_alias t -> 
        bind_type ctx t
      | _ -> ()
    end;
    {ctx with c_parent_tydecl = Some td}
  in
  let plain_f ctx decl =
    match decl with
    | D_function f | D_method {meth_fun = f} ->
      bind_function ctx f
    | D_field f ->
      bind_type ctx f.fld_type
    | D_value {val_type = Some t} ->
      bind_type ctx t
    | D_value {val_type = None} ->
      assert false
    | D_enum_entry {ee_value = Some e} ->
      bind_expr ctx e
    | D_iface_method i ->
      let ctx' = bind_typarms ctx i.ifm_typarms in
      bind_type ctx' i.ifm_type
    | D_enum_entry _ -> ()
    | D_type _ -> assert false
  in
  decl_walk type_f plain_f empty_ctx decls;
  Builtin_types.do_bindings := true

