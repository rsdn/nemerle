open Ast
open Util
open Ast_util
open Ty_util
open Cg_util
open Cg_stack

type target =
  | Targ_none           (* No target, generating value -- ICE *)
  | Targ_dev_null       (* Discard generated value *)
  | Targ_slot of slot   (* Assign generated value to specified slot *)
  | Targ_return
  
type ctx = 
  {
    c_enclosing_td : type_decl option;
    c_target : target;
    c_this_id : int;
    c_fun_id : int;
    c_ret_type : ty;
    c_closure : Closure.t option; 
  }

let fun_stack = Stack.create ()

let fun_args parms =
  let arg v =
    if v.val_name.[0] = '$' then
      xf "%s _%d" (ty (unsome v.val_type)) (next_id ())
    else
      xf "%s %s" (ty (unsome v.val_type)) v.val_name
  in "(" ^ String.concat ", " (List.map arg parms) ^ ")"

let out_expr ?(force = false) c v =
  match c.c_target with
  | Targ_none -> assert false
  | Targ_slot s -> set_slot s v
  | Targ_return ->
    begin
      match c.c_ret_type with
      | T_void ->
        if force then out (xf "%s;" v)
        else ()
      | t ->
        out (xf "return (%s) %s;" (ty t) v)
    end
  | Targ_dev_null ->
    if force then out (xf "%s;" v)
    else ()

let rec cg_expr c expr =
  let one_expr e =
    debug "one expr";
    let s = create_slot (type_of e) in
    beg_slot s;
    cg_expr {c with c_target = Targ_slot s} e;
    end_slot s
  in

  let closure_prefix defined_in in_closure =
    if not in_closure || defined_in = 0 then
      ""
    else
      if defined_in = c.c_fun_id then
        "__N__closure."
      else
        xf "this.__N__closure_%d." defined_in
  in

  let val_ref v =
    closure_prefix v.val_defined_in_fun v.val_in_closure ^ val_name v
  in

  let fun_ref v =
      closure_prefix v.fun_defined_in_fun v.fun_in_closure ^ fun_name v
  in

  let lvalue e =
    match e.e_raw with
    | E_field_ref (obj, fld) ->
      one_expr obj;
      xf "%s.%s" (pop_slot ()) (binding fld).fld_name
    | E_ref b ->
      begin
        match binding b with
        | D_value v -> val_ref v
        | _ -> assert false
      end
    | _ -> assert false
  in

  let cg_parms_lst p =
    (* this violates stack ordering, but we're going to do this
       differently in IL anyway *)
     let cg_parm p =
       if p.parm_is_ref then
         lvalue p.parm_expr
       else
         begin
           debug "next-parm";
           one_expr p.parm_expr;
           pop_slot ()
         end
     in
     List.map cg_parm p
  in
  
  let cg_parms p = String.concat ", " (cg_parms_lst p) in
  
  let cg_tymatch e cases =
    one_expr e;
    let is_simple = function
      | {tc_constraints = []; tc_tyvars = []; tc_type = T_app (_, [])} ->
        true
      | _ -> false
    in
    begin
      match cases with
      | [t1; t2] when (is_simple t1 &&
                       is_simple t2 &&
                       (ty t1.tc_type) = "Nemerle.Core.bool.True" &&
                       (ty t2.tc_type) = "Nemerle.Core.bool.False") ->
        out (xf "if (%s) {" (pop_slot ()));
        cg_expr c t1.tc_body;
        out (xf "} else {");
        cg_expr c t2.tc_body;
        out "}"
      | _ ->
        let ex = pop_slot () in
        let do_case t =
          (* FIXME: this is really OK only for is_simple cases. *)
          out (xf "if (%s is %s) {" ex (ty t.tc_type));
          cg_expr c t.tc_body;
          out "} else ";
        in
        List.iter do_case cases;
        out "throw new Nemerle.Core.Match_failure();"
    end
  in
  
  let cg_match e cases =
    one_expr e;
    let ex = pop_slot () in
    let after_block = xf "__N__end_%d" (next_id ()) in
    let used_after = ref false in
    let need_throw = ref true in
    
    let do_case mc =
      let did_if = ref false in
      let rec compile_pattern = function
        | [] ->
          cg_expr c mc.mc_body;
          begin
            match c.c_target with
            | Targ_return when c.c_ret_type <> T_void -> ()
            | _ ->
              out (xf "goto %s;" after_block);
              used_after := true
          end
        | (P_underscore, _) :: pats -> compile_pattern pats
        | (P_variable v, ex) :: pats ->
          Closure.add_val c.c_closure v;
          begin
            match v.val_type with
            | Some t ->
              out (xf "%s = (%s)%s;" (val_ref v) (ty t) ex)
            | None -> assert false
          end;
          compile_pattern pats
        | (P_tuple tpats, ex) :: pats ->
          let len = List.length tpats in
          let rec tuple_ref acc n = function
            | p :: ps -> 
              let rf = xf "(((Nemerle.Tuple%d)%s).field%d)" len ex n in
              tuple_ref ((p, rf) :: acc) (n + 1) ps
            | [] -> List.rev_append acc pats
          in compile_pattern (tuple_ref [] 1 tpats)
        | (P_record rs, ex) :: pats ->
          let record_ref (fld, pat) =
            let fld = binding fld in
            let t = generic_td_type (Ty_info.field_parent fld) in
            let rf = xf "(((%s)%s).%s)" (ty t) ex fld.fld_name in
            (pat, rf)
          in 
          let rs' = List.rev_map record_ref rs in 
          compile_pattern (List.rev_append rs' pats)
        | (P_cons (b, pat), ex) :: pats ->
          let t = generic_td_type (binding b) in
          out (xf "if (%s is %s) {" ex (ty t));
          compile_pattern ((pat, ex) :: pats);
          out "}";
          did_if := true;
      in 
      compile_pattern [(mc.mc_pattern, ex)];
      need_throw := !did_if
    in
    List.iter do_case cases;
    if !need_throw then
      out "throw new Nemerle.Core.Match_failure();"
    else ();
    if !used_after then
      out (xf "%s: ;" after_block)
    else ()
  in
  
  let rec cg () =
    debug "cg";
    match expr.e_raw with
    | E_ref b ->
    debug "ref";
      begin
        match binding b with
        | D_value v ->
          debug "ref-val";
          out_expr c (val_ref v);
          debug "out-ref-val"
          
        | D_function f ->
          debug "ref-fun";
          out_expr c (closure_prefix f.fun_defined_in_fun f.fun_in_closure ^
                      fun_name f)
        | D_enum_entry _ -> ice "FIXME: enum_entry ref"
        | D_type _
        | D_method _
        | D_field _
        | D_iface_method _ -> assert false
      end

    | E_tuple exprs ->
      let pl = List.map (fun e -> one_expr e; pop_slot ()) exprs in
      let ps = String.concat ", " pl in
      out_expr c (xf "new Nemerle.Tuple%d(%s)" (List.length exprs) ps)
      
    | E_fun_call (e, p) ->
    debug "fc";
      let pl = cg_parms_lst p in
      let ps = String.concat ", " pl in
    debug "fc-parms-done";
      let call =
        match e.e_raw with
        | E_ref {b_value = B_bound (D_function f)} 
          when f.fun_defined_in_fun = 0 ->
          (* direct call. *)
          begin
            match f.fun_body with
            | Fb_extern x ->
              if String.sub x 0 3 = "%op" then
                let op = String.sub x 3 (String.length x - 3) in
                match pl with
                | [p1] ->
                  xf "( %s %s )" op p1
                | [p1;p2] ->
                  xf "( %s %s %s )" p1 op p2
                | _ -> ice ("bad numbers of args to " ^ x)
              else
                xf "%s(%s)" x ps
            | _ -> 
              let funpar = Ty_info.td_qname (Ty_info.function_parent f) in
              xf "%s.%s(%s)" funpar f.fun_name ps
          end
        | _ ->
          debug "one-expr-fc";
          one_expr e;
          xf "%s.apply(%s)" (pop_slot ()) ps
      in
      out_expr ~force:true c call
    | E_sequence [] 
    | E_literal L_void ->
      begin
        match c.c_target with
        | Targ_return 
        | Targ_dev_null -> ()
        | _ -> ice "non-return target and ()"
      end 
    | E_literal L_null -> out_expr c "null"
    | E_literal (L_string s) -> out_expr c ("\"" ^ String.escaped s ^ "\"")
    | E_literal (L_int n) -> out_expr c (xf "%Ld" n)
    | E_literal (L_float f) -> out_expr c (xf "%f" f)
    | E_assignment (e1, e2) ->
      one_expr e2;
      out (xf "%s = %s;" (lvalue e1) (pop_slot ()))
    | E_array_access (_ar, _idxs) -> ice "FIXME: cg array access"
    | E_let (v, e) ->
      let ini = 
        match v.val_kind with
        | Val_local e -> e
        | _ -> assert false
      in
      begin
        match expand_type (type_of ini) with
        | T_void -> cg_expr {c with c_target = Targ_dev_null} ini
        | _ ->
          Closure.add_val c.c_closure v;
          one_expr ini;
          out (xf "%s = %s;" (val_ref v) (pop_slot ()))
      end;
      cg_expr c e
      
    | E_fun (fds, e) ->
      let make_arg f =
        if f.fun_id = c.c_fun_id then
          "__N__closure"
        else
          xf "this.__N__closure_%d" f.fun_id
      in
      let add_fun f =
        Closure.add_fun_val c.c_closure f;
        Stack.push (c.c_this_id, c.c_enclosing_td, f) fun_stack;
        let args = String.concat ", " (List.map make_arg f.fun_needed_closures) in
        out (xf "%s = new %s_class(%s);" (fun_ref f) (fun_name f) args)
      in
      List.iter add_fun fds;
      cg_expr c e
        
    | E_raise e ->
      one_expr e;
      out (xf "throw %s;" (pop_slot ()))
      
    | E_try_with (e1, v, e2) ->
      out "try {";
      cg_expr c e1;
      out (xf "} catch (%s __N__exn) {" (ty (unsome v.val_type)));
      Closure.add_val c.c_closure v;
      out (xf "%s = __N__exn;" (val_ref v));
      cg_expr c e2;
      out "}"
      
    | E_try_finally (e1, e2) ->
      out "try {";
      cg_expr c e1;
      out "} finally {";
      cg_expr c e2;
      out "}"
      
    | E_this ->
      if c.c_this_id = c.c_fun_id then
        out_expr c "this"
      else
        out_expr c (xf "this.__N__closure_%d.__N__this" c.c_this_id)
      
    | E_base -> 
      (* FIXME: ??? *)
      if c.c_this_id = c.c_fun_id then
        out_expr c "this"
      else
        out_expr c (xf "this.__N__closure_%d.__N__this" c.c_this_id)
        
    | E_type_conversion (e, t) ->
      one_expr e;
      out_expr c (xf "((%s) %s)" (ty t) (pop_slot ()))

    | E_type_enforcement (e, t) ->
      one_expr e;
      out_expr c (xf "((%s) %s)" (ty t) (pop_slot ()))

    | E_field_ref (_, _) ->
      debug "fr";
      out_expr c (lvalue expr);
      debug "out-fr"
      
    | E_method_call (obj, meth, parms) ->
      one_expr obj;
      let ps = cg_parms parms in
      let call = xf "((%s)%s).%s(%s)" (td_name (Ty_info.method_parent (binding meth)))
                                      (pop_slot ()) (binding meth).ifm_name ps in
      out_expr ~force:true c call
    
    | E_cons (meth, parms) ->
      let td = Ty_info.method_parent (binding meth).meth_ifm in
      let ps = cg_parms parms in
      out_expr ~force:true c (xf "new %s(%s)" (td_name td) ps)

    | E_ctor_call (_, _meth, _parms) ->
      ice "FIXME: cg ctor call"

    | E_sequence exprs -> 
      let rec cg_seq = function
        | [e] -> cg_expr c e
        | x :: xs ->
          cg_expr {c with c_target = Targ_dev_null} x;
          cg_seq xs
        | [] -> assert false
      in cg_seq exprs


    | E_tymatch (e, cases) -> cg_tymatch e cases

    | E_match (e, cases) -> cg_match e cases
  in locate expr.e_loc cg

and cg_fun ?(do_header = true) ?(implemented = []) ctx f =
  match f.fun_body with
  | Fb_expr e ->
    let ret_type = expand_type (snd (real_function_type f.fun_type)) in
    if do_header then
      if f.fun_name = "this" then
        match f.fun_kind with
        | Fun_static_ctor ->
          out (xf "%s%s {" 
                  (chop_ns (unsome ctx.c_enclosing_td).td_name)
                  (fun_args f.fun_parms))
        | _ ->
          out (xf "%s %s%s {" 
                  (mods f.fun_modifiers)
                  (chop_ns (unsome ctx.c_enclosing_td).td_name)
                  (fun_args f.fun_parms))
      else
        out (xf "%s %s %s%s {" (mods f.fun_modifiers) 
                (ty ret_type)
                f.fun_name 
                (fun_args f.fun_parms))
    else ();
    let ctx = {ctx with c_fun_id = f.fun_id} in
    let ctx =
      if f.fun_has_closure then
        {ctx with c_closure = Some (Closure.create f)}
      else
        ctx
    in
    let maybe_store_parm v =
      if v.val_in_closure then
        begin
          Closure.add_val ctx.c_closure v;
          out (xf "__N__closure.%s = %s;" (val_name v) v.val_name)
        end
      else
        ()
    in
    List.iter maybe_store_parm f.fun_parms;
    if ctx.c_this_id <> 0 then
      Closure.add_this_pointer ctx.c_closure (unsome ctx.c_enclosing_td)
    else ();
    let () = cg_expr {ctx with c_target = Targ_return; c_ret_type = ret_type} e in
    if do_header then
      out "} // end of function\n"
    else
      ();
    let push_impl i =
      let mk_arg (parms, args) fv iv =
        let i = next_id () in
        (xf "%s _%d" (ty (unsome iv.val_type)) i :: parms,
         xf "(%s) _%d" (ty (unsome fv.val_type)) i :: args)
      in
      let (parms, args) = 
        List.fold_left2 mk_arg ([], []) f.fun_parms i.ifm_parms 
      in
      let iret_type = snd (real_function_type i.ifm_type) in
      let cnv a = String.concat ", " (List.rev a) in
      let (parms, args) = (cnv parms, cnv args) in
      out (xf "private %s %s.%s(%s) {" (ty iret_type) 
              (td_name (Ty_info.method_parent i)) i.ifm_name parms);
      match ret_type with
      | T_void ->
        out (xf "%s(%s);\n}\n" f.fun_name args)
      | _ ->
        out (xf "return (%s) %s(%s);\n}\n" (ty ret_type) f.fun_name args)
    in List.iter push_impl implemented
  | Fb_extern _ -> 
    ()

let cg_iface_method i =
  out (xf "%s%s %s%s;" (if i.ifm_new_modifier then "new " else "")
                     (ty (snd (real_function_type i.ifm_type)))
                     i.ifm_name (fun_args i.ifm_parms))
  
let class_header td =
  let ext =
    match td.td_extends with
    | Some t -> ty_name t
    | None -> "System.Object"
  in
  let ext =
    match Ty_info.get td with
    | {Sem_types.ti_parent_type = Some ({td_raw = T_variant _} as par)} ->
      ty_name (generic_td_type par)
    | _ -> ext
  in
  let impl =
    match String.concat ", " (List.map ty_name td.td_implements) with
    | "" -> ""
    | x -> ", " ^ x
  in
  out (xf "class %s : %s%s {" (chop_ns td.td_name) ext impl)
  
let iface_header td =
  let impl = String.concat ", " (List.map ty_name td.td_implements) in
  let impl = if impl = "" then "" else " : " ^ impl in
  out (xf "interface %s %s {" (chop_ns td.td_name) impl)

let cg_proxy f =
  let clname = proxy_name f ^ "_class" in
  out (xf "class %s : Nemerle.Func%d {"
          clname (List.length f.fun_parms));
  let mk_apply_arg (args, parms) v =
    let n = List.length args in
    let t = ty (unsome v.val_type) in
    (xf "object arg_%d" n :: args, xf "(%s) arg_%d" t n :: parms)
  in
  let (args, parms) = List.fold_left mk_apply_arg ([], []) f.fun_parms in
  out (xf "public object apply(%s) {" (String.concat ", " (List.rev args)));
  let funpar = Ty_info.td_qname (Ty_info.function_parent f) in
  let args = String.concat ", " (List.rev parms) in
  let call = xf "%s.%s(%s);" funpar f.fun_name args in
  begin
    match real_function_type f.fun_type with
    | _, T_void -> 
      out call;
      out "return null;"
    | _ ->
      out ("return " ^ call);
  end;
  out "} // end fun\n";
  out (xf "static public %s_class __N__instance = new %s_class();" 
          (proxy_name f) (proxy_name f));
  out "} // end proxy\n"

let cg_local_fun c f =
  let clname = fun_name f ^ "_class" in
  out (xf "class %s : Nemerle.Func%d {"
          clname (List.length f.fun_parms));
          
  let mk_cons_arg (args, assigns, members) f =
    let n = List.length args in
    (xf "__N__closure_class_%d arg_%d" f.fun_id n :: args, 
     xf "this.__N__closure_%d = arg_%d;" f.fun_id n :: assigns,
     xf "private __N__closure_class_%d __N__closure_%d;" f.fun_id f.fun_id :: members)
  in
  let (args, assigns, members) = List.fold_left mk_cons_arg ([], [], []) f.fun_needed_closures in
  out (xf "public %s(%s) {" clname (String.concat ", " (List.rev args)));
  List.iter out assigns;
  out "}\n";
  List.iter out members;

  let mk_apply_arg (args, assigns) v =
    let n = List.length args in
    let t = ty (unsome v.val_type) in
    (xf "object arg_%d" n :: args, 
     xf "%s %s = (%s) arg_%d;" t v.val_name t n :: assigns)
  in
  let (args, assigns) = List.fold_left mk_apply_arg ([], []) f.fun_parms in
  out (xf "public object apply(%s) {" (String.concat ", " (List.rev args)));
  List.iter out assigns;
  cg_fun ~do_header:false c f;
  begin
    match real_function_type f.fun_type with
    | _, T_void -> out "return null;"
    | _ -> ()
  end;
  out "} // end fun\n";
  out "} // end local\n"

let cg_decls decls = 
  let rec loop ctx = function
    | x :: xs ->
      locate (snd (decl_name_loc x)) begin fun () ->
        try
          match x with
          | D_type td ->
            let ctx = {ctx with c_enclosing_td = Some td} in
            begin_ns td.td_name;
            begin
              match td.td_raw with
              | T_class decls ->
                out (mods td.td_modifiers);
                class_header td;
                loop ctx decls;
                out "} // end of class\n";
              | T_variant _ when Ty_info.td_qname td = "Nemerle.Core.bool" -> ()
              | T_variant decls ->
                out (mods td.td_modifiers);
                class_header td;
                loop ctx (List.map (fun x -> D_type x) decls);
                out "} // end of variant\n";
              | T_struct decls ->
                out (mods td.td_modifiers);
                out (xf "struct %s {" (chop_ns td.td_name));
                loop ctx decls;
                out "} // end of struct\n";
              | T_enum decls ->
                (* FIXME: td_extends *)
                out (mods td.td_modifiers);
                out (xf "enum %s {" (chop_ns td.td_name));
                loop ctx decls;
                out "} // end of enum\n";
              | T_interface decls ->
                out (mods td.td_modifiers);
                iface_header td;
                loop ctx decls;
                out "} // end of iface\n";
              | T_external _ -> ()
              | T_alias _ -> ()
            end;
            end_ns td.td_name
          | D_value v -> 
            let ts = ty (unsome v.val_type) in
            if v.val_name = "true" || v.val_name = "false" then ()
            else
              out (xf "static %s %s %s;" (mods v.val_modifiers) ts v.val_name)
          | D_function {fun_body = Fb_extern _} -> ()
          | D_method {meth_fun = {fun_body = Fb_extern _}} -> ()
          | D_function f ->
            out "static ";
            cg_fun ctx f
          | D_method m ->
            let id = m.meth_fun.fun_id in
            cg_fun ~implemented:(List.map binding m.meth_implements) 
                   {ctx with c_this_id = id} m.meth_fun
          | D_field f ->
            out (xf "%s %s %s;" (mods f.fld_modifiers) (ty f.fld_type) 
                                  f.fld_name);
          | D_enum_entry _e ->
            ice "FIXME: cg enum_entry"
          | D_iface_method i ->
            cg_iface_method i
        with Recovery -> ()
      end;
      loop ctx xs
    | [] -> ()
  in 
  let ctx =
    {
      c_target = Targ_none; 
      c_this_id = 0; 
      c_fun_id = 0; 
      c_closure = None;
      c_enclosing_td = None;
      c_ret_type = T_void;
    }
  in
  loop ctx decls;

  let rec push_funs () =
    if Stack.is_empty fun_stack then ()
    else
      begin
        let (this_id, enc_td, f) = Stack.pop fun_stack in
        let ctx' = {ctx with c_this_id = this_id; c_enclosing_td = enc_td} in
        locate f.fun_loc (fun () -> cg_local_fun ctx' f);
        push_funs ()
      end
  in push_funs ();

  List.iter cg_proxy (get_proxies ());

  Closure.push_all ()
