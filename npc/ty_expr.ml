(*
 * Copyright (c) 2003 The University of Wroclaw.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *    3. The name of the University may not be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 * NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Ast
open Util
open Ast_util
open Ty_util
open Sem_types

exception Error of string

type ctx =
  {
    c_enclosing_type_decl : type_decl option;
    c_enclosing_type : ty;
    c_this_pointer_type : ty option;
    c_in_instance_ctor : bool;
  }

type member =
  | M_field of field_decl
  | M_methods of if_method list

type callable =
  {
    c_tyvars : tyvar list;
    c_fun_type : ty;
    c_parm_names : string list;
    c_decl : decl option;
    mutable c_actuals : parameter list;
    mutable c_subst : Ty_env.t option;
    mutable c_ret_type : ty option;
  }
  
type ('a, 'b) overloading_result =
  | O_ok of callable
  | O_error of (callable * string) list
  | O_ambiguity of callable list
  
let parm_type p =
  let t = type_of p.parm_expr in
  if p.parm_is_ref then T_ref t else t

let parm_names parms =
  List.map (fun v -> v.val_name) parms
  
let is_lvalue ctx e =
  match e.e_raw with
  | E_ref b ->
    begin
      match binding b with
      | D_value v -> v.val_mutable
      | _ -> false
    end
  | E_field_ref ({e_raw = E_this}, _) when ctx.c_in_instance_ctor -> true
  | E_field_ref (_, b) -> (binding b).fld_mutable
  | _ -> false
  
let check_call ctx can_sub c =
  let sub_t = fresh_vars c.c_tyvars c.c_fun_type in
  begin
    match sub_t with
    | T_fun (_, _) -> ()
    | _ -> 
      raise (Error (xf "called value has non-functional type %s"
                       (string_of_type c.c_fun_type)))
  end;
  let (formals, ret_t) = real_function_type sub_t in
  let len_f = List.length formals in
  let len_a = List.length c.c_actuals in
  let check_ref p =
    if p.parm_is_ref && not (is_lvalue ctx p.parm_expr) then
      raise (Error "ref parameter is not lvalue")
    else
      ()
  in
  List.iter check_ref c.c_actuals;
  if len_a <> len_f then
    raise (Error (xf "needed %d arguments in call, got %d" len_f len_a))
  else
    (* sort actuals based on names *)
    begin
      match c.c_parm_names with
      | [] ->
        let check_no_name p =
          if p.parm_name <> "" then
            raise (Error (xf "keyword parameters given, but cannot infer \
                              formal parameters' names"))
          else ()
        in List.iter check_no_name c.c_actuals
      | _ ->
        let check_parm {parm_name = n} =
          if n = "" || List.mem n c.c_parm_names then ()
          else raise (Error (xf "function has no parameter named `%s'" n))
        in
        List.iter check_parm c.c_actuals;
        let rec chop_pos acc args formal_names =
          match (args, formal_names) with
          | ({parm_name = ""} as p :: args, _ :: formal_names) ->
            chop_pos (p :: acc) args formal_names
          | x -> (acc, x)
        in
        let (rev_pos, (args, formal_names)) =
                 chop_pos [] c.c_actuals c.c_parm_names in
        let act_by_name = Hashtbl.create 32 in
        List.iter (fun p -> Hashtbl.add act_by_name p.parm_name p) args;
        let rec sort_named acc = function
          | x :: xs ->
            if Hashtbl.mem act_by_name x then
              let p = Hashtbl.find act_by_name x in
              let () = Hashtbl.remove act_by_name x in
              sort_named (p :: acc) xs
            else
              raise (Error "argument was already consumed")
          | [] -> List.rev acc
        in c.c_actuals <- sort_named rev_pos formal_names
    end;
    let check_arg (arg, subst) formal p =
      let uf = if can_sub then sub_unify else unify in
      let actual = parm_type p in
      match uf ~subst:subst actual formal with
      | Some s -> (arg + 1, s)
      | None -> 
        raise (Error (xf "type clash, needed %s, got %s in arg #%d" 
                         (string_of_type (formal // subst))
                         (string_of_type (actual // subst))
                         arg))
    in
    let (_, sub) = List.fold_left2 check_arg (1, Ty_env.empty) formals c.c_actuals in
    c.c_subst <- Some sub;
    c.c_ret_type <- Some (ret_t // sub);
    ()

let resolve_overloaded_call ctx callables =
  let overloading_resolver f lst =
    let rec loop err good = function
      | x :: xs ->
        begin
          try
            let () = f x in
            loop err (x :: good) xs
          with Error e ->
            loop ((x, e) :: err) good xs
        end
      | [] ->
        match good with
        | [x] -> O_ok x
        | [] -> O_error err
        | _ -> O_ambiguity good
    in loop [] [] lst
  in
 
  let keywords_last seen_kw p =
    if seen_kw then
      if p.parm_name = "" then
        begin
          error "keyword parameters must come last";
          raise Recovery
        end
      else
        seen_kw
    else
      p.parm_name <> ""
  in
  ignore (List.fold_left keywords_last false (List.hd callables).c_actuals);
  
  let ret c =
    begin
      match c.c_subst with
      | Some s -> Ty_env.join_with_global s
      | None -> assert false
    end;
    c
  in
  
  let ambiguity_error lst =
    error "ambiguity in call, between:";
    List.iter (fun c ->
                   match c.c_decl with
                   | Some d ->
                     let (n, l) = decl_name_loc d in
                     error ~loc:l (xf " - symbol %s" n)
                   | None -> assert false) lst;
    ret (List.hd lst)
  in

  match overloading_resolver (check_call ctx false) callables with
  | O_ok x -> ret x
  | O_ambiguity lst -> ambiguity_error lst
  | O_error _ ->
    match overloading_resolver (check_call ctx true) callables with
    | O_ok x -> ret x
    | O_ambiguity lst -> ambiguity_error lst
    | O_error ((_, e) :: _)  ->
      error (xf "typing error in function call: %s" e);
      let c = List.hd callables in
      c.c_ret_type <- Some (free_variable ());
      c.c_subst <- Some Ty_env.empty;
      c
    | O_error [] -> assert false

let referenced_decl_type decl =
  let (name, loc) = decl_name_loc decl in
  match decl with
  | D_type _ ->
    error (xf "expected value reference, but `%s' is type" name);
    error ~loc:loc "defined here";
    free_variable ()
  | D_value {val_type = Some t} -> t
  | D_value {val_type = None} -> ice "untychecked value"
  | D_function {fun_type = t; fun_typarms = tp} -> fresh_vars tp.tp_tyvars t
  | D_field _ ->
    error (xf "expected value, got field `%s'" name);
    error ~loc:loc "defined here";
    free_variable ()
  | D_iface_method _ 
  | D_method _ -> 
    error (xf "expected value, got method `%s'" name);
    error ~loc:loc "defined here";
    free_variable ()
  | D_enum_entry e ->
    let td = Env.lookup_enum_parent Env.global e in
    generic_td_type td

let rec lookup_member _ctx obj_ty name =
  let rec lookup_in t =
    match expand_type t with
    | T_app (b, _) ->
      let td = binding b in
      let ti = Ty_info.get td in
      if Smap.mem name ti.ti_fields then
        Some (M_field (Smap.find name ti.ti_fields))
      else
        if Smap.mem name ti.ti_methods then
          Some (M_methods (Smap.find name ti.ti_methods))
        else
          (* FIXME: methods should be concatented, and some -- hidden. *)
          begin
            match td.td_extends with
            | Some t -> lookup_in t
            | None -> None
          end
    | _ -> None
  in
  match expand_type obj_ty with
  | T_app (_, _) ->
    begin
      match lookup_in obj_ty with
      | Some r -> r
      | None ->
        error (xf "type `%s' has no member named `%s'"
                  (string_of_type obj_ty) name);
        raise Recovery
    end
  | T_var b ->
    let collect acc t =
      match (lookup_in t, acc) with
      | Some (M_methods _), Some (M_field _)
      | Some (M_field _), Some _ ->
        error (xf "member `%s' is supplied by more then one typing constraint"
                   name);
        acc
      | Some x, None ->
        Some x
      | Some (M_methods m1), Some (M_methods m2) ->
        Some (M_methods (m1 @ m2))
      | None, acc -> acc
    in
    begin
      match List.fold_left collect None (binding b).tv_constraints with
      | Some r -> r
      | None ->
        error (xf "no typing constraint supplies member `%s'" name);
        raise Recovery
    end
  | _ ->
    error (xf "left-hand side of `#' has type `%s' that isn't component"
              (string_of_type obj_ty));
    raise Recovery

and check_fun_call ctx fun_expr parms =
  List.iter (fun p -> check_expr ctx p.parm_expr) parms;
  
  match fun_expr.e_raw with
  | E_ref d ->
    let make_callable decl =
      let (t, tv, pn) =
        match decl with
        | D_function {fun_typarms = tp; fun_type = t; fun_parms = p} -> 
          (t, tp.tp_tyvars, List.map (fun v -> v.val_name) p)
        | _ -> (referenced_decl_type decl, [], [])
      in 
        { 
          c_fun_type = t; 
          c_tyvars = tv; 
          c_decl = Some decl; 
          c_parm_names = pn;
          c_subst = None;
          c_ret_type = None;
          c_actuals = parms;
        }
    in
    let callables = List.map make_callable (bindings d) in
    let c = resolve_overloaded_call ctx callables in
    begin
      match c.c_decl, c.c_ret_type with
      | Some decl, Some t ->
        d.b_value <- B_bound decl;
        fun_expr.e_type <- Some c.c_fun_type;
        (t, c.c_actuals)
      | _ -> assert false
    end
    
  | _ ->
    check_expr ctx fun_expr;
    try
      let c =
        {
          c_fun_type = (type_of fun_expr); 
          c_tyvars = []; 
          c_decl = None; 
          c_parm_names = [];
          c_subst = None;
          c_ret_type = None;
          c_actuals = parms;
        }
      in
      check_call ctx true c;
      match c.c_subst, c.c_ret_type with
      | Some s, Some t ->
        Ty_env.join_with_global s;
        (t, c.c_actuals)
      | _ -> assert false
    with Error e ->
      error e;
      (free_variable (), parms)

and check_expr ctx expr =
  let rec get_type () =
    match expr.e_raw with
    | E_ref b ->
      (* FIXME: check if name is multibound and if so, issue error
         message more sensitive then ICE. *)
      referenced_decl_type (binding b)
    | E_fun_call (e, p) ->
      let maybe_type =
        match e.e_raw with
        | E_field_ref (o, fld) ->
          begin
            check_expr ctx o;
            match lookup_member ctx (type_of o) fld.b_name with
            | M_methods lst ->
              let b = 
                {
                  b_name = fld.b_name;
                  b_namespace = fld.b_namespace;
                  b_value = B_multi_bound lst;
                }
              in
              expr.e_raw <- E_method_call (o, b, p);
              Some (get_type ()) (* restart *)
            | M_field _ -> None
          end
        | E_ref {b_value = B_bound (D_type td)} ->
          begin
            match (Ty_info.get td).ti_ctors with
            | [] ->
              error (xf "type %s has no constructors" (full_tydecl_name td));
              Some (free_variable ())
            | l ->
              let b =
                {
                  b_name = "this"; 
                  b_namespace = ""; 
                  b_value = B_multi_bound l;
                }
              in
              expr.e_raw <- E_cons (b, p);
              Some (get_type ())
          end
          
        | (E_this | E_base) as e ->
          let b =
            {
              b_name = "this"; 
              b_namespace = "";
              b_value = B_unbound; 
            }
          in
          expr.e_raw <- E_ctor_call (e = E_base, b,  p);
          Some (get_type ())
        | _ -> None
      in
      begin
        match maybe_type with
        | Some t -> t
        | None -> 
          let (t, parms) = check_fun_call ctx e p in
          expr.e_raw <- E_fun_call (e, parms);
          t
      end
      
    | E_literal L_void -> T_void
    | E_literal L_null -> 
      (* FIXME: allow only reference types here *)
      free_variable () 
    | E_literal (L_string _) -> Builtin_types.string_ty ()
    | E_literal (L_int _) -> Builtin_types.int_ty ()
    | E_literal (L_float _) -> Builtin_types.float_ty ()
    
    | E_assignment (e1, e2) ->
      check_expr ctx e1;
      check_expr ctx e2;
      if is_lvalue ctx e1 then
        if type_of e2 >> type_of e1 then
          T_void
        else
          begin
            error (xf "type clash in assignment %s <- %s" 
                      (string_of_type (type_of e1))
                      (string_of_type (type_of e2)));
            T_void
          end
      else
        begin
          error (xf "assignment to read-only location");
          T_void
        end
        
    | E_array_access (ar, idxs) ->
      check_expr ctx ar;
      List.iter (check_expr ctx) idxs;
      let it = Builtin_types.int_ty () in
      if List.exists (fun i -> type_of i >> it) idxs then
        error "array index ain't int"
      else ();
      begin
        match expand_type (type_of ar) with
        | T_array (k, t) ->
          if List.length idxs <> k then
            error (xf "%d-dimensional array indexed with %d indices"
                      k (List.length idxs))
          else ();
          t
        | _ ->
          error (xf "index object ain't array (it's %s)" 
                    (string_of_type (type_of ar)));
          free_variable ()
      end
      
    | E_let (v, e) ->
      begin
        match v.val_kind with
        | Val_local l ->
          check_expr ctx l;
          v.val_type <- Some (type_of l)
        | _ -> ice "non-local local value"
      end;
      check_expr ctx e;
      type_of e
      
    | E_fun (fds, e) ->
      List.iter (check_fun ctx) fds;
      check_expr ctx e;
      type_of e
      
    | E_raise e ->
      check_expr ctx e;
      if type_of e >> Builtin_types.exn_ty () then ()
      else 
        error (xf "raised value is not exception, it has type %s"
                  (string_of_type (type_of e)));
      free_variable ()
      
    | E_try_with (e1, v, e2) ->
      check_expr ctx e1;
      begin
        match v.val_type with
        | Some t ->
          if t >> Builtin_types.exn_ty () then ()
          else
            error (xf "catched value is not exception, it has type %s" 
                      (string_of_type t))
        | None -> assert false
      end;
      check_expr ctx e2;
      if (type_of e2) >> (type_of e1) then ()
      else 
        begin
          error "types in `try' and `with' sections are not compatible";
          error (xf "try : %s, with : %s" 
                    (string_of_type (type_of e1))
                    (string_of_type (type_of e2)))
        end;
      type_of e1
    | E_try_finally (e1, e2) ->
      check_expr ctx e1;
      check_expr ctx e2;
      if type_of e2 >> T_void then ()
      else
        error (xf "`finally' expression should have void type, not %s" 
                  (string_of_type (type_of e2)));
      type_of e1
    | E_this ->
      begin
        match ctx.c_this_pointer_type with
        | Some t -> t
        | None -> 
          error "this pointer used outside method";
          free_variable ()
      end
    | E_base ->
      (* FIXME: ??? *)
      error "`base' only allowed to call base constructor";
      free_variable ()
    | E_type_conversion (e, t) ->
      (* FIXME: this can be function call? *)
      (* For now don't check it at all, treat as downcast. *)
      check_expr ctx e;
      t
    | E_type_enforcement (e, t) ->
      check_expr ctx e;
      if type_of e >> t then ()
      else
        error (xf "value was enforced to be %s, while it is %s" 
                  (string_of_type t) (string_of_type (type_of e)));
      t
      
    | E_field_ref (obj, fld) ->
      begin
        check_expr ctx obj;
        match lookup_member ctx (type_of obj) fld.b_name with
        | M_methods _ ->
          error (xf "%s was expected to be field, not method" fld.b_name);
          free_variable ()
        | M_field f ->
          fld.b_value <- B_bound f;
          let td = Ty_info.field_parent f in
          let helper_type = T_prod [generic_td_type td; f.fld_type] in
          match fresh_vars td.td_typarms.tp_tyvars helper_type with
          | T_prod [t; ft] -> 
            if type_of obj >> t then ft
            else begin
              error (xf "expected %s :> %s" (string_of_type (type_of obj))
              (string_of_type t));
              assert false
            end
          | _ -> assert false
      end

    | E_method_call (obj, meth, parms) ->
      List.iter (fun p -> check_expr ctx p.parm_expr) parms;
      let make_callable i =
        let (tyvars, obj_ty) =
          let td = Ty_info.method_parent i in
          (td.td_typarms.tp_tyvars, generic_td_type td)
        in
        match fresh_vars tyvars (T_prod [obj_ty; i.ifm_type]) with
        | T_prod [obj_ty; meth_ty] ->
          if (type_of obj) >> obj_ty then ()
          else assert false;
          {
            c_fun_type = meth_ty;
            c_tyvars = i.ifm_typarms.tp_tyvars;
            c_decl = Some (D_iface_method i);
            c_parm_names = parm_names i.ifm_parms;
            c_actuals = parms;
            c_ret_type = None;
            c_subst = None;
          }
        | _ -> assert false
      in
      let callables = List.map make_callable (bindings meth) in
      let c = resolve_overloaded_call ctx callables in
      begin
        match c.c_decl, c.c_ret_type with
        | Some (D_iface_method i), Some t ->
          meth.b_value <- B_bound i; 
          expr.e_raw <- E_method_call (obj, meth, c.c_actuals);
          t
        | _ -> assert false
      end
      
    | E_cons (meth, parms) ->
      List.iter (fun p -> check_expr ctx p.parm_expr) parms;
      let make_callable m =
        let i = m.meth_ifm in
        let td = Ty_info.method_parent i in
        let obj_ty = generic_td_type td in
        match i.ifm_type with
        | T_fun (args, T_void) ->
          {
            c_fun_type = T_fun (args, obj_ty);
            c_tyvars = i.ifm_typarms.tp_tyvars @ td.td_typarms.tp_tyvars;
            c_decl = Some (D_method m);
            c_parm_names = parm_names i.ifm_parms;
            c_actuals = parms;
            c_ret_type = None;
            c_subst = None;
          }
        | _ -> assert false
      in
      let callables = List.map make_callable (bindings meth) in
      let c = resolve_overloaded_call ctx callables in
      begin
        match c.c_decl, c.c_ret_type with
        | Some (D_method m), Some (T_app ({b_value = B_bound td}, args) as t) ->
          meth.b_value <- B_bound m;
          expr.e_raw <- E_cons (meth, c.c_actuals);
          begin
            match Ty_info.get td with
            | {ti_parent_type = Some ({td_raw = T_variant _} as par)} ->
              T_app (bound par.td_name par, args)
            | _ -> t
          end
        | _ -> assert false
      end
    
    | E_ctor_call (false, meth, parms) ->
      List.iter (fun p -> check_expr ctx p.parm_expr) parms;
      let make_callable m =
        let i = m.meth_ifm in
        {
          c_fun_type = i.ifm_type;
          c_tyvars = i.ifm_typarms.tp_tyvars;
          c_decl = Some (D_method m);
          c_parm_names = parm_names i.ifm_parms;
          c_actuals = parms;
          c_ret_type = None;
          c_subst = None;
        }
      in
      begin
        match ctx.c_enclosing_type_decl with
        | None -> assert false
        | Some td ->
          if ctx.c_in_instance_ctor then ()
          else error "this() called outside ctor";
          let ti = Ty_info.get td in
          let callables = List.map make_callable ti.ti_ctors in
          let c = resolve_overloaded_call ctx callables in
          match c.c_decl, c.c_ret_type with
          | Some (D_method m), Some t ->
            meth.b_value <- B_bound m; 
            expr.e_raw <- E_ctor_call (false, meth, c.c_actuals);
            t
          | _ -> assert false
      end
    
    | E_ctor_call (true, meth, parms) ->
      List.iter (fun p -> check_expr ctx p.parm_expr) parms;
      let make_callable subst m =
        let i = m.meth_ifm in
        {
          c_fun_type = i.ifm_type // subst;
          c_tyvars = i.ifm_typarms.tp_tyvars;
          c_decl = Some (D_method m);
          c_parm_names = parm_names i.ifm_parms;
          c_actuals = parms;
          c_ret_type = None;
          c_subst = None;
        }
      in
      begin
        match ctx.c_enclosing_type_decl with
        | None -> assert false
        | Some td ->
          if ctx.c_in_instance_ctor then ()
          else error "base() called outside ctor";
          begin
            match td.td_extends with
            | None ->
              error (xf "`%s' doesn't extend anything -- cannot call base()"
                        (full_tydecl_name td))
            | Some t ->
              match expand_type t with
              | T_app (tb, tp) ->
                let td' = binding tb in
                let ti = Ty_info.get td in
                let subst = make_subst td' tp in
                let callables = List.map (make_callable subst) ti.ti_ctors in
                let c = resolve_overloaded_call ctx callables in
                begin
                  match c.c_decl, c.c_ret_type with
                  | Some (D_method m), Some (T_void) ->
                    meth.b_value <- B_bound m;
                    expr.e_raw <- E_ctor_call (true, meth, c.c_actuals)
                  | _ -> assert false
                end
              | _ -> assert false
          end;
          T_void
      end
      
    | E_sequence [] ->
      expr.e_raw <- E_literal L_void;
      get_type ()

    | E_sequence exprs ->
      let rec check_void = function
        | [e] -> 
          check_expr ctx e;
          type_of e
        | x :: xs ->
          check_expr ctx x;
          if type_of x >> T_void then ()
          else
            error ~loc:x.e_loc 
                  (xf "expressions in middle of sequence need to have void \
                       type, not %s" (string_of_type (type_of x)));
          check_void xs
        | [] -> assert false
      in check_void exprs
    
    | E_tuple exprs ->
      List.iter (check_expr ctx) exprs;
      T_prod (List.map type_of exprs)
    
    | E_tymatch (e, cases) ->
      check_expr ctx e;
      let (v, orig_type) =
        match e.e_raw with
        | E_ref b ->
          begin
            match binding b with
            | D_value v -> (v, v.val_type)
            | _ -> 
              error "argument of tymatch needs to be value reference";
              raise Recovery
          end
        | _ -> 
          error "argument of tymatch needs to be value reference";
          raise Recovery
      in
      let check_case res_t c =
        v.val_type <- Some c.tc_type;
        check_expr ctx c.tc_body;
        v.val_type <- orig_type;
        match res_t with
        | None -> Some (type_of c.tc_body)
        | Some t ->
          let t' = type_of c.tc_body in
          if t' >> t then ()
          else 
              error ~loc:c.tc_body.e_loc (xf "first tymatch branch had type \
                                              %s, but this branch has type %s, \
                                              which doesn't suptype it"
                                             (string_of_type t) 
                                             (string_of_type t'));
          res_t
      in
      begin
        match List.fold_left check_case None cases with
        | Some t -> t
        | None -> assert false
      end
      
    | E_match (matched_value, cases) ->
      check_expr ctx matched_value;

      let check_case state c =
        (* FIXME: warn about unused/non-exhaustive patterns. *)
        locate c.mc_body.e_loc begin fun () ->
          let variant_parent td =
            match (Ty_info.get td).ti_parent_type with
            | Some ({td_raw = T_variant _} as par) -> par
            | _ -> 
              error (xf "`%s' isn't variant case" (Ty_info.td_qname td));
              raise Recovery
          in

          let rec check_pat expected_type = function
            | P_cons (b, pat) ->
              let td = binding b in
              let par = variant_parent td in
              let ft = fresh_vars td.td_typarms.tp_tyvars (generic_td_type td) in
              if ft >> expected_type then ()
              else
                error (xf "this pattren should match %s, not %s"
                          (string_of_type expected_type)
                          (string_of_type (generic_td_type par)));
              let pat =
                match pat, td.td_raw with
                | (P_tuple pats, T_class decls)
                | (P_tuple pats, T_struct decls) ->
                  let rec fields acc = function
                    | D_field f :: xs -> fields (bound f.fld_name f :: acc) xs
                    | _ :: xs -> fields acc xs
                    | [] -> List.rev acc
                  in
                  let flds = fields [] decls in
                  if List.length flds != List.length pats then
                    error (xf "pattern matches %d values, while type %s had %d \
                               fields" (List.length pats) (Ty_info.td_qname td)
                               (List.length flds))
                  else ();
                  let rec combine acc = function
                    | (x :: xs, y :: ys) -> combine ((x, y) :: acc) (xs, ys)
                    | _ -> List.rev acc
                  in
                  P_record (combine [] (flds, pats))
                | _ -> pat
              in
              P_cons (b, check_pat ft pat)
            | P_underscore -> P_underscore
            | P_variable v -> 
              v.val_type <- Some expected_type;
              P_variable v
            | P_tuple pats ->
              let types =
                match expected_type with
                | T_prod l when List.length l = List.length pats -> l
                | _ ->
                  match expand_type expected_type with
                  | T_prod l when List.length l = List.length pats -> l
                  | _ ->
                    error (xf "this pattren matches values of %d-tuple type, not %s"
                              (List.length pats)
                              (string_of_type expected_type));
                    raise Recovery
              in P_tuple (List.map2 check_pat types pats)
            | P_record rs ->
              let check_eq (fld, pat) =
                match lookup_member ctx expected_type fld.b_name with
                | M_methods _ ->
                  error (xf "%s was expected to be field, not method" fld.b_name);
                  raise Recovery
                | M_field f ->
                  fld.b_value <- B_bound f;
                  let td = Ty_info.field_parent f in
                  let helper_type = T_prod [generic_td_type td; f.fld_type] in
                  let ft =
                    match fresh_vars td.td_typarms.tp_tyvars helper_type with
                    | T_prod [t; ft] -> 
                      if expected_type >> t then ft
                      else assert false
                    | _ -> assert false
                  in
                  (fld, check_pat ft pat)
              in P_record (List.map check_eq rs)
          in 
          c.mc_pattern <- check_pat (type_of matched_value) c.mc_pattern;

          check_expr ctx c.mc_body;
          
          let t = type_of c.mc_body in
          match state with
          | Some t' ->
             if t >> t' then ()
             else 
               error ~loc:c.mc_body.e_loc 
                     (xf "first tymatch branch had type \
                          %s, but this branch has type %s, \
                          which doesn't suptype it"
                          (string_of_type t') 
                          (string_of_type t));
             state
          | None -> Some t
        end
      in

      begin
        match List.fold_left check_case None cases with
        | Some t -> t
        | None -> assert false
      end
  in
  
  match expr.e_type with
  | Some _ -> ()
  | None ->
    let t = locate expr.e_loc get_type in
    expr.e_type <- Some t
  
and check_fun ctx f =
  match f.fun_body with
  | Fb_expr e ->
    check_expr ctx e;
    let (_, ret_type) = real_function_type f.fun_type in
    if type_of e >> ret_type then
      ()
    else
      error (xf "function return type was declared to be %s, but is %s"
                (string_of_type ret_type)
                (string_of_type (type_of e)))
  | Fb_extern _ -> 
    ()

let validate decls =
  let type_f ctx td =
    {
      ctx with
        c_enclosing_type_decl = Some td;
        c_enclosing_type = generic_td_type td;
    }
  in
  
  let plain_f ctx decl =
    match decl with
    | D_function f ->
      check_fun ctx f
    | D_method m ->
      let ctx' = {ctx with c_this_pointer_type = Some ctx.c_enclosing_type;
                           c_in_instance_ctor = m.meth_kind = Method_ctor} in
      let f = m.meth_fun in
      check_fun ctx' f;
      let rec check_impl ok = function
        | i :: is ->
          begin
            match i.im_type with
            | T_app (b, a) ->
              let subst = make_subst (binding b) a in
              let ifm_ty = i.im_method.ifm_type // subst in
              if types_eq f.fun_type ifm_ty then
                match ok with
                | Some i' ->
                  error "ambiguity in implements list, between:";
                  error ~loc:i.im_method.ifm_loc "this";
                  error ~loc:i'.im_method.ifm_loc "and this";
                  check_impl ok is
                | None ->
                  check_impl (Some i) is
              else
                begin
                  match is, ok with
                  | [], None ->
                    error (xf "interface method `%s' has type %s, \
                                  while method has type %s"
                            i.im_method.ifm_name (string_of_type ifm_ty)
                            (string_of_type f.fun_type));
                    i
                  | _ -> check_impl ok is
                end
             | _ -> assert false
           end
        | [] -> 
          match ok with
          | Some r -> r
          | None -> assert false
      in 
      let set_impl m =
        let lst = Ty_info.lookup_implemented_method
                      (unsome ctx.c_enclosing_type_decl) 
                      m.b_name 
        in
        if lst <> [] then
          let i = check_impl None lst in
            if i.im_is_implemented then
              error (xf "interface method `%s' was already implemented"
                        i.im_method.ifm_name)
            else
              i.im_is_implemented <- true;
            m.b_value <- B_bound i.im_method
        else
          ()
      in List.iter set_impl m.meth_implements

    | D_field _ | D_value _ | D_iface_method _ ->
      ()
    | D_enum_entry {ee_value = Some e} -> 
      check_expr ctx e;
      (* FIXME: check if e subtypes enum basetype, maybe set it *)
    | D_enum_entry {ee_value = None} -> ()
    | D_type _ -> assert false
  in
  let bogus_ctx =
    {
      c_enclosing_type = T_void;
      c_enclosing_type_decl = None;
      c_this_pointer_type = None;
      c_in_instance_ctor = false;
    }
  in
  decl_walk type_f plain_f bogus_ctx decls
