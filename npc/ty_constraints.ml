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

let check_type t =
  let rec check () t =
    match t with
    | T_app (b, l) ->
      let td = binding b in
      let subst = make_subst td l in
      let subst_f = expand_type ~subst:subst in
      let check_tv tv =
        match Ty_env.find subst tv with
        | None -> assert false (* FIXME: ??? *)
        | Some t ->
          let check_constraint t' =
            if t >> (subst_f t') then
              ()
            else
              begin
                error (xf "typing constraint '%s :> %s is not satisfied"
                          tv.tv_name (string_of_type t'));
                error (xf "by %s :> %s"
                          (string_of_type t) (string_of_type (subst_f t')));
                error ~loc:td.td_loc (xf "upon instantiation of %s"
                                         (full_tydecl_name td));
              end
          in
          List.iter check_constraint tv.tv_constraints
      in
      List.iter check_tv td.td_typarms.tp_tyvars
    | _ -> ()
  in
  iter_type check () t


let validate decls =
  let check_val v =
    match v.val_type with
    | Some t -> check_type t
    | None -> assert false
  in
  
  let check_fun f =
    check_type f.fun_type;
    List.iter check_val f.fun_parms
  in
  
  let rec check_expr expr =
    let f expr =
      match expr.e_raw with
      | E_fun (f, _) ->
        List.iter check_fun f
      | E_tymatch (_, tcs) ->
        let check_tc tc =
          let check_c =
            function Tc_subtype (_, t) 
                   | Tc_type_eq (_, t) -> check_type t
          in
          List.iter check_c tc.tc_constraints;
          check_type tc.tc_type
        in
        List.iter check_tc tcs;
      | E_let (v, _) ->
        assert (v.val_type = None)
      | E_try_with (_, v, _) ->
        check_val v
      | E_type_conversion (_, t)
      | E_type_enforcement (_, t) ->
        check_type t
      | _ -> ()
    in walk_expr f expr
  in

  let check_typarms tp =
    List.iter (fun (_, t) -> check_type t) tp.tp_constraints
  in
    
  let type_f () td =
    check_typarms td.td_typarms;
    List.iter check_type (td_parents td);
    begin
      match td.td_raw with
      | T_alias t -> check_type t
      | _ -> ()
    end;
    ()
  in
  let plain_f () decl =
    match decl with
    | D_function f | D_method {meth_fun = f} ->
      check_fun f;
      begin
        match f.fun_body with
        | Fb_expr e ->
          check_expr e
        | Fb_extern _ ->
          ()
      end
    | D_field f ->
      check_type f.fld_type
    | D_value v ->
      check_val v
    | D_enum_entry {ee_value = Some e} -> 
      check_expr e
    | D_enum_entry {ee_value = None} -> ()
    | D_iface_method i ->
      check_type i.ifm_type
    | D_type _ -> assert false
  in
  decl_walk type_f plain_f () decls


