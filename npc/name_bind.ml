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
 * NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
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

type ctx = 
  {
    c_values : decl Smap.t;
    c_fun_id : int;
    c_this_id : int;
  }
  
let env = Env.global
let empty = { c_values = Smap.empty; c_fun_id = 0; c_this_id = 0; }

let bind_values decls =
  let define_decl ctx n d = 
    begin
      match d with
      | D_value v -> v.val_defined_in_fun <- ctx.c_fun_id
      | D_function f -> f.fun_defined_in_fun <- ctx.c_fun_id
      | _ -> ()
    end;
    {ctx with c_values = Smap.add n d ctx.c_values} 
  in
  let define_value ctx v = define_decl ctx v.val_name (D_value v) in
  
  let rec need_val_from ctx f2_id =
    let f1 = Ty_info.lookup_function ctx.c_fun_id in
    let f2 = Ty_info.lookup_function f2_id in
    if List.memq f2 f1.fun_needed_closures then ()
    else f1.fun_needed_closures <- f2 :: f1.fun_needed_closures;
    f2.fun_has_closure <- true;
    
    if f1.fun_defined_in_fun <> f2_id then
      need_val_from {ctx with c_fun_id = f1.fun_defined_in_fun} f2_id
    else ()
  in
      
  let lookup ctx n =
    if Smap.mem n ctx.c_values then
      let d = Smap.find n ctx.c_values in
      let non_local d = d <> 0 && d <> ctx.c_fun_id in
      begin
        match d with
        | D_value v when non_local v.val_defined_in_fun ->
          v.val_in_closure <- true;
          need_val_from ctx v.val_defined_in_fun
        | D_function f when non_local f.fun_defined_in_fun ->
          f.fun_in_closure <- true;
          need_val_from ctx f.fun_defined_in_fun
        | _ -> ()
      end;
      [d]
    else
      Env.lookup env n
  in
    
  let rec bind_expr ctx expr =
    let f expr =
      match expr.e_raw with
      | E_fun (fs, e) ->
        let define_fun ctx f = define_decl ctx f.fun_name (D_function f) in
        let ctx' = List.fold_left define_fun ctx fs in
        List.iter (bind_function ctx') fs;
        bind_expr ctx' e;
        raise Dont_descend
        
      | E_let (v, e) ->
        let ctx' = define_value ctx v in
        begin
          match v.val_kind with
          | Val_local e -> bind_expr ctx e
          | _ -> assert false
        end;
        bind_expr ctx' e;
        raise Dont_descend
        
      | E_try_with (e1, v, e2) ->
        bind_expr ctx e1;
        let ctx' = define_value ctx v in
        begin
          match v.val_kind with
          | Val_exn -> ()
          | _ -> assert false
        end;
        bind_expr ctx' e2;
        raise Dont_descend
      
      | E_match (e, mcs) ->
        bind_expr ctx e;
        let do_case c =
          let rec bind_pat ctx = function
            | P_cons (_, p) -> bind_pat ctx p
            | P_variable v -> define_value ctx v
            | P_tuple ps -> List.fold_left bind_pat ctx ps
            | P_underscore -> ctx
            | P_record rs -> 
              List.fold_left bind_pat ctx (List.map snd rs)
          in
          let ctx' = bind_pat ctx c.mc_pattern in
          bind_expr ctx' c.mc_body
        in
        List.iter do_case mcs;
        raise Dont_descend
      
      | E_ref b ->
        begin
          match lookup ctx b.b_name with
          | [x] -> b.b_value <- B_bound x
          | l -> b.b_value <- B_multi_bound l
        end
      
      | E_this | E_base ->
        if ctx.c_this_id <> 0 && ctx.c_this_id <> ctx.c_fun_id then
          need_val_from ctx ctx.c_this_id
        else
          () (* error to be catched later *)

      (* We cannot resolve field_decl nor method_decl here, before type
         checking. Fortunetely these are just global lookups with no
         context involved. *)

      | _ -> ()
    in walk_expr f expr
    
  and bind_function ctx f =
    let ctx' = List.fold_left define_value {ctx with c_fun_id = f.fun_id} f.fun_parms in
    match f.fun_body with
    | Fb_expr e ->
      bind_expr ctx' e
    | Fb_extern _ ->
      ()
    
  in

  let type_f () _ = () in
  let plain_f () decl =
    match decl with
    (* FIXME: shouldn't we do sth about meth_implements? *)
    | D_method {meth_fun = f} ->
      bind_function {empty with c_this_id = f.fun_id} f
    | D_function f ->
      bind_function empty f
    | D_iface_method _
    | D_field _
    | D_value _ -> ()
    | D_enum_entry {ee_value = Some e} ->
      bind_expr empty e
    | D_enum_entry _ -> ()
    | D_type _ -> assert false
  in
  decl_walk type_f plain_f () decls

