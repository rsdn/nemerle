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
open Cg_util

type t =
  {
    cl_fun : function_decl;
    mutable cl_values : value_decl list;
    mutable cl_funvals : function_decl list;
    mutable cl_this_td : type_decl option;
  }

let closures = Stack.create ()

let create f =
  let c = { cl_fun = f; cl_values = []; cl_funvals = []; cl_this_td = None } in
  Stack.push c closures;
  let cl = xf "__N__closure_class_%d" f.fun_id in
  out (xf "%s __N__closure = new %s();" cl cl);
  c

let iter f =
  Stack.iter f closures
 
let unsome = function
  | Some x -> x
  | None -> ice "closure: got none"
  
let add_val c v =
  if v.val_in_closure then
    let c = unsome c in
    c.cl_values <- v :: c.cl_values
  else
    out_local_var (unsome v.val_type) (val_name v)
    
let add_fun_val c v =
  if v.fun_in_closure then
    let c = unsome c in
    c.cl_funvals <- v :: c.cl_funvals
  else
    out_local_var v.fun_type (fun_name v)

let push c =
  out (xf "class __N__closure_class_%d {" c.cl_fun.fun_id);
  let push_val v =
    out (xf "public %s %s;" (ty (unsome v.val_type)) (val_name v))
  in
  let push_fun f =
    out (xf "public %s %s;" (ty f.fun_type) (fun_name f))
  in
  List.iter push_val c.cl_values;
  List.iter push_fun c.cl_funvals;
  begin
    match c.cl_this_td with
    | Some td ->
      out (xf "public %s __N__this;" (Ty_info.td_qname td))
    | None -> ()
  end;
  out "} // end clo\n"

let push_all () =
  iter push
  
let add_this_pointer c td =
  match c with
  | Some c ->
    c.cl_this_td <- Some td;
    out  "__N__closure.__N__this = this;"
  | None -> ()
