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
open Ty_util

let out_ch = ref stdout

let out s = 
  output_string !out_ch (s ^ "\n")

let set_output_file f =
  out_ch := open_out f

let close_output_file () =
  close_out !out_ch;
  out_ch := stdout

let td_name td =
  match td.td_raw with
  | T_external e -> e
  | _ -> Ty_info.td_qname td
  
let rec ty t =
  match expand_type t with
  | T_app (b, _) when td_name (binding b) = "Nemerle.Core.bool" -> "bool"
  | T_app (b, _) ->
    td_name (binding b)
  | T_var _ -> "object"
  | T_fun (_, _) as t ->
    let arity = List.length (fst (real_function_type t)) in
    xf "Nemerle.Func%d" arity
  | T_prod l ->
    xf "Nemerle.Tuple%d" (List.length l)
  | T_ref _ -> assert false
  | T_out _ -> assert false
  | T_array (_, _) -> ice "FIXME: cg_ty array"
  | T_void -> "void"

let ty_name t =
  match expand_type t with
  | T_app (b, _) -> td_name (binding b)
  | _ -> assert false

let mods m =
  let mod1 = function
    | M_new -> "new"
    | M_public -> "public"
    | M_private -> "private"
    | M_protected -> "protected"
    | M_internal -> "internal"
    | M_abstract -> "abstract"
    | M_sealed -> "sealed"
    | M_volatile -> "volatile"
    | M_extern -> "extern"
    | M_const -> "const"
    | M_attribute _a -> ice "FIXME: mod1, attr"
  in String.concat " " (List.map mod1 m)
  
let begin_ns name =
  if String.contains name '.' then
    let i = String.rindex name '.' in
    out (xf "namespace %s {" (String.sub name 0 i))
  else
    ()

let end_ns name =
  if String.contains name '.' then out "} // end ns \n" else ()

let mangle s =
  let r = String.copy s in
  for i = 0 to String.length s - 1 do
    let ch =
      match s.[i] with
      | 'a' .. 'z' 
      | 'A' .. 'Z' 
      | '0' .. '9' -> s.[i]
      | _ -> '_'
    in
    r.[i] <- ch
  done;
  r
  
let val_name v =
  match v.val_kind with
  | Val_parm when v.val_in_closure ->
    xf "__N__var_%d_%s" v.val_id (mangle v.val_name)
  | Val_parm -> v.val_name
  | Val_global -> 
    begin
      match Ty_info.td_qname (Ty_info.value_parent v) ^ "." ^ v.val_name with
      | "Nemerle.Core.true" -> "true"
      | "Nemerle.Core.false" -> "false"
      | x -> x
    end
  | Val_pattern
  | Val_exn
  | Val_local _ ->
    xf "__N__var_%d_%s" v.val_id (mangle v.val_name)

let proxies = Stack.create ()

let get_proxies () =
  let s = Stack.copy proxies in
  let rec loop acc =
    if Stack.is_empty s then acc
    else loop (Stack.pop s :: acc)
  in loop []

let fun_name f =
  match f.fun_defined_in_fun with
  | 0 ->
    if f.fun_needs_proxy then ()
    else begin
      f.fun_needs_proxy <- true;
      Stack.push f proxies
    end;
    xf "__N__proxy_%d_%s_class.__N__instance" f.fun_id (chop_ns f.fun_name)
  | _ ->
    xf "__N__fun_%d_%s" f.fun_id f.fun_name
    
let proxy_name f =
  match f.fun_defined_in_fun with
  | 0 -> xf "__N__proxy_%d_%s" f.fun_id (chop_ns f.fun_name)
  | _ -> assert false

let out_local_var t n =
  let tt = ty t in
  let ini =
    match tt with
    | "int" -> "0"
    | "bool" -> "false"
    | "float" -> "0.0f"
    | _ -> "null"
  in
  out (xf "%s %s = %s;" tt n ini)
