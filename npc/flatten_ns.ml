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

let env = Env.global

let rec flatten_one pref d = 
  locate d.nsd_loc begin fun () ->
    match d.nsd_raw with
    | ND_namespace (s, decls) ->
      flatten_list (pref ^ Env.expand_ns env s ^ ".") decls
    | ND_open s ->
      error "open allowed only at the beginning of compilation unit"; 
      Env.add_open_prefix env s;
      []
    | ND_namespace_alias (x, y) ->
      error "namespace alias allowed only at the beginning of compilation unit";
      Env.add_namespace_alias env x y;
      []
    | ND_decl (D_type td) -> 
      [D_type {td with td_name = pref ^ td.td_name}]
    | ND_decl _ -> assert false
  end

and flatten_list pref lst =
  let rec loop acc = function
    | x :: xs ->
      loop (flatten_one pref x @ acc) xs
    | [] -> acc
  in loop [] lst

let is_open_or_ns_alias d =
  locate d.nsd_loc begin fun () ->
    match d.nsd_raw with
    | ND_open s -> Env.add_open_prefix env s; true
    | ND_namespace_alias (x, y) -> Env.add_namespace_alias env x y; true
    | ND_decl _
    | ND_namespace (_, _) -> false
  end

let flatten lst =
  let rec loop = function
    | x :: xs as l ->
      if is_open_or_ns_alias x then
        loop xs
      else
        List.rev (flatten_list "" l)
    | [] -> []
  in loop lst
