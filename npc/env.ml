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

type t =
  {
    mutable e_decls : decl list Smap.t;
    mutable e_enum_parents : type_decl Imap.t;
    mutable e_namespace_aliases : string Smap.t;
    mutable e_open_namespaces : string list;
  }

let create () =
  { 
    e_decls = Smap.empty;
    e_namespace_aliases = Smap.empty;
    e_enum_parents = Imap.empty;
    e_open_namespaces = [""];
  }

let copy r =
  {r with e_decls = r.e_decls}

let expand_name e s =
  if String.contains s '.' then
    let pos = String.index s '.' in
    let first = String.sub s 0 pos in
    if Smap.mem first e.e_namespace_aliases then
      Smap.find first e.e_namespace_aliases ^ 
        String.sub s pos (String.length s - pos)
    else
      s
  else
    s

let expand_ns e s =
  let s' = expand_name e s in
  if s = s' then
    if Smap.mem s e.e_namespace_aliases then
      Smap.find s e.e_namespace_aliases
    else
      s
  else
    s'
  
let lookup e s =
  let s' = expand_name e s in
  if s' = s then
    let rec loop acc = function
      | x :: xs ->
        let prefixed = x ^ s in
        if Smap.mem prefixed e.e_decls then
          loop (Smap.find prefixed e.e_decls @ acc) xs
        else
          loop acc xs
      | [] -> acc
    in
    match loop [] e.e_open_namespaces with
    | [] ->
      error (xf "unbound symbol `%s'" s);
      raise Recovery
    | r -> r
  else
    try
      Smap.find s' e.e_decls
    with Not_found ->
      error (xf "unbound symbol `%s' (expanded to `%s')" s s');
      raise Recovery

let add c pref decl =
  let (name, loc) = decl_name_loc decl in
  let name = pref ^ name in
  let others =
    if Smap.mem name c.e_decls then
      match (decl, Smap.find name c.e_decls) with
      | (D_method _, (D_method _ :: _ as l)) 
      | (D_iface_method _, (D_iface_method _ :: _ as l)) 
      | (D_function _, (D_function _ :: _ as l)) -> 
        l (* OK, overloading *)
      | (_, d' :: _) -> 
        error ~loc:loc (xf "redefinition of symbol `%s'" name);
        let (_, l') = decl_name_loc d' in
        error ~loc:l' "first defined here";
        []
      | (_, []) -> assert false
    else
      []
  in
  c.e_decls <- Smap.add name (decl :: others) c.e_decls

let global = create ()

let add_open_prefix e s =
  let s = s ^ "." in
  if List.mem s e.e_open_namespaces then 
    ()
  else
    e.e_open_namespaces <- s :: e.e_open_namespaces

let add_namespace_alias e short long =
  if Smap.mem short e.e_namespace_aliases then
    error (xf "redefinition name namespace alias `%s'" short)
  else
    ();
  e.e_namespace_aliases <- 
    Smap.add short (expand_ns e long) e.e_namespace_aliases

let add_enum_parent e ee td =
  assert (not (Imap.mem ee.ee_id e.e_enum_parents));
  e.e_enum_parents <- Imap.add ee.ee_id td e.e_enum_parents

let lookup_enum_parent e ee =
  Imap.find ee.ee_id e.e_enum_parents

let reset () =
  global.e_open_namespaces <- [""];
  global.e_namespace_aliases <- Smap.empty

type state = string list * string Smap.t

let load (o, a) =
  global.e_open_namespaces <- o;
  global.e_namespace_aliases <- a

let save () =
  (global.e_open_namespaces, global.e_namespace_aliases)
