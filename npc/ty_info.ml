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
open Sem_types
open Util

let type_descriptors = Hashtbl.create 1024
let parents = Hashtbl.create 1024
let functions = Hashtbl.create 1024

let add_function td f =
  (* FIXME: should set parent function of function here, and later
     complete sets of needed closures up the function tree *)
  assert (not (Hashtbl.mem functions f.fun_id));
  Hashtbl.add functions f.fun_id f;
  assert (not (Hashtbl.mem parents f.fun_id));
  Hashtbl.add parents f.fun_id td

let function_parent f =
  assert (Hashtbl.mem parents f.fun_id);
  Hashtbl.find parents f.fun_id

let lookup_function id =
  assert (id <> 0);
  assert (Hashtbl.mem functions id);
  Hashtbl.find functions id
  
let set_field_parent fld td =
  assert (not (Hashtbl.mem parents fld.fld_id));
  Hashtbl.add parents fld.fld_id td

let set_method_parent ifm td =
  assert (not (Hashtbl.mem parents ifm.ifm_id));
  Hashtbl.add parents ifm.ifm_id td

let set_value_parent v td =
  assert (not (Hashtbl.mem parents v.val_id));
  Hashtbl.add parents v.val_id td

let value_parent v =
  assert (Hashtbl.mem parents v.val_id);
  Hashtbl.find parents v.val_id

let field_parent fld =
  assert (Hashtbl.mem parents fld.fld_id);
  Hashtbl.find parents fld.fld_id
   
let method_parent ifm =
  assert (Hashtbl.mem parents ifm.ifm_id);
  Hashtbl.find parents ifm.ifm_id
   
let get t =
  Hashtbl.find type_descriptors t.td_id

let td_qname td =
  (get td).ti_full_name
  
let add p t =
  assert (not (Hashtbl.mem type_descriptors t.td_id));
  let td = 
    {
      ti_methods = Smap.empty;
      ti_fields = Smap.empty;
      ti_subtypes = Imap.empty;
      ti_implemented_methods = Smap.empty;
      ti_td = t;
      ti_parent_type = p;
      ti_ctors = [];
      ti_full_name = 
        (match p with Some t -> td_qname t ^ "." | None -> "") ^ t.td_name;
      ti_variant_info = Vi_none;
      ti_add_subtypings = [];
    } in
  Hashtbl.add type_descriptors t.td_id td;
  td
  
let add_value t v =
  set_value_parent v t.ti_td

let add_method t m =
  set_method_parent m t.ti_td;
  let map = t.ti_methods in
  t.ti_methods <-
    if Smap.mem m.ifm_name map then
      Smap.add m.ifm_name (m :: Smap.find m.ifm_name map) map
    else
      Smap.add m.ifm_name [m] map
    
let add_field t f =
  set_field_parent f t.ti_td;
  t.ti_fields <- Smap.add f.fld_name f t.ti_fields

let how_subtypes ti td =
  if Imap.mem td.td_id ti.ti_subtypes then
    Some (Imap.find td.td_id ti.ti_subtypes).sr_how
  else
    None

let add_implemented_interface td t =
  let add_method iface s d =
    match d with
    | D_iface_method i ->
      let im =
        {
          im_is_implemented = false;
          im_method = i;
          im_iface = iface;
          im_type = t;
        }
      in
      let old =
        if Smap.mem i.ifm_name s then
          Smap.find i.ifm_name s
        else
          [] 
      in Smap.add i.ifm_name (im :: old) s
    | _ ->
      error "thing in interface ain't method";
      s
  in
  match t with
  | T_app (tdb, _args) ->
    let iface = binding tdb in
    begin
      match iface.td_raw with
      | T_interface methods ->
        let ti = get td in
        ti.ti_implemented_methods <-
          List.fold_left (add_method iface) ti.ti_implemented_methods methods
      | _ ->
        error "implemented thing ain't interface"
    end
  | _ -> error "implemented thing ain't interface"

let lookup_implemented_method td name =
  let ti = get td in
  try
    let l = Smap.find (chop_ns name) ti.ti_implemented_methods in
    let pref = only_ns name in
    let check i = string_ends_with (td_qname i.im_iface) pref in
    match List.filter check l with
    | [] -> raise Not_found
    | l -> l
  with Not_found ->
    error (xf "no such interfece method: `%s'" name);
    []
