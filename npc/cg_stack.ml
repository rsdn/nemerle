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

type slot = 
  {
    id : int;
    ty : ty;
    name : string;
    mutable assigned : bool;
  }

let slots = Stack.create ()

let create_slot t =
  let id = next_id () in
  let s = { id = id; ty = t; name = xf "__N__stackslot_%d" id; assigned = false } in
  out_local_var t s.name;
  Stack.push s slots;
  s
  
let set_slot s v =
  assert ((Stack.top slots).id = s.id);
  s.assigned <- true;
  out (xf "%s = (%s) %s;" s.name (ty s.ty) v)

let beg_slot s =
  assert ((Stack.top slots).id = s.id);
  s.assigned <- false

let end_slot s =
  assert ((Stack.top slots).id = s.id);
  assert s.assigned

let consume_slots n =
  let rec fetch acc = function
    | 0 -> acc
    | n -> fetch ((Stack.pop slots).name :: acc) (n - 1)
  in fetch [] n

let pop_slot () =
  match consume_slots 1 with
  | [x] -> x
  | _ -> assert false
