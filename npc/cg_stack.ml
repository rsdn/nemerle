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
  out (xf "%s %s;" (ty t) s.name);
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
