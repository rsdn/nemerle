open Ast

type slot

val create_slot : ty -> slot
val set_slot : slot -> string -> unit
val consume_slots : int -> string list
val pop_slot : unit -> string

(* Checking. *)
val beg_slot : slot -> unit
val end_slot : slot -> unit
