open Ast

(* Raised whenever some recovery after issuing error message is required 
   (for example symbol lookup function didn't find symbol, so it prints
   error message and raises this) *)
exception Recovery

exception Dont_descend

(* Error reporting functions will report given location... *)
val push_location : location -> unit
(* ...until pop. *)
val pop_location : unit -> unit

(* Top of location stack. *)
val top_location : unit -> location

(* Run given function inside push/pop_location. *)
val locate : location -> (unit -> 'a) -> 'a

(* Report warning. *)
val warning : ?loc:location -> string -> unit

(* Report error. *)
val error : ?loc:location -> string -> unit

(* Dump debuging info. *)
val debug : ?loc:location -> string -> unit

(* Report Internal Compiler Error and abort. *)
val ice : string -> 'a

(* Return value of binding, iceing in case of problems. *)
val binding : 'a binding -> 'a

(* Handle multibindings, ice when non yet bound. *)
val bindings : 'a binding -> 'a list

(* Return qualified name of given binding. *)
val qname : 'a binding -> string

(* Return type of expression. *)
val type_of : expr -> ty

val list_partial_map : ('a -> 'b option) -> 'a list -> 'b list

(* Printf.sprintf *)
val xf : ('a, unit, string) format -> 'a

(* Printf.eprintf *)
val dbg : ('a, out_channel, unit) format -> 'a

(* Sequential id generator. *)
val next_id : unit -> int

val optionalize : ('a -> 'b) -> 'a option -> 'b option

val bound : string -> 'a -> 'a binding

val debug_level : int ref

val maybe_say_bye : unit -> unit

val unsome : 'a option -> 'a

val chop_ns : string -> string
val only_ns : string -> string

val string_ends_with : string -> string -> bool

val free_variable : unit -> ty

module Smap : Map.S with type key = string
module Sset : Set.S with type elt = string
module Imap : Map.S with type key = int
module Iset : Set.S with type elt = int
