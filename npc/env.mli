open Ast

type t

val lookup : t -> string -> decl list
val add : t -> string -> decl -> unit
val create : unit -> t
val copy : t -> t
val global : t

val add_open_prefix : t -> string -> unit
val add_namespace_alias : t -> string -> string -> unit

val expand_ns : t -> string -> string

val add_enum_parent : t -> enum_entry -> type_decl -> unit
val lookup_enum_parent : t -> enum_entry -> type_decl

(* reset global env at the begging of new translation unit. *)
val reset : unit -> unit

type state
val load : state -> unit
val save : unit -> state
