open Ast

type t

val create : function_decl -> t
val push_all : unit -> unit
val add_val : t option -> value_decl -> unit
val add_fun_val : t option -> function_decl -> unit
val add_this_pointer : t option -> type_decl -> unit
