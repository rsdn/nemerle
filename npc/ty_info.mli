open Ast
open Sem_types

val get : type_decl -> type_info
val add : type_decl option -> type_decl -> type_info

val add_method : type_info -> if_method -> unit
val add_value : type_info -> value_decl -> unit
val add_field : type_info -> field_decl -> unit

val how_subtypes : type_info -> type_decl -> ty option list option

val field_parent : field_decl -> type_decl
val method_parent : if_method -> type_decl
val function_parent : function_decl -> type_decl
val value_parent : value_decl -> type_decl

val td_qname : type_decl -> string

val add_function : type_decl -> function_decl -> unit
val lookup_function : int -> function_decl

val add_implemented_interface : type_decl -> ty -> unit
val lookup_implemented_method : type_decl -> string -> implemented_method list
