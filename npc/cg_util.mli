open Ast

val out : string -> unit
val set_output_file : string -> unit
val close_output_file : unit -> unit
val td_name : type_decl -> string
val ty : ty -> string
val ty_name : ty -> string
val mods : modifier list -> string
val begin_ns : string -> unit
val end_ns : string -> unit

val val_name : value_decl -> string
val fun_name : function_decl -> string
val proxy_name : function_decl -> string

val get_proxies : unit -> function_decl list
