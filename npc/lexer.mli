open Ast
open Util

val set_file_name : string -> unit

val get_current_location : unit -> location

val token : Lexing.lexbuf -> Parser.token
