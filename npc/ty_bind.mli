open Ast

(* This inserts all symbols into global environment, not only types. *)
val scan_globals : decl list -> unit
val bind_types : decl list -> unit
