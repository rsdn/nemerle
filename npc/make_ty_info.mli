open Ast

val attach_ty_info : decl list -> unit
val set_variant_info : decl list -> unit
val normalize_reverse_subtyping : decl list -> unit
val determine_subtyping : decl list -> unit
