open Ast

val do_bindings : bool ref

val named_type : string -> ty list -> ty

val string_ty : unit -> ty
val float_ty : unit -> ty
val int_ty : unit -> ty
val bool_ty : unit -> ty
val exn_ty : unit -> ty
val true_ty : unit -> ty
val false_ty : unit -> ty
