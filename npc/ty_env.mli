open Ast

type t

val global : t ref

(* Replace T_var _ with appropriate value. But do just single step. *)
val expand : t -> ty -> ty
val add : t -> tyvar -> ty -> t
val find : t -> tyvar -> ty option
val empty : t
val join : t -> t -> t
val join_with_global : t -> unit
