open Ast

val decl_name_loc : decl -> string * location
val decl_walk : 
  ('a -> type_decl -> 'a) -> 
  ('a -> decl -> unit) -> 
    'a -> decl list -> unit
val tydecl_walk : (type_decl -> unit) -> decl list -> unit

(* [walk_expr f e] calls f e and then f on all descendant expressions of e. 
   f can raise Dont_descend to prevent that. *)
val walk_expr : (expr -> unit) -> expr -> unit

val copy_binding : 'a binding -> 'a binding
val copy_ty : ty -> ty
val copy_typarms : typarms -> typarms
val generic_td_type : type_decl -> ty
