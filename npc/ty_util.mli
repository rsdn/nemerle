open Ast
open Util

val expand_type : ?subst: Ty_env.t -> ty -> ty
val iter_type : ('a -> ty -> 'a) -> 'a -> ty -> unit
val types_eq : ty -> ty -> bool
val full_tydecl_name : type_decl -> string
val real_function_type : ty -> ty list * ty
val td_parents : type_decl -> ty list
val ( >> ) : ty -> ty -> bool
val ( // ) : ty -> Ty_env.t -> ty
val make_subst : ?subst: Ty_env.t -> type_decl -> ty list -> Ty_env.t
val string_of_type : ty -> string
val sub_unify : ?subst: Ty_env.t -> ty -> ty -> Ty_env.t option
val unify : ?subst: Ty_env.t -> ty -> ty -> Ty_env.t option
val fresh_vars : tyvar list -> ty -> ty
val fresh_var : unit -> ty
