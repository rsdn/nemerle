(* *_id : int; fields hold unique integer values.  *)

type 'a binding_option =
  | B_unbound
  | B_bound of 'a
  | B_multi_bound of 'a list

type 'a binding =
  {
    b_name : string;
    mutable b_namespace : string;
    mutable b_value : 'a binding_option;
  }

type location =
  {
    l_file : string;
    l_line : int;
    l_column : int;
  }

(* This is type returned by parser. *)
type ns_decl =
  {
    nsd_loc : location;
    nsd_raw : raw_nsd;
  }

and raw_nsd =
  | ND_namespace of string * ns_decl list
  | ND_open of string
  | ND_namespace_alias of string * string
  | ND_decl of decl

(* It is however flattened to this right away. *)
and decl =
  | D_type of type_decl
  | D_value of value_decl
  | D_function of function_decl
  | D_method of method_decl
  | D_field of field_decl
  | D_enum_entry of enum_entry
  | D_iface_method of if_method

and modifier =
  | M_new
  | M_public
  | M_private
  | M_protected
  | M_internal
  | M_abstract
  | M_sealed
  | M_volatile
  | M_extern
  | M_const
  | M_attribute of attribute

and attribute =
  {
    attr_target : attr_target;
    attr_class : type_decl binding;
    attr_positional_parms : expr list;
    attr_named_parms : (string * expr) list;
  }

and attr_target =
  | Attr_field
  | Attr_event
  | Attr_method
  | Attr_module
  | Attr_param
  | Attr_property
  | Attr_return
  | Attr_type
  | Attr_assembly
  
and type_decl =
  {
    td_loc : location;
    td_typarms : typarms;
    mutable td_modifiers : modifier list;
    td_extends : ty option;
    td_implements : ty list;
    td_name : string;
    td_raw : raw_tydecl;
    td_id : int;
  }

and raw_tydecl =
  | T_interface of decl list
  | T_class of decl list
  | T_struct of decl list
  | T_enum of decl list
  | T_alias of ty
  | T_external of string
  | T_variant of type_decl list

and if_method =
  {
    ifm_type : ty;
    ifm_typarms : typarms;
    ifm_loc : location;
    ifm_name : string;
    ifm_id : int;
    ifm_new_modifier : bool;
    ifm_parms : value_decl list;
  }

and enum_entry =
  {
    ee_loc : location;
    mutable ee_modifiers : modifier list;
    ee_name : string;
    ee_value : expr option;
    ee_id : int;
  }
  
and value_decl =
  {
    val_loc : location;
    mutable val_modifiers : modifier list;
    val_mutable : bool;
    val_name : string;
    mutable val_type : ty option;      (* Inferred for locals. *)
    val_kind : val_kind;
    val_id : int;
    mutable val_in_closure : bool;
    mutable val_defined_in_fun : int;       (* fun_id *)
  }

and val_kind = 
  | Val_global
  | Val_local of expr 
  | Val_exn
  | Val_pattern
  | Val_parm

and function_decl =
  {
    fun_loc : location;
    mutable fun_modifiers : modifier list;
    mutable fun_name : string;
    fun_typarms : typarms;
    fun_id : int;
    fun_parms : value_decl list;
    fun_type : ty;
    fun_body : fun_body;
    fun_kind : fun_kind;
    (* this functional value goes to closure. *)
    mutable fun_in_closure : bool;      
    mutable fun_defined_in_fun : int;
    (* this function needs closure itself. *)
    mutable fun_has_closure : bool;
    (* this function needs closures from following functions. *)
    mutable fun_needed_closures : function_decl list;
    (* this is global function and needs proxy object, as it was used
       as first-class value. *)
    mutable fun_needs_proxy : bool;
  }

and fun_body =
  | Fb_expr of expr
  | Fb_extern of string

and fun_kind =
  | Fun_global
  | Fun_local
  | Fun_static_ctor

and method_decl =
  {
    meth_fun : function_decl;
    meth_ifm: if_method;
    meth_implements : if_method binding list;
    meth_kind : meth_kind;
  }

and meth_kind =
  | Method_normal
  | Method_ctor

and field_decl =
  {
    fld_loc : location;
    mutable fld_modifiers : modifier list;
    fld_mutable : bool;
    fld_type : ty;
    fld_name : string;
    fld_id : int;
  }

and typarms =
  {
    tp_tyvars : tyvar list;
    tp_constraints : (tyvar binding * ty) list;
  }

and tyvar =
  {
    tv_name : string;
    tv_id : int;
    mutable tv_constraints : ty list;
    tv_is_free : bool;
  }

and ty =
  | T_app of type_decl binding * ty list
  | T_var of tyvar binding
  | T_fun of ty * ty
  | T_prod of ty list
  | T_ref of ty
  | T_out of ty
  | T_array of int * ty
  | T_void

and expr =
  {
    e_loc : location;
    mutable e_type : ty option;
    mutable e_raw : raw_expr;
  }

and raw_expr =
  | E_ref of decl binding
  | E_cons of method_decl binding * parameter list
  | E_method_call of expr * if_method binding * parameter list
  | E_ctor_call of bool (* is_base *) * method_decl binding * parameter list
  | E_field_ref of expr * field_decl binding
  | E_fun_call of expr * parameter list
  | E_assignment of expr * expr
  | E_array_access of expr * expr list
  | E_let of value_decl * expr
  | E_fun of function_decl list * expr
  | E_tymatch of expr * tymatch_case list
  | E_match of expr * match_case list
  | E_raise of expr
  | E_try_with of expr * value_decl * expr
  | E_try_finally of expr * expr
  | E_literal of literal
  | E_this
  | E_base
  | E_type_conversion of expr * ty      (* (expr :> ty) *)
  | E_type_enforcement of expr * ty     (* (expr : ty) *)
  | E_sequence of expr list
  | E_tuple of expr list

and literal =
  | L_void
  | L_null
  | L_string of string
  | L_int of int64
  | L_float of float
  
and parameter =
  {
    parm_is_ref : bool;
    parm_expr : expr;
    parm_name : string;         (* "" if no name. *)
  }

and tymatch_case =
  {
    tc_tyvars : tyvar list;
    tc_type : ty;
    tc_constraints : tymatch_constraint list;
    tc_body : expr;
  }

and tymatch_constraint =
  | Tc_subtype of tyvar binding * ty
  | Tc_type_eq of tyvar binding * ty

and pattern =
  | P_cons of type_decl binding * pattern
  | P_record of (field_decl binding * pattern) list
  | P_underscore
  | P_variable of value_decl
  | P_tuple of pattern list
  
and match_case =
  {
    mutable mc_pattern : pattern;
    mc_body : expr;
  }
