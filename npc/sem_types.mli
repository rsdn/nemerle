open Ast
open Util

type subtyping_relation =
  {
    sr_what : type_decl;
    (* None means any type will do. *)
    sr_how : ty option list;
  }

type implemented_method =
  {
    mutable im_is_implemented : bool;
    im_method : if_method;
    im_iface : type_decl;
    im_type : ty;
  }

type variant_option_info =
  {
    voi_type : ty;
    voi_name : string;
    voi_parent : type_decl;
    voi_self : type_decl;
  }
  
type variant_info = 
  | Vi_none
  | Vi_in_progress
  | Vi_options of variant_option_info list
  
type type_info =
  {
    ti_td : type_decl;
    mutable ti_methods : if_method list Smap.t;
    mutable ti_fields : field_decl Smap.t;
    mutable ti_subtypes : subtyping_relation Imap.t;
    mutable ti_ctors : method_decl list;
    mutable ti_implemented_methods : implemented_method list Smap.t;
    ti_parent_type : type_decl option;
    ti_full_name : string;
    
    (* From reverse subtyping relations. *)
    mutable ti_add_subtypings : subtyping_relation list;
    mutable ti_variant_info : variant_info;
  }
