(*
 * Copyright (c) 2003 The University of Wroclaw.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *    3. The name of the University may not be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 * NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

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
