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
open Ast_util
open Ty_util
open Sem_types

(* Pass 3. *)
let attach_ty_info decls =
  let type_f parent td =
    let ti = Ty_info.add parent td in
    begin
      match td.td_raw with
      | T_class decls | T_struct decls ->
        List.iter (function
                     | D_method {meth_ifm = m} -> Ty_info.add_method ti m
                     | D_field f -> Ty_info.add_field ti f
                     | D_value v -> Ty_info.add_value ti v
                     | _ -> ()) decls;
        ti.ti_ctors <-
          List.fold_left (fun acc -> function
                            | D_method ({meth_kind = Method_ctor} as m) -> m :: acc
                            | _ -> acc) [] decls
      | T_interface ms -> 
        List.iter (function D_iface_method i -> Ty_info.add_method ti i
                          | _ -> assert false) ms
      | T_variant _ | T_enum _ | T_alias _ | T_external _ -> ()
    end;
    Some td
  in
  let plain_f _ _ = () in
  decl_walk type_f plain_f None decls

(* This sets ti_variant_info fields for variant types. It also checks
   if variant contains proper stuff. *)
let set_variant_info decls =

  let make_voi td vo =
    {
      voi_parent = td;
      voi_name = vo.td_name;
      voi_self = vo;
      voi_type = Ast_util.generic_td_type vo;
    }
  in

  let add_option (used_types, used_names) voi =
    if Imap.mem voi.voi_self.td_id used_types then
      error "option already present"
    else ();
    (Imap.add voi.voi_self.td_id voi used_types,
     Smap.add voi.voi_name voi used_names)
  in
  

  let rec add_options_of td acc t =
    match expand_type t with
    | T_app ({b_value = B_bound ({td_raw = T_variant _} as par)}, args) ->
      set_variant_info par;
      let _subst = make_subst par args in (* XXX *)
      begin
        match (Ty_info.get par).ti_variant_info with
        | Vi_options lst ->
          let localize v =
            {
              v with voi_parent = td
            }
          in
          List.fold_left add_option acc (List.map localize lst)
        | _ -> acc
      end;
    | _ ->
      error (xf "variants can extend other variants, not %s"
                (string_of_type t));
      acc

  and set_variant_info td =
    match td.td_raw with
    | T_variant vs ->
      let ti = Ty_info.get td in
      locate td.td_loc begin fun () ->
        match ti.ti_variant_info with
        | Vi_in_progress ->
          error (xf "type definition for `%s' is cyclic" 
                    (full_tydecl_name td))
        | Vi_none ->
            
          let acc = (Imap.empty, Smap.empty) in
          let acc = List.fold_left (add_options_of td) acc td.td_implements in
          let (_, map) = List.fold_left add_option acc (List.map (make_voi td) vs) in
          let opts = Smap.fold (fun _ voi lst -> voi :: lst) map [] in
          ti.ti_variant_info <- Vi_options opts
        | Vi_options _ -> ()
      end
    | _ -> ()
  in

  let type_f () td = set_variant_info td in
  let plain_f _ _ = () in
  decl_walk type_f plain_f () decls

(* Variants hold subtyping information in reverse way, i.e. [variant X 
   extends Y] means Y :> X (whereas [class X extends Y] means X :> Y).
   Also variant X = [A|B] means A :> X and B :> X.
   
   We need to reverse this back, so later passes can close subtyping
   relation WRT to transitivity.  *)
let normalize_reverse_subtyping decls =

  (* Given td <: t, add sr record to type_decl of t. *)
  let add_normalized_subtyping_relation td t =
    match expand_type t with
    | T_app (b, args) ->
      let rec loop map = function
        | (T_var tvb :: xs, tv :: ys) -> 
          let map = Imap.add (binding tvb).tv_id tv map in
          loop map (xs, ys)
        | (t :: xs, _ :: ys) ->
          error (xf "only type variable allowed as argument here, not %s"
                    (string_of_type t));
          loop map (xs, ys)
        | ([], []) -> map
        | (_, _) -> assert false
      in 
      let map = loop Imap.empty (args, (binding b).td_typarms.tp_tyvars) in
      let subst tv =
        if Imap.mem tv.tv_id map then 
          let tv' = Imap.find tv.tv_id map in
          Some (T_var (bound tv'.tv_name tv'))
        else 
          None
      in
      let sr =
        {
          sr_what = td;
          sr_how = List.map subst td.td_typarms.tp_tyvars;
        }
      in
      let ti = Ty_info.get (binding b) in
      ti.ti_add_subtypings <- sr :: ti.ti_add_subtypings
    | _ -> assert false
  in
  
  let type_f () td = 
    match td.td_raw with
    | T_variant cases ->
      let ti = Ty_info.get td in
      begin
        match ti.ti_variant_info with
        | Vi_options _opts ->
          let f = add_normalized_subtyping_relation td in
          List.iter f td.td_implements;
          let do_case td' =
            let subst tv = Some (T_var (bound tv.tv_name tv)) in
            let sr =
              {
                sr_what = td;
                sr_how = List.map subst td'.td_typarms.tp_tyvars;
              }
            in
            let ti = Ty_info.get td' in
            ti.ti_add_subtypings <- sr :: ti.ti_add_subtypings
          in
          List.iter do_case cases
        | _ -> assert false
      end
    | _ -> () 
  in
  let plain_f _ _ = () in
  decl_walk type_f plain_f () decls

let determine_subtyping decls =

  let add_subtyping td subtypes sr errfn =
    if Imap.mem sr.sr_what.td_id subtypes then
      let sr' = Imap.find sr.sr_what.td_id subtypes in
      let types_eq' a b = 
        match (a, b) with 
        | None, None -> true 
        | Some x, Some y -> types_eq x y
        | _ -> false
      in
      if not (List.for_all2 types_eq' sr'.sr_how sr.sr_how) then
         begin
           error ~loc:td.td_loc 
                 (xf "type `%s' is implemented by type `%s' twice \
                      under different instantiations" 
                     (full_tydecl_name sr.sr_what)
                     (full_tydecl_name td));
           errfn ()
         end
      else
        ();
    else
      ();
    Imap.add sr.sr_what.td_id sr subtypes
  in

  let working_on = Hashtbl.create 100 in
  
  let rec get_subtypes td =
    (* FIXME: extends should bring all types and methods to
       type from its parent. *)
    if Hashtbl.mem working_on td.td_id then
      if Hashtbl.find working_on td.td_id then
        begin
          (* FIXME: check for cycles in type aliases. *)
          error ~loc:td.td_loc (xf "type definition for `%s' is cyclic" 
                                   (full_tydecl_name td));
          Imap.empty
        end
      else
        (Ty_info.get td).ti_subtypes
    else
      let parents td =
        let declared =
          match td.td_raw with
          | T_variant _ ->
            begin
              match td.td_extends with
              | Some x -> [x]
              | None -> []
            end
          | _ -> List.rev (td_parents td)
        in
        let make_sr t =
          match expand_type t with
          | T_app (b, args) ->
            {
              sr_what = binding b;
              sr_how = List.map (fun x -> Some x) args;
            }
          | _ ->
            error "cannot implement nor extend non-class type";
            raise Recovery
        in
        List.fold_left (fun acc e -> make_sr e :: acc) 
                       (Ty_info.get td).ti_add_subtypings
                       declared
      in
      
      let parent_is subtypes parent_sr =
        let parent_td = parent_sr.sr_what in
        let subst =
            let opt_add subst tv = function
              | Some t -> Ty_env.add subst tv t
              | None -> subst
            in
            List.fold_left2 opt_add Ty_env.empty 
                            parent_td.td_typarms.tp_tyvars 
                            parent_sr.sr_how in
        let add_sr _ sr subtypes =
          let sr' =
            {
              sr_what = sr.sr_what;
              sr_how = List.map (optionalize (expand_type ~subst:subst)) sr.sr_how;
            }
          in
          add_subtyping td subtypes sr' 
             (fun () -> 
                 error ~loc:parent_td.td_loc 
                       (xf "second one through type `%s'" 
                           (full_tydecl_name parent_td)))
        in
        let subtypes =
          add_subtyping td subtypes parent_sr 
            (* FIXME: if this sr comes from reverse, we are wrong
               in this error message. *)
            (fun () ->
                 error ~loc:td.td_loc "second one directly")
        in
        Imap.fold add_sr (get_subtypes parent_td) subtypes
      in
      Hashtbl.add working_on td.td_id true;
      let subtypes = List.fold_left parent_is Imap.empty (parents td) in
      (Ty_info.get td).ti_subtypes <- subtypes;
      Hashtbl.replace working_on td.td_id false;
      subtypes
  in
  let walk td = 
    ignore (get_subtypes td);
    List.iter (Ty_info.add_implemented_interface td) td.td_implements
  in
  tydecl_walk walk decls
