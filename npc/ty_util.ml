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
 * NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Ast
open Util

exception Error

let rec iter_type f ctx t =
  match t with
  | T_ref t' | T_out t' | T_array (_, t') ->
    iter_type f (f ctx t) t'
  | T_fun (t1, t2) ->
    let ctx = f ctx t in
    iter_type f ctx t1;
    iter_type f ctx t2
  | T_prod l
  | T_app (_, l) ->
    List.iter (iter_type f (f ctx t)) l
  | T_var _ | T_void -> 
    ignore (f ctx t)
    
let full_tydecl_name td = 
  (* FIXME: namespace should be given. *)
  td.td_name
  
let real_function_type = function
  | T_fun (T_void, r) -> ([], r)
  | T_fun (T_prod l, r) -> (l, r)
  | T_fun (t, r) -> ([t], r)
  | _ -> ice "non-function type passed to real_function_type"

let td_parents td =
  match td.td_extends with
  | Some t -> t :: td.td_implements
  | None -> td.td_implements


(* This function defines *some* ordering over types, it's used to
   sort them. 0 is returned only for types that are actually equal. 
   It should be used on expanded types. *)
let rec type_cmp t1 t2 =
  let rec lexi = function
    | x :: xs, y :: ys ->
      let c = type_cmp x y in
      if c == 0 then lexi (xs, ys)
      else c
    | [], [] -> 0
    | _ -> 1
  in
  match t1, t2 with
  | T_var t1, T_var t2 -> (binding t1).tv_id - (binding t2).tv_id
  | T_var _, _ -> 1
  | T_app (n1, l1), T_app (n2, l2) ->
    let d = (binding n1).td_id - (binding n2).td_id in
    if d == 0 then lexi (l1, l2)
    else d
  | T_app (_, _), _ -> 1
  | T_fun (t1', t1''), T_fun (t2', t2'') -> lexi ([t1'; t1''], [t2'; t2''])
  | T_fun (_, _), _ -> 1
  | T_prod l1, T_prod l2 -> lexi (l1, l2)
  | T_prod _, _ -> 1
  | T_ref t1', T_ref t2'
  | T_out t1', T_out t2' -> type_cmp t1' t2'
  | T_ref _, _
  | T_out _, _ -> 1
  | T_array (n1, t1), T_array (n2, t2) ->
    if n1 == n2 then type_cmp t1 t2
    else n1 - n2
  | T_array _, _ -> 1
  | T_void, T_void -> 0
  | T_void, _ -> 1


let make_subst ?(subst = Ty_env.empty) td args =
    List.fold_left2 Ty_env.add subst td.td_typarms.tp_tyvars args

let expand_type ?(subst = Ty_env.empty) t =
  let rec expand subst t = 
    let self = expand subst in
    let lself = List.map self in
    let t = Ty_env.expand subst t in
    match t with
    | T_app (n, a) ->
      let a' = List.map (expand subst) a in
      let td = binding n in
      begin
        match td.td_raw with
        | T_alias t ->
          expand (make_subst ~subst:subst td a') t
        | _ -> T_app (n, a')
      end
    | T_var _ -> t
    | T_fun (t1, t2) -> T_fun (self t1, self t2)
    | T_prod tl -> T_prod (lself tl)
    | T_ref t1 -> T_ref (self t1)
    | T_out t1 -> T_out (self t1)
    | T_array (n, t1) -> T_array (n, self t1)
    | T_void -> T_void
  in expand subst (Ty_env.expand !Ty_env.global t)
      
let ( // ) t sub =
  (* FIXME: do not expand here. *)
  expand_type ~subst:sub t
  
let types_eq t1 t2 =
  let t1 = expand_type t1 in
  let t2 = expand_type t2 in
  type_cmp t1 t2 == 0

let string_of_type t =
  let cnt = ref 0 in
  let used_tyvars = Hashtbl.create 16 in
  let rec f t = 
    incr cnt;
    if !cnt > 200 then "..." else
    match t with
    | T_app (b, []) ->
      full_tydecl_name (binding b)
    | T_app (b, l) ->
      let l' = String.concat ", " (List.map f l) in
      xf "%s (%s)" (full_tydecl_name (binding b)) l'
    | T_var tv ->
      let tv = binding tv in
      let v = xf "'%s{%d}" tv.tv_name tv.tv_id in
      if Hashtbl.mem used_tyvars tv.tv_id then v
      else
        let _ = Hashtbl.add used_tyvars tv.tv_id () in
        let v = if tv.tv_is_free then v ^ "*" else v in
        let c =
          match tv.tv_constraints with
          | [] -> ""
          | l -> " [:" ^ String.concat " " (List.map f l) ^ ":]"
        in v ^ c
      
    | T_fun (t1, t2) ->
      xf "(%s -> %s)" (f t1) (f t2)
    | T_prod l ->
      xf "(%s)" (String.concat " * " (List.map f l))
    | T_ref t ->
      xf "ref %s" (f t)
    | T_out t ->
      xf "out %s" (f t)
    | T_array (0, t) -> f t
    | T_array (n, t) -> 
      f (T_array (n - 1, t)) ^ "[]"
    | T_void -> "void"
  in f (Ty_env.expand !Ty_env.global t)

let unify ?(subst = Ty_env.empty) t1 t2 =
  let rec unify subst (t1, t2) =
    let rec unify_lists subst = function
      | x :: xs, y :: ys ->
        unify_lists (unify subst (x, y)) (xs, ys)
      | [], [] -> subst
      | _ -> raise Error
    in
    let no tv = (binding tv).tv_id in
    debug (xf "uu: %s %s" (string_of_type t1) (string_of_type t2));
    match (Ty_env.expand subst t1, Ty_env.expand subst t2) with
    | T_var v1, T_var v2 when no v1 == no v2 ->
      subst
    | T_var v1, t2 when (binding v1).tv_is_free ->
      debug (xf "addtv: %d -> %s" (no v1) (string_of_type t2));
      Ty_env.add subst (binding v1) t2
    | t1, T_var v2 when (binding v2).tv_is_free ->
      debug (xf "addtv(r): %d -> %s" (no v2) (string_of_type t1));
      Ty_env.add subst (binding v2) t1
    | T_app (td1, tp1), T_app (td2, tp2) 
         when (binding td1).td_id == (binding td2).td_id ->
      unify_lists subst (tp1, tp2)
    | T_fun (a, b), T_fun (c, d) ->
      unify_lists subst ([a; b], [c; d])
    | T_prod l1, T_prod l2 ->
      unify_lists subst (l1, l2)
    | T_ref t1, T_ref t2 
    | T_out t1, T_out t2 -> 
      unify subst (t1, t2)
    | T_array (n1, t1), T_array (n2, t2) when n1 == n2 ->
      unify subst (t1, t2)
    | T_void, T_void -> 
      subst
    | _ -> raise Error
  in
  try
    debug (xf "[u: %s %s" (string_of_type t1) (string_of_type t2));
    let r = Some (unify subst (t1, t2)) in
    debug (xf "ok]");
    r
  with Error -> 
    debug (xf "err]");
    None
    
let rec x_exists f ini = function
  | [] -> None
  | x :: xs ->
    match f ini x with
    | Some _ as r -> r
    | None -> x_exists f ini xs
    
let rec x_for_all f ini = function
  | [] -> Some ini
  | x :: xs ->
    match f ini x with
    | Some s -> x_for_all f s xs
    | None -> None

let rec x_for_all2 f ini = function
  | x :: xs, y :: ys ->
    begin
      match f ini x y with
      | Some s -> x_for_all2 f s (xs, ys)
      | None -> None
    end
  | [], [] -> Some ini
  | _ -> assert false
  
let no tv = (binding tv).tv_id

(* Check if t1 :> t2 *)
let sub_unify ?(subst = Ty_env.empty) t1 t2 =
  let rec sub subst t1 t2 =
    match unify ~subst:subst t1 t2 with
    | Some _ as r -> r
    | None ->
      let (t1, t2) = (Ty_env.expand subst t1, Ty_env.expand subst t2) in
      match Ty_env.expand subst t1, Ty_env.expand subst t2 with
      | T_app (c1', l1), T_app (c2', l2) ->
        let c1 = binding c1' in
        let c2 = binding c2' in
        if c1.td_id == c2.td_id then
          unify ~subst:subst t1 t2
        else
          begin
            match Ty_info.how_subtypes (Ty_info.get c1) c2 with
            | Some args ->
              let subst_f = expand_type ~subst:(make_subst c1 l1) in
              let rec filter_out (acc1, acc2 as acc) = function
                | (Some x :: xs, y :: ys) -> 
                  filter_out (subst_f x :: acc1, y :: acc2) (xs, ys)
                | (None :: xs, _ :: ys) ->
                  filter_out acc (xs, ys)
                | ([], []) -> acc
                | _ -> assert false
              in
              let (lst1, lst2) = filter_out ([], []) (args, l2) in
              unify ~subst:subst (T_prod lst1) (T_prod lst2)
            | None -> None
          end
      | T_var tv, _ ->
        x_exists (fun s t -> sub s t t2) subst (binding tv).tv_constraints
      | _, T_var _ ->
        (* We cannot be ever sure if type variable is subtyped by some
           other type, except for 'a :> 'a handled above. *)
        None
      | T_fun (a1, r1), T_fun (a2, r2) ->
        x_for_all2 sub subst ([a2; r1], [a1; r2])
      | T_prod l1, T_prod l2 ->
        if List.length l1 <> List.length l2 then None
        else
          x_for_all2 sub subst (l1, l2)
      | T_ref t1, T_ref t2
      | T_out t1, T_out t2 -> 
        sub subst t1 t2
      | T_array (_, _), T_array (_, _) ->
        (* For arrays only equivalence gives subtyping. *)
        None
      | _, _ -> None
  in
  sub subst (expand_type t1) (expand_type t2)

let ( >> ) t1 t2 =
  match sub_unify t1 t2 with
  | Some s ->
    Ty_env.join_with_global s; true
  | None -> false

let fresh_vars tvs ft =
  let add_tv subst tv =
    let tv' = {tv with tv_id = next_id (); tv_is_free = true} in
    let b = 
      {
        b_value = B_bound tv';
        b_name = tv.tv_name;
        b_namespace = "";
      }
    in
    Ty_env.add subst tv (T_var b)
  in
  let subst = List.fold_left add_tv Ty_env.empty tvs in
  expand_type ~subst:subst ft

let fresh_var () =
  let id = next_id () in
  let tv =
    {
      tv_name = xf "_fv%d" id;
      tv_id = id;
      tv_constraints = [];
      tv_is_free = true;
    }
  in
  let b =
      {
        b_value = B_bound tv;
        b_name = tv.tv_name;
        b_namespace = "";
      }
  in T_var b
