open Ast

exception Recovery
exception Dont_descend

module Int =
  struct
    type t = int
    let compare x y = x - y
  end

module Imap = Map.Make(Int)
module Iset = Set.Make(Int)
module Smap = Map.Make(String)
module Sset = Set.Make(String)

let null_location = { l_file = "-"; l_line = 1; l_column = 1; }
let locations = Stack.create ()
let err_cnt = ref 0

let debug_level = ref 0

let push_location l = 
  Stack.push l locations
  
let pop_location () = 
  if Stack.is_empty locations then 
    begin
      prerr_endline "internal compiler error: pop_location on empty stack";
      exit 1
    end
  else
    ignore (Stack.pop locations)
    
let top_location () = 
  if Stack.is_empty locations then
    null_location
  else
    Stack.top locations

let locate l f =
  push_location l;
  let r = 
    try 
      f () 
    with e -> pop_location (); raise e
  in 
  pop_location (); 
  r

let report l k s =
  Printf.eprintf "%s:%d: %s: %s\n" l.l_file l.l_line k s;
  flush stderr

let get_loc = function Some x -> x | None -> top_location ()

let warning ?loc s = report (get_loc loc) "warning" s

let debug ?loc s = 
  if !debug_level <> 0 then
    report (get_loc loc) "debug" s
  else
    ()

let error ?loc s = incr err_cnt; report (get_loc loc) "error" s

let maybe_say_bye () =
  if !err_cnt > 0 then
    begin
      Printf.eprintf "found %d error(s), bailing out.\n" !err_cnt;
      exit 1
    end
  else
    ()

let ice s = report (top_location ()) "internal compiler error" s; exit 1

let xf = Printf.sprintf

let dbg = Printf.eprintf

let qname {b_name = n; b_namespace = ns} =
  if ns = "" then n else ns ^ "." ^ n

let binding = function
  | {b_value = B_bound x} -> x
  | {b_value = B_unbound} as x -> ice (xf "unbound `%s'" (qname x))
  | {b_value = B_multi_bound _} as x -> ice (xf "multibound `%s'" (qname x))

let bindings = function
  | {b_value = B_bound x} -> [x]
  | {b_value = B_multi_bound xs} -> xs
  | {b_value = B_unbound} as x -> ice (xf "unbound `%s' (multi)" (qname x))

let type_of = function
  | {e_type = Some t} -> t
  | _ -> ice "not yet typechecked"

let rec list_partial_map f = function
  | x :: xs ->
    begin
      match f x with
      | Some y -> y :: list_partial_map f xs
      | None -> list_partial_map f xs
    end
  | [] -> []
  
let next_id =
  let cnt = ref 0 in
  fun () -> incr cnt; !cnt

let unsome = function
  | Some x -> x
  | None -> ice "unsome -- None"

let chop_ns name =
  if String.contains name '.' then
    let i = String.rindex name '.' in
    String.sub name (i + 1) (String.length name - i - 1)
  else
    name
    
let only_ns name =
  if String.contains name '.' then
    String.sub name 0 (String.rindex name '.')
  else
    ""
    
let string_ends_with s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  (l2 <= l1) && String.sub s1 (l1 - l2) l2 = s2

let bound name v =
  {
    b_value = B_bound v;
    b_namespace = "";
    b_name = name;
  }

let free_variable () =
  let id = next_id () in
  let tv = 
    {
      tv_name = xf "_fv%d" id;
      tv_id = id;
      tv_is_free = true;
      tv_constraints = [];
    }
  in
  T_var {b_name = tv.tv_name; b_namespace = ""; b_value = B_bound tv}

let optionalize f = function None -> None | Some x -> Some (f x)
