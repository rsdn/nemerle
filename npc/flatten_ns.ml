open Ast
open Util

let env = Env.global

let rec flatten_one pref d = 
  locate d.nsd_loc begin fun () ->
    match d.nsd_raw with
    | ND_namespace (s, decls) ->
      flatten_list (pref ^ Env.expand_ns env s ^ ".") decls
    | ND_open s ->
      error "open allowed only at the beginning of compilation unit"; 
      Env.add_open_prefix env s;
      []
    | ND_namespace_alias (x, y) ->
      error "namespace alias allowed only at the beginning of compilation unit";
      Env.add_namespace_alias env x y;
      []
    | ND_decl (D_type td) -> 
      [D_type {td with td_name = pref ^ td.td_name}]
    | ND_decl _ -> assert false
  end

and flatten_list pref lst =
  let rec loop acc = function
    | x :: xs ->
      loop (flatten_one pref x @ acc) xs
    | [] -> acc
  in loop [] lst

let is_open_or_ns_alias d =
  locate d.nsd_loc begin fun () ->
    match d.nsd_raw with
    | ND_open s -> Env.add_open_prefix env s; true
    | ND_namespace_alias (x, y) -> Env.add_namespace_alias env x y; true
    | ND_decl _
    | ND_namespace (_, _) -> false
  end

let flatten lst =
  let rec loop = function
    | x :: xs as l ->
      if is_open_or_ns_alias x then
        loop xs
      else
        List.rev (flatten_list "" l)
    | [] -> []
  in loop lst
