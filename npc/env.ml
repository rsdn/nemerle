open Ast
open Util
open Ast_util

type t =
  {
    mutable e_decls : decl list Smap.t;
    mutable e_enum_parents : type_decl Imap.t;
    mutable e_namespace_aliases : string Smap.t;
    mutable e_open_namespaces : string list;
  }

let create () =
  { 
    e_decls = Smap.empty;
    e_namespace_aliases = Smap.empty;
    e_enum_parents = Imap.empty;
    e_open_namespaces = [""];
  }

let copy r =
  {r with e_decls = r.e_decls}

let expand_name e s =
  if String.contains s '.' then
    let pos = String.index s '.' in
    let first = String.sub s 0 pos in
    if Smap.mem first e.e_namespace_aliases then
      Smap.find first e.e_namespace_aliases ^ 
        String.sub s pos (String.length s - pos)
    else
      s
  else
    s

let expand_ns e s =
  let s' = expand_name e s in
  if s = s' then
    if Smap.mem s e.e_namespace_aliases then
      Smap.find s e.e_namespace_aliases
    else
      s
  else
    s'
  
let lookup e s =
  let s' = expand_name e s in
  if s' = s then
    let rec loop acc = function
      | x :: xs ->
        let prefixed = x ^ s in
        if Smap.mem prefixed e.e_decls then
          loop (Smap.find prefixed e.e_decls @ acc) xs
        else
          loop acc xs
      | [] -> acc
    in
    match loop [] e.e_open_namespaces with
    | [] ->
      error (xf "unbound symbol `%s'" s);
      raise Recovery
    | r -> r
  else
    try
      Smap.find s e.e_decls
    with Not_found ->
      error (xf "unbound symbol `%s' (expanded to `%s')" s s');
      raise Recovery

let add c pref decl =
  let (name, loc) = decl_name_loc decl in
  let name = pref ^ name in
  let others =
    if Smap.mem name c.e_decls then
      match (decl, Smap.find name c.e_decls) with
      | (D_method _, (D_method _ :: _ as l)) 
      | (D_iface_method _, (D_iface_method _ :: _ as l)) 
      | (D_function _, (D_function _ :: _ as l)) -> 
        l (* OK, overloading *)
      | (_, d' :: _) -> 
        error ~loc:loc (xf "redefinition of symbol `%s'" name);
        let (_, l') = decl_name_loc d' in
        error ~loc:l' "first defined here";
        []
      | (_, []) -> assert false
    else
      []
  in
  c.e_decls <- Smap.add name (decl :: others) c.e_decls

let global = create ()

let add_open_prefix e s =
  let s = s ^ "." in
  if List.mem s e.e_open_namespaces then 
    ()
  else
    e.e_open_namespaces <- s :: e.e_open_namespaces

let add_namespace_alias e short long =
  if Smap.mem short e.e_namespace_aliases then
    error (xf "redefinition name namespace alias `%s'" short)
  else
    ();
  e.e_namespace_aliases <- 
    Smap.add short (expand_ns e long) e.e_namespace_aliases

let add_enum_parent e ee td =
  assert (not (Imap.mem ee.ee_id e.e_enum_parents));
  e.e_enum_parents <- Imap.add ee.ee_id td e.e_enum_parents

let lookup_enum_parent e ee =
  Imap.find ee.ee_id e.e_enum_parents

let reset () =
  global.e_open_namespaces <- [""];
  global.e_namespace_aliases <- Smap.empty

type state = string list * string Smap.t

let load (o, a) =
  global.e_open_namespaces <- o;
  global.e_namespace_aliases <- a

let save () =
  (global.e_open_namespaces, global.e_namespace_aliases)
