open Util

let parse_file fn = 
  let f = open_in fn in
  let lexbuf = Lexing.from_channel f in
  let () = Lexer.set_file_name fn in
  let decls = 
    try
      Parser.program Lexer.token lexbuf
    with Parsing.Parse_error ->
       let l = Lexer.get_current_location () in
       error ~loc:l "parse error";
       exit 1
  in
  let () = close_in f in
  decls

let rec pass f = function
  | (env_state, decls) :: xs ->
    Env.load env_state;
    f decls;
    pass f xs
  | [] -> ()

let rec passes decls = function
  | f :: fs -> pass f decls; passes decls fs
  | [] -> ()

let compile files =
  let last_fn = List.hd (List.rev files) in
  let parse_file fn =
    Env.reset ();
    Env.add_open_prefix Env.global "Nemerle.Core";
    let decls = Flatten_ns.flatten (parse_file fn) in
    (Env.save (), decls)
  in
  let decls = List.map parse_file files in
  
  passes decls [
    Ty_bind.scan_globals;
    Ty_bind.bind_types; 
    Make_ty_info.attach_ty_info;
    Make_ty_info.set_variant_info;
    Make_ty_info.normalize_reverse_subtyping;
    Make_ty_info.determine_subtyping;
    Ty_constraints.validate;
    Name_bind.bind_values;
  ];
  maybe_say_bye ();
  passes decls [Ty_expr.validate; Cil_check.validate];
  maybe_say_bye ();

  let rec collect_decls acc = function
    | (_, x) :: xs -> collect_decls (List.rev_append x acc) xs
    | [] -> List.rev acc
  in
  Cg_util.set_output_file (last_fn ^ ".cs");
  Cg_expr.cg_decls (collect_decls [] (List.rev decls));
  Cg_util.close_output_file ()

let main () =
  begin
    try
      let d = Sys.getenv "DEBUG" in
      if d <> "" then
        debug_level := 1
      else
        ()
    with Not_found -> ()
  end;
  if Array.length Sys.argv < 2 then
  begin
    prerr_endline ("USAGE: " ^ Sys.argv.(0) ^ " filename.n...");
    exit 1
  end;
  
  let core_name = Filename.concat (Filename.dirname Sys.argv.(0)) 
                                  "lib/core.n" in
  let rec loop acc n =
    if n <= 0 then 
      compile (core_name :: acc)
    else
      loop (Sys.argv.(n) :: acc) (n - 1)
  in loop [] (Array.length Sys.argv - 1)

let () = main ()
