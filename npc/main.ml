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
