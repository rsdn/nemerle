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

{

open Ast           (* location *)
open Util          (* error *)
open Parser        (* The type token is defined in parser.mli *)

let comment_depth = ref 0
let comment_start = ref {l_file = ""; l_line = 0; l_column = 0}
let current_location = ref {l_file = "stdin"; l_line = 1; l_column = 0}

let process_id =
  let keywords = Hashtbl.create 50 in
  let () = List.iter (fun (s, t) -> Hashtbl.add keywords s t)
    [
       ("abstract", KW_ABSTRACT);
       ("const", KW_CONST);
       ("extern", KW_EXTERN);
       ("internal", KW_INTERNAL);
       ("new", KW_NEW);
       ("private", KW_PRIVATE);
       ("protected", KW_PROTECTED);
       ("sealed", KW_SEALED);
       ("volatile", KW_VOLATILE);
       ("class", KW_CLASS);
       ("enum", KW_ENUM);
       ("extends", KW_EXTENDS);
       ("finally", KW_FINALLY);
       ("in", KW_IN);
       ("null", KW_NULL);
       ("out", KW_OUT);
       ("public", KW_PUBLIC);
       ("raise", KW_RAISE);
       ("ref", KW_REF);
       ("struct", KW_STRUCT);
       ("this", KW_THIS);
       ("variant", KW_VARIANT);
       ("interface", KW_INTERFACE);
       ("implements", KW_IMPLEMENTS);
       ("namespace", KW_NAMESPACE);
       ("where", KW_WHERE);
       ("type", KW_TYPE);
       ("let", KW_LET);
       ("in", KW_IN);
       ("fun", KW_FUN);
       ("and", KW_AND);
       ("tymatch", KW_TYMATCH);
       ("with", KW_WITH);
       ("try", KW_TRY);
       ("open", KW_OPEN);
       ("void", KW_VOID);
       ("base", KW_BASE);
       ("_", UNDERSCORE);
       ("if", KW_IF);
       ("then", KW_THEN);
       ("else", KW_ELSE);
       ("variant", KW_VARIANT);
       ("letfun", KW_LETFUN);
       ("as", KW_AS);
       ("record", KW_RECORD);
       ("match", KW_MATCH);
       ("static", KW_STATIC);
    ]
  in
  fun id ->
    if Hashtbl.mem keywords id then
      Hashtbl.find keywords id
    else
      ID id

let set_file_name s =
  current_location := {!current_location with l_file = s; 
                                              l_line = 1; 
                                              l_column = 0}

let get_file_name () = !current_location.l_file

let next_line _lexbuf =
  current_location := 
    {!current_location with 
      l_line = !current_location.l_line + 1;
      l_column = 0}

let get_current_location () = !current_location

let strip_quotes s =
  String.sub s 1 (String.length s - 2) 
}

let nl = ('\r' '\n' | '\n' | '\r')

let num = ['0'-'9']

let id_beg = ['a'-'z''A'-'Z''_']
let id_body = (id_beg | ['0'-'9''\''])

let op_char = [ '=' '<' '>' '@' '^' '|' '&' '+' '-' '*' '/' 
                '$' '%' '!' '?' '~' '.' ':' '#' ]

rule token = parse
    [' ' '\t']          { token lexbuf }     (* skip blanks *)
  | nl                  { next_line lexbuf; token lexbuf }
  | ((num+ '.' num+ 'e' (['+' '-']?) num+) | (num+ 'e' (['+' '-']?) num+))
                        { let f = float_of_string (Lexing.lexeme lexbuf) in
                          NUMBER_LITERAL (L_float f) }
  | '"' [^ '"']* '"'   (* FIXME: process escapes *) 
    { let s = Lexing.lexeme lexbuf in 
      STRING_LITERAL (String.sub s 1 (String.length s - 2)) }
  | (num+ | '0' ['x' 'X'] ['0'-'9''a'-'f''A'-'F']+ | '0' ['b' 'B'] ['0' '1']+)
                        { let n = Int64.of_string (Lexing.lexeme lexbuf) in
                          NUMBER_LITERAL (L_int n) }
  | id_beg id_body*     { process_id (Lexing.lexeme lexbuf) }
  | '\'' id_beg id_body*     
        { let s = Lexing.lexeme lexbuf in
          TYVAR (String.sub s 1 (String.length s - 1)) }
  | '`' op_char+ '`'    { ID (strip_quotes (Lexing.lexeme lexbuf)) }
  | '`' id_body* '`'
                        { OP4 (strip_quotes (Lexing.lexeme lexbuf)) }
  | '`' '`' id_body* '`' '`'
                        { ID (strip_quotes (strip_quotes (Lexing.lexeme lexbuf))) }
  | "(*"                { comment_depth := 1;
                          comment_start := !current_location;
                          comment lexbuf; 
                          token lexbuf }
  | "//" [^ '\n' '\r']* nl { next_line lexbuf; token lexbuf }
  | "{"         { L_BRACE }
  | "}"         { R_BRACE }
  | "]"         { R_SQUERE }
  | "["         { L_SQUERE }
  | ","         { COMMA }
  | ":"         { COLON }
  | ";"         { SEMICOLON }
  | "|"         { BAR }
  | ":>"        { COLON_MORE }
  | "*"         { STAR }
  | "."         { DOT }
  | "("         { L_PAREN }
  | ")"         { R_PAREN }
  | "="         { EQ }
  | "#"         { HASH }
  | "?"         { QUESTION_MARK }
  | "<-"        { LESS_MINUS }
  | "->"        { MINUS_MORE }
  | "=>"        { EQ_MORE }
  | "\\"        { BACKSLASH }
  
  | "**" op_char*                       { OP1 (Lexing.lexeme lexbuf) }
  | ['/' '*' '%'] op_char*              { OP2 (Lexing.lexeme lexbuf) }
  | ['+' '-'] op_char*                  { OP3 (Lexing.lexeme lexbuf) }
  | ['@' '^' '$' '~' '?'] op_char*      { OP4 (Lexing.lexeme lexbuf) }
  | ['=' '<' '>' '!'] op_char*          { OP5 (Lexing.lexeme lexbuf) }
  | '&' op_char*                        { OP6 (Lexing.lexeme lexbuf) }
  | '|' op_char*                        { OP7 (Lexing.lexeme lexbuf) }
  | eof                                 { EOF }

  | _                   
    { 
      let c = String.escaped (Lexing.lexeme lexbuf) in
      error ~loc:!current_location ("invalid character in input `" ^ c ^ "'");
      token lexbuf 
    }

and comment = parse
    "*)"		{ decr comment_depth; 
                          if !comment_depth > 0 then comment lexbuf }
  | "(*"                { incr comment_depth; comment lexbuf }
  | nl                  { next_line lexbuf; comment lexbuf }
  | eof                 { error ~loc:!comment_start "EOF within comment" }
  | _			{ comment lexbuf }

