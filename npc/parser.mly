%{
open Ast
open Util

let locus () = Lexer.get_current_location ()

let ice () = raise Parse_error

let unbound x = {b_name = x; b_namespace = ""; b_value = B_unbound}

let empty_val () =
  {
    val_loc = locus ();
    val_modifiers = [];
    val_mutable = false;
    val_name = "";
    val_type = None;
    val_kind = Val_global;
    val_id = next_id ();
    val_in_closure = false;
    val_defined_in_fun = 0;
  }

(* FIXME: this not going to work with method, type etc *)
let lookup_target = function
  | "field" -> Attr_field
  | "event" -> Attr_event
  | "method" -> Attr_method
  | "module" -> Attr_module
  | "param" -> Attr_param
  | "property" -> Attr_property
  | "return" -> Attr_return
  | "type" -> Attr_type
  | "assembly" -> Attr_assembly
  | s -> 
    error ~loc:(locus ()) (xf "bad attribute target: `%s'" s);
    Attr_type

let empty_typarms = { tp_tyvars = []; tp_constraints = []; }

let parm x = {parm_expr = x; parm_is_ref = false; parm_name = ""}
let valref o = 
    { 
      e_loc = locus (); 
      e_type = None; 
      e_raw = E_ref (unbound o); 
    }

let bin l o r = 
  E_fun_call (valref o, [parm l; parm r])

let unary o r =
  E_fun_call (valref o, [parm r])

let fun_type parms ret_type =
  let parm_types = 
    List.map (fun v -> 
               match v.val_type with
               | Some x -> x 
               | None -> assert false) parms
  in
  let in_type =
    match parm_types with
    | [] -> T_void
    | [x] -> x
    | t -> T_prod t
  in
  T_fun (in_type, ret_type)

let make_td attrs ((loc, name), typarms, extends, implements) body =
  {
    td_loc = loc;
    td_typarms = typarms;
    td_modifiers = attrs;
    td_extends = extends;
    td_implements = implements;
    td_name = name;
    td_raw = body;
    td_id = next_id ();
  }

let make_fun head body =
  let (is_ctor, 
       (typarms, (loc, name), parms, ret_type), 
       implements) = head in
  (is_ctor, {
     fun_loc = loc;
     fun_modifiers = [];
     fun_name = name;
     fun_typarms = typarms;
     fun_id = next_id ();
     fun_type = fun_type parms ret_type;
     fun_body = body;
     fun_parms = parms;
     fun_kind = Fun_global;
     
     fun_in_closure = false;
     fun_needed_closures = [];
     fun_defined_in_fun = 0;
     fun_has_closure = false;
     fun_needs_proxy = false;
  }, implements)

let ex e = { e_loc = locus (); e_type = None; e_raw = e; }

let mktmp e =
  let id = next_id () in
  let nam = xf "__N__tmp_%d" id in
  let v =
    {(empty_val ()) with
      val_name = nam;
      val_kind = Val_local e;
      val_id = id}
  in (v, { b_name = nam; b_namespace = ""; b_value = B_unbound })
  
(* if E1 then E2 else E3 ===>
   let tmp = (E1 : bool) in
   tymatch tmp with [
     | True => E2
     | False => E3
   ]
 *)
 
let make_if_then_else cond_ex then_ex else_ex =
  let mk t e =
    {
      tc_tyvars = [];
      tc_constraints = [];
      tc_type = t;
      tc_body = e;
    }
  in
  let t = mk (Builtin_types.true_ty ()) then_ex in
  let f = mk (Builtin_types.false_ty ()) else_ex in
  let (v, b) = mktmp (ex (E_type_enforcement (cond_ex, Builtin_types.bool_ty ()))) in
  E_let (v, ex (E_tymatch (ex (E_ref b), [t; f])))

(* fun (A) : R => E ===>
   fun TMP (A) : R = E in TMP
 *)

let make_lambda (typarms, parms, ret_type) body =
  let id = next_id () in
  let (_, name) as loc_name = (locus (), xf "__N__tmp_%d" id) in
  let (_, fn, _) = 
    make_fun (false, (typarms, loc_name, parms, ret_type), []) 
             (Fb_expr body) in
  E_fun ([fn], ex (E_ref (unbound name)))


let make_variant td lst =
  let make_td ((loc, name), decls) =
    {
      td_loc = loc;
      td_name = name;
      td_modifiers = [];
      td_typarms = Ast_util.copy_typarms td.td_typarms;
      td_extends = td.td_extends;
      td_implements = td.td_implements;
      td_raw = T_class decls;
      td_id = next_id ();
    }
  in {td with td_raw = T_variant (List.map make_td lst)}

let implicit_record_ctor fields =
  let rec loop ((parms, assigns) as acc) = function
    | D_field {fld_name = n; fld_type = t} :: xs ->
      let p =
        {(empty_val ()) with
          val_name = n;
          val_type = Some t;
          val_kind = Val_parm;
          val_id = next_id ();
        }
      in
      let fld = E_field_ref (ex E_this, unbound n) in
      let a = ex (E_assignment (ex fld, ex (E_ref (unbound n)))) in
      loop (p :: parms, a :: assigns) xs
    | _ :: xs -> loop acc xs
    | [] -> acc
  in
  let (parms, assigns) = loop ([], []) fields in
  let body = ex (E_sequence assigns) in
  let (_, fn, _) = 
    make_fun (false, 
              (empty_typarms, (locus (), "this"), parms, T_void),
              []) (Fb_expr body)
  in
  D_method
    {
      meth_fun = fn;
      meth_ifm =
        {
           ifm_new_modifier = false (* XXX *);
           ifm_name = fn.fun_name;
           ifm_typarms = fn.fun_typarms;
           ifm_type = fn.fun_type;
           ifm_id = next_id ();
           ifm_loc = fn.fun_loc;
           ifm_parms = fn.fun_parms;
        };
      meth_implements = [];
      meth_kind = Method_ctor;
    }

%}

%token <Ast.literal> NUMBER_LITERAL
%token <string> ID
%token <string> TYVAR
%token <string> STRING_LITERAL
%token <string> OP1
%token <string> OP2
%token <string> OP3
%token <string> OP4
%token <string> OP5
%token <string> OP6
%token <string> OP7

%token KW_ABSTRACT KW_CONST KW_EXTERN KW_INTERNAL KW_NEW KW_PRIVATE
%token KW_PROTECTED KW_SEALED KW_VOLATILE KW_NAMESPACE KW_BASE
%token KW_CLASS KW_ENUM KW_EXTENDS KW_FINALLY KW_IN KW_METHOD KW_NULL KW_OUT
%token KW_PUBLIC KW_RAISE KW_REF KW_STRUCT KW_THIS KW_VARIANT
%token KW_INTERFACE KW_IMPLEMENTS KW_WHERE KW_FIELD KW_VALUE KW_TYPE KW_LET
%token KW_IN KW_FUN KW_AND KW_TYMATCH KW_WITH KW_TRY KW_OPEN KW_VOID
%token KW_IF KW_THEN KW_ELSE KW_LETFUN KW_AS KW_RECORD KW_MATCH

%token L_BRACE R_BRACE R_SQUERE L_SQUERE COMMA COLON SEMICOLON BAR 
%token COLON_MORE STAR DOT L_PAREN R_PAREN EQ HASH QUESTION_MARK
%token LESS_MINUS MINUS_MORE EQ_MORE UNDERSCORE BACKSLASH

%token EOF

%nonassoc KW_RAISE LESS_MINUS KW_LET
%left OP7
%left OP6
%left OP5
%left OP4
%left OP3
%left OP2 STAR
%right OP1
%nonassoc UNARY

%start program

%type <Ast.ns_decl list> program

%%

program:
        namespace_or_type_decls EOF            { $1 }

namespace_or_type_decl:
          KW_OPEN located_qid SEMICOLON                             
                {
                  let (loc, name) = $2 in
                  {
                    nsd_loc = loc;
                    nsd_raw = ND_open name;
                  }
                }
        | KW_NAMESPACE located_id EQ located_qid SEMICOLON
                {
                  let (loc, name) = $2 in
                  let (_, value) = $4 in
                  {
                    nsd_loc = loc;
                    nsd_raw = ND_namespace_alias (name, value);
                  }
                }
        | KW_NAMESPACE located_qid L_BRACE namespace_or_type_decls R_BRACE
                { 
                  let (loc, name) = $2 in
                  {
                    nsd_loc = loc;
                    nsd_raw = ND_namespace (name, $4)
                  }
                }
        | type_decl 
                { 
                  let td = $1 in
                  {
                    nsd_loc = td.td_loc;
                    nsd_raw = ND_decl (D_type td);
                  }
                }

namespace_or_type_decls:
          namespace_or_type_decl namespace_or_type_decls        { $1 :: $2 }
        | /* */                                                 { [] }

type_header:
          located_id typarms maybe_extends maybe_implements
                {
                        ($1, $2, $3, $4)
                }
                
type_decl:
          attrs KW_TYPE type_header EQ type_declarator
                { make_td $1 $3 $5 }
        | attrs KW_INTERFACE type_header L_BRACE interface_members R_BRACE
                { make_td $1 $3 (T_interface $5) }
        | attrs KW_CLASS type_header class_body
                { make_td $1 $3 (T_class $4) }
        | attrs KW_RECORD type_header record
                { make_td $1 $3 (T_class $4) }
        | attrs KW_STRUCT type_header class_body
                { make_td $1 $3 (T_struct $4) }
        | attrs KW_ENUM type_header L_BRACE maybe_bar enum_variants R_BRACE
                { make_td $1 $3 (T_enum $6) }
        | attrs KW_VARIANT located_id typarms maybe_extends EQ
                L_SQUERE maybe_bar or_types R_SQUERE  
                { make_variant (make_td $1 ($3, $4, $5, []) (T_variant [])) $9 }

type_declarator:
          ty SEMICOLON
                { T_alias $1 }
        | KW_EXTERN STRING_LITERAL SEMICOLON
                { T_external $2 }
        
located_id:
        ID      { (locus (), $1) }
        
located_id_or_dummy:
          located_id    { $1 }
        | UNDERSCORE    { (locus (), xf "__N__%d" (next_id ())) }

qid:
          ID            { $1 }
        | ID DOT qid    { $1 ^ "." ^ $3 }

located_qid:
        qid      { (locus (), $1) }

maybe_extends:
          /* */                 { None }
        | KW_EXTENDS ty         { Some $2 }

maybe_implements:
          /* */                                 { [] }
        | KW_IMPLEMENTS comma_sep_types         { $2 }

comma_sep_types:
          ty                                    { [$1] }
        | ty COMMA comma_sep_types              { $1 :: $3 }

interface_members:
          /* */                                   { [] }
        | interface_member interface_members      { $1 :: $2 }

maybe_new:
          /* */   { false }
        | KW_NEW  { true }
        
interface_member:
        maybe_new maybe_method fun_def_head SEMICOLON
        {
          let (is_ctor, 
                (typarms, (loc, name), parms, ret_type), 
                implements) = $3 in
          if is_ctor || implements <> [] then raise Parse_error
          else
           D_iface_method {
             ifm_new_modifier = $1;
             ifm_name = name;
             ifm_typarms = typarms;
             ifm_type = fun_type parms ret_type;
             ifm_parms = parms;
             ifm_id = next_id ();
             ifm_loc = loc;
           }
        }

maybe_method:
          /* */         { () }
        | KW_METHOD     { () }
        
typarms:
          /* */  { empty_typarms }
        | L_PAREN tyvars R_PAREN maybe_where
          {
            {
              tp_tyvars = $2;
              tp_constraints = $4;
            }
          }

tyvars:
          tyvar                         { [$1] }
        | tyvar COMMA tyvars            { $1 :: $3 }

maybe_where:
          /* */                         { [] }
        | KW_WHERE where_constraints    { $2 }

maybe_bar:
          /* */ { None }
        | BAR   { None }

where_constraints:
          where_constraint                                { [$1] }
        | where_constraint COMMA where_constraints        { $1 :: $3 }

where_constraint:
        TYVAR COLON_MORE ty     { (unbound $1, $3) }

tyvar:
        TYVAR                   
          { 
            {
              tv_name = $1;
              tv_id = next_id ();
              tv_constraints = [];
              tv_is_free = false;
            }
          }

class_body:
        L_BRACE type_members R_BRACE         { $2 }

enum_variants:
          enum_variant                              { [$1] }
        | enum_variant BAR enum_variants            { $1 :: $3 }

maybe_init:
          /* */                   { None }
        | EQ expr                 { Some $2 }

enum_variant:
        attrs located_id maybe_init
        {
          let (loc, name) = $2 in
          D_enum_entry
            {
              ee_loc = loc;
              ee_modifiers = $1;
              ee_name = name;
              ee_id = next_id ();
              ee_value = $3;
            }
        }

type_members:
          /* */                              { [] }
        | type_member type_members           { $1 :: $2 }

attrs:
          /* */                              { [] }
        | attr attrs                         { $1 :: $2 }

attr:
          KW_NEW          { M_new }
        | KW_PUBLIC       { M_public }
        | KW_PROTECTED    { M_protected }
        | KW_INTERNAL     { M_internal }
        | KW_PRIVATE      { M_private }
        | KW_ABSTRACT     { M_abstract }
        | KW_SEALED       { M_sealed }
        | KW_VOLATILE     { M_volatile }
        | KW_EXTERN       { M_extern }
        | KW_CONST        { M_const }
        | attribute_section    { M_attribute $1 }
        

attribute_section:
        L_SQUERE ID COLON qid maybe_attr_args R_SQUERE
        {
           let (pos, named) = $5 in
           {
             attr_target = lookup_target $2;
             attr_class = unbound $4;
             attr_positional_parms = List.rev pos;
             attr_named_parms = List.rev named;
           }
        }

maybe_attr_args:
          /* */                   { ([], []) }
        | L_PAREN R_PAREN         { ([], []) }
        | L_PAREN non_empty_comma_sep_exprs_r COMMA named_attr_parms R_PAREN
                                  { ($2, $4) }
        | L_PAREN named_attr_parms R_PAREN
                                  { ([], $2) }
        | L_PAREN non_empty_comma_sep_exprs_r R_PAREN
                                  { ($2, []) }

non_empty_comma_sep_exprs:
        non_empty_comma_sep_exprs_r                { List.rev $1 }

non_empty_comma_sep_exprs_r:
          expr                                     { [$1] }
        | non_empty_comma_sep_exprs_r COMMA expr   { $3 :: $1 }

named_attr_parms:
          ID EQ expr                              { [($1, $3)] }
        | named_attr_parms COMMA ID EQ expr       { ($3, $5) :: $1 }

maybe_record:
          /* */
          {
            [implicit_record_ctor []]
          }
        | record { $1 }

record:
        L_BRACE record_members R_BRACE          
          { 
            let flds = List.rev $2 in
            implicit_record_ctor flds :: $2 
          }

record_members:
          /* */                              { [] }
        | record_member record_members       { $1 :: $2 }

record_member:
        attrs maybe_ref located_id COLON ty SEMICOLON
                {
                  let (loc, name) = $3 in
                  D_field
                    {
                      fld_loc = loc;
                      fld_name = name;
                      fld_mutable = $2;
                      fld_type = $5;
                      fld_modifiers = $1; 
                      fld_id = next_id ();
                    }
                }

type_member:
          attrs KW_FIELD maybe_ref located_id COLON ty SEMICOLON
                {
                  let (loc, name) = $4 in
                  D_field
                    {
                      fld_loc = loc;
                      fld_name = name;
                      fld_mutable = $3;
                      fld_type = $6;
                      fld_modifiers = $1; 
                      fld_id = next_id ();
                    }
                }
        | attrs KW_VALUE maybe_ref located_id COLON ty SEMICOLON
                {
                  let (loc, name) = $4 in
                  D_value
                    {(empty_val ()) with
                      val_loc = loc;
                      val_name = name;
                      val_mutable = $3;
                      val_type = Some $6;
                      val_modifiers = $1; 
                      val_kind = Val_global}
                }
        | attrs KW_METHOD fun_def_term
                {
                  let (is_ctor, fn, impl) = $3 in
                  let fn = {fn with fun_modifiers = $1} in
                     D_method
                       {
                         meth_fun = fn;
                         meth_implements = impl;
                         meth_ifm = 
                           {
                             ifm_new_modifier = false (* XXX *);
                             ifm_name = fn.fun_name;
                             ifm_typarms = fn.fun_typarms;
                             ifm_type = fn.fun_type;
                             ifm_id = next_id ();
                             ifm_loc = fn.fun_loc;
                             ifm_parms = fn.fun_parms;
                           };
                         meth_kind = 
                           if is_ctor then Method_ctor 
                           else Method_normal;
                       }
                }
        | attrs KW_FUN fun_def_term
                { 
                  let (_, fn, _) = $3 in
                  let fn = {fn with fun_modifiers = $1} in
                  match $3 with
                  | (true, _, []) -> 
                    D_function {fn with fun_kind = Fun_static_ctor}
                  | (false, _, []) -> 
                    D_function fn
                  | (_, _, _ :: _) -> 
                    (* FIXME: give better message *)
                    raise Parse_error
                }
        | type_decl
                { D_type $1 }

lambda_head:
          typarms KW_FUN L_PAREN fun_parms R_PAREN COLON ty
                        { ($1, $4, $7) }

lambda:
          lambda_head EQ_MORE expr %prec KW_LET
          { make_lambda $1 $3 }
        | lambda_head sequence %prec KW_LET
          { make_lambda $1 $2 }

fun_def_head:
          typarms located_id L_PAREN fun_parms R_PAREN COLON ty
          maybe_method_implements
                        { (false, ($1, $2, $4, $7), $8) }
        | typarms KW_THIS L_PAREN fun_parms R_PAREN
                        { (true, ($1, (locus (), "this"), $4, T_void), []) }

fun_body:
          expr
                { Fb_expr $1 }
        | KW_EXTERN STRING_LITERAL
                { Fb_extern $2 }
                
fun_def:
          fun_def_head EQ fun_body
           { make_fun $1 $3 }
        | fun_def_head sequence
           { make_fun $1 (Fb_expr $2) }

fun_def_term:
          fun_def_head EQ fun_body SEMICOLON
           { make_fun $1 $3 }
        | fun_def_head sequence
           { make_fun $1 (Fb_expr $2) }

ref_out_or_none:
        /* */    { 0 }
        | KW_REF { 1 }
        | KW_OUT { 2 }
        
fun_parm:
          attrs ref_out_or_none located_id_or_dummy COLON ty
            {
              let (loc, name) = $3 in
              {(empty_val ()) with
                val_loc = loc;
                val_name = name;
                val_kind = Val_parm;
                val_mutable = true;
                val_type = Some (match $2 with 0 -> $5 | 1 -> T_ref $5 | _ -> T_out $5);
                val_modifiers = $1}
            }

non_empty_fun_parms:
          fun_parm                      { [$1] }
        | fun_parm COMMA fun_parms      { $1 :: $3 }

fun_parms:
          /* */                         { [] }
        | non_empty_fun_parms           { $1 }

maybe_method_implements:
          /* */           { [] }
        | KW_IMPLEMENTS implemented_methods { $2 }

implemented_methods:
          qid                                    { [unbound $1] }
        | qid COMMA implemented_methods          { unbound $1 :: $3 }

maybe_ref:
          /* */         { false }
        | KW_REF        { true }

commas:
          /* */           { 0 }
        | COMMA commas    { $2 + 1 }

prim_ty:
          qid                                   { T_app (unbound $1, []) }
        | qid L_PAREN comma_sep_types R_PAREN   { T_app (unbound $1, $3) }
        | TYVAR                                 { T_var (unbound $1) }
        | L_PAREN ty R_PAREN                    { $2 }
        | prim_ty L_SQUERE commas R_SQUERE      { T_array ($3, $1) }
        | KW_VOID                               { T_void }

or_types:
          or_type                    { [$1] }
        | or_type BAR or_types       { $1 :: $3 }

or_type:
        located_id maybe_record       { ($1, $2) }
        
prim2_ty:
          prim_ty              { $1 }
        | KW_REF prim_ty       { T_ref $2 }
        | KW_OUT prim_ty       { T_out $2 }
        
prod_ty_x:
          prim2_ty                               { [$1] }
        | prim2_ty STAR prod_ty_x                { $1 :: $3 }

prod_ty:
        prod_ty_x          { match $1 with [x] -> x | x -> T_prod x }

ty:
          prod_ty                 { $1 }
        | prod_ty MINUS_MORE ty   { T_fun ($1, $3) }
          
prim_expr:
          raw_prim_expr             
            { { e_loc = locus (); e_type = None; e_raw = $1; } }
        | L_PAREN expr R_PAREN { $2 }
       

expr:
          raw_expr             
            { { e_loc = locus (); e_type = None; e_raw = $1; } }
        | prim_expr 
            { $1 }

raw_prim_expr:
          qid           { E_ref (unbound $1) }
        | KW_THIS       { E_this }
        | KW_BASE       { E_base }
        
        | KW_NULL               { E_literal L_null }
        | L_PAREN R_PAREN       { E_literal L_void }
        | STRING_LITERAL        { E_literal (L_string $1) }
        | NUMBER_LITERAL        { E_literal $1 }
        
        | L_PAREN expr COLON_MORE ty R_PAREN
                        { E_type_conversion ($2, $4) }
        | L_PAREN expr COLON ty R_PAREN
                        { E_type_enforcement ($2, $4) }
                        
        | prim_expr HASH qid
              { E_field_ref ($1, unbound $3) }
        | raw_sequence  { $1 }
        | L_PAREN comma_sep_exprs R_PAREN       { E_tuple (List.rev $2) }

raw_sequence:
          L_BRACE R_BRACE
              { E_sequence [] }
        | L_BRACE expr_list maybe_semicolon R_BRACE
              { E_sequence (List.rev $2) }

sequence:
          raw_sequence
            { { e_loc = locus (); e_type = None; e_raw = $1; } }

maybe_semicolon:
          /* */         { () }
        | SEMICOLON     { () }

expr_list:
          expr_list SEMICOLON expr      { $3 :: $1 }
        | expr                          { [$1] }

comma_sep_exprs:
          comma_sep_exprs COMMA expr      { $3 :: $1 }
        | expr COMMA expr                 { [$3; $1] }

raw_expr:
          prim_expr L_PAREN parameters R_PAREN
              { E_fun_call ($1, $3) }
        | prim_expr LESS_MINUS expr
              { E_assignment ($1, $3) }
        | prim_expr L_SQUERE non_empty_comma_sep_exprs R_SQUERE
              { E_array_access ($1, $3) }
        | KW_LET maybe_ref located_id_or_dummy EQ expr KW_IN expr %prec KW_LET
              {
                let (loc, name) = $3 in
                let v = {(empty_val ()) with
                  val_loc = loc;
                  val_name = name;
                  val_mutable = $2;
                  val_kind = Val_local $5;
                } in E_let (v, $7)
              }
        | KW_LET tuple_pattern EQ expr KW_IN expr %prec KW_LET
              {
                E_match ($4, [{mc_pattern =  $2; mc_body = $6}])
              }
        | KW_LETFUN fun_defs KW_IN expr %prec KW_LET
              { E_fun ($2, $4) }
        | KW_IF expr KW_THEN expr KW_ELSE expr %prec KW_LET
              { make_if_then_else $2 $4 $6 }
        | KW_TYMATCH expr KW_WITH L_SQUERE maybe_bar tymatch_cases R_SQUERE
              { E_tymatch ($2, $6) }
        | KW_MATCH expr KW_WITH L_SQUERE maybe_bar match_cases R_SQUERE
              { E_match ($2, $6) }
        | KW_RAISE expr
              { E_raise $2 }
        | KW_TRY expr KW_WITH QUESTION_MARK located_id COLON ty EQ_MORE expr %prec KW_LET
              { 
                let (loc, name) = $5 in
                let v = {(empty_val ()) with
                  val_loc = loc;
                  val_name = name;
                  val_type = Some $7;
                  val_kind = Val_exn;
                } in E_try_with ($2, v, $9)
              }
        | KW_TRY expr KW_FINALLY expr %prec KW_LET
              { E_try_finally ($2, $4) }
              
        | expr OP1 expr         { bin $1 $2 $3 }
        | expr OP2 expr         { bin $1 $2 $3 }
        | expr STAR expr        { bin $1 "*" $3 }
        | expr OP3 expr         { bin $1 $2 $3 }
        | expr OP4 expr         { bin $1 $2 $3 }
        | expr OP5 expr         { bin $1 $2 $3 }
        | expr OP6 expr         { bin $1 $2 $3 }
        | expr OP7 expr         { bin $1 $2 $3 }

        | OP1 expr %prec UNARY         { unary $1 $2 }
        | OP2 expr %prec UNARY         { unary $1 $2 }
        | OP3 expr %prec UNARY         { unary $1 $2 }
        | OP4 expr %prec UNARY         { unary $1 $2 }
        | OP5 expr %prec UNARY         { unary $1 $2 }
        | OP6 expr %prec UNARY         { unary $1 $2 }
        | OP7 expr %prec UNARY         { unary $1 $2 }

        | lambda                       { $1 }
                
non_empty_parameters:
          parameter                       { [$1] }
        | parameter COMMA parameters      { $1 :: $3 }

parameters:
          /* */                           { [] }
        | non_empty_parameters            { $1 }

parameter:
            maybe_ref expr
              { { parm_is_ref = $1; parm_expr = $2; parm_name = ""; } }
          | maybe_ref ID EQ expr
              { { parm_is_ref = $1; parm_expr = $4; parm_name = $2; } }

local_fun_def:
          fun_def
                {
                  match $1 with
                  | (true, _, _) -> raise Parse_error 
                  (* FIXME: give better message *)
                  | (false, _, _ :: _) -> raise Parse_error
                  | (_, x, _) -> {x with fun_kind = Fun_local}
                }
                
fun_defs:
          local_fun_def                        { [$1] }
        | local_fun_def KW_AND fun_defs        { $1 :: $3 }

match_cases:
          match_case                        { [$1] }
        | match_case BAR match_cases        { $1 :: $3 }

maybe_pattern:
          pattern { $1 }
        | /* */   { P_underscore }

pattern:
          qid maybe_pattern             { P_cons (unbound $1, $2) }
        | QUESTION_MARK UNDERSCORE      { P_underscore }
        | UNDERSCORE                    { P_underscore }
        | QUESTION_MARK located_id
                {
                  let (loc, name) = $2 in
                  P_variable {(empty_val ()) with 
                                val_name = name;
                                val_loc = loc;
                                val_kind = Val_pattern; }
                }
        | L_BRACE record_patterns maybe_semicolon R_BRACE       
                { P_record (List.rev $2) }
        | tuple_pattern                         { $1 }
        | L_PAREN pattern R_PAREN               { P_tuple [$2] }

tuple_pattern:
        L_PAREN comma_sep_patterns R_PAREN    { P_tuple (List.rev $2) }

comma_sep_patterns:
          pattern COMMA pattern                 { [$3; $1] }
        | comma_sep_patterns COMMA pattern      { $3 :: $1 }

record_patterns:
          record_patterns SEMICOLON record_pattern        { $3 :: $1 }
        | record_pattern                                  { [$1] }

record_pattern:
          located_id EQ pattern
             {
                  let (loc, name) = $1 in 
                  (unbound name, $3)
             }
             
match_case:
        pattern EQ_MORE expr
           { 
              {
                mc_pattern = $1;
                mc_body = $3;
              }
           }

tymatch_cases:
          tymatch_case                          { [$1] }
        | tymatch_case BAR tymatch_cases        { $1 :: $3 }

tymatch_case:
       tyvars_ty maybe_tymatch_constraints EQ_MORE expr
          {
            let (tv, ty) = $1 in
            {
              tc_tyvars = tv; 
              tc_type = ty;
              tc_constraints = $2;
              tc_body = $4;
            }
          }

tyvars_ty:
          ty                            { ([], $1) }
        | L_BRACE tyvars R_BRACE ty     { ($2, $4) }

maybe_tymatch_constraints:
         /* */                                  { [] }
       | KW_WHERE tymatch_constraints           { $2 }

tymatch_constraints:
          tymatch_constraint                                  { [$1] }
        | tymatch_constraint COMMA tymatch_constraints        { $1 :: $3 }

tymatch_constraint:
          TYVAR COLON_MORE ty
                { Tc_subtype (unbound $1, $3) }
        | TYVAR EQ ty
                { Tc_type_eq (unbound $1, $3) }
