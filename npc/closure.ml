open Ast
open Util
open Cg_util

type t =
  {
    cl_fun : function_decl;
    mutable cl_values : value_decl list;
    mutable cl_funvals : function_decl list;
    mutable cl_this_td : type_decl option;
  }

let closures = Stack.create ()

let create f =
  let c = { cl_fun = f; cl_values = []; cl_funvals = []; cl_this_td = None } in
  Stack.push c closures;
  let cl = xf "__N__closure_class_%d" f.fun_id in
  out (xf "%s __N__closure = new %s();" cl cl);
  c

let iter f =
  Stack.iter f closures
 
let unsome = function
  | Some x -> x
  | None -> ice "closure: got none"
  
let add_val c v =
  if v.val_in_closure then
    let c = unsome c in
    c.cl_values <- v :: c.cl_values
  else
    out (xf "%s %s;" (ty (unsome v.val_type)) (val_name v))
    
let add_fun_val c v =
  if v.fun_in_closure then
    let c = unsome c in
    c.cl_funvals <- v :: c.cl_funvals
  else
    out (xf "%s %s;" (ty v.fun_type) (fun_name v))

let push c =
  out (xf "class __N__closure_class_%d {" c.cl_fun.fun_id);
  let push_val v =
    out (xf "public %s %s;" (ty (unsome v.val_type)) (val_name v))
  in
  let push_fun f =
    out (xf "public %s %s;" (ty f.fun_type) (fun_name f))
  in
  List.iter push_val c.cl_values;
  List.iter push_fun c.cl_funvals;
  out "} // end clo\n"

let push_all () =
  iter push
  
let add_this_pointer c td =
  match c with
  | Some c ->
    c.cl_this_td <- Some td;
    out  "__N__closure.__N__this = this;"
  | None -> ()
