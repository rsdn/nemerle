open Ast
open Util

let do_bindings = ref false

let named_type n args =
  if !do_bindings then  
    try
      match Env.lookup Env.global n with
      | [D_type td] ->
        T_app ({b_name = n; b_namespace = ""; b_value = B_bound td}, args)
      | _ -> ice (xf "named_type: %s ain't type" n)
    with Recovery ->
      ice (xf "named_type: name %s is unbound" n)
  else
    T_app ({b_name = n; b_namespace = ""; b_value = B_unbound}, args)
  
let string_ty () =
  named_type "Nemerle.Core.string" []
  
let float_ty () =
  named_type "Nemerle.Core.float" []
  
let int_ty () =
  named_type "Nemerle.Core.int" []

let bool_ty () =
  named_type "Nemerle.Core.bool" []

let exn_ty () =
  named_type "System.Exception" []

let false_ty () =
  named_type "Nemerle.Core.bool.False" []
  
let true_ty () =
  named_type "Nemerle.Core.bool.True" []
