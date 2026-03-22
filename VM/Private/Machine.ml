
(* open Tools.XNum *)
open UTypes
open Types
let tracing_bytecode = Evaluate.tracing_bytecode

(* scopes *)

type scope = Scope.scope

let make_scope     = Primitives.initial_scope
let bind_primitive = Primitives.bind_primitive
let bind_bin_op_l  = Primitives.bind_bin_op_l
let bind_bin_op_n  = Primitives.bind_bin_op_n
let bind_bin_op_r  = Primitives.bind_bin_op_r
let bind_pre_op    = Primitives.bind_pre_op
let bind_post_op   = Primitives.bind_post_op

(* symbols *)

let string_to_symbol = Unicode.SymbolTable.string_to_symbol
let symbol_to_string = Unicode.SymbolTable.symbol_to_string

(* evaluation *)

let uc_string_to_char_list = Primitives.uc_string_to_char_list
let uc_list_to_char_list   = Primitives.uc_list_to_char_list
let ascii_to_char_list     = Primitives.ascii_to_char_list

let execute_declarations scope decls  =
  let code = Compile.compile_declarations scope decls in
  ignore (Evaluate.execute code [ref Unbound])

let evaluate_lin_form      = Evaluate.evaluate_lin_form
let evaluate_opaque        = Evaluate.evaluate_opaque
let unify                  = Evaluate.unify

let set_unknown x v =
  Evaluate.forced_unify x (ref v)

let evaluate_expression scope stream =
  let code = Compile.compile_expression scope stream in
  let x = Evaluate.execute code [] in
  !x

let evaluate_function f args =
  Evaluate.execute [| BGlobal f; BApply (List.length args) |] args

let evaluate_monad_expr scope stream init =
  let code = Compile.compile_expression scope stream in
  let f = Evaluate.execute code [] in
  let x = evaluate_function f [ref init] in
  !x

let decode_string = Primitives.evaluate_char_list
let decode_list   = Evaluate.evaluate_list
let decode_num    = Evaluate.evaluate_num

let evaluate_string_expr name scope stream =
  decode_string name (ref (evaluate_expression scope stream))

let lookup_symbol scope sym =
  let x = Scope.lookup_global scope (string_to_symbol sym) in
  !x
