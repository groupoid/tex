
open Tools.XNum
open UTypes
open Unicode
open UTypes
open Unicode.UTypes
open Types

type scope = Scope.scope

val tracing_bytecode : bool ref

val make_scope : unit -> scope
val bind_primitive : scope -> string -> partial_value -> unit
val bind_bin_op_l : scope -> string -> int -> partial_value -> unit
val bind_bin_op_n : scope -> string -> int -> partial_value -> unit
val bind_bin_op_r : scope -> string -> int -> partial_value -> unit
val bind_pre_op : scope -> string -> partial_value -> unit
val bind_post_op : scope -> string -> partial_value -> unit
val lookup_symbol : scope -> uc_string -> partial_value

val string_to_symbol : uc_string -> Unicode.SymbolTable.symbol
val symbol_to_string : Unicode.SymbolTable.symbol -> uc_string

val execute_declarations : scope -> Unicode.UCStream.istream -> unit
val evaluate_expression : scope -> Unicode.UCStream.istream -> partial_value
val evaluate_monad_expr :
  scope -> Unicode.UCStream.istream -> partial_value -> partial_value
val evaluate_string_expr : string -> scope -> Unicode.UCStream.istream -> uc_list
val evaluate_function : unknown -> unknown list -> unknown

val evaluate_lin_form : unknown -> unknown Tools.LinForm.lin_form -> unit
val unify : unknown -> unknown -> bool
val set_unknown : unknown -> partial_value -> unit

val uc_string_to_char_list : uc_string -> partial_value
val uc_list_to_char_list : uc_list -> partial_value
val ascii_to_char_list : string -> partial_value
val evaluate_opaque :
  string -> (unknown Tools.Opaque.opaque -> 'a) -> string -> unknown -> 'a
val decode_list : string -> unknown -> unknown list
val decode_string : string -> unknown -> uc_list
val decode_num : string -> unknown -> num

