
open Tools.XNum
open Vm_types.Types
open Unicode
open UTypes
open Unicode.UTypes
open Unicode.SymbolTable

module Environment : sig type environment = unknown array list end

val tracing_bytecode : bool ref

val add_unknowns : unknown -> unknown -> unknown -> unit
val sub_unknowns : unknown -> unknown -> unknown -> unit
val mul_unknowns : unknown -> unknown -> unknown -> unit
val div_unknowns : unknown -> unknown -> unknown -> unit
val prim_add : unknown -> unknown -> partial_value
val prim_sub : unknown -> unknown -> partial_value
val prim_mul : unknown -> unknown -> partial_value
val prim_div : unknown -> unknown -> partial_value

val forced_unify : unknown -> unknown -> unit
val unify : unknown -> unknown -> bool
val execute : bytecode array -> unknown list -> unknown

val evaluate_lin_form : unknown -> unknown Tools.LinForm.lin_form -> unit
val evaluate_num : string -> unknown -> num
val evaluate_list : string -> unknown -> unknown list
val evaluate_opaque :
  string -> (unknown Tools.Opaque.opaque -> 'a) -> string -> unknown -> 'a

val print_pattern : pattern_check -> unit
val print_partial : int -> partial_value -> unit
val print_bytecode : int -> bytecode array -> unit


