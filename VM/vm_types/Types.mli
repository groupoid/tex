
open Tools.XNum
open Unicode.UTypes
open Unicode.SymbolTable

exception Syntax_error of Unicode.location * uc_string
exception Runtime_error of uc_string

val runtime_error : string -> 'a

(*
type compare = Equal | Less | LessEqual | Greater | GreaterEqual | Inequal 

type relation = unit (* FIX *)
*)

type unknown = partial_value ref
and partial_value =
    Unbound
  | Constraint of unknown list
  | Bool of bool
  | Number of num
  | Char of uc_char
  | Symbol of symbol
  | LinForm of unknown Tools.LinForm.lin_form
  | Primitive1 of (unknown -> partial_value)
  | Primitive2 of (unknown -> unknown -> partial_value)
  | PrimitiveN of int * (unknown list -> partial_value)
  | Function of environment * int * bytecode array
  | Chain of environment * bytecode array
  | Relation of int * bytecode array
  | Application of partial_value * int * unknown list
  | Nil
  | List of unknown * unknown
  | Tuple of unknown array
  | Dictionary of unknown SymbolMap.t
  | Opaque of unknown Tools.Opaque.opaque
and bytecode =
    BDup
  | BPop
  | BPopN of int
  | BConst of partial_value
  | BGlobal of unknown
  | BVariable of int * int
  | BFunction of int * bytecode array
  | BDictionary of symbol array
  | BPair
  | BTuple of int
  | BSet of int * int
  | BApply of int
  | BReturn
  | BCondJump of int
  | BJump of int
  | BLocal of int
  | BEndLocal
  | BMatch1 of pattern_check list * int * int * int
  | BMatchN of pattern_check list array * int * int * int
  | BUnify
  | BRaise of string
and pattern_check =
    PCAnything
  | PCVariable of int
  | PCNumber of num
  | PCChar of uc_char
  | PCSymbol of symbol
  | PCTuple of int
  | PCNil
  | PCConsList
  | PCAssign of int
and environment = unknown array list

val identical : unknown -> unknown -> bool
val compare_unknowns : unknown -> unknown -> Tools.LinForm.compare_result

val create_unbound : unit -> unknown
val create_unknown : partial_value -> unknown
val add_constraint : unknown -> unknown list -> unknown list
val merge_constraints : unknown list -> unknown list -> unknown list

val type_name : partial_value -> string

