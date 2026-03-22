
open Tools.XNum
open Unicode
open UTypes
open Unicode.UTypes
open Unicode.SymbolTable

module UString = Unicode.UString

exception Syntax_error of Unicode.location * uc_string
exception Runtime_error of uc_string

let runtime_error msg = raise (Runtime_error (Array.of_list (UString.of_ascii msg)))

type unknown = partial_value ref

and partial_value =
  | Unbound
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
  | BDup
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
  | PCAnything
  | PCVariable of int
  | PCNumber of num
  | PCChar of uc_char
  | PCSymbol of symbol
  | PCTuple of int
  | PCNil
  | PCConsList
  | PCAssign of int

and environment = unknown array list

let create_unbound () = ref Unbound
let create_unknown v = ref v

let identical x y =
  if x == y then
    true
  else match (!x, !y) with
    | (Constraint c, Constraint d) -> c == d
    | _                            -> false

let compare_unknowns x y = match (!x, !y) with
  | (Constraint c, Constraint d) ->
      let a = List.hd c in
      let b = List.hd d in
      if a == b then
        Tools.LinForm.Eq
      else if a < b then
        Tools.LinForm.Lt
      else
        Tools.LinForm.Gt
  | _ ->
      if x == y then
        Tools.LinForm.Eq
      else if x < y then
        Tools.LinForm.Lt
      else
        Tools.LinForm.Gt

let rec add_constraint x cs = match cs with
  | [] -> [x]
  | y::ys -> match compare_unknowns x y with
      | Tools.LinForm.Lt -> x :: cs
      | Tools.LinForm.Gt -> y :: add_constraint x ys
      | Tools.LinForm.Eq -> cs

let merge_constraints c0 c1 =
  if c0 == c1 then
    c0
  else
    let rec iter c0 c1 = match (c0, c1) with
      | ([], _) -> c1 (* corrected from c0 if empty *)
      | (_, []) -> c0
      | (x::xs, y::ys) -> match compare_unknowns x y with
          | Tools.LinForm.Lt -> x :: iter xs c1
          | Tools.LinForm.Gt -> y :: iter c0 ys
          | Tools.LinForm.Eq -> x :: iter xs ys
    in iter c0 c1

let type_name x = match x with
  | Unbound       -> "<unbound>"
  | Constraint _  -> "<constraint>"
  | Bool _        -> "bool"
  | Number _      -> "number"
  | Char _        -> "character"
  | Symbol _      -> "symbol"
  | LinForm _     -> "linear form"
  | Primitive1 _  -> "function"
  | Primitive2 _  -> "function"
  | PrimitiveN _    -> "function"
  | Function _    -> "function"
  | Chain _           -> "function"
  | Relation _      -> "relation"
  | Application _ -> "<application>"
  | Nil           -> "nil"
  | List _        -> "list"
  | Tuple _       -> "tuple"
  | Dictionary _  -> "dictionary"
  | Opaque y      -> Tools.Opaque.type_name y

