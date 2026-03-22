
open Tools.XNum
open Unicode
open UTypes
open Unicode.UTypes
open Unicode.SymbolTable
open Lexer

(*
  Priorities:

   2 right ||
   3 right &&
   4 non   == <> > < >= <=
   5 left  land lor lxor lsr lsl
   6 left  + -
   7 left  * / mod
   8 left  ^
   9 left  function application
  11 right prefix operations
  12 left  postfix operations

*)

type pattern =
    PAnything
  | PId of symbol
  | PNumber of num
  | PChar of uc_char
  | PSymbol of symbol
  | PTuple of pattern list
  | PList of pattern list
  | PListTail of pattern list * pattern
  | PAssign of symbol * pattern

type term =
    TUnbound
  | TId of symbol
  | TNumber of num
  | TChar of uc_char
  | TSymbol of symbol
  | TApp of term * term list
  | TTuple of term list
  | TList of term list
  | TListTail of term list * term
  | TFun of (pattern list * term option * term) list
  | TLocal of decl list * term
  | TSequence of stmt list * term
  | TDo of stmt list
  | TIfThenElse of term * term * term
  | TMatch of term * (pattern * term option * term) list
and decl =
    DFun of symbol * pattern list * term option * term
  | DPattern of pattern * term
and stmt =
    SEquation of term * term
  | SIfThen of term * stmt
  | SIfThenElse of term * stmt * stmt
  | SFunction of term

val parse_program : lexer -> decl list
val parse_expression : lexer -> term

