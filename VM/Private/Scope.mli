
open Unicode.UTypes
open Unicode.SymbolTable
open Vm_types.Types

type scope

val create        : unit -> scope
val copy          : scope -> scope
val symbol_table  : scope -> (uc_string, Lexer.token_class) Hashtbl.t
val add_bin_op    : scope -> int -> Lexer.assoc -> symbol -> unit
val add_pre_op    : scope -> symbol -> unit
val add_post_op   : scope -> symbol -> unit
val add_global    : scope -> symbol -> partial_value -> unit
val shift         : scope -> int -> scope
val push          : scope -> symbol list -> (scope * int)
val lookup_local  : scope -> symbol -> (int * int)
val lookup_global : scope -> symbol -> unknown
val lookup        : scope -> symbol -> bytecode

