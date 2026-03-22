
open Vm_types.Types
open Unicode
open UTypes

val compile_declarations : Scope.scope -> Unicode.UCStream.istream -> bytecode array
val compile_expression : Scope.scope -> Unicode.UCStream.istream -> bytecode array

