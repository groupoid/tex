
open UTypes

type symbol = int

val alloc_symbol : unit -> symbol        (* returns a symbol without string representation *)
val string_to_symbol : uc_string -> symbol
val symbol_to_string : symbol -> uc_string

module SymbolMap : (Map.S with type key = int)
module SymbolSet : (Set.S with type elt = int)

val map_to_list : 'a SymbolMap.t -> (symbol * 'a) list

