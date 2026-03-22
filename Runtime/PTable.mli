
open Unicode.UTypes

type 'a table

val empty                 : uc_string -> 'a -> 'a table
val is_empty              : 'a table -> bool
val key                   : 'a table -> uc_string
val table                 : 'a table -> 'a Unicode.DynUCTrie.t
val select                : 'a table -> uc_string -> 'a table
val add                   : 'a table -> uc_string -> 'a -> 'a table
val get                   : 'a table -> uc_string -> 'a
val set                   : 'a table -> 'a -> 'a table
val current               : 'a table -> 'a
val sync                  : 'a table -> 'a table
val map                   : ('a -> 'b) -> 'a table -> 'b table
val mapi                  : (uc_string -> 'a -> 'b) -> 'a table -> 'b table
val iter                  : (uc_string -> 'a -> unit) -> 'a table -> unit
val fold                  : (uc_string -> 'a -> 'b -> 'b) -> 'a table -> 'b -> 'b
val update                : 'a table -> 'a Unicode.DynUCTrie.t -> 'a table
