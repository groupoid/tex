
open Unicode
open Unicode.UTypes

type 'a trie

val empty : 'a trie
val add : 'a trie -> uc_string -> 'a -> 'a trie
val find : 'a trie -> uc_string -> 'a
val mem : 'a trie -> uc_string -> bool
val iter : (uc_string -> 'a -> unit) -> 'a trie -> unit
val fold : (uc_string -> 'a -> 'b -> 'b) -> 'a trie -> 'b -> 'b
