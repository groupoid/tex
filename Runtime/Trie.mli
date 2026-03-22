
open Unicode.UTypes

type 'a trie = {
  t_tree : int array;
  t_data : 'a array;
  mutable t_data_len : int;
}

val empty : unit -> 'a trie
val get_offset : 'a trie -> int -> int
val get_char : 'a trie -> int -> int
val get_data : 'a trie -> int -> 'a option

val lookup : 'a trie -> int -> uc_string -> 'a option
val lookup_prefix : 'a trie -> int -> uc_string -> int -> 'a option
val lookup_list : 'a trie -> int -> uc_list -> 'a option
val lookup_prefix_list : 'a trie -> int -> uc_list -> 'a option

val build : int -> (uc_string * 'a) list -> 'a trie
