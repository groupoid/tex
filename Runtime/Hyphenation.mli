
open Unicode.UTypes

type hyphen_table = {
  ht_left_hyphen_min : int;
  ht_right_hyphen_min : int;
  ht_pattern_trie : int array Trie.trie;
  ht_exception_trie : (int list) Trie.trie;
  ht_char_classes : int Unicode.Charmap.charmap;
  ht_pattern_start : int;
}

val load_hyphen_table : string -> hyphen_table
val hyphenate : hyphen_table -> uc_string -> int list
