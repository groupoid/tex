
open Unicode
open Unicode.UTypes
open Charmap
open Substitute
open GlyphMetric

val undefined : glyph_desc Charmap.charmap
val uc_to_ot1 : glyph_desc Charmap.charmap
val uc_to_t1 : glyph_desc Charmap.charmap
val uc_to_ott : glyph_desc Charmap.charmap
val uc_to_oms : glyph_desc Charmap.charmap
val uc_to_oml : glyph_desc Charmap.charmap
val fake : glyph_desc Charmap.charmap

val ot1_to_uc : uc_string array
val t1_to_uc : uc_string array
val ott_to_uc : uc_string array
val oms_to_uc : uc_string array
val oml_to_uc : uc_string array

val fake_encoding : uc_string array -> glyph_desc Charmap.charmap
val charmap_encoding : glyph_desc Charmap.charmap -> uc_char -> glyph_desc
val array_decoding : uc_string array -> glyph_desc -> uc_string

val raw_encoding : uc_char -> glyph_desc
val raw_decoding : glyph_desc -> uc_string


module GlyphSpecTrie : sig
  type 'a t
  val empty : 'a t
  val add_list : Unicode.UTypes.uc_list -> 'a -> 'a t -> 'a t
  val is_empty : 'a t -> bool
end

type glyph_spec_trie = int GlyphSpecTrie.t
