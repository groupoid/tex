
open Unicode.UTypes
open Dim
open Tools.XNum

type ('box, 'cmd) glyph_item = [
  | `Glyph of (GlyphMetric.glyph_desc * FontMetric.font_metric)
  | `Kern of Dim.dim
  | `Penalty of int
  | `Glue of (Tools.XNum.num * bool * bool)
  | `Math of (MathTypes.math_code * 'box)
  | `Box of 'box
  | `Command of 'cmd
]

type ('box, 'cmd) extended_glyph_item = [
  | ('box, 'cmd) glyph_item
  | `Break of (Tools.XNum.num * bool * 'box array * 'box array * 'box array)
]

type hyphen_params = {
  hp_left_hyphen_min  : int;
  hp_right_hyphen_min : int;
  hp_hyphen_table     : Hyphenation.hyphen_table;
  hp_hyphen_penalty   : int;
  hp_ex_hyphen_penalty: int;
  hp_script_lang      : int;
}

type ('box, 'cmd) char_item =
  | Char of uc_char
  | Item of ('box, 'cmd) extended_glyph_item

val hyphenate : hyphen_params -> uc_string -> int list

val convert_to_glyphs : FontMetric.font_metric -> (FontMetric.font_metric, 'box, 'cmd) GlyphMetric.glyph_composer -> ('box, 'cmd) char_item list -> ('box, 'cmd) extended_glyph_item list
val convert_to_glyphs_and_add_breaks : hyphen_params -> FontMetric.font_metric -> (FontMetric.font_metric, 'box, 'cmd) GlyphMetric.glyph_composer -> ('box, 'cmd) char_item list -> ('box, 'cmd) extended_glyph_item list

val strip_composer : ('box, 'cmd) extended_glyph_item -> ('box, 'cmd) extended_glyph_item
val add_lig_kern : bool -> ('box, 'cmd) extended_glyph_item list -> ('box, 'cmd) extended_glyph_item list
val add_lig_kern_iterative_list : bool -> ('box, 'cmd) extended_glyph_item list -> ('box, 'cmd) extended_glyph_item list -> (('box, 'cmd) extended_glyph_item list * int * ('box, 'cmd) extended_glyph_item list)
val add_lig_kern_iterative_array : bool -> ('box, 'cmd) extended_glyph_item list -> int -> int -> ('box, 'cmd) extended_glyph_item array -> (('box, 'cmd) extended_glyph_item list * int)


