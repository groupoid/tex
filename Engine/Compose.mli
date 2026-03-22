
open Runtime
open Runtime.Substitute
open FontMetric
open GlyphMetric
open Box

val discard_glue :
  extended_glyph_item list ->
    extended_glyph_item list * extended_glyph_item list
val discard_glue_array :
  int -> int -> extended_glyph_item array ->
    extended_glyph_item list * int

val just_hyph_builder :
  Runtime.FontMetric.font_metric -> Box.glyph_composer -> Runtime.JustHyph.hyphen_params  ->
    (Box.box list) Builder.builder
val ligature_builder :
  Runtime.FontMetric.font_metric -> Box.glyph_composer -> Runtime.JustHyph.hyphen_params  ->
    (Box.box list) Builder.builder
val char_item_builder :
  Runtime.FontMetric.font_metric -> Box.glyph_composer -> Runtime.JustHyph.hyphen_params  ->
    ((Box.box, Box.box) Runtime.JustHyph.char_item list) Builder.builder
val hyph_only_builder :
  Runtime.FontMetric.font_metric -> Box.glyph_composer -> Runtime.JustHyph.hyphen_params  ->
    (Box.extended_glyph_item list) Builder.builder
val glyph_item_builder :
  Runtime.FontMetric.font_metric -> Box.glyph_composer -> Runtime.JustHyph.hyphen_params  ->
    (Box.extended_glyph_item list) Builder.builder

val box_add_lig_kern : box list -> box list
