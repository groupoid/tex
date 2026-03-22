
open Tools.XNum
open Runtime
open Runtime.Substitute
open FontMetric
open Box

val attach_accent :
  font_metric -> glyph_desc -> font_metric -> glyph_desc -> box
val vertical_extendable :
  num -> font_metric -> glyph_desc -> glyph_desc -> glyph_desc ->
    glyph_desc -> box
val horizontal_extendable :
  num -> font_metric -> glyph_desc -> glyph_desc -> glyph_desc ->
    glyph_desc -> box

