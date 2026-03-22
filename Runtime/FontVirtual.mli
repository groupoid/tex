
open Tools.XNum
open Unicode.UTypes

type vf_glyph = {
  vg_width : num;
  vg_height : num;
  vg_depth : num;
  vg_italic : num;
  vg_commands : FontMetric.simple_box array;
}

val load_virtual_font : string -> num -> (int * vf_glyph) list
val make_virtual_font : uc_string -> num -> vf_glyph array -> GlyphMetric.lig_kern array -> FontMetric.font_load_params -> FontMetric.font_metric
