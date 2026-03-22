
open Tools.XNum
open Dim
open FontMetric
open GlyphMetric

type vf_glyph = {
  vg_width : num;
  vg_height : num;
  vg_depth : num;
  vg_italic : num;
  vg_commands : FontMetric.simple_box array;
}

and box = font_metric * int

let make_glyph_metric i glyphs = {
  gm_width = glyphs.(i).vg_width;
  gm_height = glyphs.(i).vg_height;
  gm_depth = glyphs.(i).vg_depth;
  gm_italic = glyphs.(i).vg_italic;
  gm_extra = `Normal;
  gm_extra_kern = { ki_after_space = num_zero; ki_before_space = num_zero; ki_after_margin = num_zero; ki_before_margin = num_zero; ki_after_foreign = num_zero; ki_before_foreign = num_zero };
}

let load_virtual_font _ _ = []

let make_virtual_font name size _ _ _ = {
  fm_name = name;
  fm_design_size = size;
  fm_size = size;
  fm_checksum = 0l;
  fm_slant = num_zero;
  fm_space = num_zero;
  fm_space_stretch = num_zero;
  fm_space_shrink = num_zero;
  fm_x_height = num_zero;
  fm_quad = num_zero;
  fm_extra_space = num_zero;
  fm_num1 = num_zero;
  fm_num2 = num_zero;
  fm_num3 = num_zero;
  fm_denom1 = num_zero;
  fm_denom2 = num_zero;
  fm_sup1 = num_zero;
  fm_sup2 = num_zero;
  fm_sup3 = num_zero;
  fm_sub1 = num_zero;
  fm_sub2 = num_zero;
  fm_sup_drop = num_zero;
  fm_sub_drop = num_zero;
  fm_delim1 = num_zero;
  fm_delim2 = num_zero;
  fm_axis_height = num_zero;
  fm_default_rule_thickness = num_zero;
  fm_big_op_spacing1 = num_zero;
  fm_big_op_spacing2 = num_zero;
  fm_big_op_spacing3 = num_zero;
  fm_big_op_spacing4 = num_zero;
  fm_big_op_spacing5 = num_zero;
  fm_char_metrics = [| |];
  fm_hyphen_char = -1;
  fm_skew_char = -1;
  fm_skew_glyph = `Undef;
  fm_type = `Virtual;
}
