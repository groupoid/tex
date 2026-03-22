
open Tools.XNum
open Dim
open LoadImage
open Unicode.UTypes
open Graphic

type font_format = [ `PK | `TFM| `Type1 | `TrueType | `OpenType | `Virtual ]

type font_parameter = {
  quad : num;
  x_height : num;
  slant : num;
  space : num;
  space_stretch : num;
  space_shrink : num;
  extra_space : num;
}

type font_load_params = {
  flp_size : num;
  flp_magnification : num;
  flp_letter_spacing : num;
  flp_encoding : int array array;
  flp_hyphen_glyph : GlyphMetric.glyph_desc;
  flp_skew_glyph : GlyphMetric.glyph_desc;
  flp_extra_kern : (GlyphMetric.glyph_desc * GlyphMetric.extra_kern_info) list;
  flp_extra_pos : Encodings.glyph_spec_trie;
  flp_extra_subst : Encodings.glyph_spec_trie;
}

type char_metric = {
  cm_char : uc_char;
  cm_glyph : GlyphMetric.glyph_desc;
  cm_width : num;
  cm_height : num;
  cm_depth : num;
  cm_italic : num;
  cm_top_accent : num option;
  cm_bot_accent : num option;
  cm_top_left_kern : num;
  cm_top_right_kern : num;
  cm_bot_left_kern : num;
  cm_bot_right_kern : num;
  cm_lig_kern : (int * GlyphMetric.lig_kern) list;
  cm_extensible : (int * int * int * int) option;
}

type font_metric = {
  fm_name : uc_string;
  fm_design_size : num;
  fm_size : num;
  fm_checksum : int32;
  fm_slant : num;
  fm_space : num;
  fm_space_stretch : num;
  fm_space_shrink : num;
  fm_x_height : num;
  fm_quad : num;
  fm_extra_space : num;
  fm_num1 : num;
  fm_num2 : num;
  fm_num3 : num;
  fm_denom1 : num;
  fm_denom2 : num;
  fm_sup1 : num;
  fm_sup2 : num;
  fm_sup3 : num;
  fm_sub1 : num;
  fm_sub2 : num;
  fm_sup_drop : num;
  fm_sub_drop : num;
  fm_delim1 : num;
  fm_delim2 : num;
  fm_axis_height : num;
  fm_default_rule_thickness : num;
  fm_big_op_spacing1 : num;
  fm_big_op_spacing2 : num;
  fm_big_op_spacing3 : num;
  fm_big_op_spacing4 : num;
  fm_big_op_spacing5 : num;
  fm_char_metrics : char_metric array;
  fm_hyphen_char : int;
  fm_skew_char : int;
  fm_skew_glyph : GlyphMetric.glyph_desc;
  fm_type : font_format;
}

type ('f, 'box, 'cmd) glyph_item =
  | Glyph of (GlyphMetric.glyph_desc * 'f)
  | Char of uc_char
  | Kern of (num * num)
  | Box of 'box
  | Command of 'cmd
  | Break of int

type simple_box = 
  | Empty
  | SimpleGlyph of GlyphMetric.glyph_desc * font_metric
  | Rule of num * num
  | Image of num * num * string * LoadImage.format
  | Group of (num, simple_box) Graphic.graphic_command list
  | Command of simple_cmd
and simple_cmd = [ `DVI_Special of string ]

type page = {
  p_number : int;
  p_width : num;
  p_height : num;
  p_contents : simple_box;
}

val get_glyph : font_metric -> uc_char -> GlyphMetric.glyph_desc
val draw_glyph : font_metric -> GlyphMetric.glyph_desc -> unit
val get_unicode : font_metric -> GlyphMetric.glyph_desc -> uc_string
val default_bitmap_resolution : int ref
val default_mf_mode : string ref

val load_font_metric : font_format -> uc_string -> num -> font_metric
val log_font_metric : font_metric -> unit

val get_glyph_metric : font_metric -> GlyphMetric.glyph_desc -> GlyphMetric.glyph_metric
val get_lig_kern : font_metric -> int -> int -> GlyphMetric.lig_kern
val empty_load_params : font_load_params
val empty_font : font_metric

val get_glyph_composer : font_metric -> Unicode.UTypes.uc_string -> Unicode.SymbolTable.SymbolSet.t -> (font_metric, 'box, 'cmd) GlyphMetric.glyph_composer

