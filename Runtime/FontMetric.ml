
open Tools.XNum
open Dim
open LoadImage
open Unicode.UTypes
open Graphic

type font_format = [ `PK | `TFM| `Type1 | `TrueType | `OpenType | `Virtual ]

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

let empty_font = {
  fm_name = [||];
  fm_design_size = num_zero;
  fm_size = num_zero;
  fm_checksum = Int32.zero;
  fm_slant = num_zero;
  fm_space = num_zero;
  fm_space_stretch = num_zero;
  fm_space_shrink = num_zero;
  fm_x_height = num_zero;
  fm_quad = num_zero;
  fm_extra_space = num_zero;
  fm_num1 = num_zero; fm_num2 = num_zero; fm_num3 = num_zero;
  fm_denom1 = num_zero; fm_denom2 = num_zero;
  fm_sup1 = num_zero; fm_sup2 = num_zero; fm_sup3 = num_zero;
  fm_sub1 = num_zero; fm_sub2 = num_zero;
  fm_sup_drop = num_zero; fm_sub_drop = num_zero;
  fm_delim1 = num_zero; fm_delim2 = num_zero;
  fm_axis_height = num_zero;
  fm_default_rule_thickness = num_zero;
  fm_big_op_spacing1 = num_zero; fm_big_op_spacing2 = num_zero;
  fm_big_op_spacing3 = num_zero; fm_big_op_spacing4 = num_zero;
  fm_big_op_spacing5 = num_zero;
  fm_char_metrics = [||];
  fm_hyphen_char = -1;
  fm_skew_char = -1;
  fm_skew_glyph = `Undef;
  fm_type = `TFM;
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

let default_bitmap_resolution = ref 600
let default_mf_mode = ref "ljfour"

type font_parameter = {
  quad : num;
  x_height : num;
  slant : num;
  space : num;
  space_stretch : num;
  space_shrink : num;
  extra_space : num;
}

let empty_load_params = {
  flp_size = num_zero;
  flp_magnification = num_one;
  flp_letter_spacing = num_zero;
  flp_encoding = [||];
  flp_hyphen_glyph = `Undef;
  flp_skew_glyph = `Undef;
  flp_extra_kern = [];
  flp_extra_pos = Encodings.GlyphSpecTrie.empty;
  flp_extra_subst = Encodings.GlyphSpecTrie.empty;
}

let get_glyph _ _ = `Undef
let get_unicode _ _ = [| |]
let draw_glyph _ _ = ()

let get_glyph _ _ = `Undef
let get_unicode _ _ = [| |]

let load_font_metric _ _ _ = empty_font
let log_font_metric _ = ()
let get_glyph_metric _ _ = GlyphMetric.empty_glyph_metric

let get_lig_kern font c1 c2 =
  try
    let m = font.fm_char_metrics.(c1) in
    List.assoc c2 m.cm_lig_kern
  with _ -> `NoLigKern

let get_glyph_composer _ _ _ =
  let open GlyphMetric in
  {
    gc_get_glyph = (fun f g -> (f, g));
    gc_get_kerning = (fun _ _ _ -> Tools.XNum.num_zero);
    gc_get_ligature = (fun _ _ _ -> None);
    gc_compose = (fun _ _ -> []);
  }

