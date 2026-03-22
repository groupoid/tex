
open Tools.XNum

type border = Space | Margin | Foreign

type glyph_desc = [ `GlyphIndex of int | `GlyphName of string | `Char of int | `Undef | `Simple of int | `Border of border | `Accent of int * int | `Sequence of glyph_desc list | `Extendable of glyph_desc * glyph_desc * glyph_desc * glyph_desc ]
type lig_kern = [ `Ligature of int * int * bool * bool | `Kern of num | `NoLigKern ]

type ('f, 'box, 'cmd) glyph_composer = {
  gc_get_glyph : 'f -> glyph_desc -> 'f * glyph_desc;
  gc_get_kerning : 'f -> glyph_desc -> glyph_desc -> num;
  gc_get_ligature : 'f -> glyph_desc -> glyph_desc -> glyph_desc option;
  gc_compose : 'f -> glyph_desc list -> 'box list;
}

type extra_kern_info = {
  ki_after_space : num;
  ki_before_space : num;
  ki_after_margin : num;
  ki_before_margin : num;
  ki_after_foreign : num;
  ki_before_foreign : num;
}

type glyph_metric = {
  gm_width : num;
  gm_height : num;
  gm_depth : num;
  gm_italic : num;
  gm_extra : [ `Normal | `LigKern of int | `List of int | `Extendable of int * int * int * int ];
  gm_extra_kern : extra_kern_info;
}

let empty_glyph_metric = {
  gm_width = num_zero;
  gm_height = num_zero;
  gm_depth = num_zero;
  gm_italic = num_zero;
  gm_extra = `Normal;
  gm_extra_kern = {
    ki_after_space = num_zero;
    ki_before_space = num_zero;
    ki_after_margin = num_zero;
    ki_before_margin = num_zero;
    ki_after_foreign = num_zero;
    ki_before_foreign = num_zero;
  }
}

let get_after_kerning gm = function
  | Space -> gm.gm_extra_kern.ki_after_space
  | Margin -> gm.gm_extra_kern.ki_after_margin
  | Foreign -> gm.gm_extra_kern.ki_after_foreign

let get_before_kerning gm = function
  | Space -> gm.gm_extra_kern.ki_before_space
  | Margin -> gm.gm_extra_kern.ki_before_margin
  | Foreign -> gm.gm_extra_kern.ki_before_foreign

let zero_kern_info = {
  ki_after_space = Tools.XNum.num_zero;
  ki_before_space = Tools.XNum.num_zero;
  ki_after_margin = Tools.XNum.num_zero;
  ki_before_margin = Tools.XNum.num_zero;
  ki_after_foreign = Tools.XNum.num_zero;
  ki_before_foreign = Tools.XNum.num_zero
}
