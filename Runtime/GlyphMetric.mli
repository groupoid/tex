
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

val empty_glyph_metric : glyph_metric

val get_after_kerning : glyph_metric -> border -> num
val get_before_kerning : glyph_metric -> border -> num

val zero_kern_info : extra_kern_info
