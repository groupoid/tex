
open Tools.XNum
open Unicode.UTypes
open Runtime
open Runtime.Substitute
open GlyphMetric
open FontMetric
open Box

type builder_interface =
  { add_char : uc_char -> unit;
    add_break :
      num -> bool -> (box, box) JustHyph.char_item list -> (box, box) JustHyph.char_item list ->
        (box, box) JustHyph.char_item list -> unit;
    add_kern : Dim.dim -> Dim.dim -> unit;
    add_box : box -> unit;
    add_cmd : box -> unit;
    set_font : font_metric -> glyph_composer -> unit;
    set_hyphen_params : JustHyph.hyphen_params -> unit }

type 'a builder = builder_interface * (unit -> 'a)

val add_char : builder_interface -> uc_char -> unit
val add_break :
  builder_interface -> num -> bool -> (box, box) JustHyph.char_item list ->
    (box, box) JustHyph.char_item list -> (box, box) JustHyph.char_item list -> unit
val add_kern : builder_interface -> Dim.dim -> Dim.dim -> unit
val add_box : builder_interface -> box -> unit
val add_cmd : builder_interface -> box -> unit
val set_font : builder_interface -> font_metric -> glyph_composer -> unit
val set_hyphen_params : builder_interface -> JustHyph.hyphen_params -> unit
val add_char_list : builder_interface -> uc_char list -> unit
val add_box_list : builder_interface -> box list -> unit
val add_cmd_list : builder_interface -> box list -> unit

val void_builder : builder_interface
val simple_builder : font_metric -> glyph_composer -> box list builder
