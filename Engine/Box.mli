
open Tools.XNum
open Unicode.UTypes
open Runtime.Substitute
open Runtime.FontMetric
open Runtime.Dim
open Runtime.Graphic
open Runtime.GlyphMetric
open Runtime.LoadImage
open Runtime.JustHyph


open Runtime.MathTypes
type math_code = Runtime.MathTypes.math_code
type index_position = Runtime.MathTypes.index_position
type glyph_desc = Runtime.GlyphMetric.glyph_desc

type page_info =
  { pi_page_no : int;
    pi_width : num;
    pi_height : num;
    pi_depth : num;
    pi_top_margin : num;
    pi_bottom_margin : num;
    pi_left_margin : num;
    pi_right_margin : num }

(* basic box *)

type box = basic_box
and basic_box =
{
  b_width    : dim;
  b_height   : dim;
  b_depth    : dim;
  b_contents : contents_type }

and contents_type = 
  | CharBox of Runtime.GlyphMetric.glyph_desc * Runtime.FontMetric.font_metric
  | GlueBox of bool * bool
  | RuleBox
  | ImageBox of string * format
  | BreakBox of num * bool * box list * box list * box list
  | ProcBox of (page_info -> num * num -> box -> gfx_cmd list)
  | MathBox of math_code * box
  | CompBox of gfx_cmd list
  | EmptyBox
  | CommandBox of box_cmd
and box_cmd = [
  | `GfxCmd of gfx_cmd
  | `ParCmd of par_cmd
  | `PageCmd of page_cmd
  | `Special of simple_cmd 
]
and gfx_cmd = (dim, box) graphic_command
and par_cmd =
    VInsert of bool * box list
  | CallParFunction of (int -> unit)
and floating = uc_string * extended_glyph_item list
and page_cmd =
    SetNextLayout of uc_string
  | SetMark of uc_string * uc_string
  | CallPageFunction of (page_info -> num * num -> unit)
  | Float of floating



and glyph_item = (box, box) Runtime.JustHyph.glyph_item
and extended_glyph_item = (box, box) Runtime.JustHyph.extended_glyph_item

type glyph_composer = (Runtime.FontMetric.font_metric, box, box) Runtime.GlyphMetric.glyph_composer

val empty_box : box
val is_empty_box : box -> bool

val is_real_box      : box -> bool
val is_discardable_glue : box -> bool
val is_char_box           : box -> bool

val new_glyph_box : Runtime.GlyphMetric.glyph_desc -> Runtime.FontMetric.font_metric -> box
val new_char_box : uc_char -> Runtime.FontMetric.font_metric -> box
val new_glue_box : dim -> dim -> bool -> bool -> box
val new_kern_box : dim -> dim -> box
val new_rule_box : dim -> dim -> dim -> box

val new_compound_box : dim -> dim -> dim -> gfx_cmd list -> box
val is_compound_box  : box -> bool
val wrap_in_compound_box : box -> box
val add_to_compound_box : box -> gfx_cmd list -> box
val scale_box_horiz : box -> num * int -> box
val scale_box_vert : box -> num * int -> box
val shift_compound_vert : box -> dim -> box
val scale_box_horiz_upto : box -> num * int -> box
val scale_box_vert_upto : box -> num * int -> box

val new_image_box : string -> Runtime.LoadImage.format -> dim -> dim -> dim -> box

val new_math_box : math_code -> box -> box
val is_math_box : box -> bool

val new_break_box : num -> bool -> box list -> box list -> box list -> box

val new_command_box : box_cmd -> box

val discard_glue : box list -> box list * box list
val remove_breaks : box list -> box list
val simple_item_to_box : glyph_item -> box
val extended_item_to_box : extended_glyph_item -> box
val box_to_extended_item : box -> extended_glyph_item
val draw_box : page_info -> num -> num -> box -> Runtime.FontMetric.simple_box
val log_box : box -> unit
val long_dump_box : box -> unit
val short_dump_box : box -> unit
val short_dump_box_list : box list -> unit


