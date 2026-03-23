
open Tools.XNum
open Runtime
open Dim
open Unicode.UTypes
open Runtime.GlyphMetric
open Runtime.FontMetric
open Logging

open MathTypes
type math_code = MathTypes.math_code
type index_position = MathTypes.index_position
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

type box = basic_box
and box_cmd = [
  | `GfxCmd of gfx_cmd
  | `ParCmd of par_cmd
  | `PageCmd of page_cmd
  | `Special of simple_cmd 
]
and gfx_cmd = (dim, box) Runtime.Graphic.graphic_command
and par_cmd =
    VInsert of bool * box list
  | CallParFunction of (int -> unit)
and floating = uc_string * extended_glyph_item list
and page_cmd =
    SetNextLayout of uc_string
  | SetMark of uc_string * uc_string
  | CallPageFunction of (page_info -> num * num -> unit)
  | Float of floating


and contents_type =
  | CharBox of Runtime.GlyphMetric.glyph_desc * Runtime.FontMetric.font_metric
  | GlueBox of bool * bool
  | RuleBox
  | ImageBox of string * LoadImage.format
  | BreakBox of num * bool * box list * box list * box list
  | ProcBox of (page_info -> num * num -> box -> gfx_cmd list)
  | MathBox of math_code * box
  | CompBox of gfx_cmd list
  | EmptyBox
  | CommandBox of box_cmd

and basic_box =
{
  b_width    : dim;
  b_height   : dim;
  b_depth    : dim;
  b_contents : contents_type
}

and glyph_composer = (Runtime.FontMetric.font_metric, box, box) Runtime.GlyphMetric.glyph_composer
and glyph_item = (box, box) Runtime.JustHyph.glyph_item
and extended_glyph_item = (box, box) Runtime.JustHyph.extended_glyph_item

(* |log_box <box>| prints <box> in human readable form. *)
let rec log_box box = ()
let long_dump_box box = ()
let short_dump_box box = ()
let short_dump_box_list boxes = ()

let empty_box = 
{
  b_width    = dim_zero;
  b_height   = dim_zero;
  b_depth    = dim_zero;
  b_contents = EmptyBox
}

let is_empty_box box = box.b_contents = EmptyBox

let is_math_box box = false

let new_char_box c font =
  let gm = Runtime.FontMetric.get_glyph_metric font (`Char c) in
  {
    b_width    = fixed_dim gm.gm_width;
    b_height   = fixed_dim gm.gm_height;
    b_depth    = fixed_dim gm.gm_depth;
    b_contents = CharBox (`Char c, font)
  }

let new_glyph_box g font =
  let gm = Runtime.FontMetric.get_glyph_metric font g in
  {
    b_width    = fixed_dim gm.gm_width;
    b_height   = fixed_dim gm.gm_height;
    b_depth    = fixed_dim gm.gm_depth;
    b_contents = CharBox (g, font)
  }

let new_rule_box width height depth =
{
  b_width    = width;
  b_height   = height;
  b_depth    = depth;
  b_contents = RuleBox
}

let new_kern_box width height =
{
  b_width    = width;
  b_height   = height;
  b_depth    = dim_zero;
  b_contents = EmptyBox (* Was: SGlyph (`Undef, empty_load_params) *)
}

let new_glue_box width height stretch shrink = 
{
  b_width    = width;
  b_height   = height;
  b_depth    = dim_zero;
  b_contents = GlueBox (stretch, shrink)
}

let new_image_box file format width height depth =
{
  b_width    = width;
  b_height   = height;
  b_depth    = depth;
  b_contents = ImageBox (file, format)
}

let new_break_box penalty is_hyphen pre_break post_break no_break =
{
  b_width    = dim_zero;
  b_height   = dim_zero;
  b_depth    = dim_zero;
  b_contents = BreakBox (penalty, is_hyphen, pre_break, post_break, no_break)
}

let new_command_box cmd =
{
  b_width    = dim_zero;
  b_height   = dim_zero;
  b_depth    = dim_zero;
  b_contents = CommandBox cmd
}

let new_bbox w h d c =
  { b_width = w; b_height = h; b_depth = d; b_contents = c }

let is_char_box box = match box.b_contents with
  | CharBox _ -> true
  | _ -> false

let is_real_box box = match box.b_contents with
  | CharBox _ | RuleBox | MathBox _ | CompBox _ | ImageBox _ -> true
  | _ -> false

let is_discardable_glue box = match box.b_contents with
  | GlueBox _ -> true
  | _ -> false


let new_math_box code box =
{
  b_width    = box.b_width;
  b_height   = box.b_height;
  b_depth    = box.b_depth;
  b_contents = MathBox (code, box)
}

let rec extended_item_to_box = function
  | `Box b -> b
  | `Glyph (g, f) -> new_glyph_box g f
  | `Kern d -> new_rule_box d dim_zero dim_zero
  | `Glue (w, st, sh) -> new_glue_box (fixed_dim w) dim_zero st sh
  | `Penalty _ -> empty_box
  | `Math (c, b) -> new_math_box c b
  | `Break (w, is_hyphen, pre, post, no) -> new_break_box w is_hyphen (Array.to_list pre) (Array.to_list post) (Array.to_list no)
  | `Command b -> b

let box_to_extended_item b = `Box b

let discard_glue boxes = ([], boxes)

let remove_breaks boxes = List.filter (fun b -> match b.b_contents with BreakBox _ -> false | _ -> true) boxes

let simple_item_to_box item = match item with
  | `Glyph (g, f) -> new_glyph_box g f
  | `Kern x -> new_rule_box x dim_zero dim_zero
  | `Box b -> b
  | _ -> empty_box

let rec draw_box page_info x y box =
  match box.b_contents with
  | CharBox (g, f) -> Runtime.FontMetric.SimpleGlyph (g, f)
  | RuleBox -> 
      let w = box.b_width.d_base in
      let h = box.b_height.d_base in
      let d = box.b_depth.d_base in
      Runtime.FontMetric.Rule (w, add_num h d)
  | ImageBox (f, fmt) ->
      Runtime.FontMetric.Image (box.b_width.d_base, add_num box.b_height.d_base box.b_depth.d_base, f, fmt)
  | MathBox (_, b) -> draw_box page_info x y b
  | CompBox cmds ->
      let conv_path p =
        List.map (fun (ax, ay, bx, by, cx, cy, dx, dy) ->
          (ax.d_base, ay.d_base, bx.d_base, by.d_base,
           cx.d_base, cy.d_base, dx.d_base, dy.d_base)) p in
      let conv_cmd cmd = match cmd with
        | Runtime.Graphic.PutBox (dx, dy, b, opt) ->
            Runtime.Graphic.PutBox (dx.d_base, dy.d_base, (draw_box page_info (x +/ dx.d_base) (y +/ dy.d_base) b), opt)
        | Runtime.Graphic.SetColour col     -> Runtime.Graphic.SetColour col
        | Runtime.Graphic.SetBgColour col   -> Runtime.Graphic.SetBgColour col
        | Runtime.Graphic.SetAlpha a        -> Runtime.Graphic.SetAlpha a
        | Runtime.Graphic.Draw (pc, p)      -> Runtime.Graphic.Draw (pc, conv_path p)
        | Runtime.Graphic.Fill (pc, p)      -> Runtime.Graphic.Fill (pc, conv_path p)
        | Runtime.Graphic.Clip p            -> Runtime.Graphic.Clip (conv_path p)
        | Runtime.Graphic.SetLineWidth w    -> Runtime.Graphic.SetLineWidth w.d_base
        | Runtime.Graphic.SetLineCap c      -> Runtime.Graphic.SetLineCap c
        | Runtime.Graphic.SetLineJoin j     -> Runtime.Graphic.SetLineJoin j
        | Runtime.Graphic.SetMiterLimit l   -> Runtime.Graphic.SetMiterLimit l
      in
      Runtime.FontMetric.Group (List.map conv_cmd cmds)
  | _ -> Runtime.FontMetric.Empty
let new_compound_box width height depth contents =
{
  b_width    = width;
  b_height   = height;
  b_depth    = depth;
  b_contents = CompBox contents
}

let is_compound_box box = match box.b_contents with
  | CompBox _ -> true
  | _         -> false

let wrap_in_compound_box box = match box.b_contents with
  | CompBox _ -> box
  | _         ->
      new_compound_box box.b_width box.b_height box.b_depth [Runtime.Graphic.PutBox (dim_zero, dim_zero, box, None)]

let add_to_compound_box box gfx_cmds =
  match box.b_contents with
  | CompBox cmds -> new_compound_box box.b_width box.b_height box.b_depth (gfx_cmds @ cmds)
  | _            -> new_compound_box
                      box.b_width box.b_height box.b_depth
                      (Runtime.Graphic.PutBox (dim_zero, dim_zero, box, None) :: gfx_cmds)

let rec scale_box_horiz box ratio =
  match box.b_contents with
  | CompBox contents ->
      new_compound_box
        (dim_scale box.b_width ratio)
        box.b_height
        box.b_depth
        (List.map (scale_graphics ratio) contents)
  | _ -> { box with b_width = dim_scale box.b_width ratio }

and scale_box_horiz_upto box ratio =
  match box.b_contents with
  | CompBox contents ->
      new_compound_box
        (dim_scale_upto box.b_width ratio)
        box.b_height
        box.b_depth
        (List.map (scale_graphics ratio) contents)
  | _ -> { box with b_width = dim_scale_upto box.b_width ratio }

and scale_box_vert box ratio =
  match box.b_contents with
  | CompBox contents ->
      new_compound_box
        box.b_width
        (dim_scale box.b_height ratio)
        (dim_scale box.b_depth  ratio)
        (List.map (scale_graphics ratio) contents)
  | _ -> { box with
           b_height = dim_scale box.b_height ratio;
           b_depth  = dim_scale box.b_depth  ratio }

and scale_box_vert_upto box ratio =
  match box.b_contents with
  | CompBox contents ->
      new_compound_box
        box.b_width
        (dim_scale_upto box.b_height ratio)
        (dim_scale_upto box.b_depth  ratio)
        (List.map (scale_graphics ratio) contents)
  | _ -> { box with
           b_height = dim_scale_upto box.b_height ratio;
           b_depth  = dim_scale_upto box.b_depth  ratio }

and shift_compound_vert box shift =
  match box.b_contents with
  | CompBox contents ->
      new_compound_box
        box.b_width
        (dim_add box.b_height shift)
        (dim_sub box.b_depth  shift)
        (List.map (shift_gfx shift) contents)
  | _ -> box (* Or raise exception *)

and scale_graphics ratio cmd =
  match cmd with
  | Runtime.Graphic.PutBox (x, y, box, name) ->
      Runtime.Graphic.PutBox (dim_scale x ratio, dim_scale y ratio, scale_box_horiz box ratio, name)
  | _ -> cmd

and shift_gfx shift cmd =
  match cmd with
  | Runtime.Graphic.PutBox (x, y, box, name) ->
      Runtime.Graphic.PutBox (x, dim_add y shift, shift_compound_vert box shift, name)
  | _ -> cmd

