open Tools.XNum
open Runtime
open Dim
open Box

module ListBuilder = Tools.ListBuilder

type line_params =
{
  baseline_skip      : dim;               (* amount of glue between baselines           *)
  line_skip_limit    : num;               (* minimal amout of glue between lines        *)
  line_skip          : dim;               (* the amout if the lines are closer together *)
  leading            : box -> box -> line_params -> dim;
  club_widow_penalty : int -> int -> num  (* penalty between two lines                  *)
}

type space_params =
{
  space_factor      : num;
  space_skip        : dim option;
  xspace_skip       : dim option;
  victorian_spacing : bool
}

type graphics_params =
{
  gp_colour    : Graphic.colour;
  gp_bg_colour : Graphic.colour;
  gp_alpha     : num
}

type galley =
{
  lines                     : box list;      (* list of all lines and glue in reverse order *)
  glue                      : box list;      (* glue at the end of the galley               *)
  measure                   : num;           (* the width of the galley                     *)
  graphics_params           : graphics_params;
  current_par_params        : ParLayout.par_params;
  current_line_params       : line_params;
  current_line_break_params : ParLayout.line_break_params;
  current_hyphen_params     : JustHyph.hyphen_params;
  current_space_params      : space_params;
  current_math_params       : MathLayout.math_params;
  par_params                : ParLayout.par_params;
  line_params               : line_params;
  line_break_params         : ParLayout.line_break_params;
  hyphen_params             : JustHyph.hyphen_params;
  space_params              : space_params;
  math_params               : MathLayout.math_params
}

(*
  |skyline_dist <line 1> <line 2>| determines the distance between the baselines of <line 1>
  and <line 2> if they were set without glue between them.
*)
let skyline_dist line1 line2 =
  let rec dist off b1 b2 = match (b1.b_contents, b2.b_contents) with
    | (CompBox c1, CompBox _)  -> dist_comp_comp     off c1 b2
    | (CompBox c1, _)          -> dist_comp_simple   off c1 b2
    | (_,          CompBox c2) -> dist_simple_comp   off b1 c2
    | (_,          _)          -> dist_simple_simple off b1 b2
  and dist_simple_simple off b1 b2 =
    if off >=/ b2.b_width.d_base then
      dim_max b1.b_depth b2.b_height    (* boxes do not intersect *)
    else
      dim_add b1.b_depth b2.b_height
  and dist_simple_comp off b1 c2 = match c2 with
    | [] -> b1.b_depth
    | PutBox (x, y, b, _) :: cs ->
        let d = dim_add (dist (off +/ x.d_base) b1 b) y in
        dim_max d (dist_simple_comp off b1 cs)
    | _ :: cs -> dist_simple_comp off b1 cs
  and dist_comp_simple off c1 b2 = match c1 with
    | [] -> b2.b_height
    | PutBox (x, y, b, _) :: cs ->
        let d = dim_sub (dist (off -/ x.d_base) b b2) y in
        dim_max d (dist_comp_simple off cs b2)
    | _ :: cs -> dist_comp_simple off cs b2
  and dist_comp_comp off c1 b2 = match c1 with
    | [] -> dim_zero
    | PutBox (x, y, b, _) :: cs ->
        let d = dim_sub (dist (off -/ x.d_base) b b2) y in
        dim_max d (dist_comp_comp off cs b2)
    | _ :: cs -> dist_comp_comp off cs b2
  in
  dist num_zero line1 line2

(*
  |leading_<version> <line 1> <line 2> <line-params>| determines the amount of glue that has to be inserted
  between two lines.
*)
let leading_fixed line1 line2 line_params =
  let dist = dim_add line1.b_depth line2.b_height in
  dim_sub line_params.baseline_skip dist

let leading_register line1 line2 line_params =
  let dist = dim_add line1.b_depth line2.b_height in
  let fac  = ceiling_num ((dist.d_base +/ line_params.line_skip_limit)
                        // line_params.baseline_skip.d_base) in
  dim_sub (fixed_dim (fac */ line_params.baseline_skip.d_base)) dist

let leading_TeX line1 line2 line_params =
  let dist = dim_add line1.b_depth line2.b_height in
  if line_params.baseline_skip.d_base >=/
     dist.d_base +/ line_params.line_skip_limit then
    dim_sub line_params.baseline_skip dist
  else
    line_params.line_skip

let leading_skyline line1 line2 line_params =
  let simple_dist = dim_add line1.b_depth line2.b_height in
  if line_params.baseline_skip.d_base >=/
     simple_dist.d_base +/ line_params.line_skip_limit then
    dim_sub line_params.baseline_skip simple_dist
  else (
    let dist = skyline_dist line1 line2 in
    if line_params.baseline_skip.d_base >=/ dist.d_base +/ line_params.line_skip_limit then
      dim_sub line_params.baseline_skip simple_dist
    else
      dim_sub line_params.line_skip (dim_sub simple_dist dist)
  )

(* galleys *)
let new_galley measure line_params par_params line_break_params hyphen_params space_params math_params =
{
  lines                     = [];
  glue                      = [];
  measure                   = measure;
  par_params                = { par_params with ParLayout.measure = measure };
  line_params               = line_params;
  line_break_params         = line_break_params;
  hyphen_params             = hyphen_params;
  space_params              = space_params;
  math_params               = math_params;
  current_par_params        = { par_params with ParLayout.measure = measure };
  current_line_params       = line_params;
  current_line_break_params = line_break_params;
  current_hyphen_params     = hyphen_params;
  current_space_params      = space_params;
  current_math_params       = math_params;
  graphics_params           =
    {
      gp_colour    = `Grey num_zero;
      gp_bg_colour = `Grey num_one;
      gp_alpha     = num_zero
    }
}

let galley_lines galley = List.rev (galley.glue @ galley.lines)
let get_line galley i = List.nth (galley.glue @ galley.lines) i

let keep_lines galley lines =
  {
    galley with
    lines = List.rev lines
  }

let last_line galley = match galley.lines with
  | []     -> empty_box
  | b :: _ -> b

let modify_glue galley f =
  {
    galley with
    glue = f galley.glue
  }

let measure                   galley = galley.measure
let graphics_params           galley = galley.graphics_params
let par_params                    galley = galley.par_params
let line_params                   galley = galley.line_params
let line_break_params             galley = galley.line_break_params
let hyphen_params                 galley = galley.hyphen_params
let space_params                  galley = galley.space_params
let math_params                   galley = galley.math_params
let current_par_params            galley = galley.current_par_params
let current_line_params           galley = galley.current_line_params
let current_line_break_params     galley = galley.current_line_break_params
let current_hyphen_params         galley = galley.current_hyphen_params
let current_space_params          galley = galley.current_space_params
let current_math_params           galley = galley.current_math_params

let set_line_params               galley p = { galley with line_params               = p }
let set_line_break_params         galley p = { galley with line_break_params         = p }
let set_hyphen_params             galley p = { galley with hyphen_params             = p }
let set_space_params              galley p = { galley with space_params              = p }
let set_math_params               galley p = { galley with math_params               = p }
let set_graphics_params           galley p = { galley with graphics_params           = p }
let set_current_line_params       galley p = { galley with current_line_params       = p }
let set_current_line_break_params galley p = { galley with current_line_break_params = p }
let set_current_hyphen_params     galley p = { galley with current_hyphen_params     = p }
let set_current_space_params      galley p = { galley with current_space_params      = p }
let set_current_math_params       galley p = { galley with current_math_params       = p }

let set_par_params galley p =
  {
    galley with
    par_params = { p with ParLayout.measure = galley.measure }
  }

let set_current_par_params galley p =
  {
    galley with
    current_par_params = { p with ParLayout.measure = galley.measure }
  }

let copy_params galley from_galley =
  {
    galley with
    graphics_params           = from_galley.graphics_params;
    par_params                = { from_galley.par_params with ParLayout.measure = galley.measure };
    line_params               = from_galley.line_params;
    line_break_params         = from_galley.line_break_params;
    hyphen_params             = from_galley.hyphen_params;
    space_params              = from_galley.space_params;
    math_params               = from_galley.math_params;
    current_par_params        = { from_galley.current_par_params with ParLayout.measure = galley.measure };
    current_line_params       = from_galley.current_line_params;
    current_line_break_params = from_galley.current_line_break_params;
    current_hyphen_params     = from_galley.current_hyphen_params;
    current_space_params      = from_galley.current_space_params;
    current_math_params       = from_galley.current_math_params
  }

let reset_params galley =
  {
    galley with
    current_par_params        = { galley.par_params with ParLayout.measure = galley.measure };
    current_line_params       = galley.line_params;
    current_line_break_params = galley.line_break_params;
    current_hyphen_params     = galley.hyphen_params;
    current_space_params      = galley.space_params;
    current_math_params       = galley.math_params
  }

(* add a line to the galley *)
let add_line galley line =
  Printf.printf "[Galley] add_line: adding box type\n%!";
  (* We need to keep track of graphic state changes. *)
  let update_gfx_cmds ((fg, bg, alpha) as gfx) c = match c with
    | Graphic.SetColour   _ -> (Some c, bg, alpha)
    | Graphic.SetBgColour _ -> (fg, Some c, alpha)
    | Graphic.SetAlpha    _ -> (fg, bg, Some c)
    | _                     -> gfx
  in
  let make_gfx_cmd_boxes (fg, bg, alpha) =
    (match fg with
     | Some c -> [new_command_box (`GfxCmd c)]
     | None   -> [])
    @ (match bg with
     | Some c -> [new_command_box (`GfxCmd c)]
     | None   -> [])
    @ (match alpha with
     | Some c -> [new_command_box (`GfxCmd c)]
     | None   -> [])
  in
  let leading = match galley.lines with
                | []     -> dim_zero
                | b :: _ -> galley.current_line_params.leading b line galley.current_line_params
  in
  let gfx = match line.b_contents with
            | CompBox bs -> List.fold_left update_gfx_cmds (None, None, None) bs
            | _          -> (None, None, None)
  in
  {
    galley with
    lines = line :: (new_glue_box dim_zero leading true true) :: (galley.glue @ galley.lines);
    glue  = make_gfx_cmd_boxes gfx
  }

(* add glue and control boxes to the galley *)
let add_glue galley box =
  {
    galley with
    glue = box :: galley.glue
  }

(* add a paragraph to the galley *)
let add_paragraph galley loc items =
  (* search for v-insert boxes *)
  let rec extract_inserts boxes result above below = match boxes with
    | []      -> (Tools.ListBuilder.get result, List.rev above, List.rev below)
    | b :: bs -> match b.b_contents with
        | CommandBox (`ParCmd (VInsert (below_flag, contents))) ->
            if below_flag then
              extract_inserts bs result above (List.rev contents @ below)
            else
              extract_inserts bs result (List.rev contents @ above) below
        | _ ->
            Tools.ListBuilder.add result b;
            extract_inserts bs result above below
  in
  let lines =
    ParLayout.break_paragraph
      loc
      items
      galley.current_par_params
      galley.current_line_break_params
      galley.current_hyphen_params
  in
  Printf.printf "[Galley] add_paragraph: broken into %d lines\n%!" (List.length lines);
  let rec box_lines result_builder line_no boxes_list = match boxes_list with
    | []      -> (line_no, Tools.ListBuilder.get result_builder)
    | b :: bs ->
        let (body, above, below) = extract_inserts b (Tools.ListBuilder.make ()) [] [] in
        Tools.ListBuilder.add result_builder
          (ParLayout.layout_line galley.measure line_no body galley.current_par_params, above, below);
        box_lines result_builder (line_no + 1) bs
  in
  let insert_break galley_state penalty =
    add_glue galley_state (new_break_box penalty false [] [] [])
  in
  let insert_par_skip galley_state =
    add_glue
      (insert_break galley_state num_zero)
      (new_glue_box dim_zero galley_state.current_par_params.ParLayout.par_skip true true)
  in
  let rec insert_insertion galley_state boxes_list = match boxes_list with
    | []      -> galley_state
    | b :: bs -> insert_insertion (add_glue galley_state b) bs
  in
  let add_line_and_insertions galley_state line_box above_list below_list =
    insert_insertion
      (add_line (insert_insertion galley_state above_list) line_box)
      below_list
  in
  match box_lines (Tools.ListBuilder.make ()) 0 lines with
  | (_,   []) -> galley
  | (num, (line, above, below) :: ls) ->
      let rec iter lines_above lines_below lines_list galley_state = match lines_list with
        | [] -> galley_state
        | (l, a, b) :: rest ->
            iter (lines_above + 1) (lines_below - 1) rest
                 (add_line_and_insertions
                   (insert_break
                     galley_state
                     (galley_state.current_line_params.club_widow_penalty lines_above lines_below))
                   l a b)
      in
      iter 1 (num - 1) ls (add_line_and_insertions (insert_par_skip galley) line above below)

(* |put_in_vbox <galley>| and |put_in_vtop <galley>| return a box containing the entire material of the
  <galley>.
*)
let put_in_vbox galley = VBox.make (List.rev galley.lines)

let put_in_vtop galley = VBox.make_top (List.rev galley.lines)
