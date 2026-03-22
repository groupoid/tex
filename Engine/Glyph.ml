
open Tools.XNum
open Runtime
open Unicode.UTypes
open Dim
open Runtime.Substitute
open GlyphMetric
open FontMetric
open Box

(*
  |attach_accent <acc-font> <acc> <chr-font> <chr>| attaches the character <acc> of <acc-font> to
  <chr> as accent.
*)
let accent_position _acc_font acc_gm _chr_font chr_gm =
  let pos_x = (chr_gm.gm_width -/ acc_gm.gm_width) // num_of_int 2 in
  let pos_y = chr_gm.gm_height -/ num_zero in (* Default vertical offset *)
  (pos_x, pos_y)

let attach_accent acc_font acc chr_font chr =
  let acc_gm = get_glyph_metric acc_font acc in
  let chr_gm = get_glyph_metric chr_font chr in
  let (pos_x, pos_y) = accent_position acc_font acc_gm chr_font chr_gm in
  let height = max_num chr_gm.gm_height (acc_gm.gm_height +/ pos_y) in
  let depth  = max_num chr_gm.gm_depth  (acc_gm.gm_depth  -/ pos_y) in
  new_compound_box
    (fixed_dim chr_gm.gm_width)
    (fixed_dim height)
    (fixed_dim depth)
    [PutBox (dim_zero, dim_zero, (new_glyph_box chr chr_font), None);
     PutBox (fixed_dim pos_x, fixed_dim pos_y, (new_glyph_box acc acc_font), None)]

(*
  |vertical_extendable <height> <font> <top> <mid> <bot> <rep>| assembles an extendable glyph
  of the given height from <top>, <mid>, <bot>, and <rep>.
*)
let vertical_extendable height font top mid bot rep =
  let top_gm = get_glyph_metric font top in
  let mid_gm = get_glyph_metric font mid in
  let bot_gm = get_glyph_metric font bot in
  let rep_gm = get_glyph_metric font rep in
  let total_height gm = gm.gm_height +/ gm.gm_depth in
  let top_height = total_height top_gm in
  let mid_height = total_height mid_gm in
  let bot_height = total_height bot_gm in
  let rep_height = total_height rep_gm in
  let min_height = top_height +/ mid_height +/ bot_height in
  let num_reps = if rep_height =/ num_zero then
                   0
                 else
                   let delta = height -/ min_height in
                   let num   = int_of_num (ceiling_num (delta // rep_height)) in
                   if mid = `Undef then
                     num
                   else
                    (num + 1) / 2
  in
  let rep_list = Tools.XList.repeat num_reps (new_glyph_box rep font) in
  VBox.make
    ((if top <> `Undef then [new_glyph_box top font] else [])
    @ rep_list
    @ (if mid <> `Undef then (new_glyph_box mid font :: rep_list) else [])
    @ (if bot <> `Undef then [new_glyph_box bot font] else []))

(*
  |horizontal_extendable <width> <font> <left> <mid> <right> <rep>| assembles an extendable glyph
  of the given width from <left>, <mid>, <right>, and <rep>.
*)
let horizontal_extendable width font left mid right rep =
  let left_gm  = get_glyph_metric font left in
  let mid_gm   = get_glyph_metric font mid in
  let right_gm = get_glyph_metric font right in
  let rep_gm   = get_glyph_metric font rep in
  let left_width  = left_gm.gm_width in
  let mid_width   = mid_gm.gm_width in
  let right_width = right_gm.gm_width in
  let rep_width   = rep_gm.gm_width in
  let min_width   = left_width +/ mid_width +/ right_width in
  let num_reps    = if rep_width =/ num_zero then
                      0
                    else
                      let delta = width -/ min_width in
                      let num   = int_of_num (ceiling_num (delta // rep_width)) in
                      if mid = `Undef then
                        num
                      else
                       (num + 1) / 2
  in
  let rep_list = Tools.XList.repeat num_reps (new_glyph_box rep font) in
  HBox.make HBox.LR
    ((if left  <> `Undef then [new_glyph_box left  font] else [])
    @ rep_list
    @ (if mid   <> `Undef then [new_glyph_box mid   font] else [])
    @ rep_list
    @ (if right <> `Undef then [new_glyph_box right font] else []))