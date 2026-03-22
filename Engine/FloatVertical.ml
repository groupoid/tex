open Tools.XNum
open Runtime
open Logging
open Dim
open Graphic
open Box

type vert_alignment = Top | Bottom 

type float_params =
{
  alignment   : vert_alignment;
  top_skip    : num;    (* minimal whitespace above floats   *)
  bottom_skip : num;    (* minimal whitespace below floats   *)
  float_sep   : dim     (* minimal whitespace between floats *)
}

let layout params page area floats page_state =
  let width = area.Page.as_width in
  let float_sep_glue = new_glue_box dim_zero params.float_sep false false in
  let make_box items =
    let boxes = List.map extended_item_to_box items in
    let box   = HBox.make_to HBox.LR width boxes in
    let shift = fixed_dim ((width -/ box.b_width.d_base) // num_two) in
    new_compound_box
      (fixed_dim width)
      box.b_height
      box.b_depth
      [Graphic.PutBox (shift, dim_zero, box, None)]
  in

  let rec make_boxes floats = match floats with
    | []      -> []
    | [f]     -> [make_box f]
    | f::fs -> make_box f :: float_sep_glue :: make_boxes fs
  in

  match make_boxes floats with
  | []    -> PageLayout.simple_page_update page page_state
  | boxes ->
    let (top, height, bottom) = VBox.calc_vert_dimensions boxes in
      (* FIX: process page-commands in <boxes> *)
      (* FIX: consider top-sep and bottom-sep  *)

      let placement_function = match params.alignment with
        | Top    -> Page.find_place_in_area_top
        | Bottom -> Page.find_place_in_area_bottom
      in
      match placement_function page area top height bottom with
      | None        -> None
      | Some (y, r) ->
          let x        = area.Page.as_pos_x in
          let box      = VBox.to_top (VBox.layout_scaled r boxes) in
          let new_page = Page.put_box_on_page page x y box in
          if !PageLayout.tracing_page_layout then (
            log_string "\n#P: Placing float at ";
            log_num (page.Page.p_height -/ y);

            if box.b_height.d_base <>/ height.xd_base then (
              log_string " scaled to ";
              log_num box.b_height.d_base
            )
            else ()
          )
          else ();

          PageLayout.simple_page_update new_page page_state
