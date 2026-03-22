open Tools.XNum
open Runtime
open Logging
open Dim
open Graphic
open Box

type footnote_params =
{
  separator         : box    (* box inserted above footnotes *);
  top_skip          : num;    (* minimal whitespace above footnotes *)
  bottom_skip       : num;    (* minimal whitespace below footnotes *)
  grid_size         : num;
  line_params       : Galley.line_params;
  par_params        : ParLayout.par_params;
  line_break_params : ParLayout.line_break_params;
  hyphen_params     : JustHyph.hyphen_params;
  space_params      : Galley.space_params;
  math_params       : MathLayout.math_params
}

let layout params page area floats page_state = match floats with
| [] -> PageLayout.simple_page_update page page_state
| _  ->
    let width  = area.Page.as_width in
    let galley = Galley.new_galley width
                   params.line_params       params.par_params
                   params.line_break_params params.hyphen_params
                   params.space_params      params.math_params in
    let galley = Galley.add_glue galley params.separator in
    let galley =
      List.fold_left
        (fun g p ->
           Galley.add_paragraph g ("", 0, 0) p)
        galley
        floats in
    let boxes = Galley.galley_lines galley in
    let (top, height, bottom) = VBox.calc_vert_dimensions boxes in
    (* FIX: process page-commands in <boxes> *)
    (* FIX: consider top-sep and bottom-sep  *)

    match Page.find_place_in_area_bottom page area top height bottom with
    | None        -> None
    | Some (y, r) ->
        let x        = area.Page.as_pos_x in
        let box      = VBox.to_top (VBox.layout_scaled r boxes) in
        let new_page = Page.put_box_on_page page x y box in
        if !PageLayout.tracing_page_layout then (
          log_string "\n#P: Placing footnotes at ";
          log_num (page.Page.p_height -/ y);
          if box.b_height.d_base <>/ height.xd_base then (
            log_string " scaled to ";
            log_num box.b_height.d_base
          )
          else ()
        )
        else ();

        PageLayout.simple_page_update new_page page_state
