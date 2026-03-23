
open Tools.XNum
open Runtime
open Logging
open Dim
open Box

(* The maximal badness tolerated by make. *)
let max_v_badness = ref (num_of_int 1000)

(*
  |calc_vert_dimension <boxes>| returns the triple (<top>, <height>, <bottom>)
  where <top> is the height of the first box, <bottom> is the depth of the last box,
  and <height> is the sum of the remaining heights and depths.
*)

let calc_vert_dimensions boxes = match boxes with
  | []      -> (dim_zero, xdim_zero, dim_zero)
  | b :: bs ->
      let top = b.b_height in
      let rec iter height depth boxes = match boxes with
        | []      -> (top, height, depth)
        | b :: bs -> iter (xdim_add_dim (xdim_add_dim height depth) b.b_height) b.b_depth bs
      in
      iter xdim_zero b.b_depth bs

let calc_height boxes =
  let (top, height, bottom) = calc_vert_dimensions boxes in
  (xdim_to_dim (xdim_add_dim height top), bottom)

let make boxes =
  let (h, d) = calc_height boxes in
  let rec iter width height result boxes = match boxes with
    | []      ->
        Printf.printf "[VBox] make: returning CompBox with %d cmds\n%!" (List.length result);
        new_compound_box width (xdim_to_dim height) d result
    | b :: bs -> match b.b_contents with
      | CommandBox (`GfxCmd c) -> iter width height (c :: result) bs
      | _ ->
          let v_pos = xdim_add_dim height b.b_depth in
          iter (dim_max width b.b_width)
               (xdim_add_dim (xdim_add_dim height b.b_height) b.b_depth)
               (Graphic.PutBox (dim_zero, (xdim_select_order v_pos h.d_stretch_order h.d_shrink_order), b, None) :: result)
               bs
  in
  iter dim_zero xdim_zero [] (List.rev boxes)

let layout_scaled (factor, order) boxes =
  if boxes = [] then
    new_compound_box dim_zero dim_zero dim_zero []
  else
    let ratio = if factor </ num_of_int (-1) && order = 0 then
                  (num_of_int (-1), 0)
                else
                  (factor, order)
    in
    let bad = dim_scale_badness (factor, order) in
    let boxes_rev = List.rev boxes in
    let depth = (scale_box_vert (List.hd boxes_rev) ratio).b_depth in
    if bad >/ !max_v_badness then (
      if factor </ num_zero then
        log_string "\nWarning: Overfull vbox (badness "
      else
        log_string "\nWarning: Underfull vbox (badness ";

      if bad </ infinite then
        log_num bad
      else
        log_string "infinite";
      log_string ")!\n"
    );

    let rec iter width height result boxes = match boxes with
      | []      ->
          Printf.printf "[VBox] layout_scaled: returning CompBox with %d cmds\n%!" (List.length result);
          new_compound_box width (fixed_dim height.xd_base) depth result
      | b :: bs -> match b.b_contents with
        | CommandBox (`GfxCmd c) -> iter width height (c :: result) bs
        | _ ->
            let box = scale_box_vert b ratio in
            let v_pos = xdim_add_dim height b.b_depth in
            iter (dim_max width box.b_width)
                 (xdim_add_dim (xdim_add_dim height box.b_height) box.b_depth)
                 (Graphic.PutBox (dim_zero, fixed_dim v_pos.xd_base, box, None) :: result)
                 bs
    in
    iter dim_zero xdim_zero [] boxes_rev

let to_top box =
  let rec get_baseline cmds = match cmds with
    | [] -> num_zero
    | Graphic.PutBox (_, y, _, _) :: _ -> minus_num y.d_base
    | _ :: cs -> get_baseline cs
  in
  match box.b_contents with
  | CompBox cmds -> shift_compound_vert box (fixed_dim (get_baseline cmds))
  | _            -> box

let make_to height boxes =
  let (h, _) = calc_height boxes in
  let ratio  = adjustment_ratio h height in
  layout_scaled ratio boxes

let make_scaled factor boxes =
  let (h, _) = calc_height boxes in
  let height = factor */ h.d_base in
  let ratio  = adjustment_ratio h height in
  layout_scaled ratio boxes

let make_spread amount boxes =
  let (h, _) = calc_height boxes in
  let height = amount +/ h.d_base in
  let ratio  = adjustment_ratio h height in
  layout_scaled ratio boxes

let make_top boxes = to_top (make boxes)

let make_top_to height boxes =
  let (h, _) = calc_height boxes in
  let ratio  = adjustment_ratio h height in
  to_top (layout_scaled ratio boxes)

let make_top_scaled factor boxes =
  let (h, d) = calc_height boxes in
  let height = factor */ (h.d_base +/ d.d_base) in
  let ratio  = adjustment_ratio (dim_add h d) height in
  to_top (layout_scaled ratio boxes)

let make_top_spread amount boxes =
  let (h, _) = calc_height boxes in
  let height = amount +/ h.d_base in
  let ratio  = adjustment_ratio h height in
  to_top (layout_scaled ratio boxes)
