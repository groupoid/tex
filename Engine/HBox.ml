open Tools.XNum
open Runtime
open Logging
open Dim
open Box

module ListBuilder = Tools.ListBuilder

type direction = LR | RL 

(* The maximal badness tolerated by make. *)
let max_h_badness = ref (num_of_int 1000)

(* h-boxes *)

(* |dimensions <boxes>| returns the dimensions of an h-box containing <boxes>. *)
let dimensions boxes =
  let rec iter width height depth boxes = match boxes with
    | []      -> (xdim_to_dim width, height, depth)
    | b :: bs -> iter
                   (xdim_add_dim width  b.b_width)
                   (dim_max      height b.b_height)
                   (dim_max      depth  b.b_depth)
                   bs
  in
  iter xdim_zero dim_zero dim_zero boxes

let calc_xwidth boxes =
  let rec iter w boxes = match boxes with
    | []      -> w
    | b :: bs -> iter (xdim_add_dim w b.b_width) bs
  in
  iter xdim_zero boxes

let calc_width boxes = xdim_to_dim (calc_xwidth boxes)

(* |calc_width_and_glue <boxes>| calculates the total width of <boxes> and intervals. *)
let calc_width_and_glue boxes =
  let rec iter glue width delta boxes = match boxes with
    | []      -> (width, delta :: glue)
    | b :: bs ->
        let new_width = xdim_add_dim width b.b_width in
        (* Check whether it is explicit glue. *)
        match b.b_contents with
        | GlueBox (false, _) -> 
            iter (xdim_add_dim delta b.b_width :: glue)
                 new_width
                 xdim_zero
                 bs
        | _ -> iter glue new_width (xdim_add_dim delta b.b_width) bs
  in
  iter [] xdim_zero xdim_zero boxes

(* |layout <direction> <boxes>| computes the positions of the boxes. *)
let layout dir boxes = match boxes with
  | []  -> empty_box
  | [b] -> (match b.b_contents with
          | CommandBox (`GfxCmd c) -> new_compound_box dim_zero dim_zero dim_zero [c]
          | _                      -> b)
  | _ ->
      let (width, height, depth) = dimensions boxes in
      let builder = Tools.ListBuilder.make () in
      let start_x = match dir with
        | LR -> xdim_zero
        | RL -> dim_to_xdim width
      in
      let rec iter x boxes = match boxes with
        | []      -> new_compound_box width height depth (Tools.ListBuilder.get builder)
        | b :: bs -> match b.b_contents with
          | CommandBox (`GfxCmd c) ->
              Tools.ListBuilder.add builder c;
              iter x bs
          | _ -> (match dir with
                | LR ->
                    Tools.ListBuilder.add builder
                      (Graphic.PutBox (xdim_select_order x width.d_stretch_order width.d_shrink_order,
                               dim_zero, b, None));
                    iter (xdim_add_dim x b.b_width) bs
                | RL ->
                    let new_x = xdim_sub_dim x b.b_width in
                    Tools.ListBuilder.add builder
                      (Graphic.PutBox (xdim_select_order new_x width.d_stretch_order width.d_shrink_order,
                               dim_zero, b, None));
                    iter new_x bs)
      in
      iter start_x boxes

let layout_scaled dir (orig_width, height, depth) scaled_width boxes =
  let (factor, order) = adjustment_ratio orig_width scaled_width in
  let ratio = if factor </ num_of_int (-1) && order = 0 then
                (num_of_int (-1), 0)
              else
                (factor, order)
  in
  let bad = dim_scale_badness (factor, order) in
  if bad >/ !max_h_badness then (
    if factor </ num_zero then
      log_string "\nWarning: Overfull hbox (badness "
    else
      log_string "\nWarning: Underfull hbox (badness ";

    if bad </ infinite then
      log_num bad
    else
      log_string "infinite";
    log_string ")!\n";
    List.iter log_box boxes;
    log_string "\n"
  ) else ();
  
  let builder = Tools.ListBuilder.make () in
  let start_x = match dir with
    | LR -> xdim_zero
    | RL -> dim_to_xdim (fixed_dim scaled_width)
  in
  let rec iter x boxes = match boxes with
    | []      -> new_compound_box (fixed_dim scaled_width) height depth (Tools.ListBuilder.get builder)
    | b :: bs -> match b.b_contents with
      | CommandBox (`GfxCmd c) ->
          Tools.ListBuilder.add builder c;
          iter x bs
      | _ -> (match dir with
            | LR ->
                let box = scale_box_horiz b ratio in
                Tools.ListBuilder.add builder (Graphic.PutBox (fixed_dim x.xd_base, dim_zero, box, None));
                iter (xdim_add_dim x box.b_width) bs
            | RL ->
                let box = scale_box_horiz b ratio in
                let new_x = xdim_sub_dim x box.b_width in
                Tools.ListBuilder.add builder (Graphic.PutBox (fixed_dim new_x.xd_base, dim_zero, box, None));
                iter new_x bs)
  in
  iter start_x boxes

let make dir boxes = layout dir boxes

let make_to dir width boxes =
  layout_scaled dir (dimensions boxes) width boxes

let make_scaled dir factor boxes =
  let (w, h, d) = dimensions boxes in
  layout_scaled dir (w, h, d) (factor */ w.d_base) boxes

let make_spread dir amount boxes =
  let (w, h, d) = dimensions boxes in
  layout_scaled dir (w, h, d) (w.d_base +/ amount) boxes
