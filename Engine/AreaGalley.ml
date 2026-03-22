
open Tools.XNum
open Runtime
open Unicode.UTypes
open Logging
open Dim
open Box
open Page
open PageLayout

let tracing_page_breaks = ref true

type area_params =
{
  galley      : uc_string;
  top_skip    : num;
  bottom_skip : num;
  min_size    : num;
  grid_size   : num  (* if non-zero then all y coordinates are rounded  *)
}

type break_state =
{
  mutable bs_height        : dim;
  mutable bs_depth         : dim;
  mutable bs_height_goal   : num;
  mutable bs_best_badness  : num;
  bs_chosen_lines  : box Tools.ListBuilder.builder;
  bs_pending_lines : box Tools.ListBuilder.builder;
  bs_chosen_marks  : [ `PageCmd of page_cmd | `GfxCmd of Box.gfx_cmd ] Tools.ListBuilder.builder;
  bs_pending_marks : [ `PageCmd of page_cmd | `GfxCmd of Box.gfx_cmd ] Tools.ListBuilder.builder
}

(*
  |process_marks <marks> <page-state>| processes the marks found in the last area and updates
  <page-state> accordingly.
*)
let rec process_marks marks page_state = match marks with
  | []      -> page_state
  | m :: ms -> match m with
      | `PageCmd cmd -> 
          (match cmd with
           | SetNextLayout layout ->
               (try
                  let next_layout = Unicode.DynUCTrie.find_string layout page_state.ps_layouts in
                  process_marks ms { page_state with ps_next_layout = next_layout }
                with
                | Not_found ->
                    log_string "\nWarning: unknown page layout `";
                    log_uc_string layout;
                    log_string "'";
                    process_marks ms page_state)
           | SetMark (mark, str) ->
               process_marks ms { page_state with ps_new_marks = (mark, str) :: page_state.ps_new_marks }
           | Float f ->
               process_marks ms { page_state with ps_new_floats = f :: page_state.ps_new_floats }
           | CallPageFunction _ -> process_marks ms page_state)
      | `GfxCmd _ -> process_marks ms page_state

(* Layout of a single area *)

let calc_top_skip line max_top line_params =
  let top_params = { line_params with Galley.line_skip_limit = num_zero; Galley.line_skip = dim_zero } in
  let top_skip = dim_sub line_params.Galley.baseline_skip max_top in
  top_params.Galley.leading (new_rule_box line.b_width dim_zero top_skip) line top_params

let calc_bot_skip line max_bot line_params =
  let bot_params = { line_params with Galley.line_skip_limit = num_zero; Galley.line_skip = dim_zero } in
  let bot_skip = dim_sub line_params.Galley.baseline_skip max_bot in
  bot_params.Galley.leading line (new_rule_box line.b_width bot_skip dim_zero) bot_params

let calc_top_shift line max_top line_params =
  let skip = calc_top_skip line max_top line_params in
  dim_sub (dim_add skip line.b_height) max_top

let calc_bot_shift line max_bot line_params =
  let skip = calc_bot_skip line max_bot line_params in
  dim_sub (dim_add skip line.b_depth) max_bot

let assemble_interval lines (top, height, bottom) line_params =
  let rec get_first_and_last_line lines = match lines with
    | []      -> None
    | b :: bs ->
        if is_real_box b then
          Some (b, get_last_line b bs)
        else
          get_first_and_last_line bs
  and get_last_line last lines = match lines with
    | []      -> last
    | b :: bs ->
        if is_real_box b then
          get_last_line b bs
        else
          get_last_line last bs
  in
  match get_first_and_last_line lines with
  | None -> new_compound_box dim_zero dim_zero (fixed_dim height) []
  | Some (first_line, last_line) ->
      let t_skip = calc_top_skip first_line (fixed_dim top)    line_params in
      let b_skip = calc_bot_skip last_line  (fixed_dim bottom) line_params in
      shift_compound_vert
        (VBox.make_to (height +/ top +/ bottom)
                      ([new_glue_box dim_zero t_skip false true]
                      @ lines
                      @ [new_glue_box dim_zero b_skip false true;
                         new_glue_box dim_zero dim_zero false true]))
        (fixed_dim (minus_num (height +/ bottom)))

let break_page (top, height, bottom) lines line_params =
  let rec add_lines break_state boxes = match boxes with
    | [] -> (Tools.ListBuilder.get break_state.bs_chosen_lines,
             Tools.ListBuilder.get break_state.bs_chosen_marks,
             [],
             break_state.bs_best_badness)
    | b :: bs ->
        let skip = calc_bot_shift b (fixed_dim bottom) line_params in
        let bh = dim_add b.b_height skip in
        let bd = dim_sub b.b_depth  skip in
        let new_height = dim_add (dim_add break_state.bs_height break_state.bs_depth) bh in
        let (factor, order) = adjustment_ratio break_state.bs_height break_state.bs_height_goal in
        let bad = dim_scale_badness (factor, order) in
        if break_state.bs_height.d_base >/ break_state.bs_height_goal && bad >=/ infinite then
          (if break_state.bs_best_badness </ infinite then
             (Tools.ListBuilder.get break_state.bs_chosen_lines,
              Tools.ListBuilder.get break_state.bs_chosen_marks,
              Tools.ListBuilder.get break_state.bs_pending_lines @ boxes,
              break_state.bs_best_badness)
           else
             (Tools.ListBuilder.append break_state.bs_chosen_lines break_state.bs_pending_lines;
              Tools.ListBuilder.append break_state.bs_chosen_marks break_state.bs_pending_marks;
              (Tools.ListBuilder.get break_state.bs_chosen_lines,
               Tools.ListBuilder.get break_state.bs_chosen_marks,
               boxes,
               break_state.bs_best_badness)))
        else match b.b_contents with
          | BreakBox (p, _, _, _, _) ->
              if !tracing_page_breaks then (
                log_string "\n#G t = "; log_dim break_state.bs_height;
                log_string "; g = "; log_num break_state.bs_height_goal;
                log_string "; b = "; log_num bad;
                log_string "; p = "; log_num p;
                log_string "; c = "; log_num (bad +/ p);
                if bad +/ p </ break_state.bs_best_badness then log_string "#" else ()
              );
              if p <=/ minus_num infinite then (
                Tools.ListBuilder.append break_state.bs_chosen_lines break_state.bs_pending_lines;
                Tools.ListBuilder.append break_state.bs_chosen_marks break_state.bs_pending_marks;
                (Tools.ListBuilder.get break_state.bs_chosen_lines,
                 Tools.ListBuilder.get break_state.bs_chosen_marks,
                 bs,
                 minus_num infinite)
              ) else if bad +/ p </ break_state.bs_best_badness then (
                break_state.bs_height <- new_height;
                break_state.bs_depth <- bd;
                break_state.bs_best_badness <- bad +/ p;
                Tools.ListBuilder.append break_state.bs_chosen_lines break_state.bs_pending_lines;
                Tools.ListBuilder.append break_state.bs_chosen_marks break_state.bs_pending_marks;
                add_lines break_state bs
              ) else (
                break_state.bs_height <- new_height;
                break_state.bs_depth <- bd;
                Tools.ListBuilder.add break_state.bs_pending_lines b;
                add_lines break_state bs
              )
          | CommandBox cmd -> 
              (match cmd with
               | (`PageCmd _) as mark ->
                   break_state.bs_height <- new_height;
                   break_state.bs_depth  <- bd;
                   Tools.ListBuilder.add break_state.bs_pending_marks mark;
                   Tools.ListBuilder.add break_state.bs_pending_lines b;
                   add_lines break_state bs
               | (`GfxCmd c) as mark -> 
                   (match c with
                    | Graphic.SetColour _ | Graphic.SetBgColour _ | Graphic.SetAlpha _ ->
                        break_state.bs_height <- new_height;
                        break_state.bs_depth  <- bd;
                        Tools.ListBuilder.add break_state.bs_pending_marks mark;
                        Tools.ListBuilder.add break_state.bs_pending_lines b;
                        add_lines break_state bs
                    | _ ->
                        break_state.bs_height <- new_height;
                        break_state.bs_depth  <- bd;
                        Tools.ListBuilder.add break_state.bs_pending_lines b;
                        add_lines break_state bs)
               | _ ->
                   break_state.bs_height <- new_height;
                   break_state.bs_depth  <- bd;
                   Tools.ListBuilder.add break_state.bs_pending_lines b;
                   add_lines break_state bs)
          | _ ->
              break_state.bs_height <- new_height;
              break_state.bs_depth  <- bd;
              Tools.ListBuilder.add break_state.bs_pending_lines b;
              add_lines break_state bs
  in
  let rec filter_cmds cmds = match cmds with
    | [] -> []
    | { b_contents = CommandBox c; _ } :: cs ->
        (match c with
         | (`PageCmd _) as mark -> mark :: filter_cmds cs
         | (`GfxCmd _) as mark -> mark :: filter_cmds cs
         | _ -> filter_cmds cs)
    | _ :: cs -> filter_cmds cs
  in
  match discard_glue lines with
  | (cmds, []) -> ([], filter_cmds cmds, [], num_zero)
  | (cmds, first_line :: ls) ->
      let first_skip = calc_top_skip first_line (fixed_dim top) line_params in
      add_lines
        {
          bs_height        = fixed_dim (minus_num top);
          bs_depth         = first_skip;
          bs_height_goal   = height;
          bs_best_badness  = infinite;
          bs_chosen_lines  = Tools.ListBuilder.make ();
          bs_pending_lines = Tools.ListBuilder.make ();
          bs_chosen_marks  = Tools.ListBuilder.make ();
          bs_pending_marks = Tools.ListBuilder.make ()
        }
        (cmds @ (first_line :: ls))

let update_gfx_cmds ((fg, bg, alpha) as gfx) cmd = match cmd with
  | `GfxCmd c -> 
      (match c with
       | Graphic.SetColour _ -> (Some c, bg, alpha)
       | Graphic.SetBgColour _ -> (fg, Some c, alpha)
       | Graphic.SetAlpha _ -> (fg, bg, Some c)
       | _ -> gfx)
  | _ -> gfx

let make_gfx_cmd_boxes (fg, bg, alpha) =
  (match fg with Some c -> [new_command_box (`GfxCmd c)] | None -> []) @
  (match bg with Some c -> [new_command_box (`GfxCmd c)] | None -> []) @
  (match alpha with Some c -> [new_command_box (`GfxCmd c)] | None -> [])

let layout_interval page page_state x y interval lines line_params =
  let (boxes, marks, remaining, badness) = break_page interval lines line_params in
  let gfx_cmds = make_gfx_cmd_boxes (List.fold_left update_gfx_cmds (None, None, None) marks) in
  let ps = process_marks marks page_state in
  let ps2 = { ps with
              ps_badness = ps.ps_badness +/ badness;
              ps_finished = ps.ps_finished && (boxes == []) }
  in
  if boxes <> [] then
    (put_box_on_page page x y (assemble_interval boxes interval line_params), ps2, gfx_cmds @ remaining)
  else
    (page, ps2, gfx_cmds @ remaining)

let contents_from_galley params page area _floats page_state =
  match Unicode.DynUCTrie.lookup_string params.galley page_state.ps_galleys with
  | None ->
      log_error ("", 0, 0) "unknown galley `";
      log_uc_string params.galley;
      log_string "'!";
      None
  | Some (lines, g) ->
      let line_params = Galley.line_params g in
      let rec iter (page, page_state, lines) intervals = match intervals with
        | [] ->
            Some (page, (fun p -> p), { page_state with ps_galleys = Unicode.DynUCTrie.add_string params.galley (lines, g) page_state.ps_galleys })
        | (a, b) :: is ->
            let b2 = b -/ params.top_skip in
            let a2 = a +/ params.bottom_skip in
            let max_y = if b2 >=/ area.as_pos_y then area.as_pos_y
                        else if params.grid_size >/ num_zero then
                          area.as_pos_y +/ (params.grid_size */ floor_num ((b2 -/ area.as_pos_y) // params.grid_size))
                        else b2 in
            let min_y = if a2 <=/ area.as_pos_y -/ area.as_height then area.as_pos_y -/ area.as_height
                        else if params.grid_size >/ num_zero then
                          area.as_pos_y +/ (params.grid_size */ ceiling_num ((a2 -/ area.as_pos_y) // params.grid_size))
                        else a2 in
            let max_top = min_num area.as_top (b2 -/ max_y) in
            let max_bot = min_num area.as_bottom (min_y -/ a2) in
            if max_y -/ min_y <=/ params.min_size then
              iter (page, page_state, lines) is
            else
              iter (layout_interval page page_state area.as_pos_x max_y (max_top, max_y -/ min_y, max_bot) lines line_params) is
      in
      iter (page, page_state, lines) (area_free_vert page area)