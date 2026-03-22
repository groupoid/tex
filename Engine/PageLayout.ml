
open Tools.XNum
open Runtime
open Unicode.UTypes
open Logging
open Dim
open Runtime.Substitute
open FontMetric
open Box
open Page

type area =
{
  ar_name     : uc_string;
  ar_shape    : area_shape;
  ar_contents : area_contents_function
}
and page_update = (page * area_finaliser * page_state)
and area_contents_function =
  page -> area_shape -> (extended_glyph_item list) list -> page_state -> page_update option
and page_layout =
{
  pl_width  : num;
  pl_height : num;
  pl_areas  : area array
}
and area_finaliser = page -> page
and page_state =
{
  ps_page_no        : int;
  ps_old_marks      : uc_string Unicode.DynUCTrie.t;
  ps_new_marks      : (uc_string * uc_string) list;
  ps_galleys        : (box list * Galley.galley) Unicode.DynUCTrie.t;
  ps_layouts        : page_layout Unicode.DynUCTrie.t;
  ps_next_layout    : page_layout;
  ps_new_floats     : floating list;
  ps_badness        : num;
  ps_finished       : bool
}

type page_run_state =
{
  mutable rs_page_no     : int;
  mutable rs_marks       : uc_string Unicode.DynUCTrie.t;
  mutable rs_galleys     : (box list * Galley.galley) Unicode.DynUCTrie.t;
  rs_layouts             : page_layout Unicode.DynUCTrie.t;
  mutable rs_next_layout : page_layout;
  mutable rs_finished    : bool;
  rs_float_misplacement_demerits : num;
  mutable rs_floats      : (int * floating) list
}

let tracing_page_layout = ref false

let simple_page_update page page_state = Some (page, (fun p -> p), page_state)

let get_page_info page ps =
{
  pi_page_no   = 0;
  pi_width     = page.p_width;
  pi_height    = page.p_height;
  pi_depth = num_zero;
  pi_top_margin = num_zero;
  pi_bottom_margin = num_zero;
  pi_left_margin = num_zero;
  pi_right_margin = num_zero;
}

let lookup_area page_layout area_name =
  let rec iter_areas i =
    if i >= Array.length page_layout.pl_areas then (
      log_warn ("", 0, 0) "unknown area: ";
      log_uc_string area_name;
      0
    ) else if page_layout.pl_areas.(i).ar_name = area_name then
      i
    else
      iter_areas (i + 1)
  in
  iter_areas 0

let layout_one_sided layout _ = layout

let layout_two_sided even odd page_no =
  if page_no land 1 = 0 then even else odd

let assemble_page page =
  Box.new_compound_box
    (Dim.fixed_dim page.p_width)
    dim_zero
    (Dim.fixed_dim page.p_height)
    (List.map
      (fun (x, y, b) ->
        Graphic.PutBox (Dim.fixed_dim x, Dim.fixed_dim (y -/ page.p_height), b, None))
      page.p_boxes)

let page_no rs = rs.rs_page_no

let new_page_run_state page_no float_demerits galleys layouts =
  let last_break = [
    new_glue_box dim_zero dim_fil true true;
    new_break_box (minus_num infinite) false [] [] []
  ] in
  {
    rs_page_no     = page_no;
    rs_marks       = Unicode.DynUCTrie.empty;
    rs_galleys     = Unicode.DynUCTrie.map (fun g -> (Galley.galley_lines g @ last_break, g)) galleys;
    rs_layouts     = layouts;
    rs_next_layout = { pl_width = num_zero; pl_height = num_zero; pl_areas = [||] };
    rs_finished    = true;
    rs_floats      = [];
    rs_float_misplacement_demerits = float_demerits
  }

let get_galley_table page_run_state =
  let rec remove_last x y z = match z with
    | []      -> []
    | u :: us -> x :: remove_last y u us
  in
  let store_lines (lines, galley) = match lines with
    | [] | [_] | [_; _] -> Galley.keep_lines galley lines
    | x :: y :: z       -> Galley.keep_lines galley (remove_last x y z)
  in
  Unicode.DynUCTrie.map store_lines page_run_state.rs_galleys

let layout_page_with_floats page_layout floats state =
  if !tracing_page_layout then (
    log_string "\n#P: Trying to layout page ";
    log_int state.ps_page_no;
    log_string " with ";
    log_int (List.length floats);
    log_string " floats."
  ) else ();
  let page = new_page page_layout.pl_width page_layout.pl_height in
  let float_array = Array.make (Array.length page_layout.pl_areas) [] in
  let finalisers  = Array.make (Array.length page_layout.pl_areas) (fun p -> p) in
  List.iter (fun (i, f) -> float_array.(i) <- f :: float_array.(i)) floats;

  let rec iter i page state =
    if i >= Array.length page_layout.pl_areas then (
      let p = Array.fold_left (fun p f -> f p) page finalisers in
      if !tracing_page_layout then (
        log_string "\n#P: successful layout: badness ";
        log_num state.ps_badness
      ) else ();
      Some (p, state)
    ) else (
      if !tracing_page_layout then (
        log_string "\n#P: Layouting area \"";
        log_uc_string page_layout.pl_areas.(i).ar_name;
        log_string "\" ..."
      ) else ();
      match page_layout.pl_areas.(i).ar_contents page page_layout.pl_areas.(i).ar_shape float_array.(i) state with
        | None -> if !tracing_page_layout then log_string "\n#P: layout failed." else (); None
        | Some (page_v, finaliser, state_v) ->
            finalisers.(i) <- finaliser;
            iter (i + 1) page_v state_v
    )
  in
  iter 0 page state

let rec execute_command_boxes page_info x y box = match box.b_contents with
  | CommandBox (`PageCmd cmd) -> 
      (match cmd with
       | CallPageFunction f -> f page_info (x, y); box
       | _ -> box)
  | ProcBox f ->
      Box.new_compound_box box.b_width box.b_height box.b_depth (f page_info (x, y) box)
  | CompBox cmds ->
      let rec iter_cmds x y cmds = match cmds with
        | [] -> []
        | Graphic.PutBox (dx, dy, b, _) :: bs ->
            Graphic.PutBox (dx, dy, (execute_command_boxes page_info (x +/ dx.d_base) (y +/ dy.d_base) b), None) :: iter_cmds x y bs
        | c :: cs -> c :: iter_cmds x y cs
      in
      Box.new_compound_box box.b_width box.b_height box.b_depth (iter_cmds x y cmds)
  | MathBox (c, b) -> new_math_box c (execute_command_boxes page_info x y b)
  | _ -> box

let collect_floats run_state page_state =
  run_state.rs_floats @ (List.map (fun f -> (0, f)) page_state.ps_new_floats)

let build_page run_state (page, state, num_floats_to_choose) =
  let all_floats = collect_floats run_state state in
  let remaining_floats = Tools.XList.drop num_floats_to_choose all_floats in
  if !tracing_page_layout then (
    let n = List.length remaining_floats in
    if n = 0 then
      log_string "\n#P: Choosing layout with all floats."
    else (
      log_string "\n#P: Choosing layout with ";
      log_int num_floats_to_choose;
      log_string " floats, defering ";
      log_int (List.length remaining_floats);
      log_string " floats to the next pages."
    )
  ) else ();
  let info = get_page_info page state in
  let contents_val =
    List.map
      (fun (x, y, b) ->
         Graphic.PutBox (x, (y -. page.p_height), (draw_box info x y b), None))
      page.p_boxes
  in
  run_state.rs_page_no     <- run_state.rs_page_no + 1;
  run_state.rs_floats      <- List.map (fun (i, f) -> (i + 1, f)) remaining_floats;
  run_state.rs_next_layout <- state.ps_next_layout;
  run_state.rs_galleys     <- state.ps_galleys;
  run_state.rs_finished    <- state.ps_finished;
  run_state.rs_marks       <-
    List.fold_left
      (fun marks (k, v) -> Unicode.DynUCTrie.add_string k v marks)
      state.ps_old_marks
      (List.rev state.ps_new_marks);
  Some {
         FontMetric.p_contents = FontMetric.Group contents_val;
         FontMetric.p_number   = run_state.rs_page_no - 1;
         FontMetric.p_width    = page.p_width;
         FontMetric.p_height   = page.p_height
       }

let choose_best_layout misplacement_demerits num_old_floats results =
  let demerits (_, page_state, nf) =
    misplacement_demerits */ num_of_int (List.length page_state.ps_new_floats + num_old_floats - nf)
    +/ page_state.ps_badness
  in
  let rec find_best best_dem best res_list = match res_list with
    | [] ->
        if !tracing_page_layout then (
          log_string "\n#P: Best layout with ";
          log_num best_dem;
          log_string " demerits."
        ) else ();
        best
    | r :: rs ->
        let d = demerits r in
        if d </ best_dem then find_best d r rs else find_best best_dem best rs
  in
  match results with
    | []      -> assert false
    | r :: rs -> find_best (demerits r) r rs

let break_page_fn layout run_state =
  let page_state =
  {
    ps_page_no     = run_state.rs_page_no;
    ps_old_marks   = run_state.rs_marks;
    ps_new_marks   = [];
    ps_galleys     = run_state.rs_galleys;
    ps_layouts     = run_state.rs_layouts;
    ps_next_layout = layout (run_state.rs_page_no + 1);
    ps_new_floats  = [];
    ps_badness     = num_zero;
    ps_finished    = run_state.rs_finished
  } in
  let current_layout = run_state.rs_next_layout in
  match layout_page_with_floats current_layout [] page_state with
  | None        -> None
  | Some (p, s) ->
      let num_old_floats = List.length run_state.rs_floats in
      let floats         = collect_floats run_state s in
      let rec iter_fs nf results_acc fs_on_page remaining_fs = match remaining_fs with
        | []      -> results_acc
        | (_, (a_name, f_val)) :: fs ->
            let new_fs = (a_name, f_val) :: fs_on_page in
            match layout_page_with_floats current_layout (List.map (fun (a, f) -> (lookup_area current_layout a, f)) new_fs) page_state with
              | None       -> results_acc
              | Some (p_v, s_v) ->
                  let num_new_floats = List.length s_v.ps_new_floats in
                  if nf > num_old_floats + num_new_floats then results_acc
                  else iter_fs (nf + 1) ((p_v, s_v, nf) :: results_acc) new_fs fs
      in
      let best_layout_opt = build_page run_state (choose_best_layout run_state.rs_float_misplacement_demerits num_old_floats (iter_fs 1 [(p, s, 0)] [] (List.map (fun (i,f) -> (i,f)) floats))) in
      log_string "\n#P: Page built.";
      best_layout_opt

let layout_run_of_pages layout abort page_run_state =
  let old_prs =
  {
    rs_page_no     = page_run_state.rs_page_no;
    rs_marks       = page_run_state.rs_marks;
    rs_galleys     = page_run_state.rs_galleys;
    rs_layouts     = page_run_state.rs_layouts;
    rs_next_layout = page_run_state.rs_next_layout;
    rs_finished    = page_run_state.rs_finished;
    rs_float_misplacement_demerits = page_run_state.rs_float_misplacement_demerits;
    rs_floats      = page_run_state.rs_floats
  } in
  let pages_builder = Tools.ListBuilder.make () in
  page_run_state.rs_next_layout <- layout page_run_state.rs_page_no;
  let rec iter_pages () =
    old_prs.rs_page_no     <- page_run_state.rs_page_no;
    old_prs.rs_marks       <- page_run_state.rs_marks;
    old_prs.rs_galleys     <- page_run_state.rs_galleys;
    old_prs.rs_next_layout <- page_run_state.rs_next_layout;
    old_prs.rs_finished    <- page_run_state.rs_finished;
    old_prs.rs_floats      <- page_run_state.rs_floats;
    page_run_state.rs_finished <- true;
    match break_page_fn layout page_run_state with
    | None -> (Tools.ListBuilder.get pages_builder, page_run_state)
    | Some p ->
        if abort page_run_state then (Tools.ListBuilder.get pages_builder, old_prs)
        else (Tools.ListBuilder.add pages_builder p; iter_pages ())
  in
  iter_pages ()

let abort_when_done state = state.rs_finished
let abort_on_page no state = state.rs_page_no > no
