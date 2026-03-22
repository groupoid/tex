
open Tools.XNum
open Unicode.UTypes
open Runtime
open Logging
open Dim
open Runtime.Substitute
open FontMetric
open Box

let tracing_line_breaks = ref false

let num_100 = num_of_int 100
let num_13  = num_of_int 13
let num_m13 = num_of_int (-13)
let num_m1  = num_of_int (-1)
let num_1_2 = num_of_ints 1 2

type line_break_params =
{
  pre_tolerance          : num;
  tolerance              : num;
  looseness              : int;
  line_penalty           : num;
  adj_demerits           : num;
  double_hyphen_demerits : num;
  final_hyphen_demerits  : num;
  emergency_stretch      : num;
  river_demerits         : num;
  river_threshold        : num;
  simple_breaking        : bool
}

type par_params =
{
  measure           : num;
  par_indent        : dim;
  par_fill_skip     : dim;
  par_skip          : dim;
  left_skip         : dim;
  right_skip        : dim;
  par_shape         : int -> (num * num);
  pre_break         : extended_glyph_item list;
  post_break        : extended_glyph_item list;
  post_process_line : box list -> box list
}

(*
  |calc_adjust_ratio <line-width> <goal-width> <background-width>| calculates the adjustment
  ratio of the line.
*)
let calc_adjust_ratio line_width goal_width background_width =
  let width = xdim_add line_width background_width in
  let delta = goal_width -/ width.xd_base in
  let s     = if delta </ num_zero then
                xdim_max_shrink  width
              else
                xdim_max_stretch width in
  if s >=/ infinite then
    (num_zero, 0)
  else if s >/ num_zero then
    (delta // s, 0)
  else if delta >=/ num_zero then
    (infinite, 0)
  else
    (minus_infinite, 0)

(*
  |calc_demerits <line_break_params> <badness> <penalty> <hyphen-demerits> <cur-hyphen> <prev-hyphen> <cur-fit> <prev-fit>|
  calculates the demerits of the line from its <badness>, the <penalty> of the break-point, the penalty
  <hyphen-demerits> for two consecutive hyphenations, two flags <cur-hyphen> and <prev-hyphen> indicating
  whether the current and the previous break-point is due to a hyphenation, and the fitness of the current
  and the previous line.
*)
let calc_demerits line_break_params badness penalty hyphen_demerits cur_hyphen prev_hyphen cur_fit prev_fit =
  let alpha = if cur_hyphen && prev_hyphen then
                hyphen_demerits
              else
                num_zero in
  let beta  = line_break_params.line_penalty +/ badness in
  let gamma = if abs (cur_fit - prev_fit) > 1 then
                line_break_params.adj_demerits
              else
                num_zero in
  if penalty >=/ num_zero then
    beta */ beta +/ penalty */ penalty +/ alpha +/ gamma
  else if penalty >/ minus_infinite then
    beta */ beta -/ penalty */ penalty +/ alpha +/ gamma
  else
    beta */ beta +/ alpha +/ gamma

let very_loose_fit = 0 (* lines wider than their stretchability                                  *)
let loose_fit      = 1 (* lines stretched by 1/2 to 1 times their stretchability                 *)
let decent_fit     = 2 (* lines stretched or shrinkt at most 1/2 of their stretch-/shrinkability *)
let tight_fit      = 3 (* lines shrinked by 1/2 to 1 times their shrinkability                   *)

(*
  |calc_fitness <badness>| calculates the fitness of the line. <badness> is negativ if the line has
  to be shrinked.
*)
let calc_fitness badness =
  if badness >=/ num_100 then
    very_loose_fit
  else if badness >=/ num_13 then
    loose_fit
  else if badness <=/ num_m13 then
    tight_fit
  else
    decent_fit

module Fast =
struct
  (* list of break_points *)
  type break_point =
  {
    bp_pos        : int;   (* index into the list of boxes                  *)
    bp_prev       : int;   (* number of the previous break point            *)
    bp_line       : int;   (* the number of the line before the break point *)
    bp_fit        : int;   (* fitness of this break                         *)
    bp_hyph       : bool;  (* is this break point due to hyphenation?       *)
    bp_forced     : bool;  (* is this break forced?                         *)
    bp_demerits   : num    (* demerits of the break                         *)
  }

  type break_delta = 
    | Break of break_point
    | Delta of xdim

  (* |log_break_point <break_point> <number>| prints a description of <break_point>. *)
  let log_break_point bp n =
    log_string "@@";
    log_int n;
    log_string ": line ";
    log_int bp.bp_line;
    log_string ".";
    log_int bp.bp_fit;
    if bp.bp_hyph then log_string "-" else ();
    log_string " t=";
    log_num bp.bp_demerits;
    log_string " -> @@";
    log_int bp.bp_prev;
    log_string "\n"

  let update_breaks pos prev line fit hyph forced demerits new_breaks line_break_params =
    let rec iter breaks = match breaks with
      | []        -> [Break {
                          bp_pos        = pos;
                          bp_prev       = prev;
                          bp_line       = line;
                          bp_fit        = fit;
                          bp_hyph       = hyph;
                          bp_forced     = forced;
                          bp_demerits   = demerits
                        }]
      | b :: bs -> match b with
          | Delta _  -> assert false
          | Break bp ->
              if bp.bp_line = line then
                if bp.bp_fit = fit then
                  if bp.bp_demerits >/ demerits then
                    Break {
                       bp_pos        = pos;
                       bp_prev       = prev;
                       bp_line       = line;
                       bp_fit        = fit;
                       bp_hyph       = hyph;
                       bp_forced     = forced;
                       bp_demerits   = demerits
                    } :: bs
                  else
                    breaks
                else if demerits >/ bp.bp_demerits +/ line_break_params.adj_demerits then
                  breaks
                else if demerits +/ line_break_params.adj_demerits </ bp.bp_demerits then
                  iter bs
                else
                  b :: iter bs
              else
                b :: iter bs
    in
    iter new_breaks

  type break_state =
  {
    bs_number     : int;
    bs_position   : int;
    bs_dist       : xdim;
    bs_discarding : xdim option;
    bs_active     : break_delta list;
    bs_passive    : break_delta list
  }

  let break_lines loc boxes par_params line_break_params =
    let left_right_skip = xdim_add_dim (dim_to_xdim par_params.left_skip) par_params.right_skip in
    let insert_delta dist active = match active with
      | []              -> [Delta dist]
      | Delta d :: ds   -> Delta (xdim_add d dist) :: ds
      | Break b :: _    -> 
          let rec iter pos active = match active with
            | []              -> [Delta dist]
            | Delta d :: ds   -> Delta (xdim_add d dist) :: ds
            | Break b :: bs   -> if pos = b.bp_pos then Break b :: iter pos bs else Delta dist :: active
          in
          iter b.bp_pos active
    in
    let inc_pos box state =
    {
      bs_number     = state.bs_number;
      bs_position   = state.bs_position + 1;
      bs_dist       = xdim_add_dim state.bs_dist box.b_width;
      bs_discarding = None;
      bs_active     = (match state.bs_discarding with
                       | Some dist -> insert_delta dist state.bs_active
                       | None      -> state.bs_active);
      bs_passive    = state.bs_passive
    }
    in
    let skip_glue box state = match state.bs_discarding with
      | Some dist -> if is_discardable_glue box then
                       {
                         bs_number     = state.bs_number;
                         bs_position   = state.bs_position + 1;
                         bs_dist       = state.bs_dist;
                         bs_discarding = Some (xdim_add_dim dist box.b_width);
                         bs_active     = state.bs_active;
                         bs_passive    = state.bs_passive
                       }
                     else
                       {
                         bs_number     = state.bs_number;
                         bs_position   = state.bs_position + 1;
                         bs_dist       = xdim_add_dim state.bs_dist box.b_width;
                         bs_discarding = None;
                         bs_active     = insert_delta dist state.bs_active;
                         bs_passive    = state.bs_passive
                       }
      | None      -> {
                       bs_number     = state.bs_number;
                       bs_position   = state.bs_position + 1;
                       bs_dist       = xdim_add_dim state.bs_dist box.b_width;
                       bs_discarding = None;
                       bs_active     = state.bs_active;
                       bs_passive    = state.bs_passive
                     }
    in
    let try_break threshold hyphen_demerits background_width force_active break_box state =
      let (penalty, hyph, pre_break, post_break) =
        match break_box.b_contents with
        | BreakBox (p, h, pre, post, _) -> (p, h, pre, post)
        | _ -> assert false
      in
      let no_break_width   = break_box.b_width in
      let pre_break_width  = HBox.calc_xwidth pre_break in
      let post_break_width = HBox.calc_xwidth post_break in
      let forced_break     = penalty <=/ minus_infinite in
      let rec iter_breaks cur_dist n act new_breaks new_act =
        match act with
        | [] -> (new_breaks, Tools.ListBuilder.get new_act, state.bs_passive)
        | cur_act :: acts -> match cur_act with
            | Delta dist ->
                Tools.ListBuilder.add new_act cur_act;
                iter_breaks (xdim_add cur_dist dist) n acts new_breaks new_act
            | Break bp ->
                let (left_indent, right_indent) = par_params.par_shape bp.bp_line in
                let width            = par_params.measure -/ left_indent -/ right_indent in
                let adj_ratio        = calc_adjust_ratio cur_dist width background_width in
                let badness_val      = dim_scale_badness adj_ratio in
                if (fst adj_ratio) </ num_m1 then
                  if force_active && Tools.ListBuilder.is_empty new_act && new_breaks = [] then (
                    if !tracing_line_breaks then (
                      if new_breaks = [] then log_string "\n" else ();
                      log_string "@emergency break via @@"; log_int n; log_string "\n"
                    );
                    (update_breaks state.bs_position n (bp.bp_line + 1) tight_fit false forced_break bp.bp_demerits [] line_break_params,
                     [],
                     act @ state.bs_passive)
                  ) else (
                    (new_breaks, Tools.ListBuilder.get new_act, act @ state.bs_passive)
                  )
                else if badness_val >/ threshold then
                  if not bp.bp_forced then (
                    Tools.ListBuilder.add new_act cur_act;
                    iter_breaks cur_dist (n - 1) acts new_breaks new_act
                  ) else (
                    Tools.ListBuilder.add new_act cur_act;
                    (new_breaks, Tools.ListBuilder.get new_act, acts @ state.bs_passive)
                  )
                else (
                  let fit = calc_fitness (if (fst adj_ratio) </ num_zero then minus_num badness_val else badness_val) in
                  let demerits = calc_demerits line_break_params badness_val penalty hyphen_demerits hyph bp.bp_hyph fit bp.bp_fit in
                  let total_demerits = bp.bp_demerits +/ demerits in
                  if !tracing_line_breaks then (
                    if new_breaks = [] then log_string "\n" else ();
                    log_string "@break via @@"; log_int n; log_string " b="; log_num badness_val;
                    log_string " p="; log_num penalty; log_string " d="; log_num demerits; log_string "\n"
                  );
                  Tools.ListBuilder.add new_act cur_act;
                  iter_breaks cur_dist (n - 1) acts
                    (update_breaks state.bs_position n (bp.bp_line + 1) fit hyph forced_break total_demerits new_breaks line_break_params)
                    new_act
                )
      in
      let new_act_builder = Tools.ListBuilder.make () in
      let (new_breaks, act, pas) = iter_breaks (xdim_add state.bs_dist pre_break_width) state.bs_number state.bs_active [] new_act_builder in
      if !tracing_line_breaks then
        ignore (List.fold_right (fun bp n_val -> match bp with Break b -> log_break_point b n_val; n_val + 1 | _ -> n_val + 1) new_breaks (state.bs_number + 1));
      if new_breaks = [] then
        { state with bs_position = state.bs_position + 1; bs_dist = xdim_add_dim state.bs_dist no_break_width; bs_active = act; bs_passive = pas }
      else
        let inter_dist = xdim_sub (xdim_add_dim state.bs_dist no_break_width) post_break_width in
        if post_break = [] then
          {
            bs_number     = state.bs_number + List.length new_breaks;
            bs_position   = state.bs_position + 1;
            bs_dist       = xdim_zero;
            bs_discarding = Some xdim_zero;
            bs_active     = new_breaks @ (match state.bs_discarding with Some dist -> [Delta (xdim_add inter_dist dist)] | None -> [Delta inter_dist]) @ act;
            bs_passive    = pas
          }
        else
          {
            bs_number     = state.bs_number + List.length new_breaks;
            bs_position   = state.bs_position + 1;
            bs_dist       = post_break_width;
            bs_discarding = None;
            bs_active     = new_breaks @ (match state.bs_discarding with Some dist -> [Delta (xdim_add inter_dist dist)] | None -> [Delta inter_dist]) @ act;
            bs_passive    = pas
          }
    in
    let split_lines cur_break breaks_arr boxes_arr =
      let get_subrange arr from_pos to_pos =
        let rec iter res i = if i < from_pos then res else iter (arr.(i) :: res) (i - 1) in
        iter [] to_pos
      in
      let rec iter cur_bp lines =
        if cur_bp.bp_pos = 0 then lines
        else
          let prev = breaks_arr.(cur_bp.bp_prev) in
          let (post_br, first_pos) = match boxes_arr.(prev.bp_pos).b_contents with
            | BreakBox (_, _, _, p, _) -> (List.map box_to_extended_item p, prev.bp_pos + 1)
            | _ -> ([], prev.bp_pos)
          in
          let pre_br = match boxes_arr.(cur_bp.bp_pos).b_contents with
            | BreakBox (_, _, p, _, _) -> List.map box_to_extended_item p
            | _ -> []
          in
          let line_boxes = get_subrange boxes_arr first_pos (cur_bp.bp_pos - 1) in
          let (cmds, new_line) = Compose.discard_glue (post_br @ (List.map box_to_extended_item (remove_breaks line_boxes)) @ pre_br) in
          iter prev ((List.map extended_item_to_box (cmds @ new_line)) :: lines)
      in
      iter cur_break []
    in
    let check_result boxes_list state =
      let end_pos = List.length boxes_list - 1 in
      let breaks_arr = Array.of_list (List.fold_left (fun l bp -> match bp with Break b -> b :: l | _ -> l) [] (state.bs_active @ state.bs_passive)) in
      let num_breaks = Array.length breaks_arr in
      if num_breaks = 0 then None
      else
        let find_best () =
          let rec iter i best =
            if i < 0 then breaks_arr.(best)
            else if breaks_arr.(i).bp_pos < end_pos then breaks_arr.(best)
            else if breaks_arr.(i).bp_demerits </ breaks_arr.(best).bp_demerits then iter (i - 1) i
            else iter (i - 1) best
          in
          iter (num_breaks - 2) (num_breaks - 1)
        in
        let find_line best_bp looseness_val =
          let rec iter i cur_best =
            if i < 0 then cur_best
            else
              let delta = breaks_arr.(i).bp_line - cur_best.bp_line in
              if breaks_arr.(i).bp_pos < end_pos then cur_best
              else if looseness_val = 0 then cur_best
              else if looseness_val > 0 then
                if delta > 0 && delta <= looseness_val then iter (i - 1) breaks_arr.(i) else iter (i - 1) cur_best
              else
                if delta < 0 && delta >= looseness_val then iter (i - 1) breaks_arr.(i) else iter (i - 1) cur_best
          in
          iter (num_breaks - 1) best_bp
        in
        let best_bp = if line_break_params.looseness = 0 then find_best () else find_line (find_best ()) line_break_params.looseness in
        if breaks_arr.(num_breaks - 1).bp_pos <> end_pos then None
        else Some (split_lines best_bp breaks_arr (Array.of_list boxes_list))
    in
    let rec pass_iter tolerance_val ls_skip allow_hyph force_act state boxes_sub =
      match boxes_sub with
      | [] -> check_result boxes state
      | b :: bs ->
          if state.bs_active = [] && not force_act then None
          else (
            if !tracing_line_breaks then log_box b else ();
            match b.b_contents with
            | BreakBox (_, h, _, _, _) ->
                if not h || allow_hyph then
                  pass_iter tolerance_val ls_skip allow_hyph force_act
                    (try_break tolerance_val (if bs = [] then line_break_params.final_hyphen_demerits else line_break_params.double_hyphen_demerits) ls_skip force_act b state) bs
                else if is_real_box b then
                  pass_iter tolerance_val ls_skip allow_hyph force_act (inc_pos b state) bs
                else
                  pass_iter tolerance_val ls_skip allow_hyph force_act { state with bs_position = state.bs_position + 1 } bs
            | GlueBox _ -> pass_iter tolerance_val ls_skip allow_hyph force_act (skip_glue b state) bs
            | _ -> if is_real_box b then
                     pass_iter tolerance_val ls_skip allow_hyph force_act (inc_pos b state) bs
                   else
                     pass_iter tolerance_val ls_skip allow_hyph force_act { state with bs_position = state.bs_position + 1 } bs
          )
    in
    let initial_state =
    {
      bs_number     = 0;
      bs_position   = 0;
      bs_dist       = xdim_zero;
      bs_discarding = None;
      bs_active     = [Break { bp_pos = 0; bp_prev = 0; bp_line = 0; bp_fit = decent_fit; bp_hyph = false; bp_forced = true; bp_demerits = num_zero }];
      bs_passive    = []
    }
    in
    if !tracing_line_breaks then log_info loc "@firstpass\n" else ();
    match pass_iter line_break_params.pre_tolerance left_right_skip false false initial_state boxes with
    | Some x -> x
    | None ->
        if !tracing_line_breaks then log_info loc "@secondpass\n" else ();
        match pass_iter line_break_params.tolerance left_right_skip true false initial_state boxes with
        | Some x -> x
        | None ->
            if !tracing_line_breaks then log_info loc "@emergencypass\n" else ();
            let emergency_skip = xdim_add_dim left_right_skip { d_base = num_zero; d_stretch_factor = line_break_params.emergency_stretch; d_stretch_order = 1; d_shrink_factor = num_zero; d_shrink_order = 0 } in
            match pass_iter line_break_params.tolerance emergency_skip true true initial_state boxes with
            | Some x -> x
            | None -> assert false
end

module Good =
struct
  type break_graph =
  {
    bg_items             : extended_glyph_item array;
    bg_breaks            : int array;
    bg_left_right_skip   : xdim;
    bg_threshold         : num;
    bg_allow_breaks      : bool;
    bg_par_params        : par_params;
    bg_line_break_params : line_break_params;
    bg_hyphen_params     : JustHyph.hyphen_params
  }

  let make_break_graph  (items : extended_glyph_item array) threshold left_right_skip allow_breaks par_params line_break_params hyphen_params =
    let num_breaks = Array.fold_left (fun n (i : extended_glyph_item) -> match (i :> [> `Break of Tools.XNum.num * bool * box array * box array * box array]) with `Break (_, h, _, _, _) -> if not h || allow_breaks then n + 1 else n | _ -> n) 0 items in
    let breaks = Array.make num_breaks 0 in
    let _ = Array.fold_left (fun (n, k) (i : extended_glyph_item) -> match (i :> [> `Break of Tools.XNum.num * bool * box array * box array * box array]) with `Break (_, h, _, _, _) -> if not h || allow_breaks then (breaks.(n) <- k; (n+1, k+1)) else (n, k+1) | _ -> (n, k+1)) (0, 0) items in
    { bg_items = items; bg_breaks = breaks; bg_left_right_skip = left_right_skip; bg_threshold = threshold; bg_allow_breaks = allow_breaks; bg_par_params = par_params; bg_line_break_params = line_break_params; bg_hyphen_params = hyphen_params }

  type river = num * num * num

  type break_point =
  {
    bp_previous : break_point;
    bp_line_no  : int;
    bp_line     : box list;
    bp_fit      : int;
    bp_rivers   : river list;
    bp_hyph     : bool;
    bp_forced   : bool;
    bp_demerits : num
  }

  let is_forced_break_point graph bp_idx = match graph.bg_items.(graph.bg_breaks.(bp_idx)) with
    | `Break (p, _, _, _, _) -> p <=/ minus_infinite
    | _ -> assert false

  let insert_break_point graph break_points new_bp =
    let rec iter bps = match bps with
      | [] -> [new_bp]
      | b :: bs ->
          if b.bp_line_no < new_bp.bp_line_no then b :: iter bs
          else if b.bp_line_no > new_bp.bp_line_no then new_bp :: bps
          else if b.bp_fit = new_bp.bp_fit then
            if b.bp_demerits <=/ new_bp.bp_demerits then bps else new_bp :: bs
          else if new_bp.bp_demerits >/ b.bp_demerits +/ graph.bg_line_break_params.adj_demerits then bps
          else if new_bp.bp_demerits +/ graph.bg_line_break_params.adj_demerits </ b.bp_demerits then iter bs
          else if b.bp_fit < new_bp.bp_fit then b :: iter bs
          else new_bp :: bps
    in
    iter break_points

  let calc_rivers params adj_ratio width last_rivers current_rivers =
    if params.river_demerits <=/ num_zero then (num_zero, [])
    else
      let scale d = (dim_scale (xdim_select_order d width.d_stretch_order width.d_shrink_order) adj_ratio).d_base in
      let rec scale_rivers_list x rs = match rs with
        | [] | [_] -> []
        | a :: b :: rest ->
            let delta = scale a in
            let w = scale b in
            let right = delta +/ w in
            if w >/ params.river_threshold then (x +/ delta, right) :: scale_rivers_list right rest
            else scale_rivers_list right rest
      in
      let scaled_rivers = scale_rivers_list num_zero current_rivers in
      let result_builder = Tools.ListBuilder.make () in
      let rec merge_rivers demerits_acc last_bps cur_bps = match cur_bps with
        | [] -> (demerits_acc, Tools.ListBuilder.get result_builder)
        | (a_start, b_end) :: cs ->
            let rec iter_last zs = match zs with
              | [] -> List.iter (fun (s, e) -> Tools.ListBuilder.add result_builder (s, e, num_zero)) cur_bps; (demerits_acc, Tools.ListBuilder.get result_builder)
              | (x_start, y_end, d_val) :: zzs ->
                  if y_end <=/ a_start then iter_last zzs
                  else if x_start >=/ b_end then (Tools.ListBuilder.add result_builder (a_start, b_end, num_zero); merge_rivers demerits_acc zs cs)
                  else
                    let u = max_num a_start x_start in
                    let v = min_num b_end y_end in
                    let new_dem = d_val +/ (v -/ u) */ params.river_demerits in
                    Tools.ListBuilder.add result_builder (a_start, b_end, new_dem);
                    merge_rivers (demerits_acc +/ new_dem) zs cs
            in
            iter_last last_bps
      in
      merge_rivers num_zero last_rivers scaled_rivers

  let calc_line_demerits graph prev_bp is_final hyph fit badness penalty =
    let hyphen_demerits = if is_final then graph.bg_line_break_params.final_hyphen_demerits else graph.bg_line_break_params.double_hyphen_demerits in
    let demerits = calc_demerits graph.bg_line_break_params badness penalty hyphen_demerits hyph prev_bp.bp_hyph fit prev_bp.bp_fit in
    prev_bp.bp_demerits +/ demerits

  let make_break_point graph prev_bp line_boxes rivers_list width_val is_final forced_break hyph_val adj_ratio badness_val penalty_val =
    let fit_val = calc_fitness (if (fst adj_ratio) </ num_zero then minus_num badness_val else badness_val) in
    let (river_dem, new_rivers_list) = calc_rivers graph.bg_line_break_params adj_ratio width_val prev_bp.bp_rivers rivers_list in
    let demerits_val = river_dem +/ calc_line_demerits graph prev_bp is_final hyph_val fit_val badness_val penalty_val in
    if !tracing_line_breaks then (
      List.iter log_box line_boxes;
      log_string "\n@break: line "; log_int (prev_bp.bp_line_no + 1); log_string "."; log_int fit_val;
      if hyph_val then log_string "-" else ();
      log_string " b="; log_num badness_val; log_string " p="; log_num penalty_val; log_string " t="; log_num demerits_val; log_string "\n"
    );
    { bp_previous = prev_bp; bp_line_no = prev_bp.bp_line_no + 1; bp_line = line_boxes; bp_fit = fit_val; bp_rivers = new_rivers_list; bp_hyph = hyph_val; bp_forced = forced_break; bp_demerits = demerits_val }

  type partial_line =
  {
    pl_prefix       : box list;
    pl_rivers       : xdim list;
    pl_position     : int;
    pl_right_margin : extended_glyph_item list;
    pl_width        : xdim
  }

  let compute_line graph pl prev_idx cur_idx =
    let return_res pl_state new_pos rm boxes1 boxes2 =
      if graph.bg_line_break_params.river_demerits <=/ num_zero then
        let w1 = HBox.calc_xwidth boxes1 in
        let w2 = HBox.calc_xwidth boxes2 in
        let npl = { pl_prefix = boxes1 @ pl_state.pl_prefix; pl_rivers = []; pl_position = new_pos; pl_right_margin = rm; pl_width = xdim_add pl_state.pl_width w1 } in
        (List.rev_append npl.pl_prefix boxes2, [], xdim_add npl.pl_width w2, npl)
      else
        let rec rev_append_rivers r1 r2 = match r1 with [] -> assert false | x::xs -> match r2 with [] -> assert false | y::ys -> List.rev_append xs (xdim_add x y :: ys) in
        let (w1, r1) = HBox.calc_width_and_glue boxes1 in
        let (w2, r2) = HBox.calc_width_and_glue boxes2 in
        let rivs = rev_append_rivers pl_state.pl_rivers (rev_append_rivers r1 r2) in
        let npl = { pl_prefix = boxes1 @ pl_state.pl_prefix; pl_rivers = rev_append_rivers r1 pl_state.pl_rivers; pl_position = new_pos; pl_right_margin = rm; pl_width = xdim_add pl_state.pl_width w1 } in
        (List.rev_append npl.pl_prefix boxes2, rivs, xdim_add npl.pl_width w2, npl)
    in
    let make_margin_glyph item = match item with `Glyph (_, f) -> [`Glyph (`Border GlyphMetric.Margin, f)] | _ -> [] in
    let prefix_with_list lst arr = match lst with
      | [] -> arr
      | x :: xs ->
          let l = List.length lst in
          let a = Array.make (l + Array.length arr) x in
          let rec fill i items = match items with [] -> () | y::ys -> a.(i) <- y; fill (i+1) ys in
          fill 1 xs;
          for i = 0 to Array.length arr - 1 do a.(l + i) <- arr.(i) done;
          a
    in
    let postfix_with_list lst arr = match lst with
      | [] -> arr
      | x :: xs ->
          let l = List.length lst in
          let a = Array.make (l + Array.length arr) x in
          for i = 0 to Array.length arr - 1 do a.(i) <- arr.(i) done;
          let rec fill i items = match items with [] -> () | y::ys -> a.(i) <- y; fill (i+1) ys in
          fill (Array.length arr) lst;
          a
    in
    if pl.pl_position < 0 then
      let post_br = match graph.bg_items.(prev_idx) with `Break (_, _, _, p, _) -> prefix_with_list graph.bg_par_params.post_break (Array.map (fun x -> (`Box x :> extended_glyph_item)) p) | _ -> assert false in
      let pre_br = match graph.bg_items.(cur_idx) with `Break (_, _, p, _, _) -> postfix_with_list graph.bg_par_params.pre_break (Array.map (fun x -> (`Box x :> extended_glyph_item)) p) | _ -> assert false in
      let (cmds1, pos1) = Compose.discard_glue_array (prev_idx + 1) (cur_idx - 1) graph.bg_items in
      let left_margin = match Runtime.Substitute.first_real_item (Tools.XList.from_array post_br) 0 (Array.length post_br - 1) with
        | Some i -> make_margin_glyph i
        | None -> match Runtime.Substitute.first_real_item (Tools.XList.from_array graph.bg_items) (prev_idx + 1) (cur_idx - 1) with
            | Some i -> make_margin_glyph i
            | None -> match Runtime.Substitute.first_real_item (Tools.XList.from_array pre_br) 0 (Array.length pre_br - 1) with Some i -> make_margin_glyph i | None -> []
      in
      if pos1 >= cur_idx then
        let right_margin = match Runtime.Substitute.last_real_item (Tools.XList.from_array pre_br) 0 (Array.length pre_br - 1) with
          | Some i -> make_margin_glyph i
          | None -> match Runtime.Substitute.last_real_item (Tools.XList.from_array graph.bg_items) (prev_idx + 1) (cur_idx - 1) with
              | Some i -> make_margin_glyph i
              | None -> match Runtime.Substitute.last_real_item (Tools.XList.from_array post_br) 0 (Array.length post_br - 1) with Some i -> make_margin_glyph i | None -> []
        in
        let (cmds2, pos2) = Compose.discard_glue_array 0 (Array.length pre_br - 1) pre_br in
        let bxs = (Tools.XList.append_sub_array pre_br pos2 (Array.length pre_br - 1) (right_margin :> Box.extended_glyph_item list)) in
        let line = Tools.XList.append_sub_array post_br 0 (Array.length post_br - 1) (cmds1 @ cmds2 @ bxs) in
        let line_boxes = List.map extended_item_to_box (List.map (fun i -> (i :> extended_glyph_item)) (left_margin @ line)) in
        if graph.bg_line_break_params.river_demerits <=/ num_zero then (line_boxes, [], HBox.calc_xwidth line_boxes, pl)
        else let (w, r) = HBox.calc_width_and_glue line_boxes in (line_boxes, r, w, pl)
      else
        let items1 = Tools.XList.from_sub_array graph.bg_items pos1 (cur_idx - 1) in
        let line_prefix = left_margin @ (Tools.XList.append_sub_array post_br 0 (Array.length post_br - 1) (cmds1 @ items1)) in
        let (prefix, len, rest) = JustHyph.add_lig_kern_iterative_list false [] (List.map (fun i -> (i :> extended_glyph_item)) line_prefix) in
        let prefix_pos = pos1 + len - Array.length post_br - List.length cmds1 - List.length left_margin in
        let pr_rm = match Runtime.Substitute.last_real_item (Tools.XList.from_array graph.bg_items) pos1 (prefix_pos - 1) with
          | Some i -> make_margin_glyph i
          | None -> match Runtime.Substitute.last_real_item (Tools.XList.from_array post_br) 0 (Array.length post_br - 1) with Some i -> make_margin_glyph i | None -> []
        in
        let rm = match Runtime.Substitute.last_real_item (Tools.XList.from_array pre_br) 0 (Array.length pre_br - 1) with
          | Some i -> make_margin_glyph i
          | None -> match Runtime.Substitute.last_real_item (Tools.XList.from_array graph.bg_items) prefix_pos (cur_idx - 1) with Some i -> make_margin_glyph i | None -> pr_rm
        in
        let suffix = JustHyph.add_lig_kern false (rest @ (List.filter_map (fun i -> match i with `Break _ -> None | #glyph_item as g -> Some g) (Tools.XList.append_sub_array pre_br 0 (Array.length pre_br - 1) (rm :> extended_glyph_item list)))) in
        return_res pl prefix_pos pr_rm (List.map extended_item_to_box (List.map (fun i -> (i :> extended_glyph_item)) prefix)) (List.map extended_item_to_box (List.map (fun i -> (i :> extended_glyph_item)) suffix))
    else
      let pre_br = match graph.bg_items.(cur_idx) with `Break (_, _, p, _, _) -> postfix_with_list graph.bg_par_params.pre_break (Array.map (fun b -> (`Box b :> extended_glyph_item)) p) | _ -> assert false in
      let (new_prefix, prefix_pos) = JustHyph.add_lig_kern_iterative_array false [] pl.pl_position (cur_idx - 1) graph.bg_items in
      let pr_rm = match Runtime.Substitute.last_real_item (Tools.XList.from_array graph.bg_items) pl.pl_position (prefix_pos - 1) with | Some i -> make_margin_glyph i | None -> pl.pl_right_margin in
      let rm = match Runtime.Substitute.last_real_item (Tools.XList.from_array pre_br) 0 (Array.length pre_br - 1) with
        | Some i -> make_margin_glyph i
        | None -> match Runtime.Substitute.last_real_item (Tools.XList.from_array graph.bg_items) prefix_pos (cur_idx - 1) with Some i -> make_margin_glyph i | None -> pr_rm
      in
      let l_suffix = JustHyph.add_lig_kern false (List.filter_map (fun i -> match i with `Break _ -> None | #glyph_item as g -> Some (g :> extended_glyph_item)) ((Tools.XList.append_sub_array graph.bg_items prefix_pos (cur_idx - 1) (List.map (fun i -> (i :> extended_glyph_item)) (Tools.XList.append_sub_array pre_br 0 (Array.length pre_br - 1) (rm :> extended_glyph_item list)))))) in
      return_res pl prefix_pos pr_rm (List.map extended_item_to_box (List.map (fun i -> (i :> extended_glyph_item)) new_prefix)) (List.map extended_item_to_box (List.map (fun i -> (i :> extended_glyph_item)) l_suffix))

  let update_breaks graph previous_breaks pl_state prev_idx cur_idx current_breaks_list =
    let new_line_ref = ref None in
    let rec iter last_line_no_val new_bps bps_list = match bps_list with
      | [] -> (match !new_line_ref with None -> (new_bps, Some pl_state) | Some (_, _, _, npl) -> (new_bps, Some npl))
      | b :: bs ->
          if b.bp_line_no = last_line_no_val then iter last_line_no_val new_bps bs
          else
            let (left_ind, right_ind) = graph.bg_par_params.par_shape b.bp_line_no in
            let goal_w = graph.bg_par_params.measure -/ right_ind -/ left_ind in
            if goal_w </ xdim_min_value (xdim_add pl_state.pl_width graph.bg_left_right_skip) && prev_idx + 1 < cur_idx then iter b.bp_line_no new_bps bs
            else
              let prev_p = graph.bg_breaks.(prev_idx) in
              let cur_p = graph.bg_breaks.(cur_idx) in
              let (p_val, h_val) = match graph.bg_items.(cur_p) with `Break (pv, hv, _, _, _) -> (pv, hv) | _ -> assert false in
              let forced = p_val <=/ minus_infinite in
              let (line_bxs, rivs, l_w, _) = match !new_line_ref with Some l -> l | None -> let l = compute_line graph pl_state prev_p cur_p in new_line_ref := Some l; l in
              let t_w = xdim_to_dim (xdim_add l_w graph.bg_left_right_skip) in
              let adj_r = adjustment_ratio t_w goal_w in
              let bad_val = dim_scale_badness adj_r in
              let bad2 = if bad_val >/ graph.bg_threshold && t_w.d_base >/ goal_w && prev_idx + 1 = cur_idx then num_zero else bad_val in
              if bad2 >/ graph.bg_threshold then iter b.bp_line_no new_bps bs
              else
                let new_bp = make_break_point graph b line_bxs rivs t_w (cur_idx = Array.length graph.bg_breaks - 1) forced h_val adj_r bad2 p_val in
                iter b.bp_line_no (insert_break_point graph new_bps new_bp) bs
    in
    iter (-1) current_breaks_list previous_breaks

  let compute_best_break graph breaks_arr =
    let num_br = Array.length breaks_arr in
    match breaks_arr.(num_br - 1) with
    | [] -> None
    | b :: bs ->
        let rec find_best best bps = match bps with [] -> best | b2::bs2 -> if b2.bp_demerits </ best.bp_demerits then find_best b2 bs2 else find_best best bs2 in
        let rec get_br best looseness_val bps = match bps with
          | [] -> best
          | b2::bs2 ->
              let delta = b2.bp_line_no - best.bp_line_no in
              if delta = 0 then (if b2.bp_demerits </ best.bp_demerits then get_br b2 looseness_val bs2 else get_br best looseness_val bs2)
              else if (delta > 0 && delta <= looseness_val) || (delta < 0 && delta >= looseness_val) then get_br b2 (looseness_val - delta) bs2
              else get_br best looseness_val bs2
        in
        Some (get_br (find_best b bs) graph.bg_line_break_params.looseness breaks_arr.(num_br - 1))

  let get_lines bp =
    let rec iter curr res = if curr.bp_line_no > 0 then iter curr.bp_previous (curr.bp_line :: res) else res in
    iter bp []

  let break_lines loc items par_params line_break_params hyphen_params =
    let rec initial_bp = { bp_previous = initial_bp; bp_line_no = 0; bp_line = []; bp_fit = decent_fit; bp_rivers = []; bp_hyph = false; bp_forced = true; bp_demerits = num_zero } in
    let pass tolerance_val ls_skip allow_br =
      let g = make_break_graph (Array.of_list (`Break (num_zero, false, [||], [||], [||]) :: items)) tolerance_val ls_skip allow_br par_params line_break_params hyphen_params in
      ShortestPath.find_shortest_path update_breaks is_forced_break_point compute_best_break initial_bp { pl_prefix = []; pl_rivers = []; pl_position = -1; pl_right_margin = []; pl_width = xdim_zero } g (Array.length g.bg_breaks)
    in
    let ls_skip = xdim_add_dim (dim_to_xdim par_params.left_skip) par_params.right_skip in
    if !tracing_line_breaks then log_string "\n@firstpass\n" else ();
    match pass line_break_params.pre_tolerance ls_skip false with
    | Some bp -> get_lines bp
    | None ->
        if !tracing_line_breaks then log_string "\n\n@secondpass\n" else ();
        match pass line_break_params.tolerance ls_skip true with
        | Some bp -> get_lines bp
        | None ->
            log_warn loc "Inserting emergency stretch!";
            let rec iter_emergency n em =
              if n >= 999 then (
                if !tracing_line_breaks then log_string "\n\n@final emergencypass\n" else ();
                match pass line_break_params.tolerance (dim_to_xdim { d_base = ls_skip.xd_base; d_stretch_factor = num_one; d_stretch_order = 1; d_shrink_factor = num_one; d_shrink_order = 1 }) true with
                | Some bp -> get_lines bp
                | None -> assert false
              ) else (
                if !tracing_line_breaks then (log_string "\n\n@emergencypass "; log_int n; log_string "\n") else ();
                match pass line_break_params.tolerance (xdim_add_dim ls_skip { d_base = num_zero; d_stretch_factor = em; d_stretch_order = 0; d_shrink_factor = num_zero; d_shrink_order = 0 }) true with
                | Some bp -> get_lines bp
                | None -> iter_emergency (n+1) (num_two */ em)
              )
            in
            iter_emergency 1 (max_num num_1_2 line_break_params.emergency_stretch)
end

let check_shrinkage loc (items : extended_glyph_item list) =
  let check_box box =
    let width = box.b_width in
    if width.d_shrink_factor = num_zero || width.d_shrink_order = 0 then box
    else (
      log_warn loc "Paragraph contains infinitely shrinkable glue!\n";
      { box with b_width = { width with d_shrink_factor = width.d_base; d_shrink_order = 0 } }
    )
  in
  let check_item (item : extended_glyph_item) = match item with
    | `Box box -> `Box (check_box box)
    | _ -> item
  in
  (List.map check_item items : extended_glyph_item list)

let add_par_fill_skip items par_params =
  let (cmds, par) = Compose.discard_glue (List.rev items) in
  `Box (new_glue_box par_params.par_indent dim_zero true false) ::
  List.rev (`Break (minus_infinite, false, [||], [||], [||]) ::
           `Box (new_glue_box par_params.par_fill_skip dim_zero true true) ::
           (cmds @ par))

let break_paragraph loc (items : extended_glyph_item list) par_params line_break_params hyphen_params =
  let par = add_par_fill_skip (check_shrinkage loc items) par_params in
  if line_break_params.simple_breaking then
    Fast.break_lines loc (List.map extended_item_to_box (JustHyph.add_lig_kern true (List.map (fun x -> match x with `Box b | `Command b -> (`Box b :> extended_glyph_item) | _ -> assert false) par))) par_params line_break_params
  else
    Good.break_lines loc (List.map (fun x -> match x with `Box b | `Command b -> `Box b | _ -> assert false) par) par_params line_break_params hyphen_params

let layout_line width line_no line par_params =
  let rec process_commands boxes = match boxes with
    | [] -> []
    | b :: bs -> (match b.b_contents with
        | CommandBox (`ParCmd (CallParFunction f)) -> f line_no; process_commands bs
        | _ -> b :: process_commands bs)
  in
  let (left_indent, right_indent) = par_params.par_shape line_no in
  let boxes = process_commands
    (par_params.post_process_line
      ([new_glue_box (dim_add par_params.left_skip (fixed_dim left_indent)) dim_zero true false] @
       line @
       [new_glue_box (dim_add par_params.right_skip (fixed_dim right_indent)) dim_zero true false]))
  in
  HBox.make_to HBox.LR width boxes