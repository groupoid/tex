
open XNum
open Unicode.UTypes
open Runtime.Logging
open Runtime.Dim
open Runtime.Substitute
open Runtime.GlyphMetric
open Runtime.FontMetric
open Box

module Graphic  = Runtime.Graphic
module JustHyph = Runtime.JustHyph

(* styles *)

type math_style =
  | Display | CrampedDisplay | Text    | CrampedText
  | Script  | CrampedScript  | Script2 | CrampedScript2

let cramped_style style = match style with
   Display | CrampedDisplay -> CrampedDisplay
| Text    | CrampedText    -> CrampedText
| Script  | CrampedScript  -> CrampedScript
| _                        -> CrampedScript2

let is_cramped style = match style with
   CrampedDisplay | CrampedText | CrampedScript | CrampedScript2 -> true
| _                                                             -> false

let is_display style = match style with
   Display | CrampedDisplay -> true
| _                        -> false

(*
  These functions return the style of, respectively, a sub-script, a super-script, the numerator, and
  the denominator of a formula of style `style'.
*)

let sub_style style = match style with
   Display | CrampedDisplay | Text | CrampedText -> CrampedScript
| _                                             -> CrampedScript2

let super_style style = match style with
   Display        | Text           -> Script
| CrampedDisplay | CrampedText    -> CrampedScript
| Script         | Script2        -> Script2
| CrampedScript  | CrampedScript2 -> CrampedScript2

let numerator_style style = match style with
   Display          -> Text
| CrampedDisplay   -> CrampedText
| Text             -> Script
| CrampedText      -> CrampedScript
| Script | Script2 -> Script2
| _                -> CrampedScript2

let denominator_style style = match style with
   Display | CrampedDisplay -> CrampedText
| Text    | CrampedText    -> CrampedScript
| _                        -> CrampedScript2

(* font parameters *)

type math_font_params = (Runtime.FontMetric.font_metric * Runtime.FontMetric.font_metric * Runtime.FontMetric.font_metric)

let make_font_params symbol_font operator_font =
{ symbol_font with
  fm_default_rule_thickness = operator_font.fm_default_rule_thickness;
  fm_big_op_spacing1 = operator_font.fm_big_op_spacing1;
  fm_big_op_spacing2 = operator_font.fm_big_op_spacing2;
  fm_big_op_spacing3 = operator_font.fm_big_op_spacing3;
  fm_big_op_spacing4 = operator_font.fm_big_op_spacing4;
  fm_big_op_spacing5 = operator_font.fm_big_op_spacing5
}
let get_font_params (a,b,c) style = match style with
   Script  | CrampedScript  -> b
| Script2 | CrampedScript2 -> c
| _                        -> a

let get_super_shift font_params style = begin
  if is_cramped style then
    font_params.fm_sup3
  else if style = Display then
    font_params.fm_sup1
  else
    font_params.fm_sup2
end


let get_num_shift font_params style thickness = match style with
   Display | CrampedDisplay -> font_params.fm_num1
| _                        -> if thickness = num_zero then
                                font_params.fm_num3
                              else
                                font_params.fm_num2

let get_denom_shift font_params style = match style with
   Display | CrampedDisplay -> font_params.fm_denom1
| _                        -> font_params.fm_denom2

type math_params =
{
  thin_math_skip       : dim;
  med_math_skip        : dim;
  thick_math_skip      : dim;
  script_space         : dim;
  rel_penalty          : float;
  binop_penalty        : float;
  delimiter_factor     : float;
  delimiter_shortfall  : float;
  null_delimiter_space : dim;
}
type delimiter_code = (uc_char * font_metric list * uc_char * font_metric list)

let font_metric_to_dim font num = fixed_dim num
let math_dim_to_points_dim p num = fixed_dim num

let next_glyph font glyph =
  let gm = get_glyph_metric font glyph in
  match gm.gm_extra with
  | `List next -> Some (`GlyphIndex next)
  | _ -> None

let math_units_to_points params x = x */ params.fm_quad // num_of_int 18

let math_dim_to_points params dim =
let mu = params.fm_quad // num_of_int 18 in
  {
    d_base           = mu */ dim.d_base;
    d_stretch_factor = mu */ dim.d_stretch_factor;
    d_stretch_order  = dim.d_stretch_order;
    d_shrink_factor  = mu */ dim.d_shrink_factor;
    d_shrink_order   = dim.d_shrink_order
}
(* auxillary functions *)

(* Make a glyph box and add italic correction to its width. *)

let make_glyph_box glyph font = begin
let gm = get_glyph_metric font glyph in
    let box = new_glyph_box glyph font in
  {
    box

    with

    b_width = dim_add box.b_width (fixed_dim gm.gm_italic)
  }
end


(* make_char_box <char> <font> creates a char-box and automatically adds italic-correction. *)

let make_char_box char font = begin
  make_glyph_box (get_glyph font char) font
end


(* Remove the italic correction again. *)

let remove_icorr box = match box.b_contents with
   CharBox (g, f) ->
      let gm = get_glyph_metric f g in
      { box with b_width = dim_add box.b_width (fixed_dim gm.gm_italic) }
| _ -> box

(*
  We are dealing with lists consisting both of math-boxes and of ``raw'' boxes. The following
  functions save a lot of conditionals.
*)

let remove_math_box box = match box.b_contents with
   MathBox (_, b) -> b
| _           -> box

(*
  |center-on-axis <box> <axis-height>| centers <box> vertically at height <axis-height>.
  The returned box has any enclosing math-box of <box> removed.
*)

let center_on_axis box fm_axis_height = begin
let b = remove_math_box box in
    let height = b.b_height.d_base in
let depth = b.b_depth.d_base in

  if height =/ fm_axis_height && depth =/ fm_axis_height then
    box
  else
    shift_compound_vert (wrap_in_compound_box b) (fixed_dim (fm_axis_height -/ (height -/ depth) // num_of_int 2))
end


(*
  |attach_scripts <mbox> <scripts> <style> <super-shift> <font-params> <math-params>| attachs a list of
  sub- and super-scripts to a math-box. <super-shift> specifies the amout the super-script is shiftet w.r.t.
  the sub-script.
*)

(* FIX: display style scripts *)

let attach_scripts mbox (lt, lb, vt, vb, rt, rb) style super_shift font_params math_params =
  let make_script script = begin
    if script = [] then
      empty_box
    else begin
let space = math_params.script_space in

      if dim_is_zero space then
        HBox.make HBox.LR (Compose.box_add_lig_kern script)
      else
        HBox.make HBox.LR (Compose.box_add_lig_kern (script @ [new_glue_box space dim_zero false false]))
end
  end in


    if lt = [] && lb = [] && vt = [] && vb = [] && rt = [] && rb = [] then
      mbox
    else begin
let box = remove_math_box mbox in
    let body_params = get_font_params font_params style in
let script_params = get_font_params font_params (sub_style style) in
    let lt_script = make_script lt in
let lb_script = make_script lb in
    let vt_script = make_script vt in
let vb_script = make_script vb in
    let rt_script = make_script rt in
let rb_script = make_script rb in
    let super_h_pos = dim_add box.b_width (fixed_dim super_shift) in
    let shift_up_1    = if is_char_box box then
                          num_zero
                        else
                          box.b_height.d_base -/ script_params.fm_sup_drop in
    let shift_down_1  = if is_char_box box then
                          num_zero
                        else
                          box.b_depth.d_base +/ script_params.fm_sub_drop in
    let shift_up_2    = max_num
                          (max_num
                            shift_up_1
                            (get_super_shift body_params style)
                          )
                          (max_num lt_script.b_depth.d_base rt_script.b_depth.d_base
                            +/ abs_num body_params.fm_x_height // num_of_int 4) in
    let shift_down_2  = if lt <> [] || rt <> [] then
                          max_num shift_down_1 body_params.fm_sub2
                        else
                          max_num
                            (max_num shift_down_1 body_params.fm_sub1)
                            (max_num lb_script.b_height.d_base rb_script.b_height.d_base
                              -/ num_of_int 4 */ abs_num body_params.fm_x_height // num_of_int 5) in
    let lseparator    = num_of_int 4 */ body_params.fm_default_rule_thickness +/
                        lt_script.b_depth.d_base -/ shift_up_2  +/
                        lb_script.b_height.d_base  -/ shift_down_2 in
    let rseparator    = num_of_int 4 */ body_params.fm_default_rule_thickness +/
                        rt_script.b_depth.d_base -/ shift_up_2  +/
                        rb_script.b_height.d_base  -/ shift_down_2 in
    let final_lshift  = if lseparator >/ num_zero then
                          min_num
                            num_zero
                            (shift_up_2 -/ lt_script.b_depth.d_base
                                        -/ abs_num (num_of_int 4 */ body_params.fm_x_height // num_of_int 5))
                        else
                          num_zero in
    let final_rshift  = if rseparator >/ num_zero then
                          min_num
                            num_zero
                            (shift_up_2 -/ rt_script.b_depth.d_base
                                        -/ abs_num (num_of_int 4 */ body_params.fm_x_height // num_of_int 5))
                        else
                          num_zero in
let lt_v_pos = fixed_dim (shift_up_2 -/ final_lshift) in
    let rt_v_pos = fixed_dim (shift_up_2 -/ final_rshift) in
    let lb_v_pos      = if lt <> [] && lseparator >/ num_zero then
                          fixed_dim (minus_num (shift_down_2 +/ lseparator +/ final_lshift))
                        else
                          fixed_dim (minus_num shift_down_2) in
    let rb_v_pos      = if rt <> [] && rseparator >/ num_zero then
                          fixed_dim (minus_num (shift_down_2 +/ rseparator +/ final_rshift))
                        else
                          fixed_dim (minus_num shift_down_2) in
    let vt_shift      = max_num (body_params.fm_big_op_spacing3 -/ vt_script.b_depth.d_base)
                                body_params.fm_big_op_spacing1 in
    let vb_shift      = max_num (body_params.fm_big_op_spacing4 -/ vb_script.b_height.d_base)
                                body_params.fm_big_op_spacing2 in
    let vt_h_pos      = fixed_dim ((box.b_width.d_base -/ vt_script.b_width.d_base +/ super_shift)
                                   // num_two) in
    let vb_h_pos      = fixed_dim ((box.b_width.d_base -/ vb_script.b_width.d_base -/ super_shift)
                                   // num_two) in
let vt_v_pos = fixed_dim (box.b_height.d_base +/ vt_script.b_depth.d_base +/ vt_shift) in
    let vb_v_pos = fixed_dim (minus_num (box.b_depth.d_base +/ vb_script.b_height.d_base +/ vb_shift)) in
    let vt_height     = if vt <> [] then
                          vt_script.b_height.d_base +/ vt_script.b_depth.d_base
                           +/ vt_shift +/ body_params.fm_big_op_spacing5
                        else
                          num_zero in
    let vb_height     = if vb <> [] then
                          vb_script.b_height.d_base +/ vb_script.b_depth.d_base
                           +/ vb_shift +/ body_params.fm_big_op_spacing5
                        else
                          num_zero in
    let total_rwidth  = dim_max
                          (dim_max (dim_add box.b_width rb_script.b_width)
                                   (dim_add super_h_pos rt_script.b_width))
                          (dim_max (dim_add vt_h_pos vt_script.b_width)
                                   (dim_add vb_h_pos vb_script.b_width)) in
    let total_lwidth  = dim_max
                          (dim_max lb_script.b_width
                                   (dim_sub lt_script.b_width (fixed_dim super_shift)))
                          (dim_max (dim_neg vt_h_pos) (dim_neg vb_h_pos)) in
    let total_height  = dim_max (dim_add box.b_height (fixed_dim vt_height))
                          (dim_max (dim_add lt_v_pos lt_script.b_height)
                                   (dim_add rt_v_pos rt_script.b_height)) in
    let total_depth   = dim_max (dim_add box.b_depth (fixed_dim vb_height))
                          (dim_max (dim_sub lb_script.b_depth lb_v_pos)
                                   (dim_sub rb_script.b_depth rb_v_pos)) in
    let formula       = new_compound_box
                          (dim_add total_lwidth total_rwidth)
                          total_height total_depth
                          ( [Graphic.PutBox (total_lwidth, dim_zero, box, None)]
                          @ (if lb <> [] then
                               [Graphic.PutBox (dim_sub total_lwidth lb_script.b_width, lb_v_pos, lb_script, None)]
                             else
                               [])
                          @ (if lt <> [] then
                               [Graphic.PutBox (dim_add (dim_sub total_lwidth rb_script.b_width) (fixed_dim super_shift), lt_v_pos, lt_script, None)]
                             else
                               [])
                          @ (if vb <> [] then
                               [Graphic.PutBox (dim_add total_lwidth vb_h_pos, vb_v_pos, vb_script, None)]
                             else
                               [])
                          @ (if vt <> [] then
                               [Graphic.PutBox (dim_add total_lwidth vt_h_pos, vt_v_pos, vt_script, None)]
                             else
                               [])
                          @ (if rb <> [] then
                               [Graphic.PutBox (dim_add total_lwidth box.b_width, rb_v_pos, rb_script, None)]
                             else
                               [])
                          @ (if rt <> [] then
                               [Graphic.PutBox (dim_add total_lwidth super_h_pos, rt_v_pos, rt_script, None)]
                             else
                               [])
                          ) in

    match mbox.b_contents with
    | MathBox (c, _) -> new_math_box c formula
    | _           -> formula
end


(*
  |merge_scripts <style> <boxes> <font-params> <spacing-params> collects all super- and sub-scripts and attaches them
  to their bases.
*)

let math_box_code box = match box.b_contents with
  | MathBox (code, _) -> code
  | _ -> Runtime.MathTypes.Ordinary

let rec merge_scripts style boxes font_params math_params = match boxes with
| []            -> []
| (base_box :: bs) -> match math_box_code base_box with
   SubScript | SuperScript -> begin
      log_string "\nWarning: Sub/super-script without reference!";

      (* add en empty box *)

      merge_scripts
        style
        (new_glue_box dim_zero dim_zero true false
          :: boxes)
        font_params
        math_params
end
  | IndexPosition _ -> merge_scripts style bs font_params math_params
  | _ -> begin
let box = remove_math_box base_box in
    let super_shift = match box.b_contents with
   CharBox (c, f) -> (get_glyph_metric f c).gm_italic
                        | _           -> num_zero in

      let rec iter pos (lt, lb, vt, vb, rt, rb) boxes = match boxes with
      | [] -> [attach_scripts
                      base_box
                      (List.rev lt, List.rev lb,
                       List.rev vt, List.rev vb,
                       List.rev rt, List.rev rb)
                      style
                      super_shift
                      font_params
                      math_params ]

      | (b :: bs) -> match math_box_code b with
   | SubScript   -> (match pos with
   | Runtime.MathTypes.LeftIndex  -> iter pos (lt, (remove_math_box b :: lb), vt, vb, rt, rb) bs
          | Runtime.MathTypes.VertIndex  -> iter pos (lt, lb, vt, (remove_math_box b :: vb), rt, rb) bs
          | Runtime.MathTypes.RightIndex -> iter pos (lt, lb, vt, vb, rt, (remove_math_box b :: rb)) bs)

        | SuperScript -> (match pos with
   | Runtime.MathTypes.LeftIndex  -> iter pos ((remove_math_box b :: lt), lb, vt, vb, rt, rb) bs
          | Runtime.MathTypes.VertIndex  -> iter pos (lt, lb, (remove_math_box b :: vt), vb, rt, rb) bs
          | Runtime.MathTypes.RightIndex -> iter pos (lt, lb, vt, vb, (remove_math_box b :: rt), rb) bs)
        | IndexPosition p -> iter p (lt, lb, vt, vb, rt, rb) bs
        | _               -> (attach_scripts
                                base_box
                                (List.rev lt, List.rev lb,
                                 List.rev vt, List.rev vb,
                                 List.rev rt, List.rev rb)
                                style
                                super_shift
                                font_params
                                math_params
                              :: merge_scripts style boxes font_params math_params)
        in iter RightIndex ([], [], [], [], [], []) bs
end
(*
  |check_bin_ops <boxes>| makes sure that there are no two consecutive binary operators in <boxes> by
  converting the second one to an ordinary math-box.
*)

let check_bin_ops boxes = begin
  let bin_to_ord box =
    new_math_box Ordinary (remove_math_box box) in
  let buf = ListBuilder.make () in
  let spaces = ListBuilder.make () in

  let rec check last boxes = match boxes with
    | [] -> begin
        begin match math_box_code last with
        | BinOp -> ListBuilder.add buf (bin_to_ord last)
        | _     -> ListBuilder.add buf last
        end;
        ListBuilder.append buf spaces;
        ListBuilder.get buf
      end
    | (next :: bs) -> begin match next.b_contents with
      | MathBox (c, _) -> begin match c with
        | BinOp -> begin match math_box_code last with
          | BinOp | Operator | Relation | Open | Punct ->
              check last (bin_to_ord next :: bs)
          | _ -> begin
              ListBuilder.add    buf last;
              ListBuilder.append buf spaces;
              check next bs
            end
          end
        | Relation | Punct | Close -> begin
            if math_box_code last = BinOp then
              ListBuilder.add buf (bin_to_ord last)
            else
              ListBuilder.add buf last;
            ListBuilder.append buf spaces;
            check next bs
          end
        | _ -> begin
            ListBuilder.add buf last;
            ListBuilder.append buf spaces;
            check next bs
          end
        end
      | _ -> begin
          ListBuilder.add spaces next;
          check last bs
        end
      end
  in
  if boxes = [] then
    []
  else
    List.tl (check (new_math_box Operator empty_box) boxes)
end


(*
  |add-spaces <style> <boxes>| adds spaces and break-boxes around operators.
*)

let add_spaces style boxes font_params math_params = begin
let params = get_font_params font_params style in
    let thin_skip = math_dim_to_points params math_params.thin_math_skip in
let med_skip = math_dim_to_points params math_params.med_math_skip in
    let thick_skip = math_dim_to_points params math_params.thick_math_skip in
let rel_break = new_break_box math_params.rel_penalty   false [] [] [] in
let binop_break = new_break_box math_params.binop_penalty false [] [] [] in

  let code_to_index (code : Runtime.MathTypes.math_code) = match code with
   Ordinary -> 0
  | Operator -> 1
  | BinOp    -> 2
  | Relation -> 3
  | Open     -> 4
  | Close    -> 5
  | Punct    -> 6
  | Inner    -> 7
  | _        -> 0 in

  let spacing_index_table =
  [|
    0;  2;  3;  4;  0;  0;  0;  1;  (* Ordinary *)
    2;  2; -1;  4;  0;  0;  0;  1;  (* Operator *)
    3;  3; -1; -1;  3; -1; -1;  3;  (* BinOp    *)
    4;  4; -1;  0;  4;  0;  0;  4;  (* Relation *)
    0;  0; -1;  0;  0;  0;  0;  0;  (* Open     *)
    0;  2;  3;  4;  0;  0;  0;  1;  (* Close    *)
    1;  1; -1;  1;  1;  1;  1;  1;  (* Punct    *)
    4;  4;  4;  0;  4;  0;  0;  4   (* Inner    *)
  |] in

  let spacing_table = match style with
  | Display | CrampedDisplay | Text | CrampedText ->
    [| dim_zero; thin_skip;    thin_skip; med_skip;     thick_skip   |]
  | _ ->
    [| dim_zero; dim_zero; thin_skip; dim_zero; dim_zero |]
  in

  let get_spacing first second =
    spacing_table.(spacing_index_table.(8 * code_to_index first + code_to_index second)) in

  let get_break code = match code with
  | Runtime.MathTypes.BinOp -> if math_params.binop_penalty <> num_zero then
                  [binop_break]
                else
                  []
  | Runtime.MathTypes.Relation -> if math_params.rel_penalty <> num_zero then
                  [rel_break]
                else
                  []
  | _ -> [] in
let buf = ListBuilder.make () in
    let spaces = ListBuilder.make () in

  let rec add_spacing boxes = match boxes with
  | []     -> ListBuilder.get buf
  | (b :: bs) -> begin
      if is_math_box b then begin
        let rec iter last boxes = match boxes with
  | [] -> begin
            ListBuilder.add    buf last;
            ListBuilder.append buf spaces;
            ListBuilder.get buf
end
        | (next :: bs) -> match next.b_contents with
   MathBox (next_code, _) -> begin
let last_code = math_box_code last in
    let spacing = get_spacing last_code next_code in
let break = get_break last_code in

              if dim_is_zero spacing then begin
                ListBuilder.add buf last;
                ListBuilder.add_list buf break;
                ListBuilder.append   buf spaces;
                iter next bs
end
              else begin
                ListBuilder.add buf last;
                ListBuilder.add buf (new_glue_box spacing dim_zero false true);
                ListBuilder.add_list buf break;
                ListBuilder.append   buf spaces;
                iter next bs
end
end
          | _ -> begin
              ListBuilder.add spaces next;
              iter last bs
            end
      in iter b bs
      end
      else begin
        ListBuilder.add buf b;
        add_spacing bs
      end
  end in
  let rec add_italic boxes = match boxes with
  | []     -> []
  | (box :: bs) -> match box.b_contents with
   MathBox (new_code, { b_contents = CharBox (c, f); _ }) -> begin
let gm = get_glyph_metric f c in
let italic = if new_code = Ordinary then num_zero else
                 gm.gm_italic in

        if italic = num_zero then
          box :: add_italic bs
        else
          box :: (new_kern_box (fixed_dim italic) (fixed_dim num_zero)) :: add_italic bs
    end
    | _ -> box :: add_italic bs


  in add_italic (add_spacing (check_bin_ops boxes))
end


(*
  |layout <style> <boxes> <font-params> <math-params>| enrichs a list of math-boxes by typesetting
  information. <style> is one of |Display|, |Text|, |Script|, |Script2|, |CrampedScript|, and
  |CrampedScript2|.
*)

let layout style boxes font_params math_params = begin
  let add_math_box box = match box.b_contents with
   CharBox (_, _) | CompBox _ -> new_math_box Ordinary box
  | _                       -> box
  in

  List.map remove_math_box
    (add_spaces
      style
      (merge_scripts
        style
        (List.map add_math_box boxes)
        font_params
        math_params
      )
      font_params
      math_params
    )
end


(* layout routines *)

let construct_delimiter
  delim_height
  (small_char, small_fonts, large_char, large_fonts)
  math_params = begin
  let total_height gm = gm.gm_height +/ gm.gm_depth in
  let make_delim font glyph = match glyph with
  | `Extendable (top, mid, bot, rep) ->
      Glyph.vertical_extendable delim_height font top mid bot rep
  | _ -> make_glyph_box glyph font
  in
  let rec loop_fonts fonts best_glyph best_height glyph = match fonts with
  | [] -> None
  | (f :: fs) -> begin
      let rec loop_chars glyph_ best_glyph_ best_height_ = match glyph_ with
      | `Undef -> loop_fonts fs best_glyph_ best_height_ glyph
      | `Extendable (_, _, _, _) -> Some (make_delim f glyph_)
      | _ -> begin
          let gm = get_glyph_metric f glyph_ in
          let height = total_height gm in
          if height >/ best_height_ then begin
            if height >=/ delim_height then
              Some (make_delim f glyph_)
            else match next_glyph f glyph_ with
            | None -> loop_fonts fs glyph_ height glyph
            | Some g     -> loop_chars g glyph_ height
          end
          else match next_glyph f glyph_ with
          | None -> loop_fonts fs best_glyph_ best_height_ glyph
          | Some g -> loop_chars g best_glyph_ best_height_
        end
      in
      loop_chars (`GlyphIndex glyph) best_glyph best_height
    end
  in
  let try_delim glyph fonts =
    if glyph < 0 then None else loop_fonts fonts `Undef num_zero glyph
  in
  match try_delim small_char small_fonts with
  | Some box -> box
  | _ -> match try_delim large_char large_fonts with
    | Some box -> box
    | _ -> new_compound_box math_params.null_delimiter_space dim_zero dim_zero []
end

let make_delimiter style delim_height delim font_params math_params = begin
let params = get_font_params font_params style in

  center_on_axis
    (construct_delimiter delim_height delim math_params)
    params.fm_axis_height
end


(*
  |simple-attach-delimiters <style> <left-delim> <right-delim> <body> <font-params> <math-params>|
  attaches delimiters to <body>. <left-delim> and <right-delim> are tuples of the form
  |(<small-char>, <small-fonts>, <large-char>, <large-fonts>)|.
*)

let simple_attach_delimiters style left_delim right_delim body font_params math_params = begin
  let make_delim size code delim =
    new_math_box
      code
      (make_delimiter style size delim font_params math_params) in

  let get_max get_dim boxes =
    List.fold_left
      (fun max_val x -> max_num max_val (get_dim x).d_base)
      num_zero
      boxes in
let layouted_body = layout style body font_params math_params in
    let max_height = get_max (fun b -> b.b_height) layouted_body in
let max_depth = get_max (fun b -> b.b_depth)  layouted_body in
let fm_axis_height = (get_font_params font_params style).fm_axis_height in
    let delta_1 = num_of_int 2 */ max_num (max_depth +/ fm_axis_height) (max_height -/ fm_axis_height) in
let delta_2 = math_params.delimiter_factor */ delta_1 in
let size = max_num delta_2 (delta_1 -/ math_params.delimiter_shortfall) in
    let left_del = make_delim size Open  left_delim in
let right_del = make_delim size Close right_delim in

  new_math_box
    Inner
    (HBox.make HBox.LR
      (Compose.box_add_lig_kern
        (layout style (left_del :: body @ [right_del]) font_params math_params)
      )
    )
end


(*
  |attach-delimiters <style> <delims> <bodies> <font-params> <math-params>|
  attaches delimiters to <bodies>. The number of delimiters must equal the number
  of bodies plus one. <delims> is a list of tuples of the form
  |(<small-char>, <small-fonts>, <large-char>, <large-fonts>)|.
*)

let attach_delimiters style delims bodies font_params math_params = begin
  let make_delim size code delim =
    new_math_box
      code
      (make_delimiter style size delim font_params math_params) in

  let get_max get_dim boxes =
    List.fold_left
      (fun max_val x -> max_num max_val (get_dim x).d_base)
      num_zero
      boxes in

  let layouted_bodies = List.map
                          (fun b -> layout style b font_params math_params)
                          bodies in
  let (max_height, max_depth) =
    List.fold_left
      (fun (mh, md) body ->
        (max_num mh (get_max (fun b -> b.b_height) body),
         max_num md (get_max (fun b -> b.b_depth)  body)))
      (num_zero, num_zero)
      layouted_bodies in
let fm_axis_height = (get_font_params font_params style).fm_axis_height in
    let delta_1 = num_of_int 2 */ max_num (max_depth +/ fm_axis_height) (max_height -/ fm_axis_height) in
let delta_2 = math_params.delimiter_factor */ delta_1 in
let size = max_num delta_2 (delta_1 -/ math_params.delimiter_shortfall) in
    let new_body = ListBuilder.make () in

  match delims with
  | []      -> raise (Invalid_argument "attach_delimiters: empty delimiter list")
  | (l :: ds) ->
      let rec iter delims_ bodies_ = match (delims_, bodies_) with
      | ([r], [b]) -> begin
          ListBuilder.add_list new_body b;
          ListBuilder.add      new_body (make_delim size Close r);

          new_math_box
            Inner
            (HBox.make HBox.LR
              (Compose.box_add_lig_kern
                (layout style (ListBuilder.get new_body) font_params math_params)))
        end
      | ((d :: ds), (b :: bs)) -> begin
          ListBuilder.add_list new_body b;
          ListBuilder.add      new_body (make_delim size Relation d);
          iter ds bs
        end
      | _ -> raise (Invalid_argument "attach_delimiters: mismatched number of delimiters and bodies")
      in
      ListBuilder.add new_body (make_delim size Open l);
      iter ds bodies
end


let make_operator style glyph font font_params = begin
  let make_op glyph =
    new_math_box
      Runtime.MathTypes.Operator
      (center_on_axis (new_glyph_box glyph font)
                      (get_font_params font_params style).fm_axis_height) in

  if is_display style then begin
    match next_glyph font glyph with
  | None -> make_op glyph
  | Some g     -> make_op g
end
  else
    make_op glyph
end


let attach_overline box clearance thickness = begin
  new_compound_box
    box.b_width
    (dim_add box.b_height (fixed_dim (clearance +/ num_of_int 2 */ thickness)))
    box.b_depth
    [Graphic.PutBox (dim_zero, dim_add box.b_height (fixed_dim clearance),
       new_rule_box box.b_width (fixed_dim thickness) dim_zero, None);
     Graphic.PutBox (dim_zero, dim_zero, box, None)]
end


let make_overline style boxes font_params math_params = begin
let body = HBox.make HBox.LR (Compose.box_add_lig_kern (layout style boxes font_params math_params)) in
    let thick = (get_font_params font_params style).fm_default_rule_thickness in

  new_math_box
    Ordinary
    (attach_overline body (num_of_int 3 */ thick) thick)
end


let make_underline style boxes font_params math_params = begin
let body = HBox.make HBox.LR (Compose.box_add_lig_kern (layout style boxes font_params math_params)) in
    let thick = (get_font_params font_params style).fm_default_rule_thickness in

  new_math_box
    Ordinary
    (new_compound_box
      body.b_width body.b_height (dim_add body.b_depth (fixed_dim (num_of_int 5 */ thick)))
      [Graphic.PutBox (dim_zero, dim_zero, body, None);
       Graphic.PutBox
         (dim_zero,
         dim_sub (fixed_dim (num_of_int (-4) */ thick)) body.b_depth,
         new_rule_box body.b_width (fixed_dim thick) dim_zero,
         None)]
    )
end


(*
  |make_fraction <style> <num> <denom> <left-delim> <right-delim> <thickness> <font-params> <math-params>|
  creates a fraction in style <style> with numerator <num> and denominator <denom>.
  <thickness> denotes the thickness of the fraction line. If it is negative a default let is used.
*)

let make_fraction style num denom left_delim right_delim thickness
                    font_params math_params = begin
  let rebox width boxes = match boxes with
  | [ ({ b_contents = CharBox (_, _); _ } as box) ] -> begin
      (* remove italic correction *)

      HBox.make_to HBox.LR
        width.d_base
        (Compose.box_add_lig_kern
          [new_glue_box dim_ss dim_zero false false;
           remove_icorr box;
           new_glue_box dim_ss dim_zero false false]
        )
    end
  | _ -> HBox.make_to HBox.LR
           width.d_base
           (Compose.box_add_lig_kern
             (new_glue_box dim_ss dim_zero false false
               :: boxes
                @ [new_glue_box dim_ss dim_zero false false])
           ) in
let params = get_font_params font_params style in
    let thick        = if thickness >= num_zero then
                       thickness
                     else
                       params.fm_default_rule_thickness in
let num_boxes = layout (numerator_style style)   num   font_params math_params in
    let denom_boxes = layout (denominator_style style) denom font_params math_params in
let num_width = HBox.calc_width num_boxes in
    let denom_width = HBox.calc_width denom_boxes in
  let num_box      = if num_width.d_base </ denom_width.d_base then
                       rebox denom_width num_boxes
                     else
                       HBox.make HBox.LR (Compose.box_add_lig_kern num_boxes) in
  let denom_box    = if denom_width.d_base </ num_width.d_base then
                       rebox num_width denom_boxes
                     else
                       HBox.make HBox.LR (Compose.box_add_lig_kern denom_boxes) in
let shift_up_1 = get_num_shift params style thick in
    let shift_down_1 = get_denom_shift params style in

  let make_fract shift_up shift_down rule_shift =
    simple_attach_delimiters
      style
      left_delim
      right_delim
      [new_math_box
         Ordinary
         (new_compound_box
           num_box.b_width
           (dim_add num_box.b_height  (fixed_dim shift_up))
           (dim_add denom_box.b_depth (fixed_dim shift_down))
           ([Graphic.PutBox (dim_zero, fixed_dim shift_up, num_box, None)]
           @ (if thick <>/ num_zero then
                [Graphic.PutBox
                   (dim_zero,
                   fixed_dim rule_shift,
                   new_rule_box num_box.b_width (fixed_dim thick) dim_zero,
                   None)]
              else
                [])
           @ [Graphic.PutBox (dim_zero, fixed_dim (minus_num shift_down), denom_box, None)]
           )
         )]
      font_params
      math_params in

  if thick = num_zero then begin
    let clear = match style with
   Display | CrampedDisplay -> num_of_int 7 */ params.fm_default_rule_thickness
                | _                        -> num_of_int 3 */ params.fm_default_rule_thickness in

    let delta = (clear -/ (shift_up_1   -/ num_box.b_depth.d_base)
                       -/ (shift_down_1 -/ denom_box.b_height.d_base))
                  // num_of_int 2 in

    make_fract
      (shift_up_1   +/ max_num num_zero delta)
      (shift_down_1 +/ max_num num_zero delta)
      num_zero
end
  else begin
    let clear      = match style with
   Display | CrampedDisplay -> num_of_int 3 */ thick
                     | _                        -> thick in
let delta = thick // num_of_int 2 in
    let delta_up = clear +/ delta +/ num_box.b_depth.d_base    -/ shift_up_1   +/ params.fm_axis_height in
let delta_down = clear +/ delta +/ denom_box.b_height.d_base -/ shift_down_1 -/ params.fm_axis_height in

    make_fract
      (shift_up_1   +/ max_num num_zero delta_up)
      (shift_down_1 +/ max_num num_zero delta_down)
      (params.fm_axis_height -/ delta)
  end
end

let make_root style box delim font_params math_params = begin
let param = get_font_params font_params style in
    let clearance    = if is_display style then
                       param.fm_default_rule_thickness +/ abs_num param.fm_axis_height // num_of_int 4
                     else
                       num_of_int 5 */ param.fm_default_rule_thickness // num_of_int 4 in
let total_height = box.b_height.d_base +/ box.b_depth.d_base +/ clearance in
    let root         = construct_delimiter
                       (total_height +/ param.fm_default_rule_thickness)
                       delim
                       math_params in
let delta = root.b_depth.d_base -/ total_height in
    let real_clear   = if delta >/ num_zero then
                       clearance +/ delta // num_of_int 2
                     else
                       clearance in

  HBox.make HBox.LR
    (Compose.box_add_lig_kern
      [shift_compound_vert (wrap_in_compound_box root) (fixed_dim (box.b_height.d_base +/ real_clear));
       attach_overline box real_clear root.b_height.d_base]
    )
end


let make_accent style char font boxes font_params math_params = begin
let body = HBox.make HBox.LR (Compose.box_add_lig_kern (layout (cramped_style style) boxes font_params math_params)) in
    let width = body.b_width.d_base in
let height = body.b_height.d_base
 in
    let get_skew () = match boxes with
| [b] -> (match (remove_math_box b).b_contents with
   | CharBox (c, f) -> begin
  let sg = f.fm_skew_glyph in
  let char_index (g: GlyphMetric.glyph_desc) = match g with
    | `GlyphIndex i -> i
    | `Simple i -> i
    | `Char i -> i
    | `Accent (i, _) -> i
    | _ -> -1
  in
  let ci = char_index c in
  let sgi = char_index sg in
        if sg <> `Undef then
          match get_lig_kern f ci sgi with
   `Kern k -> k
          | _      -> num_zero

        else
          num_zero
       end
    | _ -> num_zero)
  | _ -> num_zero
  in

  let find_char char = begin
    let rec iter char = match next_glyph font char with
    | None -> char
    | Some next  -> begin
        let new_gm = get_glyph_metric font next in

        if new_gm.gm_width <=/ width then
          iter next
        else
          char
        end
      in iter char
    end in
  let x_height = font.fm_x_height in
    let delta = min_num height x_height in
let accent = make_glyph_box (find_char (`Simple char)) font in
    let skew = get_skew () in
  let box      = new_compound_box
                   (fixed_dim width)
                   (fixed_dim (height +/ accent.b_depth.d_base +/ accent.b_height.d_base -/ delta))
                   (fixed_dim body.b_depth.d_base)
                   [Graphic.PutBox (fixed_dim (skew +/ (width -/ accent.b_width.d_base) // num_of_int 2),
                      fixed_dim (height +/ accent.b_depth.d_base -/ delta), accent, None);
                    Graphic.PutBox (dim_zero, dim_zero, body, None)] in

  new_math_box
    (match boxes with
     | [{ b_contents = MathBox (c, _) ; _}] -> c
     | _                                 -> Ordinary
    )
    (if box.b_height.d_base </ height then
       { (box) with b_height = fixed_dim height }
     else
       box
    )
end


