
open Tools.XNum
open Runtime
open Dim
open Runtime.Substitute
open FontMetric
open Box

val tracing_line_breaks : bool ref

type line_break_params =
  { pre_tolerance : num;
    tolerance : num;
    looseness : int;
    line_penalty : num;
    adj_demerits : num;
    double_hyphen_demerits : num;
    final_hyphen_demerits : num;
    emergency_stretch : num;
    river_demerits : num;
    river_threshold : num;
    simple_breaking : bool }

type par_params =
  { measure : num;
    par_indent : dim;
    par_fill_skip : dim;
    par_skip : dim;
    left_skip : dim;
    right_skip : dim;
    par_shape : int -> num * num;
    pre_break : Box.extended_glyph_item list;
    post_break : Box.extended_glyph_item list;
    post_process_line : box list -> box list }

val layout_line : num -> int -> box list -> par_params -> box

val break_paragraph :
  Unicode.UCStream.location -> Box.extended_glyph_item list -> par_params ->
    line_break_params -> JustHyph.hyphen_params -> box list list
