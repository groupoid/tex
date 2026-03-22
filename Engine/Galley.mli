
open Tools.XNum
open Runtime
open Dim
open Runtime.Substitute
open FontMetric
open Box

type line_params =
{
  baseline_skip      : dim;              (* amount of glue between baselines           *)
  line_skip_limit    : num;              (* minimal amout of glue between lines        *)
  line_skip          : dim;              (* the amout if the lines are closer together *)
  leading            : box -> box -> line_params -> dim;
  club_widow_penalty : int -> int -> num  (* penalty between two lines                  *)
}

type space_params =
{
  space_factor      : num;
  space_skip        : dim option;
  xspace_skip       : dim option;
  victorian_spacing : bool
}

type graphics_params =
{
  gp_colour    : Graphic.colour;
  gp_bg_colour : Graphic.colour;
  gp_alpha     : num
}

type galley

val leading_fixed    : box -> box -> line_params -> dim
val leading_register : box -> box -> line_params -> dim
val leading_TeX      : box -> box -> line_params -> dim
val leading_skyline  : box -> box -> line_params -> dim

val new_galley    : num -> line_params -> ParLayout.par_params -> ParLayout.line_break_params ->
                      JustHyph.hyphen_params -> space_params -> MathLayout.math_params -> galley
val measure       : galley -> num
val galley_lines  : galley -> box list
val get_line      : galley -> int -> box
val keep_lines    : galley -> box list -> galley
val modify_glue   : galley -> (box list -> box list) -> galley

val graphics_params               : galley -> graphics_params
val par_params                    : galley -> ParLayout.par_params
val line_params                   : galley -> line_params
val line_break_params             : galley -> ParLayout.line_break_params
val hyphen_params                 : galley -> JustHyph.hyphen_params
val space_params                  : galley -> space_params
val math_params                   : galley -> MathLayout.math_params
val current_par_params            : galley -> ParLayout.par_params
val current_line_params           : galley -> line_params
val current_line_break_params     : galley -> ParLayout.line_break_params
val current_hyphen_params         : galley -> JustHyph.hyphen_params
val current_space_params          : galley -> space_params
val current_math_params           : galley -> MathLayout.math_params
val set_graphics_params           : galley -> graphics_params -> galley
val set_par_params                : galley -> ParLayout.par_params -> galley
val set_line_params               : galley -> line_params -> galley
val set_line_break_params         : galley -> ParLayout.line_break_params -> galley
val set_hyphen_params             : galley -> JustHyph.hyphen_params -> galley
val set_space_params              : galley -> space_params -> galley
val set_math_params               : galley -> MathLayout.math_params -> galley
val set_current_par_params        : galley -> ParLayout.par_params -> galley
val set_current_line_params       : galley -> line_params -> galley
val set_current_line_break_params : galley -> ParLayout.line_break_params -> galley
val set_current_hyphen_params     : galley -> JustHyph.hyphen_params -> galley
val set_current_space_params      : galley -> space_params -> galley
val set_current_math_params       : galley -> MathLayout.math_params -> galley

val copy_params           : galley -> galley -> galley
val reset_params          : galley -> galley

val add_line      : galley -> box -> galley
val add_glue      : galley -> box -> galley
val add_paragraph : galley -> Unicode.UCStream.location -> Box.extended_glyph_item list -> galley

val put_in_vbox   : galley -> box
val put_in_vtop   : galley -> box
