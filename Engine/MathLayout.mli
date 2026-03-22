
open Tools.XNum
open Runtime
open Unicode.UTypes
open Dim
open Runtime.Substitute
open FontMetric

type math_style =
    Display
  | CrampedDisplay
  | Text
  | CrampedText
  | Script
  | CrampedScript
  | Script2
  | CrampedScript2

val cramped_style : math_style -> math_style
val sub_style : math_style -> math_style
val super_style : math_style -> math_style
val numerator_style : math_style -> math_style
val denominator_style : math_style -> math_style

type math_font_params = FontMetric.font_metric * FontMetric.font_metric * FontMetric.font_metric

val make_font_params : FontMetric.font_metric -> FontMetric.font_metric -> FontMetric.font_metric
val get_font_params : math_font_params -> math_style -> FontMetric.font_metric

type math_params =
  { thin_math_skip : dim;
    med_math_skip : dim;
    thick_math_skip : dim;
    script_space : dim;
    rel_penalty : float;
    binop_penalty : float;
    delimiter_factor : float;
    delimiter_shortfall : float;
    null_delimiter_space : dim; }

val math_units_to_points : FontMetric.font_metric -> num -> num
val remove_math_box : Box.box -> Box.box

type delimiter_code = Unicode.UTypes.uc_char * FontMetric.font_metric list * Unicode.UTypes.uc_char * FontMetric.font_metric list

val make_operator : 
  math_style -> Runtime.GlyphMetric.glyph_desc -> FontMetric.font_metric -> math_font_params -> Box.box 

val make_fraction :
  math_style -> Box.box list -> Box.box list -> delimiter_code -> delimiter_code ->
    num -> math_font_params -> math_params -> Box.box
val attach_delimiters :
  math_style -> delimiter_code list -> Box.box list list -> math_font_params -> math_params -> Box.box
val make_root :
  math_style -> Box.box -> delimiter_code -> math_font_params -> math_params ->
    Box.box
val make_overline :
  math_style -> Box.box list -> math_font_params -> math_params -> Box.box
val make_underline :
  math_style -> Box.box list -> math_font_params -> math_params -> Box.box
val make_accent :
  math_style -> uc_char -> font_metric -> Box.box list -> math_font_params ->
    math_params -> Box.box
val layout :
  math_style -> Box.box list -> (FontMetric.font_metric * FontMetric.font_metric * FontMetric.font_metric) -> math_params -> Box.box list
