
open Tools.XNum
open Box

type footnote_params =
  { separator : box;
    top_skip : num;
    bottom_skip : num;
    grid_size : num;
    line_params : Galley.line_params;
    par_params : ParLayout.par_params;
    line_break_params : ParLayout.line_break_params;
    hyphen_params : Runtime.JustHyph.hyphen_params;
    space_params : Galley.space_params;
    math_params : MathLayout.math_params }

val layout : footnote_params -> PageLayout.area_contents_function
