
open Tools.XNum
open Runtime
open Dim
open Box

type vert_alignment = Top | Bottom

type float_params =
  { alignment : vert_alignment;
    top_skip : num;
    bottom_skip : num;
    float_sep : dim }

val layout : float_params -> PageLayout.area_contents_function
