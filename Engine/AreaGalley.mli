
open Tools.XNum
open Runtime
open Unicode.UTypes

val tracing_page_breaks : bool ref

type area_params =
  { galley : uc_string;
    top_skip : num;
    bottom_skip : num;
    min_size : num;
    grid_size : num }

val contents_from_galley : area_params -> PageLayout.area_contents_function
