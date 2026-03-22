
open Tools.XNum
open Unicode.UTypes
open Runtime
open Runtime.Substitute
open FontMetric
(* (* open Typesetting *) *)
open Box

type font_definition = {
  fd_name : uc_string;
  fd_family : uc_string;
  fd_series : uc_string;
  fd_shape : uc_string;
  fd_min_size : num;
  fd_max_size : num;
  mutable fd_loaded_sizes : (num * font_metric) list;
  fd_data : font_load_params;
}

type font = {
  f_font_def : font_definition;
  f_metric : font_metric;
  f_size : num;
}

type font_table

val get_font : font_table -> uc_string -> uc_string -> uc_string -> num -> font option
val declare_font : font_table -> uc_string -> uc_string -> uc_string -> uc_string -> (num * num) -> font_load_params -> font_table

val initialise_font_table : unit -> font_table

val make_virtual_font : uc_string -> num -> Box.box array -> num array -> GlyphMetric.lig_kern array -> font_load_params -> font_metric
