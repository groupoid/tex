
open Tools.XNum
open Unicode.UTypes
open Runtime
open FontMetric
open Dim
(* open Typesetting *)
open Box

type environment

type env_cmd = Unicode.UCStream.location -> environment -> environment

type skip_arg = environment -> num
type dim_arg  = environment -> dim

type line_param_arg =
  (dim_arg * skip_arg * dim_arg * (box -> box -> Galley.line_params -> dim) * (int -> int -> num))
type par_param_arg  =
  (num * dim_arg * dim_arg * dim_arg * dim_arg * (environment -> int -> (num * num)) * dim_arg *
   (environment -> Box.extended_glyph_item list) *
   (environment -> Box.extended_glyph_item list) *
   (environment -> box list -> box list))
type line_break_param_arg = (num * num * int * num * num * num * num * skip_arg * num * skip_arg * bool)
type hyphen_param_arg = (uc_string * num * num * int * int * uc_string)
type space_param_arg = (num * dim_arg option * dim_arg option * bool)
type math_param_arg =
  (dim_arg * dim_arg * dim_arg * dim_arg * num * num * num * skip_arg * dim_arg)

type par_param_modifier  =
  (num option * dim_arg option * dim_arg option * dim_arg option * dim_arg option *
   (environment -> int -> (num * num)) option * dim_arg option *
   (environment -> Box.extended_glyph_item list) option *
   (environment -> Box.extended_glyph_item list) option *
   (environment -> box list -> box list) option)
type line_param_modifier =
  (dim_arg option * skip_arg option * dim_arg option * (box -> box -> Galley.line_params -> dim) option *
   (int -> int -> num) option)
type line_break_param_modifier =
  (num option * num option * int option * num option * num option * num option *
   num option * skip_arg option * num option * skip_arg option * bool option)
type hyphen_param_modifier =
  (uc_string option * num option * num option * int option * int option * uc_string option)
type space_param_modifier =
  (num option * dim_arg option * dim_arg option * bool option)
type math_param_modifier =
  (dim_arg option * dim_arg option * dim_arg option * dim_arg option *
   num option * num option * num option * skip_arg option * dim_arg option)

type font_spec = (uc_string option * uc_string option * uc_string option * num option * uc_string option * uc_string list option)

val galley_table                        : environment -> Galley.galley Runtime.PTable.table
val current_galley                      : environment -> Galley.galley
val page_layout_table                   : environment -> PageLayout.page_layout PTable.table
val current_page_layout                 : environment -> PageLayout.page_layout
val current_page_number                 : environment -> int
val current_float_misplacement_demerits : environment -> num
val current_math_style                  : environment -> MathLayout.math_style
val current_font                        : environment -> Fonts.font
val current_math_fonts                  : environment -> (Fonts.font * Fonts.font * Fonts.font) array
val current_math_font_params            : environment -> MathLayout.math_font_params
val current_script_size                 : environment -> num
val current_script_script_size          : environment -> num
val current_font_metric                 : environment -> font_metric
val current_composer                    : environment -> glyph_composer
val get_pages                           : environment -> FontMetric.page list

val set_math_style : environment -> MathLayout.math_style -> environment

val sync_tables         : environment -> environment
val save_environment    : environment -> environment
val restore_environment : environment -> environment

val get_hyphen_table : Unicode.UCStream.location -> uc_string -> Hyphenation.hyphen_table

val new_galley       : uc_string -> num -> env_cmd
val select_galley    : uc_string -> env_cmd
val set_galley_table : environment -> Galley.galley PTable.table -> environment
val set_galley       : Galley.galley -> env_cmd

val set_par_params                : par_param_modifier -> env_cmd
val set_line_params               : line_param_modifier -> env_cmd
val set_line_break_params         : line_break_param_modifier -> env_cmd
val set_hyphen_params             : hyphen_param_modifier -> env_cmd
val set_space_params              : space_param_modifier -> env_cmd
val set_math_params               : math_param_modifier -> env_cmd
val set_current_par_params        : par_param_modifier -> env_cmd
val set_current_line_params       : line_param_modifier -> env_cmd
val set_current_line_break_params : line_break_param_modifier -> env_cmd
val set_current_hyphen_params     : hyphen_param_modifier -> env_cmd
val set_current_space_params      : space_param_modifier -> env_cmd
val set_current_math_params       : math_param_modifier -> env_cmd

val modify_par_params        : par_param_modifier -> environment ->
                               ParLayout.par_params -> ParLayout.par_params
val modify_line_params       : line_param_modifier -> environment ->
                               Galley.line_params -> Galley.line_params
val modify_line_break_params : line_break_param_modifier -> environment ->
                               ParLayout.line_break_params -> ParLayout.line_break_params
val modify_hyphen_params     : hyphen_param_modifier -> Unicode.UCStream.location ->
                               JustHyph.hyphen_params -> JustHyph.hyphen_params
val modify_space_params      : space_param_modifier -> environment ->
                               Galley.space_params -> Galley.space_params
val modify_math_params       : math_param_modifier -> environment ->
                               MathLayout.math_params -> MathLayout.math_params

val set_colour               : Graphic.colour -> env_cmd

val adjust_graphics_state : environment -> environment -> box list

val new_page_layout     : uc_string -> num -> num -> env_cmd
val select_page_layout  : uc_string -> env_cmd
val set_page_layout     : PageLayout.page_layout -> env_cmd
val add_pages           : int -> FontMetric.page list -> env_cmd

val declare_font        : uc_string -> uc_string -> uc_string ->
                          uc_string -> (num * num) -> font_load_params -> env_cmd
val set_font            : font_spec -> env_cmd
val set_font_metric     : Fonts.font -> env_cmd
val get_math_font       : environment -> MathLayout.math_style -> int -> font_metric
val set_math_font       : (int option * uc_string option * uc_string option * uc_string option
                           * num option * num option * num option) -> env_cmd

val adapt_fonts_to_math_style : env_cmd

val set_space_factor    : environment -> num -> environment
val get_space_factor    : environment -> uc_char -> num
val adjust_space_factor : uc_char -> env_cmd

val initialise_environment : unit -> environment
