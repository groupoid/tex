
open XNum
open Runtime
open Unicode.UTypes
open Unicode.SymbolTable

open Types
open Engine
open Environment
open ALCoding

val wrap_env : environment -> partial_value
val unwrap_env : string -> unknown -> environment

val decode_env_cmd : string -> unknown -> env_cmd
val encode_env_cmd : string -> env_cmd -> partial_value

val encode_skip_arg : skip_arg -> partial_value
val decode_skip_arg : string -> unknown -> skip_arg

val encode_dim_arg : dim_arg -> partial_value
val decode_dim_arg : string -> unknown -> dim_arg

val lookup_skip : string -> unknown SymbolMap.t -> symbol -> skip_arg option
val lookup_dim : string -> unknown SymbolMap.t -> symbol -> dim_arg option

val env_quad : unknown -> unknown -> partial_value
val env_x_height : unknown -> unknown -> partial_value
val env_math_unit : unknown -> unknown -> partial_value

val decode_leading :
  string -> symbol option ->
    (Box.box -> Box.box ->
       Galley.line_params -> Dim.dim)
      option

val decode_par_params : string -> unknown -> par_param_modifier
val decode_line_params : string -> unknown -> line_param_modifier
val decode_line_break_params : string -> unknown -> line_break_param_modifier
val decode_hyphen_params : string -> unknown -> hyphen_param_modifier
val decode_space_params : string -> unknown -> space_param_modifier
val decode_math_params : string -> unknown -> math_param_modifier

val prim_set_par_params : unknown -> partial_value
val prim_set_current_par_params : unknown -> partial_value
val prim_set_line_params : unknown -> partial_value
val prim_set_current_line_params : unknown -> partial_value
val prim_set_line_break_params : unknown -> partial_value
val prim_set_current_line_break_params : unknown -> partial_value
val prim_set_hyphen_params : unknown -> partial_value
val prim_set_current_hyphen_params : unknown -> partial_value
val prim_set_space_params : unknown -> partial_value
val prim_set_current_space_params : unknown -> partial_value
val prim_set_math_params : unknown -> partial_value
val prim_set_current_math_params : unknown -> partial_value
val prim_new_galley : unknown -> unknown -> partial_value
val prim_select_galley : unknown -> partial_value
(*value prim_set_par_shape                 : unknown -> unknown -> unit*)
val prim_set_colour : unknown -> partial_value
val prim_new_page_layout : unknown list -> partial_value
val prim_select_page_layout : unknown -> partial_value
(*value prim_get_math_font                 : unknown -> list unknown -> unit*)
val prim_set_math_font : unknown -> partial_value
val prim_adapt_fonts_to_math_style : partial_value
val prim_get_space_factor : unknown -> unknown -> partial_value
val prim_adjust_space_factor : unknown -> partial_value

