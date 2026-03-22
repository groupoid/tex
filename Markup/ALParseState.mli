
open XNum
open Unicode.UTypes
open Runtime

open Types
open ParseState

val tracing_al_commands : bool ref

val execute_ps_command_unknown : string -> unknown -> parse_state -> unit
val execute_ps_command : string -> UCStream.istream -> parse_state -> unit
val encode_command : string -> command -> partial_value
val decode_command : string -> unknown -> unknown -> command

val call_at_exit : parse_state -> unit

val set_num_global : parse_state -> uc_string -> num -> unit
val set_string_global : parse_state -> uc_string -> uc_string -> unit

val ps_get_global : unknown list -> partial_value
val ps_set_global : unknown list -> partial_value
val ps_next_char : unknown -> unknown -> partial_value
val ps_get_char : unknown list -> partial_value
val ps_remove_chars : unknown -> unknown -> partial_value
val ps_insert_string : unknown -> unknown -> partial_value
val ps_location : unknown -> unknown -> partial_value
val ps_read_arg : unknown -> unknown -> partial_value
val ps_arg_expanded : unknown -> unknown -> partial_value
val ps_arg_execute : unknown list -> partial_value
val ps_arg_num : unknown -> unknown -> partial_value
val ps_arg_int : unknown -> unknown -> partial_value
val ps_arg_skip : unknown -> unknown -> partial_value
val ps_arg_dim : unknown -> unknown -> partial_value
val ps_arg_key_val : unknown -> unknown -> partial_value
val ps_arg_dict : unknown list -> partial_value
val ps_opt_expanded : unknown list -> partial_value
val ps_opt_key_val : unknown -> unknown -> partial_value
val ps_opt_int : unknown list -> partial_value
val ps_arg_TeX_dim : unknown -> unknown -> partial_value
val ps_current_mode : unknown -> unknown -> partial_value
val ps_open_node_list : unknown -> unknown -> partial_value
val ps_close_node_list : unknown list -> partial_value
val ps_add_node : unknown -> unknown -> partial_value
val ps_set_default_char_cmd : unknown list -> partial_value
val ps_define_command : unknown list -> partial_value
val ps_define_pattern : unknown list -> partial_value
val ps_define_macro : unknown list -> partial_value
val ps_save_command : unknown -> unknown -> partial_value
val ps_restore_command : unknown -> unknown -> partial_value
val ps_save_pattern : unknown -> unknown -> partial_value
val ps_restore_pattern : unknown -> unknown -> partial_value
val ps_lookup_command : unknown list -> partial_value
val ps_push_env : unknown list -> partial_value
val ps_pop_env : unknown list -> partial_value
val ps_set_env_args : unknown -> unknown -> partial_value
val ps_top_env : unknown list -> partial_value
val ps_lookup_env : unknown list -> partial_value
val ps_define_env : unknown list -> partial_value
val ps_shipout_pages : unknown list -> partial_value
val ps_new_page_layout : unknown list -> partial_value
val ps_new_galley : unknown list -> partial_value
val ps_new_area : unknown list -> partial_value
val ps_declare_font : unknown list -> partial_value
val ps_define_math_symbol : unknown list -> partial_value
val ps_define_root_symbol : unknown list -> partial_value
val ps_define_math_accent : unknown list -> partial_value
val ps_set_colour : unknown -> unknown -> partial_value
val ps_set_bg_colour : unknown -> unknown -> partial_value
val ps_set_alpha : unknown -> unknown -> partial_value
val ps_draw :
  string -> Graphic.path_cmd -> unknown -> unknown -> partial_value
val ps_set_line_width : unknown -> unknown -> partial_value
val ps_set_line_cap : unknown -> unknown -> partial_value
val ps_set_line_join : unknown -> unknown -> partial_value
val ps_set_miter_limit : unknown -> unknown -> partial_value
val ps_page_command : unknown -> unknown -> partial_value
val ps_par_command : unknown -> unknown -> partial_value
val ps_set_math_code : unknown list -> partial_value
val ps_new_counter : unknown list -> partial_value
val ps_get_counter : unknown list -> partial_value
val ps_set_counter : unknown list -> partial_value
val ps_dvi_special : unknown -> unknown -> partial_value
val ps_warning : unknown -> unknown -> partial_value
val ps_error : unknown -> unknown -> partial_value
val ps_execute_next_char : unknown -> unknown -> partial_value
val ps_execute_stream : unknown -> unknown -> partial_value
val ps_execute_argument : unknown -> partial_value
val ps_run_parser : unknown list -> partial_value

