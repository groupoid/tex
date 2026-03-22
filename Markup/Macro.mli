
open Runtime
open Unicode.UTypes
open ParseState

type arg_specifier =
    Arg
  | Opt of uc_list
  | Bool

val parse_arg_template : UCStream.location -> uc_list -> arg_specifier list
val substitute : uc_list list -> uc_list -> uc_list
val expand : parse_state -> uc_list
val noexpand : parse_state -> uc_list -> uc_list
val expand_string : parse_state -> uc_list -> uc_list

val execute_macro : arg_specifier list -> uc_list -> parse_state -> unit
val expand_macro :
  arg_specifier list -> uc_list -> parse_state -> uc_list -> uc_list
val execute_command : parse_state -> uc_list -> unit

val begin_env : parse_state -> uc_list -> unit
val end_env : parse_state -> uc_list -> unit

val execute_begin_environment :
  uc_list -> arg_specifier list -> uc_list -> parse_state -> unit
val expand_begin_environment :
  uc_list -> arg_specifier list -> uc_list -> parse_state -> uc_list ->
    uc_list
val execute_end_environment : uc_list -> parse_state -> unit
val expand_end_environment : uc_list -> parse_state -> uc_list -> uc_list

