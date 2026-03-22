
open ParseState

val init_source_specials : Engine.Job.job -> unit

val begin_paragraph : parse_state -> unit
val end_paragraph : parse_state -> unit
val begin_math : parse_state -> unit
val end_math : parse_state -> unit
val set_mode : parse_state -> mode -> bool
val ensure_par_mode : parse_state -> unit
val leave_par_mode : parse_state -> unit

