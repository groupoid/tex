
open Runtime
open Unicode.UTypes

(* counters *)

type counter_table

val empty_table : counter_table
val new_counter : UCStream.location -> counter_table -> uc_string -> int -> uc_string option -> counter_table
val get_counter : UCStream.location -> counter_table -> uc_string -> int
val set_counter : UCStream.location -> counter_table -> uc_string -> int -> counter_table

