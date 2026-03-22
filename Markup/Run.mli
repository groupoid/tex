
open Runtime
open Unicode.UTypes
open Engine
open ParseState

(* hooks for loaded modules *)

val register_parse_state_hook : (parse_state -> unit) -> unit
val register_init_hook : (unit -> unit) -> unit

(* routines to start ant *)

val initialise : Job.job -> unit
val parse_document : parse_state -> Node.node list
val parse_file : Job.job -> string -> Node.node list * parse_state
val parse_string : Job.job -> uc_string -> Node.node list * parse_state

