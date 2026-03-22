
open Engine
open ParseState

(* |begin_group| starts a new group. |end_group| ends the current group. *)

let begin_group ps = add_node ps (Node.BeginGroup (location ps))

let end_group ps = add_node ps (Node.EndGroup (location ps))

