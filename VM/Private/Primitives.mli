
open Unicode.UTypes
open Vm_types.Types

val uc_string_to_char_list : uc_string -> partial_value
val uc_list_to_char_list : uc_list -> partial_value
val ascii_to_char_list : string -> partial_value
val evaluate_char_list : string -> unknown -> uc_list

val initial_scope : unit -> Scope.scope

val bind_primitive : Scope.scope -> string -> partial_value -> unit
val bind_bin_op_l : Scope.scope -> string -> int -> partial_value -> unit
val bind_bin_op_n : Scope.scope -> string -> int -> partial_value -> unit
val bind_bin_op_r : Scope.scope -> string -> int -> partial_value -> unit
val bind_pre_op : Scope.scope -> string -> partial_value -> unit
val bind_post_op : Scope.scope -> string -> partial_value -> unit

