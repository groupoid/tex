
open Vm_types.Types

val make_path : unknown -> partial_value
val close_path : unknown -> unknown -> partial_value
val add_point : unknown -> unknown -> partial_value
val add_in_dir : unknown -> unknown -> partial_value
val add_in_angle : unknown -> unknown -> partial_value
val add_in_curl : unknown -> unknown -> partial_value
val add_in_tension : unknown -> unknown -> partial_value
val add_out_dir : unknown -> unknown -> partial_value
val add_out_angle : unknown -> unknown -> partial_value
val add_out_curl : unknown -> unknown -> partial_value
val add_out_tension : unknown -> unknown -> partial_value
val add_control_points : unknown -> unknown -> unknown -> partial_value

