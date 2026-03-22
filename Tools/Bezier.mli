open XNum

type spline = (num * num * num * num * num * num * num * num)
type path_spec

val angle_of_vec : num -> num -> float

val make_spec          : num -> num -> path_spec
val close_spec         : path_spec -> bool -> spline array

val add_point          : path_spec -> num -> num -> path_spec
val add_in_dir         : path_spec -> float -> path_spec
val add_in_curl        : path_spec -> num -> path_spec
val add_in_tension     : path_spec -> num -> path_spec
val add_out_dir        : path_spec -> float -> path_spec
val add_out_curl       : path_spec -> num -> path_spec
val add_out_tension    : path_spec -> num -> path_spec
val add_control_points : path_spec -> num -> num -> num -> num -> path_spec
