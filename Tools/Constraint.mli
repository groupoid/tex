
open XNum

exception Inconsistent

type var = int
type bound = var * num
type equation = var LinForm.lin_form

type coordinate = {
  min          : num;
  max          : num;
  lower_bounds : bound list;
  upper_bounds : bound list;
  equations    : equation list;
}

type system

val create : unit -> system
val clone : system -> system
val add_variable : system -> num -> num -> num -> num -> unit
val update : system -> unit
val set_x_coord : system -> var -> num -> unit
val set_y_coord : system -> var -> num -> unit
val add_x_equation : system -> var LinForm.lin_form -> unit
val add_y_equation : system -> var LinForm.lin_form -> unit
val add_x_bound : system -> var -> var -> num -> unit
val add_y_bound : system -> var -> var -> num -> unit
