
(* stub to replace Num by Gmp *)

open Gmp

type num = Q.t

val num_zero : num
val num_one : num
val num_minus_one : num
val num_two : num
val num_three : num
val num_ten : num

val add_num : num -> num -> num
val minus_num : num -> num
val sub_num : num -> num -> num
val mult_num : num -> num -> num
val square_num : num -> num
val div_num : num -> num -> num
val quo_num : num -> num -> num
val mod_num : num -> num -> num
val power_num : num -> num -> num
val abs_num : num -> num
val succ_num : num -> num
val pred_num : num -> num
val incr_num : num ref -> unit
val decr_num : num ref -> unit
val is_integer_num : num -> bool

val integer_num : num -> num
val floor_num : num -> num
val round_num : num -> num
val ceiling_num : num -> num
val sign_num : num -> int
val eq_num : num -> num -> bool
val lt_num : num -> num -> bool
val le_num : num -> num -> bool
val gt_num : num -> num -> bool
val ge_num : num -> num -> bool

val compare_num : num -> num -> int
val max_num : num -> num -> num
val min_num : num -> num -> num

val land_num : num -> num -> num
val lor_num : num -> num -> num
val lxor_num : num -> num -> num
val lneg_num : num -> num

val string_of_num : num -> string
val num_of_string : string -> num

val int_of_num : num -> int
val num_of_int : int -> num
val num_of_ints : int -> int -> num
val float_of_num : num -> float
val num_of_float : float -> num

val serialise_num : [> IO_Base.io_w ] IO_Base.io -> num -> unit
val unserialise_num : [> IO_Base.io_r ] IO_Base.io -> num

