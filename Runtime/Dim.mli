
open Tools.XNum

type dim = {
  d_base : num;
  d_stretch_factor : num;
  d_stretch_order : int;
  d_shrink_factor : num;
  d_shrink_order : int;
}

type xdim = {
  xd_base : num;
  xd_stretch : (num * int) list;
  xd_shrink : (num * int) list;
}

val fixed_dim : num -> dim
val dim_zero : dim
val dim_1pt : dim
val dim_12pt : dim
val dim_fil : dim
val dim_fill : dim
val dim_ss : dim
val dim_filneg : dim
val xdim_zero : xdim

val xdim_stretch : xdim -> num * int
val xdim_shrink : xdim -> num * int

val xdim_to_dim : xdim -> dim
val dim_to_xdim : dim -> xdim

val dim_equal : dim -> dim -> bool
val dim_is_zero : dim -> bool
val log_dim : dim -> unit

val of_ascii : string -> dim
val fixed_of_ascii : string -> dim
val dim_add : dim -> dim -> dim
val dim_neg : dim -> dim
val dim_sub : dim -> dim -> dim
val dim_mult : num -> dim -> dim
val xdim_add : xdim -> xdim -> xdim
val xdim_neg : xdim -> xdim
val xdim_sub : xdim -> xdim -> xdim
val xdim_mult : num -> xdim -> xdim
val xdim_add_dim : xdim -> dim -> xdim
val xdim_sub_dim : xdim -> dim -> xdim

val dim_max : dim -> dim -> dim
val dim_min : dim -> dim -> dim

val dim_max_stretch : dim -> num
val dim_max_shrink : dim -> num
val xdim_max_stretch : xdim -> num
val xdim_max_shrink : xdim -> num
val xdim_select_order : xdim -> int -> int -> dim

val dim_max_value : dim -> num
val dim_min_value : dim -> num
val xdim_max_value : xdim -> num
val xdim_min_value : xdim -> num

val dim_shift_base : dim -> num -> dim
val dim_shift_base_upto : dim -> num -> dim
val dim_inc_upto : dim -> num -> dim
val dim_dec_upto : dim -> num -> dim
val dim_resize_upto : dim -> num -> dim

val adjustment_ratio : dim -> num -> num * int
val dim_scale_badness : num * int -> num
val dim_scale : dim -> num * int -> dim
val dim_scale_upto : dim -> num * int -> dim

val inch : num
val infinite : num
val minus_infinite : num
val badness : num -> num
