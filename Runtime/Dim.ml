
open Tools.XNum
open Logging

let inch = num_of_ints 7227 100
let infinite = num_of_int 10000
let minus_infinite = num_of_int (-10000)

let badness ratio =
  if ratio </ num_of_int (-1) then infinite
  else round_num (abs_num (num_of_int 100 */ ratio */ ratio */ ratio))

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

let fixed_dim x = {
  d_base = x;
  d_stretch_factor = num_zero;
  d_stretch_order = 0;
  d_shrink_factor = num_zero;
  d_shrink_order = 0;
}

let dim_zero = fixed_dim num_zero
let dim_1pt = fixed_dim num_one
let dim_12pt = fixed_dim (num_of_int 12)

let dim_fil = {
  d_base = num_zero;
  d_stretch_factor = num_one;
  d_stretch_order = 1;
  d_shrink_factor = num_zero;
  d_shrink_order = 0;
}

let dim_fill = {
  d_base = num_zero;
  d_stretch_factor = num_one;
  d_stretch_order = 2;
  d_shrink_factor = num_zero;
  d_shrink_order = 0;
}

let dim_ss = {
  d_base = num_zero;
  d_stretch_factor = num_one;
  d_stretch_order = 1;
  d_shrink_factor = num_one;
  d_shrink_order = 1;
}

let dim_filneg = {
  d_base = num_zero;
  d_stretch_factor = minus_num num_one;
  d_stretch_order = 1;
  d_shrink_factor = num_zero;
  d_shrink_order = 0;
}

let xdim_zero = { xd_base = num_zero; xd_stretch = []; xd_shrink = [] }

let xdim_stretch xd =
  match xd.xd_stretch with
  | [] -> num_zero, 0
  | (v, o) :: _ -> v, o

let xdim_shrink xd =
  match xd.xd_shrink with
  | [] -> num_zero, 0
  | (v, o) :: _ -> v, o

let xdim_to_dim xd =
  let (sv, so) = xdim_stretch xd in
  let (hv, ho) = xdim_shrink xd in
  {
    d_base = xd.xd_base;
    d_stretch_factor = sv;
    d_stretch_order = so;
    d_shrink_factor = hv;
    d_shrink_order = ho;
  }

let dim_to_xdim d =
  {
    xd_base = d.d_base;
    xd_stretch = if d.d_stretch_factor =/ num_zero then [] else [d.d_stretch_factor, d.d_stretch_order];
    xd_shrink = if d.d_shrink_factor =/ num_zero then [] else [d.d_shrink_factor, d.d_shrink_order];
  }

let dim_equal d1 d2 =
  d1.d_base =/ d2.d_base &&
  d1.d_stretch_factor =/ d2.d_stretch_factor &&
  d1.d_stretch_order = d2.d_stretch_order &&
  d1.d_shrink_factor =/ d2.d_shrink_factor &&
  d1.d_shrink_order = d2.d_shrink_order

let dim_is_zero d = dim_equal d dim_zero

let log_dim d =
  Logging.log_num d.d_base;
  if d.d_stretch_factor <>/ num_zero then (
    Logging.log_string " plus ";
    Logging.log_num d.d_stretch_factor;
    Logging.log_string " fil^";
    Logging.log_int d.d_stretch_order
  );
  if d.d_shrink_factor <>/ num_zero then (
    Logging.log_string " minus ";
    Logging.log_num d.d_shrink_factor;
    Logging.log_string " fil^";
    Logging.log_int d.d_shrink_order
  )

let dim_add d1 d2 =
  {
    d_base = d1.d_base +/ d2.d_base;
    d_stretch_factor = d1.d_stretch_factor +/ d2.d_stretch_factor;
    d_stretch_order = max d1.d_stretch_order d2.d_stretch_order;
    d_shrink_factor = d1.d_shrink_factor +/ d2.d_shrink_factor;
    d_shrink_order = max d1.d_shrink_order d2.d_shrink_order;
  }

let dim_neg d =
  {
    d_base = minus_num d.d_base;
    d_stretch_factor = minus_num d.d_stretch_factor;
    d_stretch_order = d.d_stretch_order;
    d_shrink_factor = minus_num d.d_shrink_factor;
    d_shrink_order = d.d_shrink_order;
  }

let dim_sub d1 d2 = dim_add d1 (dim_neg d2)

let dim_mult n d =
  {
    d_base = n */ d.d_base;
    d_stretch_factor = n */ d.d_stretch_factor;
    d_stretch_order = d.d_stretch_order;
    d_shrink_factor = n */ d.d_shrink_factor;
    d_shrink_order = d.d_shrink_order;
  }

let xdim_add x1 x2 =
  {
    xd_base = x1.xd_base +/ x2.xd_base;
    xd_stretch = x1.xd_stretch @ x2.xd_stretch; (* Simplified *)
    xd_shrink = x1.xd_shrink @ x2.xd_shrink;
  }

let xdim_neg x =
  {
    xd_base = minus_num x.xd_base;
    xd_stretch = List.map (fun (v, o) -> minus_num v, o) x.xd_stretch;
    xd_shrink = List.map (fun (v, o) -> minus_num v, o) x.xd_shrink;
  }

let xdim_sub x1 x2 = xdim_add x1 (xdim_neg x2)

let xdim_mult n x =
  {
    xd_base = n */ x.xd_base;
    xd_stretch = List.map (fun (v, o) -> n */ v, o) x.xd_stretch;
    xd_shrink = List.map (fun (v, o) -> n */ v, o) x.xd_shrink;
  }

let xdim_add_dim x d = xdim_add x (dim_to_xdim d)
let xdim_sub_dim x d = xdim_sub x (dim_to_xdim d)

let dim_max d1 d2 = if d1.d_base >/ d2.d_base then d1 else d2
let dim_min d1 d2 = if d1.d_base </ d2.d_base then d1 else d2

let dim_max_stretch d = d.d_stretch_factor
let dim_max_shrink d = d.d_shrink_factor
let xdim_max_stretch xd = fst (xdim_stretch xd)
let xdim_max_shrink xd = fst (xdim_shrink xd)

let xdim_select_order _ _ _ = dim_zero (* Placeholder *)

let dim_max_value d = d.d_base +/ d.d_stretch_factor
let dim_min_value d = d.d_base -/ d.d_shrink_factor
let xdim_max_value x = x.xd_base (* Simplified *)
let xdim_min_value x = x.xd_base

let dim_shift_base d n = { d with d_base = d.d_base +/ n }
let dim_shift_base_upto d n = { d with d_base = d.d_base +/ n }
let dim_inc_upto d n = { d with d_base = d.d_base +/ n }
let dim_dec_upto d n = { d with d_base = d.d_base -/ n }
let dim_resize_upto d n = { d with d_base = n }

let adjustment_ratio d _ = num_zero, 0
let dim_scale_badness (r, _) = badness r
let dim_scale d (r, _) = { d with d_base = d.d_base +/ (r */ d.d_base) }
let dim_scale_upto d r = dim_scale d r
let of_ascii s = dim_zero (* Placeholder *)
let fixed_of_ascii s = fixed_dim (of_ascii s).d_base
