
open XNum
open Runtime

open ALCoding

(* opaque type for dimensions *)
let apply_dim dim x_ref = match !x_ref with
  | Types.Symbol s ->
      if s = sym_Base then
        ref (Types.Number dim.Dim.d_base)
      else if s = sym_Stretch       then
        ref (Types.Tuple [| ref (Types.Number dim.Dim.d_stretch_factor);
                           ref (Types.Number (num_of_int dim.Dim.d_stretch_order)) |])
      else if s = sym_StretchFactor then
        ref (Types.Number dim.Dim.d_stretch_factor)
      else if s = sym_StretchOrder  then
        ref (Types.Number (num_of_int dim.Dim.d_stretch_order))
      else if s = sym_Shrink        then
        ref (Types.Tuple [| ref (Types.Number dim.Dim.d_shrink_factor);
                           ref (Types.Number (num_of_int dim.Dim.d_shrink_order)) |])
      else if s = sym_ShrinkFactor  then
        ref (Types.Number dim.Dim.d_shrink_factor)
      else if s = sym_ShrinkOrder   then
        ref (Types.Number (num_of_int dim.Dim.d_shrink_order))
      else
        Types.runtime_error "invalid argument"
  | _ -> Types.runtime_error "invalid argument"

let cmp_dim = Dim.dim_equal

let (dim_wrapper, dim_unwrapper) = Opaque.declare_type "dimension" apply_dim cmp_dim cmp_dim

let wrap_dim dim = Types.Opaque (dim_wrapper dim)

let unwrap_dim = decode_opaque "dimension" dim_unwrapper

(* primitives *)

let prim_make_dim args = match args with
  | [base; st; st_ord; sh; sh_ord] ->
      let a = Machine.decode_num "make_dim" base in
      let b = Machine.decode_num "make_dim" st in
      let c = decode_int         "make_dim" st_ord in
      let d = Machine.decode_num "make_dim" sh in
      let e = decode_int         "make_dim" sh_ord in
      wrap_dim
        {
          Dim.d_base           = a;
          Dim.d_stretch_factor = b;
          Dim.d_stretch_order  = c;
          Dim.d_shrink_factor  = d;
          Dim.d_shrink_order   = e
        }
  | _ -> assert false

let prim_fixed_dim base =
  let x = Machine.decode_num "fixed_dim" base in
  wrap_dim (Dim.fixed_dim x)

let prim_dim_zero   = wrap_dim Dim.dim_zero
let prim_dim_1pt    = wrap_dim Dim.dim_1pt
let prim_dim_12pt   = wrap_dim Dim.dim_12pt
let prim_dim_fil    = wrap_dim Dim.dim_fil
let prim_dim_fill   = wrap_dim Dim.dim_fill
let prim_dim_ss     = wrap_dim Dim.dim_ss
let prim_dim_filneg = wrap_dim Dim.dim_filneg

let prim_dim_equal dim0 dim1 =
  let d0 = unwrap_dim "dim_equal" dim0 in
  let d1 = unwrap_dim "dim_equal" dim1 in
  Types.Bool (Dim.dim_equal d0 d1)

let prim_dim_add dim0 dim1 =
  let d0 = unwrap_dim "dim_add" dim0 in
  let d1 = unwrap_dim "dim_add" dim1 in
  wrap_dim (Dim.dim_add d0 d1)

let prim_dim_neg dim =
  let d = unwrap_dim "dim_neg" dim in
  wrap_dim (Dim.dim_neg d)

let prim_dim_sub dim0 dim1 =
  let d0 = unwrap_dim "dim_sub" dim0 in
  let d1 = unwrap_dim "dim_sub" dim1 in
  wrap_dim (Dim.dim_sub d0 d1)

let prim_dim_mult x dim =
  let a = Machine.decode_num "dim_mult" x in
  let d = unwrap_dim         "dim_mult" dim in
  wrap_dim (Dim.dim_mult a d)

let prim_dim_max dim0 dim1 =
  let d0 = unwrap_dim "dim_max" dim0 in
  let d1 = unwrap_dim "dim_max" dim1 in
  wrap_dim (Dim.dim_max d0 d1)

let prim_dim_min dim0 dim1 =
  let d0 = unwrap_dim "dim_min" dim0 in
  let d1 = unwrap_dim "dim_min" dim1 in
  wrap_dim (Dim.dim_min d0 d1)

let prim_dim_max_stretch dim =
  let d = unwrap_dim "dim_max_stretch" dim in
  Types.Number (Dim.dim_max_stretch d)

let prim_dim_max_shrink dim =
  let d = unwrap_dim "dim_max_shrink" dim in
  Types.Number (Dim.dim_max_shrink d)

let prim_dim_max_value dim =
  let d = unwrap_dim "dim_max_value" dim in
  Types.Number (Dim.dim_max_value d)

let prim_dim_min_value dim =
  let d = unwrap_dim "dim_min_value" dim in
  Types.Number (Dim.dim_min_value d)

let prim_dim_shift_base dim x =
  let d = unwrap_dim         "dim_shift_base" dim in
  let y = Machine.decode_num "dim_shift_base" x in
  wrap_dim (Dim.dim_shift_base d y)

let prim_dim_shift_base_upto dim x =
  let d = unwrap_dim         "dim_shift_base_upto" dim in
  let y = Machine.decode_num "dim_shift_base_upto" x in
  wrap_dim (Dim.dim_shift_base_upto d y)

let prim_dim_inc_upto dim x =
  let d = unwrap_dim         "dim_inc_upto" dim in
  let y = Machine.decode_num "dim_inc_upto" x in
  wrap_dim (Dim.dim_inc_upto d y)

let prim_dim_dec_upto dim x =
  let d = unwrap_dim         "dim_dec_upto" dim in
  let y = Machine.decode_num "dim_dec_upto" x in
  wrap_dim (Dim.dim_dec_upto d y)

let prim_dim_resize_upto dim x =
  let d = unwrap_dim         "dim_resize_upto" dim in
  let y = Machine.decode_num "dim_resize_upto" x in
  wrap_dim (Dim.dim_resize_upto d y)

let prim_adjustment_ratio dim x =
  let d = unwrap_dim         "dim_adjustment_ratio" dim in
  let y = Machine.decode_num "adjustment_ratio" x in
  let (a,b) = Dim.adjustment_ratio d y in
  Types.Tuple [| ref (Types.Number a); ref (Types.Number (num_of_int b)) |]

let prim_dim_scale_badness ratio_ref = match !ratio_ref with
  | Types.Tuple [|x; y|] ->
      let a = Machine.decode_num "dim_scale_badness" x in
      let b = decode_int         "dim_scale_badness" y in
      Types.Number (Dim.dim_scale_badness (a,b))
  | _ -> Types.runtime_error "dim_scale_badness: invalid argument"

let prim_dim_scale dim ratio_ref =
  let d = unwrap_dim "dim_scale" dim in
  match !ratio_ref with
  | Types.Tuple [|y; z|] ->
      let a = Machine.decode_num "dim_scale" y in
      let b = decode_int         "dim_scale" z in
      wrap_dim (Dim.dim_scale d (a,b))
  | _ -> Types.runtime_error "dim_scale: invalid argument"

let prim_dim_scale_upto dim ratio_ref =
  let d = unwrap_dim "dim_scale_upto" dim in
  match !ratio_ref with
  | Types.Tuple [|y; z|] ->
      let a = Machine.decode_num "dim_scale_upto" y in
      let b = decode_int         "dim_scale_upto" z in
      wrap_dim (Dim.dim_scale_upto d (a,b))
  | _ -> Types.runtime_error "dim_scale_upto: invalid argument"