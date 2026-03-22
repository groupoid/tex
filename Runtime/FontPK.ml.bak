
open Tools.XNum
open Substitute
open GlyphMetric
open FontMetric

let rec skip_specials (ic : Tools.IO.irstream) =
  let x = Tools.IO.read_be_u8 (Tools.IO.coerce_ir_i ic) in
  if x < 0 then x
  else if x = 240 then (Tools.IO.skip ic (Tools.IO.read_be_u8 (Tools.IO.coerce_ir_i ic)); skip_specials ic)
  else if x = 241 then (Tools.IO.skip ic (Tools.IO.read_be_u16 (Tools.IO.coerce_ir_i ic)); skip_specials ic)
  else if x = 242 then (Tools.IO.skip ic (Tools.IO.read_be_u24 (Tools.IO.coerce_ir_i ic)); skip_specials ic)
  else if x = 243 then (Tools.IO.skip ic (int_of_num (Tools.IO.read_be_u32 (Tools.IO.coerce_ir_i ic))); skip_specials ic)
  else if x = 244 then (Tools.IO.skip ic 5; skip_specials ic)
  else if x = 245 then (-1)
  else if x = 246 then skip_specials ic
  else x

let read_pk_font fm dpi =
  (* KPathSea is currently dummied or missing, this is a placeholder for the pure OCaml implementation *)
  None
