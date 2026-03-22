
type bitmap = {
  bm_width : int;
  bm_height : int;
  bm_bytes_per_row : int;
  mutable bm_data : bytes;
}

let make width height =
  let bpr = (width + 7) / 8 in
  { bm_width = width; bm_height = height; bm_bytes_per_row = bpr;
    bm_data = Bytes.make (bpr * height) '\000' }

let get_index bm x y = y * bm.bm_bytes_per_row + x / 8
let get_bit x = 1 lsl (7 - x land 7)

let unsafe_point bm x y =
  let i = get_index bm x y in
  (Char.code (Bytes.get bm.bm_data i) land get_bit x) <> 0

let unsafe_set_point bm x y =
  let i = get_index bm x y in
  let c = Char.code (Bytes.get bm.bm_data i) in
  Bytes.set bm.bm_data i (Char.chr (c lor get_bit x))

let unsafe_unset_point bm x y =
  let i = get_index bm x y in
  let c = Char.code (Bytes.get bm.bm_data i) in
  Bytes.set bm.bm_data i (Char.chr (c land lnot (get_bit x)))

let point bm x y =
  if 0 <= x && x < bm.bm_width && 0 <= y && y < bm.bm_height then
    unsafe_point bm x y
  else false

let set_point bm x y =
  if 0 <= x && x < bm.bm_width && 0 <= y && y < bm.bm_height then
    unsafe_set_point bm x y

let unset_point bm x y =
  if 0 <= x && x < bm.bm_width && 0 <= y && y < bm.bm_height then
    unsafe_unset_point bm x y

let set_line bm x1 x2 y =
  for x = x1 to x2 do set_point bm x y done

let unset_line bm x1 x2 y =
  for x = x1 to x2 do unset_point bm x y done

let copy_line bm y1 y2 =
  if 0 <= y1 && y1 < bm.bm_height && 0 <= y2 && y2 < bm.bm_height then
    for x = 0 to bm.bm_width - 1 do
      if unsafe_point bm x y1 then unsafe_set_point bm x y2
      else unsafe_unset_point bm x y2
    done

let print io bm black white end_line =
  for y = 0 to bm.bm_height - 1 do
    for x = 0 to bm.bm_width - 1 do
      if unsafe_point bm x y then Tools.IO.write_string io black
      else Tools.IO.write_string io white
    done;
    Tools.IO.write_string io end_line
  done
