
type tag = int32
module TagMap = Map.Make(Int32)

let compose_tag high low =
  Int32.logor
    (Int32.shift_left (Int32.of_int high) 16)
    (Int32.of_int low)

let make_tag str =
  if String.length str <> 4 then
    invalid_arg "make_tag: 4 characters expected"
  else begin
    let d0 = int_of_char str.[0] in
    let d1 = int_of_char str.[1] in
    let d2 = int_of_char str.[2] in
    let d3 = int_of_char str.[3] in
    if d0 >= 32 && d0 <= 126
    && d1 >= 32 && d1 <= 126
    && d2 >= 32 && d2 <= 126
    && d3 >= 32 && d3 <= 126 then
      compose_tag ((d0 lsl 8) lor d1)
                  ((d2 lsl 8) lor d3)
    else
      invalid_arg "make_tag: invalid character"
  end

let make_tag_uc str =
  if Array.length str <> 4 then
    invalid_arg "make_tag_uc: 4 characters expected"
  else begin
    let d0 = str.(0) in
    let d1 = str.(1) in
    let d2 = str.(2) in
    let d3 = str.(3) in
    if d0 >= 32 && d0 <= 126
    && d1 >= 32 && d1 <= 126
    && d2 >= 32 && d2 <= 126
    && d3 >= 32 && d3 <= 126 then
      compose_tag ((d0 lsl 8) lor d1)
                  ((d2 lsl 8) lor d3)
    else
      invalid_arg "make_tag_uc: invalid character"
  end

let tag_to_string tag =
  let str = Bytes.make 4 ' ' in
  let x = Int32.to_int (Int32.logand (Int32.shift_right tag 16)
                                     (Int32.of_int 0xffff)) in
  let y = Int32.to_int (Int32.logand tag (Int32.of_int 0xffff)) in
  let d0 = (x lsr 8) land 0xff in
  let d1 = x land 0xff in
  let d2 = (y lsr 8) land 0xff in
  let d3 = y land 0xff in
  Bytes.set str 0 (char_of_int d0);
  Bytes.set str 1 (char_of_int d1);
  Bytes.set str 2 (char_of_int d2);
  Bytes.set str 3 (char_of_int d3);
  Bytes.to_string str

let empty_tag    = make_tag "    "
let latn_tag     = make_tag "latn"
let dflt_tag     = make_tag "dflt"
let size_tag     = make_tag "size"

let ttcf_tag = make_tag "ttcf"
let true_tag = make_tag "true"
let otto_tag = make_tag "OTTO"
let cff_tag  = make_tag "CFF "
let cmap_tag = make_tag "cmap"
let glyf_tag = make_tag "glyf"
let gdef_tag = make_tag "GDEF"
let gpos_tag = make_tag "GPOS"
let gsub_tag = make_tag "GSUB"
let head_tag = make_tag "head"
let hhea_tag = make_tag "hhea"
let hmtx_tag = make_tag "hmtx"
let kern_tag = make_tag "kern"
let loca_tag = make_tag "loca"
let maxp_tag = make_tag "maxp"
let name_tag = make_tag "name"
let post_tag = make_tag "post"
let os2_tag  = make_tag "OS/2"
