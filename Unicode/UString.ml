(* conversion functions to/from unicode *)

open UTypes

let read_uc     = ref (fun _ -> -1)
let write_uc    = ref (fun _ _ -> ())
let to_u_conv   = ref (fun s -> s)
let from_u_conv = ref (fun s -> s)

let read_uc_char  cs   = !read_uc  (Tools.IO.coerce_i cs)
let write_uc_char cs x = !write_uc (Tools.IO.coerce_o cs) x
let to_unicode    str  = !to_u_conv str
let from_unicode  str  = !from_u_conv str

(* ASCII encoding *)

let read_ascii_char cs = Tools.IO.read_byte cs

let write_ascii_char cs x =
  if x < 128 then Tools.IO.write_byte cs x

let conv_ascii_to_uc str = str
let conv_uc_to_ascii str = List.filter (fun c -> c < 128) str

(* latin 1 encoding *)

let read_latin1_char cs = Tools.IO.read_byte cs

let write_latin1_char cs x =
  if x < 256 then Tools.IO.write_byte cs x

let conv_latin1_to_uc str = str
let conv_uc_to_latin1 str = List.filter (fun c -> c < 256) str

(* UTF-8 encoding *)

let rec conv_utf8_to_uc str =
  match str with
  | [] -> []
  | c :: cs ->
      if c < 0x80 then
        c :: conv_utf8_to_uc cs
      else if c < 0xc0 then
        c :: conv_utf8_to_uc cs  (* should never happen *)
      else if c < 0xe0 then
        match cs with
        | c2 :: cs -> (0x40 * (c - 0xc0) + c2 - 0x80) :: conv_utf8_to_uc cs
        | _        -> [0x40 * (c - 0xc0)]
      else
        match cs with
        | c2 :: c3 :: cs -> (0x1000 * (c - 0xe0) + 0x40 * (c2 - 0x80) + c3 - 0x80) :: conv_utf8_to_uc cs
        | _              -> [0x1000 * (c - 0xe0)]

let rec conv_uc_to_utf8 str =
  match str with
  | [] -> []
  | c :: cs ->
      if c < 0x80 then
        c :: conv_uc_to_utf8 cs
      else if c < 0x800 then
        (0xc0 + (c lsr 6)) :: (0x80 + (c land 0x3f)) :: conv_uc_to_utf8 cs
      else
        (0xe0 + (c lsr 12)) :: (0x80 + ((c lsr 6) land 0x3f)) :: (0x80 + (c land 0x3f)) :: conv_uc_to_utf8 cs

let set_string_format fmt =
  match fmt with
  | `ASCII ->
      read_uc     := read_ascii_char;
      write_uc    := write_ascii_char;
      to_u_conv   := conv_ascii_to_uc;
      from_u_conv := conv_uc_to_ascii
  | `Latin1 ->
      read_uc     := read_latin1_char;
      write_uc    := write_latin1_char;
      to_u_conv   := conv_latin1_to_uc;
      from_u_conv := conv_uc_to_latin1
  | `UTF8 ->
      read_uc     := Tools.IO.read_utf8_char;
      write_uc    := Tools.IO.write_utf8_char;
      to_u_conv   := conv_utf8_to_uc;
      from_u_conv := conv_uc_to_utf8
  | `Unicode ->
      read_uc     := Tools.IO.read_be_u16;
      write_uc    := Tools.IO.write_be_u16;
      to_u_conv   := (fun s -> s);
      from_u_conv := (fun s -> s)
  | _ -> raise (Invalid_argument "unknown format")

let string_to_bytes str =
  let rec iter list i =
    if i < 0 then list
    else iter (int_of_char str.[i] :: list) (i - 1)
  in
  iter [] (String.length str - 1)

let bytes_to_string list =
  let len = List.length list in
  let str = Bytes.create len in
  let rec iter i list =
    match list with
    | [] -> str
    | c :: cs ->
        Bytes.set str i (char_of_int c);
        iter (i + 1) cs
  in
  Bytes.to_string (iter 0 list)

let of_ascii = string_to_bytes
let to_ascii = bytes_to_string

let uc_string_of_ascii str =
  let len = String.length str in
  Array.init len (fun i -> int_of_char str.[i])

let uc_string_to_ascii arr =
  let len = Array.length arr in
  let str = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set str i (char_of_int arr.(i))
  done;
  Bytes.to_string str

let of_string str = to_unicode (string_to_bytes str)
let to_string list = bytes_to_string (from_unicode list)

let append s1 s2 =
  let len1 = Array.length s1 in
  let len2 = Array.length s2 in
  Array.init (len1 + len2) (fun i -> if i < len1 then s1.(i) else s2.(i - len1))

let rec compare_uc_strings s1 s2 =
  match (s1, s2) with
  | ([], []) -> 0
  | ([], _)  -> -1
  | (_, [])  -> 1
  | (c1 :: cs1, c2 :: cs2) ->
      if c1 < c2 then -1
      else if c1 > c2 then 1
      else compare_uc_strings cs1 cs2
