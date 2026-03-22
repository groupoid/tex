
include Num

let num_zero = Int 0
let num_one = Int 1
let num_minus_one = Int (-1)
let num_two = Int 2
let num_three = Int 3
let num_ten = Int 10

let num_of_ints x y = Int x // Int y

let power_num_int x exp = power_num x (Int exp)

let land_num x y =
  if is_integer_num x && is_integer_num y then
    Int (int_of_num x land int_of_num y)
  else invalid_arg "land_num"

let lor_num x y =
  if is_integer_num x && is_integer_num y then
    Int (int_of_num x lor int_of_num y)
  else invalid_arg "lor_num"

let lxor_num x y =
  if is_integer_num x && is_integer_num y then
    Int (int_of_num x lxor int_of_num y)
  else invalid_arg "lxor_num"

let lneg_num x =
  if is_integer_num x then Int (lnot (int_of_num x))
  else invalid_arg "lneg_num"

let old_num_of_string = num_of_string

let num_of_string s =
  try
    let n = String.index s '/' in
    old_num_of_string (String.sub s 0 n) //
    old_num_of_string (String.sub s (n + 1) (String.length s - n - 1))
  with Not_found -> old_num_of_string (s ^ "/1")

let num_of_float x =
  let (f, n) = frexp x in
  let factor = power_num_int num_two n in
  let str = string_of_float f in
  let len = String.length str in
  if str.[0] = '-' then
    let factor2 = power_num_int num_ten (len - 3) in
    let z =
      if str.[1] = '1' then num_one
      else num_of_string (String.sub str 3 (len - 3))
    in
    minus_num (z */ factor // factor2)
  else
    let factor2 = power_num_int num_ten (len - 2) in
    let z =
      if str.[0] = '1' then num_one
      else num_of_string (String.sub str 2 (len - 2))
    in
    z */ factor // factor2

let serialise_num os x =
  let n = Ratio.numerator_ratio (ratio_of_num x) in
  let d = Ratio.denominator_ratio (ratio_of_num x) in
  let s1 = Big_int.string_of_big_int n in
  let s2 = Big_int.string_of_big_int d in
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let b10 = l1 land 0xff in
  let b11 = (l1 lsr 8) land 0xff in
  let b12 = (l1 lsr 16) land 0xff in
  let b13 = (l1 lsr 24) land 0xff in
  let b20 = l2 land 0xff in
  let b21 = (l2 lsr 8) land 0xff in
  let b22 = (l2 lsr 16) land 0xff in
  let b23 = (l2 lsr 24) land 0xff in
  IO_Base.io_write_char os (char_of_int b13);
  IO_Base.io_write_char os (char_of_int b12);
  IO_Base.io_write_char os (char_of_int b11);
  IO_Base.io_write_char os (char_of_int b10);
  IO_Base.io_write_char os (char_of_int b23);
  IO_Base.io_write_char os (char_of_int b22);
  IO_Base.io_write_char os (char_of_int b21);
  IO_Base.io_write_char os (char_of_int b20);
  IO_Base.io_write_string os s1;
  IO_Base.io_write_string os s2

let unserialise_num (_is : IO_Base.irstream) =
  let b13 = int_of_char (IO_Base.io_read_char _is) in
  let b12 = int_of_char (IO_Base.io_read_char _is) in
  let b11 = int_of_char (IO_Base.io_read_char _is) in
  let b10 = int_of_char (IO_Base.io_read_char _is) in
  let b23 = int_of_char (IO_Base.io_read_char _is) in
  let b22 = int_of_char (IO_Base.io_read_char _is) in
  let b21 = int_of_char (IO_Base.io_read_char _is) in
  let b20 = int_of_char (IO_Base.io_read_char _is) in
  let len1 = b10 lor (b11 lsl 8) lor (b12 lsl 16) lor (b13 lsl 24) in
  let len2 = b20 lor (b21 lsl 8) lor (b22 lsl 16) lor (b23 lsl 24) in
  let s1 = IO_Base.io_read_string _is len1 in
  let s2 = IO_Base.io_read_string _is len2 in
  let d = Big_int.big_int_of_string s1 in
  let n = Big_int.big_int_of_string s2 in
  Ratio (Ratio.create_ratio d n)
