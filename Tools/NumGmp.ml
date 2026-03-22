
(* stub to replace Num by Gmp *)

open Gmp

type num = Q.t
  let num_of_int  x   = Q.make_int x 1
let num_of_ints x y = Q.make_int x y
let float_of_num    = Q.to_float
let string_of_num   = Q.to_string

let num_zero      = Q.zero
let num_one       = num_of_int 1
let num_minus_one = num_of_int (-1)
let num_two       = num_of_int 2
let num_three     = num_of_int 3
let num_ten       = num_of_int 10

let sgn x = Q.compare x Q.zero
let equal_int x y = Z.compare_int x y = 0

let add_num     = Q.add
let minus_num   = Q.neg
let sub_num     = Q.sub
let mult_num    = Q.mul
let div_num     = Q.div
let sign_num    = sgn
let compare_num = Q.compare

let square_num x = mult_num x x

let is_integer_num x = equal_int (Q.den x) 1

let power_num_int x exp = begin
  if exp = 0 then
    num_one
  else if exp > 0 then
    Q.make_z (Z.pow_int (Q.num x) exp)
              (Z.pow_int (Q.den x) exp)
  else
    Q.make_z (Z.pow_int (Q.den x) (~-exp))
              (Z.pow_int (Q.num x) (~-exp))
}

let power_num x y = begin
  if is_integer_num y then
    power_num_int x (Z.to_int (Q.num y))
  else
    invalid_arg "power_num"
}

let abs_num x = begin
  match sgn x with
  [ (-1) -> Q.neg x
  | _    -> x
  ]
}

let succ_num x = Q.add x num_one
let pred_num x = Q.sub x num_one

let incr_num x = !x := succ_num !x
let decr_num x = !x := pred_num !x

let fdiv_q x y = fst (Gmp .fdiv x y)
let cdiv_q x y = fst (Gmp .cdiv x y)

let floor_num   x = Q.of_z (fdiv_q (Q.num x) (Q.den x))
let ceiling_num x = Q.of_z (cdiv_q (Q.num x) (Q.den x))

let integer_num x = begin
  let n = Q.num x in
  let d = Q.den x in
  let (q,r) = Z.tdiv n d in
  if Z.compare_int n 0 < 0 then
    if Z.add d r < r then
      Q.of_z (Z.sub_int q 1)
    else
      Q.of_z q
  else
    if Z.sub d r < r then
      Q.of_z (Z.add_int q 1)
    else
      Q.of_z q
}
let round_num x = begin
  let n = Q.num x in
  let d = Q.den x in
  let (q,r) = Z.tdiv n d in
  if Z.compare_int n 0 < 0 then
    if Z.add d r <= r then
      Q.of_z (Z.sub_int q 1)
    else
      Q.of_z q
  else
    if Z.sub d r <= r then
      Q.of_z (Z.add_int q 1)
    else
      Q.of_z q
}

let quo_num x y = floor_num (div_num x y)
let mod_num x y = sub_num x (mult_num y (quo_num x y))

let eq_num x y = (Q.compare x y) =  0
let lt_num x y = (Q.compare x y) <  0
let le_num x y = (Q.compare x y) <= 0
let gt_num x y = (Q.compare x y) >  0
let ge_num x y = (Q.compare x y) >= 0

let max_num x y = begin
  if lt_num x y then y else x
}
let min_num x y = begin
  if gt_num x y then y else x
}

let land_num x y = begin
  if is_integer_num x && is_integer_num y then
    Q.of_z (Z.logand (Q.num x) (Q.num y))
  else
    invalid_arg "land_num"
}

let lor_num x y = begin
  if is_integer_num x && is_integer_num y then
    Q.of_z (Z.logor (Q.num x) (Q.num y))
  else
    invalid_arg "lor_num"
}

let lxor_num x y = begin
  if is_integer_num x && is_integer_num y then
    Q.of_z (Z.logxor (Q.num x) (Q.num y))
  else
    invalid_arg "lxor_num"
}

let lneg_num x = begin
  if is_integer_num x then
    Q.of_z (Z.lognot (Q.num x))
  else
    invalid_arg "lneg_num"
}

let num_of_string s = begin
  try
    let n = String.index s '/' in
    Q.make_z (Z.of_string (String.sub s 0 n))
             (Z.of_string (String.sub s (n+1) (String.length s - n - 1)))
  with
  [ Not_found -> Q.of_z (Z.of_string s) ]
}

let int_of_num x = begin
  if is_integer_num x then
    Z.to_int (Q.num x)
  else
    failwith "integer argument required"
}

let num_of_float x = begin
  let (f, n) = frexp x in
  let factor = power_num_int num_two n in
  let str    = string_of_float f in
  let len    = String.length str in
  if str.[0] = '-' then do
  {
    let factor2 = power_num_int num_ten (len - 3) in
    let z       = if str.[1] = '1' then         (* check whether str = "-1." *)
                    num_one
                  else
                    num_of_string (String.sub str 3 (len - 3)) in
    minus_num (z */ factor // factor2)
  }
  else do
  {
    let factor2 = power_num_int num_ten (len - 2) in
    let z       = if str.[0] = '1' then         (* check whether str = "1." *)
                    num_one
                  else
                    num_of_string (String.sub str 2 (len - 2)) in
    z */ factor // factor2
  }
}

let serialise_num os x = begin
  let n = Q.num x in
  let d = Q.den x in
  let s1 = Z.of_based_string 16 (Z.to_string n) in
  let s2 = Z.of_based_string 16 (Z.to_string d) in
  let l1 = String.length (Z.to_string s1) in
  let l2 = String.length (Z.to_string s2) in
  let b10 = l1          land 0xff in
  let b11 = (l1 lsr  8) land 0xff in
  let b12 = (l1 lsr 16) land 0xff in
  let b13 = (l1 lsr 24) land 0xff in
  let b20 = l2          land 0xff in
  let b21 = (l2 lsr  8) land 0xff in
  let b22 = (l2 lsr 16) land 0xff in
  let b23 = (l2 lsr 24) land 0xff in
  IO_Base.io_write_char os (char_of_int b13)
  IO_Base.io_write_char os (char_of_int b12) in
  IO_Base.io_write_char os (char_of_int b11);
  IO_Base.io_write_char os (char_of_int b10);
  IO_Base.io_write_char os (char_of_int b23);
  IO_Base.io_write_char os (char_of_int b22);
  IO_Base.io_write_char os (char_of_int b21);
  IO_Base.io_write_char os (char_of_int b20);
  IO_Base.io_write_string os (Z.to_string s1);
  IO_Base.io_write_string os (Z.to_string s2);
};

let unserialise_num is = begin
  let b13 = int_of_char (IO_Base.io_read_char is) in
  let b12 = int_of_char (IO_Base.io_read_char is) in
  let b11 = int_of_char (IO_Base.io_read_char is) in
  let b10 = int_of_char (IO_Base.io_read_char is) in
  let b23 = int_of_char (IO_Base.io_read_char is) in
  let b22 = int_of_char (IO_Base.io_read_char is) in
  let b21 = int_of_char (IO_Base.io_read_char is) in
  let b20 = int_of_char (IO_Base.io_read_char is) in
  let len1 = b10 lor (b11 lsl 8) lor (b12 lsl 16) lor (b13 lsr 24) in
  let len2 = b20 lor (b21 lsl 8) lor (b22 lsl 16) lor (b23 lsr 24) in
  let s1 = IO_Base.io_read_string is len1 in
  let s2 = IO_Base.io_read_string is len2 in
  let d = Z.of_based_string 16 s1 in
  let n = Z.of_based_string 16 s2 in
  Q.make_z d n
}

