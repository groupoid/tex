open IO_Base
type num = float
let num_of_int = float_of_int
let num_of_ints x y = float_of_int x /. float_of_int y
let float_of_num (x:num) = x
let string_of_num = string_of_float
let num_zero = 0.0
let num_one = 1.0
let num_minus_one = -1.0
let num_two = 2.0
let num_three = 3.0
let num_ten = 10.0
let add_num x y = x +. y
let minus_num x = -. x
let sub_num x y = x -. y
let mult_num x y = x *. y
let div_num x y = x /. y
let sign_num (x:num) = if x < 0.0 then -1 else if x > 0.0 then 1 else 0
let compare_num (x:num) (y:num) = compare x y
let square_num x = x *. x
let is_integer_num x = (x = floor x)
let power_num_int x exp = x ** float_of_int exp
let power_num x y = x ** y
let abs_num = abs_float
let succ_num x = x +. 1.0
let pred_num x = x -. 1.0
let floor_num = floor
let ceiling_num = ceil
let integer_num x = let (f, z) = modf x in if f > 0.5 then z +. 1.0 else if f < -0.5 then z -. 1.0 else z
let round_num x = let (f, z) = modf x in if f >= 0.5 then z +. 1.0 else if f <= -0.5 then z -. 1.0 else z
let quo_num x y = floor (x /. y)
let mod_num x y = x -. y *. floor (x /. y)
let eq_num (x:num) (y:num) = (x = y)
let lt_num (x:num) (y:num) = (x < y)
let le_num (x:num) (y:num) = (x <= y)
let gt_num (x:num) (y:num) = (x > y)
let ge_num (x:num) (y:num) = (x >= y)
let max_num (x:num) (y:num) = max x y
let min_num (x:num) (y:num) = min x y
let land_num (x:num) (y:num) = float_of_int (int_of_float x land int_of_float y)
let lor_num (x:num) (y:num) = float_of_int (int_of_float x lor int_of_float y)
let lxor_num (x:num) (y:num) = float_of_int (int_of_float x lxor int_of_float y)
let lsl_num (x:num) (y:int) = x *. (2.0 ** float_of_int y)
let lsr_num (x:num) (y:int) = x /. (2.0 ** float_of_int y)
let lneg_num (x:num) = float_of_int (lnot (int_of_float x))
let num_of_string s = try let n = String.index s '/' in float_of_string (String.sub s 0 n) /. float_of_string (String.sub s (n + 1) (String.length s - n - 1)) with Not_found -> float_of_string s
let int_of_num = int_of_float
let num_of_float x = x
let serialise_num (os : ostream) x = let z = Int64.bits_of_float x in let mask = Int64.of_int 0xff in let b = Array.init 8 (fun i -> Int64.to_int (Int64.logand (Int64.shift_right_logical z (56 - i * 8)) mask)) in Array.iter (fun v -> io_write_char os (char_of_int v)) b
let unserialise_num (is : istream) = let b = Array.init 8 (fun _ -> Int64.of_int (int_of_char (io_read_char is))) in let rec loop i acc = if i = 8 then acc else loop (i + 1) (Int64.logor (Int64.shift_left acc 8) b.(i)) in Int64.float_of_bits (loop 0 0L)
