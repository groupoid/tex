open Tools.XNum

let digits_lc = [| 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 97; 98; 99; 100; 101; 102 |]
let digits_uc = [| 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 65; 66; 67; 68; 69; 70 |]

let rec num_to_ar digits base x =
  if x </ num_zero then 45 :: num_to_ar digits base (minus_num x)
  else if x =/ num_zero then [48]
  else
    let b = num_of_int base in
    let rec iter str x =
      if x =/ num_zero then str
      else
        let d = mod_num x b in
        let y = quo_num x b in iter (digits.(int_of_num d) :: str) y
    in
    iter [] x

let num_to_arabic base x = num_to_ar digits_lc base x
let num_to_ARABIC base x = num_to_ar digits_uc base x

let roman_digit d i v x =
  match d with
    1 -> [i]
  | 2 -> [i; i]
  | 3 -> [i; i; i]
  | 4 -> [i; v]
  | 5 -> [v]
  | 6 -> [v; i]
  | 7 -> [v; i; i]
  | 8 -> [v; i; i; i]
  | 9 -> [i; x]
  | _ -> []

let rec num_to_rm m d c l x v i n =
  if n </ num_zero then 45 :: num_to_rm m d c l x v i (minus_num n)
  else if n =/ num_zero then [48]
  else
    let mm = int_of_num (quo_num n (num_of_int 1000)) in
    let z = int_of_num (mod_num n (num_of_int 1000)) in
    let cc = z / 100 in
    let xx = (z / 10) - (10 * cc) in
    let ii = z - (100 * cc) - (10 * xx) in
    List.init mm (fun _ -> m) @ roman_digit cc c d m @ roman_digit xx x l c @
    roman_digit ii i v x

let num_to_roman n = num_to_rm 109 100 99 108 120 118 105 n
let num_to_ROMAN n = num_to_rm 77 68 67 76 88 86 73 n

let num_26 = num_of_int 26

let rec num_to_alpha first n =
  if n </ num_zero then 45 :: num_to_alpha first (minus_num n)
  else if n =/ num_zero then [48]
  else
    let k = (n -/ num_one) // num_26 in
    let i = mod_num (n -/ num_one) num_26 in
    List.init (int_of_num k + 1) (fun _ -> first + int_of_num i)

let num_to_alphabetic n = num_to_alpha 97 n
let num_to_ALPHABETIC n = num_to_alpha 65 n
