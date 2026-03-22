
open Tools.XNum
open Vm_types.Types
open Unicode
open UTypes
open Unicode.UTypes
open Unicode.SymbolTable

module UString = Unicode.UString
module UChar   = Unicode.UChar
module Format  = Unicode.Format

let rec uc_list_to_char_list str = match str with
  | []      -> Nil
  | c :: cs -> List (ref (Char c), ref (uc_list_to_char_list cs))

let uc_string_to_char_list str =
  let rec iter i list =
    if i < 0 then
      list
    else
      iter (i - 1) (List (ref (Char str.(i)), ref list))
  in iter (Array.length str - 1) Nil

let ascii_to_char_list str =
  uc_string_to_char_list (UString.uc_string_of_ascii str)

let rec evaluate_char_list name x = match !x with
  | Nil      -> []
  | List (a, b) ->
      (match !a with
       | Char c -> c :: evaluate_char_list name b
       | _      -> runtime_error (name ^ ": invalid argument"))
  | Unbound
  | Constraint _ -> runtime_error (name ^ ": argument undefined")
  | _            -> runtime_error (name ^ ": invalid argument")

(* control *)

let prim_error msg =
  let str = evaluate_char_list "error" msg in
  raise (Runtime_error (Array.of_list str))

(* types *)

let prim_is_unbound x = match !x with
  | Unbound
  | Constraint _
  | LinForm _    -> Bool true
  | _            -> Bool false

let prim_is_bool x = match !x with
  | Bool _ -> Bool true
  | _      -> Bool false

let prim_is_number x = match !x with
  | Number _ -> Bool true
  | _        -> Bool false

let prim_is_char x = match !x with
  | Char _ -> Bool true
  | _      -> Bool false

let prim_is_symbol x = match !x with
  | Symbol _ -> Bool true
  | _        -> Bool false

let prim_is_function x = match !x with
  | Primitive1 _
  | Primitive2 _
  | PrimitiveN _
  | Function _
  | Chain _
  | Application _
  | Dictionary _ -> Bool true
  | _            -> Bool false

let prim_is_list x = match !x with
  | Nil
  | List _ -> Bool true
  | _        -> Bool false

let prim_is_tuple x = match !x with
  | Tuple _ -> Bool true
  | _       -> Bool false

(* logic *)

let prim_or x y = match (!x, !y) with
  | (Bool a, Bool b) -> Bool (a || b)
  | _                -> runtime_error "||: invalid argument"

let prim_and x y = match (!x, !y) with
  | (Bool a, Bool b) -> Bool (a && b)
  | _                -> runtime_error "&&: invalid argument"

let prim_not x = match !x with
  | Bool a -> Bool (not a)
  | _      -> runtime_error "not: invalid argument"

(* comparisons *)

let rec cmp x y = match (!x, !y) with
  | (Unbound, _)                 -> runtime_error "argument unbound during comparison"
  | (_, Unbound)                 -> runtime_error "argument unbound during comparison"
  | (Constraint a, Constraint b) -> (a == b)
  | (Bool a,       Bool b)       -> (a = b)
  | (Char a,       Char b)       -> (a = b)
  | (Symbol a,     Symbol b)     -> (a = b)
  | (Nil,          Nil)          -> true
  | (List (a1, a2), List (b1, b2)) -> cmp a1 b1 && cmp a2 b2
  | (Number a,     Number b)     -> (a =/ b)
  | (Number a,     LinForm lin)  ->
      (Evaluate.evaluate_lin_form y lin;
       match !y with
       | Number b -> (b =/ a)
       | _        -> false)
  | (LinForm lin, Number a) ->
      (Evaluate.evaluate_lin_form x lin;
       match !x with
       | Number b -> (b =/ a)
       | _        -> false)
  | (LinForm a, LinForm b) ->
      (Evaluate.evaluate_lin_form x a;
       Evaluate.evaluate_lin_form y b;
       let l = Tools.LinForm.lin_comb num_one a (minus_num num_one) b in
       let z = ref (LinForm l) in
       Evaluate.evaluate_lin_form z l;
       match !z with
       | Number c -> (c =/ num_zero)
       | _        -> false)
  | (Tuple a, Tuple b) ->
      (let len = Array.length a in
       if Array.length b <> len then
         false
       else
         let rec iter i =
           if i >= len then
             true
           else if cmp a.(i) b.(i) then
             iter (i+1)
           else
             false
         in iter 0)
  | (Dictionary a, Dictionary b) ->
      (let l0 = Unicode.SymbolTable.map_to_list a in
       let l1 = Unicode.SymbolTable.map_to_list b in
       let rec iter l0 l1 = match (l0, l1) with
         | ([], []) -> true
         | ([], _)  -> false
         | (_, [])  -> false
         | ((k0, v0) :: kv0, (k1, v1) :: kv1) ->
             if k0 <> k1 then
               false
             else if cmp v0 v1 then
               iter kv0 kv1
             else
               false
       in iter l0 l1)
  | (Primitive1 a, Primitive1 b) -> (a == b)
  | (Primitive2 a, Primitive2 b) -> (a == b)
  | (PrimitiveN _, PrimitiveN _) -> false 
  | (Function (a, _, _), Function (b, _, _)) -> (a == b)
  | (Relation (a1, a2), Relation (b1, b2)) -> (a1 = b1 && a2 = b2)
  | _ -> false

let prim_eq x y = Bool (cmp x y)

let prim_neq x y = Bool (not (cmp x y))

let rec prim_gt x y = match (!x, !y) with
  | (Number m, Number n) -> Bool (m >/ n)
  | (Char a,   Char b)   -> Bool (a > b)
  | (Nil, Nil)           -> Bool false
  | (Nil, List _)      -> Bool false
  | (List _, Nil)      -> Bool true
  | (List (a, b), List (c, d)) ->
      (match prim_gt a c with
       | Bool true  -> Bool true
       | _ -> if cmp a c then prim_gt b d else Bool false)
  | (Tuple xs, Tuple ys) ->
      if Array.length xs <> Array.length ys then
        runtime_error ">: invalid argument"
      else
        let rec iter i =
          if i >= Array.length xs then
            Bool false
          else
            match prim_gt xs.(i) ys.(i) with
            | Bool true -> Bool true
            | _ -> if cmp xs.(i) ys.(i) then iter (i + 1) else Bool false
        in iter 0
  | (LinForm l, _) ->
      (Evaluate.evaluate_lin_form x l;
       match !x with
       | Number _ -> prim_gt x y
       | _        -> runtime_error (">: invalid argument"))
  | (_, LinForm l) ->
      (Evaluate.evaluate_lin_form y l;
       match !y with
       | Number _ -> prim_gt x y
       | _        -> runtime_error (">: invalid argument"))
  | _ -> runtime_error ">: invalid argument"

let rec prim_lt x y = match (!x, !y) with
  | (Number m, Number n) -> Bool (m </ n)
  | (Char a,   Char b)   -> Bool (a < b)
  | (Nil, Nil)           -> Bool false
  | (Nil, List _)      -> Bool true
  | (List _, Nil)      -> Bool false
  | (List (a, b), List (c, d)) ->
      (match prim_lt a c with
       | Bool true  -> Bool true
       | _ -> if cmp a c then prim_lt b d else Bool false)
  | (Tuple xs, Tuple ys) ->
      if Array.length xs <> Array.length ys then
        runtime_error "<: invalid argument"
      else
        let rec iter i =
          if i >= Array.length xs then
            Bool false
          else
            match prim_lt xs.(i) ys.(i) with
            | Bool true -> Bool true
            | _ -> if cmp xs.(i) ys.(i) then iter (i + 1) else Bool false
        in iter 0
  | (LinForm l, _) ->
      (Evaluate.evaluate_lin_form x l;
       match !x with
       | Number _ -> prim_lt x y
       | _        -> runtime_error ("<: invalid argument"))
  | (_, LinForm l) ->
      (Evaluate.evaluate_lin_form y l;
       match !y with
       | Number _ -> prim_lt x y
       | _        -> runtime_error ("<: invalid argument"))
  | _ -> runtime_error "<: invalid argument"

let rec prim_ge x y = match (!x, !y) with
  | (Number m, Number n) -> Bool (m >=/ n)
  | (Char a,   Char b)   -> Bool (a >= b)
  | (Nil, Nil)           -> Bool true
  | (Nil, List _)      -> Bool false
  | (List _, Nil)      -> Bool true
  | (List (a, b), List (c, d)) ->
      (match prim_gt a c with
       | Bool true  -> Bool true
       | _ -> if cmp a c then prim_ge b d else Bool false)
  | (Tuple xs, Tuple ys) ->
      if Array.length xs <> Array.length ys then
        runtime_error ">=: invalid argument"
      else
        let rec iter i =
          if i >= Array.length xs then
            Bool true
          else
            match prim_gt xs.(i) ys.(i) with
            | Bool true -> Bool true
            | _ -> if cmp xs.(i) ys.(i) then iter (i + 1) else Bool false
        in iter 0
  | (LinForm l, _) ->
      (Evaluate.evaluate_lin_form x l;
       match !x with
       | Number _ -> prim_ge x y
       | _        -> runtime_error (">=: invalid argument"))
  | (_, LinForm l) ->
      (Evaluate.evaluate_lin_form y l;
       match !y with
       | Number _ -> prim_ge x y
       | _        -> runtime_error (">=: invalid argument"))
  | _ -> runtime_error ">=: invalid argument"

let rec prim_le x y = match (!x, !y) with
  | (Number m, Number n) -> Bool (m <=/ n)
  | (Char a,   Char b)   -> Bool (a <= b)
  | (Nil, Nil)           -> Bool true
  | (Nil, List _)      -> Bool true
  | (List _, Nil)      -> Bool false
  | (List (a, b), List (c, d)) ->
      (match prim_lt a c with
       | Bool true  -> Bool true
       | _ -> if cmp a c then prim_le b d else Bool false)
  | (Tuple xs, Tuple ys) ->
      if Array.length xs <> Array.length ys then
        runtime_error "<=: invalid argument"
      else
        let rec iter i =
          if i >= Array.length xs then
            Bool true
          else
            match prim_lt xs.(i) ys.(i) with
            | Bool true -> Bool true
            | _ -> if cmp xs.(i) ys.(i) then iter (i + 1) else Bool false
        in iter 0
  | (LinForm l, _) ->
      (Evaluate.evaluate_lin_form x l;
       match !x with
       | Number _ -> prim_le x y
       | _        -> runtime_error ("<=: invalid argument"))
  | (_, LinForm l) ->
      (Evaluate.evaluate_lin_form y l;
       match !y with
       | Number _ -> prim_le x y
       | _        -> runtime_error ("<=: invalid argument"))
  | _ -> runtime_error "<=: invalid argument"

let prim_min x y =
  match prim_le x y with
  | Bool true -> !x
  | _         -> !y

let prim_max x y =
  match prim_ge x y with
  | Bool true -> !x
  | _         -> !y

(* general arithmetic *)

let unary_number_prim f name x = match !x with
  | Number n ->
    (try Number (f n)
     with _ -> runtime_error (name ^ ": invalid argument"))
  | LinForm l ->
    (Evaluate.evaluate_lin_form x l;
     match !x with
     | Number n ->
       (try Number (f n)
        with _ -> runtime_error (name ^ ": invalid argument"))
     | _ -> runtime_error (name ^ ": invalid argument"))
  | _ -> runtime_error (name ^ ": invalid argument")

let rec binary_number_prim f name x y = match (!x, !y) with
  | (Number m, Number n) ->
    (try Number (f m n)
     with _ -> runtime_error (name ^ ": invalid argument"))
  | (LinForm l, _) ->
    (Evaluate.evaluate_lin_form x l;
     match !x with
     | Number _ -> binary_number_prim f name x y
     | _        -> runtime_error (name ^ ": invalid argument"))
  | (_, LinForm l) ->
    (Evaluate.evaluate_lin_form y l;
     match !y with
     | Number _ -> binary_number_prim f name x y
     | _        -> runtime_error (name ^ ": invalid argument"))
  | _ -> runtime_error (name ^ ": invalid argument")

let prim_quot = binary_number_prim quo_num "quot"
let prim_mod  = binary_number_prim mod_num "mod"
let prim_pow  = binary_number_prim
                      (fun x y -> if is_integer_num y then
                                    power_num x y
                                  else
                                    num_of_float (float_of_num x ** float_of_num y))
                      "^"

let rec prim_negate x = match !x with
  | Number n  -> Number (minus_num n)
  | LinForm l -> LinForm (Tools.LinForm.scale num_minus_one l)
  | Unbound   -> LinForm (Tools.LinForm.of_scaled_unknown compare_unknowns num_minus_one x)
  | Tuple xs  ->
    let len = Array.length xs in
    Tuple (Array.init len (fun i -> ref (prim_negate xs.(i))))
  | _ -> runtime_error "~: invalid argument"

(* integer arithmetic *)

let prim_round    = unary_number_prim round_num   "round"
let prim_truncate = unary_number_prim integer_num "truncate"
let prim_ceiling  = unary_number_prim ceiling_num "ceiling"
let prim_floor    = unary_number_prim floor_num   "floor"

let prim_land = binary_number_prim land_num "land"
let prim_lor  = binary_number_prim lor_num  "lor"
let prim_lxor = binary_number_prim lxor_num "lxor"
let prim_lneg = unary_number_prim  lneg_num "lneg"

let num_two = num_of_int 2

let prim_lsr = binary_number_prim (fun m n -> m // power_num num_two n) "lsr"
let prim_lsl = binary_number_prim (fun m n -> m */ power_num num_two n) "lsl"

(* "real" arithmetic *)

let pi      = 4.0 *. atan 1.0
let pi_inv  = 1.0 /. pi
let num_180 = num_of_int 180

let float_wrapper f x = num_of_float (f (float_of_num x))

let sind x    = num_of_float (sin (pi *. float_of_num (x // num_180)))
let cosd x    = num_of_float (cos (pi *. float_of_num (x // num_180)))
let tand x    = num_of_float (tan (pi *. float_of_num (x // num_180)))
let arcsind x = num_of_float (pi_inv *. asin (float_of_num x)) */ num_180
let arccosd x = num_of_float (pi_inv *. acos (float_of_num x)) */ num_180
let arctand x = num_of_float (pi_inv *. atan (float_of_num x)) */ num_180

let arcsinh x = log (x +. sqrt(x *. x +. 1.0))
let arccosh x = log (x +. sqrt(x *. x -. 1.0))
let arctanh x = 0.5 *. (log (1.0 +. x) -. log (1.0 -. x))

let prim_sqrt    = unary_number_prim (float_wrapper sqrt)    "sqrt"
let prim_exp     = unary_number_prim (float_wrapper exp)     "exp"
let prim_log     = unary_number_prim (float_wrapper log)     "log"
let prim_sin     = unary_number_prim (float_wrapper sin)     "sin"
let prim_cos     = unary_number_prim (float_wrapper cos)     "cos"
let prim_tan     = unary_number_prim (float_wrapper tan)     "tan"
let prim_arcsin  = unary_number_prim (float_wrapper asin)    "arcsin"
let prim_arccos  = unary_number_prim (float_wrapper acos)    "arccos"
let prim_arctan  = unary_number_prim (float_wrapper atan)    "arctan"
let prim_sind    = unary_number_prim sind                    "sind"
let prim_cosd    = unary_number_prim cosd                    "cosd"
let prim_tand    = unary_number_prim tand                    "tand"
let prim_arcsind = unary_number_prim arcsind                 "arcsind"
let prim_arccosd = unary_number_prim arcsind                 "arccosd"
let prim_arctand = unary_number_prim arcsind                 "arctand"
let prim_sinh    = unary_number_prim (float_wrapper sinh)    "sinh"
let prim_cosh    = unary_number_prim (float_wrapper cosh)    "cosh"
let prim_tanh    = unary_number_prim (float_wrapper tanh)    "tanh"
let prim_arcsinh = unary_number_prim (float_wrapper arcsinh) "arcsinh"
let prim_arccosh = unary_number_prim (float_wrapper arccosh) "arccosh"
let prim_arctanh = unary_number_prim (float_wrapper arctanh) "arctanh"

let prim_abs x = match !x with
  | Number n    -> Number (abs_num n)
  | LinForm l   ->
      (Evaluate.evaluate_lin_form x l;
       match !x with
       | Number n -> Number (abs_num n)
       | _        -> runtime_error "abs: invalid argument")
  | Tuple xs ->
      (let len = Array.length xs in
       let ss  = Array.init len (fun _ -> create_unbound ()) in
       for i = 0 to len - 1 do
         Evaluate.mul_unknowns ss.(i) xs.(i) xs.(i)
       done;
       Number
         (float_wrapper sqrt
            (Array.fold_left
               (fun sum s -> match !s with
                 | Number c -> sum +/ c
                 | _        -> runtime_error "abs: invalid argument")
               num_zero
               ss)))
  | _ -> runtime_error "abs: invalid argument"

(* lists *)

let prim_length x =
  let rec count_len len x = match !x with
    | Nil          -> len
    | List (_, a)  -> count_len (len + 1) a
    | Tuple xs     -> Array.length xs
    | Dictionary d -> SymbolMap.fold (fun _ _ n -> n + 1) d 0
    | Unbound
    | Constraint _ -> runtime_error "length: argument undefined"
    | _            -> 1
  in Number (num_of_int (count_len 0 x))

let rec prim_to_string x = match !x with
  | Number n ->
      if n </ num_zero then
        List (ref (Char 126), ref (ascii_to_char_list (string_of_num (minus_num n))))
      else
        let str = string_of_num n in
        if String.length str >= 2 && str.[String.length str - 2] = '/' &&
           str.[String.length str - 1] = '1' then
          ascii_to_char_list (String.sub str 0 (String.length str - 2))
        else
          ascii_to_char_list str
  | Bool b    -> if b then
                   ascii_to_char_list "true"
                 else
                   ascii_to_char_list "false"
  | Char _    -> List (x, ref Nil)
  | Symbol s  -> uc_string_to_char_list (symbol_to_string s)
  | Nil       -> ascii_to_char_list "[]"
  | LinForm l ->
      (Evaluate.evaluate_lin_form x l;
       match !x with
       | Number _ -> prim_to_string x
       | _        -> runtime_error "to_string: argument undefined")
  | Unbound
  | Constraint _ -> runtime_error "to_string: argument undefined"
  | _            -> runtime_error "to_string: invalid argument"

(* format_string *)

type number_format = NF_Decimal
  | NF_Hexadecimal
  | NF_HEXADECIMAL
  | NF_Roman
  | NF_ROMAN
  | NF_Alpha
  | NF_ALPHA

type format_spec = FS_Literal of uc_list
  | FS_String of bool * int
  | FS_List of format_spec * uc_list
  | FS_Number of bool * bool * int * int * number_format

let parse_token fmt =
  let rec parse_type align sign len len2 fmt = match fmt with
    | 115 :: xs -> (* s *)
        if sign || len2 > 0 then
          runtime_error "format_string: invalid format"
        else
          (FS_String (align, len), xs)
    | 100 :: xs -> (FS_Number (align, sign, len, len2, NF_Decimal),     xs)
    | 120 :: xs -> (FS_Number (align, sign, len, len2, NF_Hexadecimal), xs)
    | 88 :: xs  -> (FS_Number (align, sign, len, len2, NF_HEXADECIMAL), xs)
    | 114 :: xs -> (FS_Number (align, sign, len, len2, NF_Roman),       xs)
    | 82 :: xs  -> (FS_Number (align, sign, len, len2, NF_ROMAN),       xs)
    | 97 :: xs -> (FS_Number (align, sign, len, len2, NF_Alpha),       xs)
    | 65 :: xs  -> (FS_Number (align, sign, len, len2, NF_ALPHA),       xs)
    | _ -> runtime_error "format_string: invalid format"
  and parse_len2 align sign len len2 fmt = match fmt with
    | c :: cs ->
        if c >= 48 && c < 58 then
          parse_len2 align sign len (10 * len2 + c - 48) cs
        else
          parse_type align sign len len2 fmt
    | _ -> runtime_error "format_string: invalid format"
  and parse_frac align sign len fmt = match fmt with
    | 46 :: xs -> parse_len2 align sign len 0 xs    (* . *)
    | _          -> parse_type align sign len 0 fmt
  and parse_len align sign len fmt = match fmt with
    | c :: cs ->
        if c >= 48 && c < 58 then
          parse_len align sign (10 * len + c - 48) cs
        else
          parse_frac align sign len fmt
    | _ -> runtime_error "format_string: invalid format"
  and parse_sign align fmt = match fmt with
    | 43 :: xs -> parse_len align true  0 xs   (* + *)
    | _          -> parse_len align false 0 fmt
  and parse_list_spec fmt =
    let (spec, f) = parse_align fmt in
    let rec iter sep f = match f with
      | []         -> runtime_error "format_string: invalid format"
      | 93 :: cs -> (FS_List (spec, List.rev sep), cs) (* ] *)
      | c :: cs  -> iter (c :: sep) cs
    in iter [] f
  and parse_align fmt = match fmt with
    | 37 :: xs -> (FS_Literal [37], xs)  (* % *)
    | 91 :: xs -> parse_list_spec xs     (* [ *)
    | 45 :: xs -> parse_sign true  xs    (* - *)
    | _          -> parse_sign false fmt
  in
  parse_align fmt

let parse_format_string fmt =
  let rec iter res n fmt = match fmt with
    | []         -> (List.rev res, n)
    | 37 :: xs -> (* % *)
        let (t, ys) = parse_token xs in
        (match t with
         | FS_Literal _ -> iter (t :: res) n     ys
         | _            -> iter (t :: res) (n+1) ys)
    | _ ->
        let rec iter2 lit fmt = match fmt with
          | []
          | 37 :: _ -> iter (FS_Literal (List.rev lit) :: res) n fmt
          | c :: cs -> iter2 (c :: lit) cs
        in iter2 [] fmt
  in iter [] 0 fmt

let number_to_string sign len2 nf x =
  let to_str nf x = match nf with
    | NF_Decimal     -> Format.num_to_arabic  10 x
    | NF_Hexadecimal -> Format.num_to_arabic  10 x
    | NF_HEXADECIMAL-> Format.num_to_ARABIC  10 x
    | NF_Roman       -> Format.num_to_roman      x
    | NF_ROMAN       -> Format.num_to_ROMAN      x
    | NF_Alpha       -> Format.num_to_alphabetic x
    | NF_ALPHA       -> Format.num_to_ALPHABETIC x
  in
  let pos_num_to_string len2 nf x =
    let y  = floor_num x in
    let z  = x -/ y in
    let s1 = to_str nf y in
    let add_fractional len x =
      let s = to_str nf (floor_num (x */ power_num num_ten (num_of_int len))) in
      let l = List.length s in
      46 :: Tools.XList.repeat (len - l) 48 @ s
    in
    if len2 <= 0 then
      if z <=/ (num_of_int 1 // num_of_int 10000000) then
        s1
      else
        s1 @ add_fractional 7 z
    else
      s1 @ add_fractional len2 z
  in
  if x </ num_zero then
    45 :: pos_num_to_string len2 nf (minus_num x)
  else if sign then
    43 :: pos_num_to_string len2 nf x
  else
    pos_num_to_string len2 nf x

let output_format_string fmt args =
  let rec add_string tail str = match str with
    | []      -> tail
    | c::cs ->
        let x = ref Unbound in
        tail := List (ref (Char c), x);
        add_string x cs
  in
  let add_aligned_string tail align pad len str =
    let rec add_padding tail pad n =
      if n <= 0 then
        tail
      else
        let x = ref Unbound in
        tail := List (ref (Char pad), x);
        add_padding x pad (n-1)
    in
    if len <= 0 then
      add_string tail str
    else
      let l = List.length str in
      if align then
        add_padding
          (add_string tail str)
          pad
          (len - l)
      else
        add_string
          (add_padding tail pad (len - l))
          str
  in
  let rec format_argument tail fmt arg = match fmt with
    | FS_Literal str ->
        add_string tail str
    | FS_String (align, len) ->
        (match !arg with
         | Char c   -> add_aligned_string tail align 32 len [c]
         | Symbol s -> add_aligned_string tail align 32 len (Array.to_list (symbol_to_string s))
         | Nil      -> add_aligned_string tail align 32 len []
         | List _ ->
             let str = evaluate_char_list "format_string" arg in
             add_aligned_string tail align 32 len str
         | _ -> runtime_error "format_string: invalid argument for %s")
    | FS_List (spec, sep) ->
        let lst = Evaluate.evaluate_list "format_string" arg in
        (match lst with
         | []      -> tail
         | x::xs ->
             let r = format_argument tail spec x in
             let rec iter tail lst = match lst with
               | []      -> tail
               | x::xs ->
                   iter
                     (format_argument (add_string tail sep) spec x)
                     xs
             in iter r xs)
    | FS_Number (align, sign, len, len2, nf) ->
        let n = Evaluate.evaluate_num "format_string" arg in
        add_aligned_string tail align 32 len
          (number_to_string sign len2 nf n)
  in
  let result = ref Unbound in
  let rec iter tail fmt args = match fmt with
    | [] ->
        tail := Nil;
        !result
    | FS_Literal str :: fs -> iter (add_string tail str) fs args
    | f :: fs -> (match args with
        | b::bs -> iter (format_argument tail f b) fs bs
        | _       -> assert false)
  in
  iter result fmt args

let prim_format_string fmt_string =
  let fmt = evaluate_char_list "format_string" fmt_string in
  let (f, n) = parse_format_string fmt in
  if n = 0 then
    !fmt_string
  else
    PrimitiveN (n, output_format_string f)

let prim_sort_strings cmp key_val =
  let kv      = Evaluate.evaluate_list "sort_strings" key_val in
  let classes =
    List.map
      (evaluate_char_list "sort_strings")
      (Evaluate.evaluate_list "sort_strings" cmp) in
  let class_map =
    snd
      (List.fold_left
         (fun (n,map) cls ->
            (n+1,
             List.fold_left
               (fun m c -> DynamicCharMap.add c n m)
               map
               cls))
         (0, DynamicCharMap.empty)
         classes) in
  let default_class = try
    DynamicCharMap.find 46 class_map  (* . *)
  with Not_found -> 0 in
  let char_class c = try
    DynamicCharMap.find c class_map
  with Not_found -> default_class in
  let rec sort_iter trie kv = match kv with
    | [] ->
        let result = ref Unbound in
        let tail =
          Unicode.DynUCTrie.fold
            (fun _ vals res ->
               let group =
                 List.fold_left
                   (fun l v -> List (v, ref l))
                   Nil
                   vals in
               let new_res = ref Unbound in
               res := List (ref group, new_res);
               new_res)
            trie
            result in
        tail := Nil;
        !result
    | x :: xs -> (match !x with
        | Tuple arr ->
            if Array.length arr <> 2 then
              runtime_error ("sort_strings: pair expected but got tuple of length " ^ string_of_int (Array.length arr))
            else
              let k  = evaluate_char_list "sort_strings" arr.(0) in
              let v  = arr.(1) in
              let kc = List.map char_class k in
              let old = try
                Unicode.DynUCTrie.find_list kc trie
              with Not_found -> [] in
              sort_iter (Unicode.DynUCTrie.add_list kc (v :: old) trie) xs
        | _ -> runtime_error ("sort_strings: pair expected but got " ^ type_name !x))
  in sort_iter Unicode.DynUCTrie.empty kv

let prim_to_tuple x =
  Tuple (Array.of_list (Evaluate.evaluate_list "to_tuple" x))

let prim_to_list x = match !x with
  | Tuple xs     -> Array.fold_right
                       (fun a b -> List (a, ref b))
                       xs
                       Nil
  | Unbound
  | Constraint _ -> runtime_error "to_list: argument undefined"
  | _            -> runtime_error "to_list: invalid argument"

let rec unary_vec2_prim f name x = match !x with
  | Tuple [| a; b |] -> (match (!a, !b) with
      | (Number n, Number m) ->
          (try f n m
           with _ -> runtime_error (name ^ ": invalid argument"))
      | _ -> runtime_error (name ^ ": invalid argument"))
  | LinForm l ->
      (Evaluate.evaluate_lin_form x l;
       match !x with
       | Tuple _ -> unary_vec2_prim f name x
       | _       -> runtime_error (name ^ ": invalid argument"))
  | _ -> runtime_error (name ^ ": invalid argument")

let rec prim_dir x =
  Tuple [| ref (prim_cosd x); ref (prim_sind x) |]

and prim_cosd x = unary_number_prim cosd "cosd" x
and prim_sind x = unary_number_prim sind "sind" x

let prim_angle = unary_vec2_prim
    (fun x y -> match sign_num x with
      | 1 -> Number (arctand (y // x))
      | 0 -> if y >/ num_zero then
               Number (num_of_int 90)
             else if y =/ num_zero then
               runtime_error "angle: invalid argument"
             else
               Number (num_of_int 270)
      | -1 -> Number (num_of_int 180 -/ arctand (minus_num y // x))
      | _  -> assert false)
    "angle"

let rec prim_rotate a vec = match !a with
  | Number n    -> unary_vec2_prim
                     (fun x y ->
                        let x2 = cosd n */ x -/ sind n */ y in
                        let y2 = sind n */ x +/ cosd n */ y in
                        Tuple [| ref (Number x2); ref (Number y2) |])
                     "rotate"
                     vec
  | LinForm l   ->
      (Evaluate.evaluate_lin_form a l;
       match !a with
       | Number _ -> prim_rotate a vec
       | _        -> runtime_error "rotate: invalid argument")
  | _ -> runtime_error "rotate: invalid argument"

let prim_add_to_dict args = match args with
  | [sym; value; dict] -> (match !sym with
      | Symbol s -> (match !dict with
          | Dictionary d -> Dictionary (Unicode.SymbolTable.SymbolMap.add s value d)
          | Unbound
          | Constraint _ -> runtime_error "add_to_dict: argument undefined"
          | _            -> runtime_error ("add_to_dict: invalid argument (got " ^ type_name !dict ^ " instead of dictionary)"))
      | Unbound
      | Constraint _ -> runtime_error "add_to_dict: argument undefined"
      | _            -> runtime_error ("add_to_dict: invalid argument (got " ^ type_name !dict ^ " instead of symbol)"))
  | _ -> assert false

(* characters *)

let unary_char_prim f name x = match !x with
  | Char n ->
      (try f n with
       | _ -> runtime_error (name ^ ": invalid argument"))
  | _ -> runtime_error (name ^ ": invalid argument")

let prim_is_letter    = unary_char_prim (fun c -> Bool (UChar.is_letter c))    "is_letter"
let prim_is_mark      = unary_char_prim (fun c -> Bool (UChar.is_mark c))      "is_mark"
let prim_is_number    = unary_char_prim (fun c -> Bool (UChar.is_number c))    "is_number"
let prim_is_punct     = unary_char_prim (fun c -> Bool (UChar.is_punct c))     "is_punct"
let prim_is_symbol    = unary_char_prim (fun c -> Bool (UChar.is_symbol c))    "is_symbol"
let prim_is_separator = unary_char_prim (fun c -> Bool (UChar.is_separator c)) "is_separator"
let prim_is_control   = unary_char_prim (fun c -> Bool (UChar.is_control c))   "is_control"
let prim_is_space     = unary_char_prim (fun c -> Bool (UChar.is_space c))     "is_space"
let prim_to_upper     = unary_char_prim (fun c -> Char (UChar.to_upper c))     "to_upper"
let prim_to_lower     = unary_char_prim (fun c -> Char (UChar.to_lower c))     "to_lower"
let prim_to_title     = unary_char_prim (fun c -> Char (UChar.to_title c))     "to_title"
let prim_char_name    = unary_char_prim (fun c -> ascii_to_char_list (UChar.name c))  "char_name"

let symbol_Lu = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lu"))
let symbol_Ll = Symbol (string_to_symbol (UString.uc_string_of_ascii "Ll"))
let symbol_Lt = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lt"))
let symbol_Lm = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lm"))
let symbol_Lo = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lo"))
let symbol_Mn = Symbol (string_to_symbol (UString.uc_string_of_ascii "Mn"))
let symbol_Mc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Mc"))
let symbol_Me = Symbol (string_to_symbol (UString.uc_string_of_ascii "Me"))
let symbol_Nd = Symbol (string_to_symbol (UString.uc_string_of_ascii "Nd"))
let symbol_Nl = Symbol (string_to_symbol (UString.uc_string_of_ascii "Nl"))
let symbol_No = Symbol (string_to_symbol (UString.uc_string_of_ascii "No"))
let symbol_Pc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pc"))
let symbol_Pd = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pd"))
let symbol_Ps = Symbol (string_to_symbol (UString.uc_string_of_ascii "Ps"))
let symbol_Pe = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pe"))
let symbol_Pi = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pi"))
let symbol_Pf = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pf"))
let symbol_Po = Symbol (string_to_symbol (UString.uc_string_of_ascii "Po"))
let symbol_Sm = Symbol (string_to_symbol (UString.uc_string_of_ascii "Sm"))
let symbol_Sc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Sc"))
let symbol_Sk = Symbol (string_to_symbol (UString.uc_string_of_ascii "Sk"))
let symbol_So = Symbol (string_to_symbol (UString.uc_string_of_ascii "So"))
let symbol_Zs = Symbol (string_to_symbol (UString.uc_string_of_ascii "Zs"))
let symbol_Zl = Symbol (string_to_symbol (UString.uc_string_of_ascii "Zl"))
let symbol_Zp = Symbol (string_to_symbol (UString.uc_string_of_ascii "Zp"))
let symbol_Cc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cc"))
let symbol_Cf = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cf"))
let symbol_Cs = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cs"))
let symbol_Co = Symbol (string_to_symbol (UString.uc_string_of_ascii "Co"))
let symbol_Cn = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cn"))

let prim_char_category =
  unary_char_prim
    (fun c -> match UChar.category c with
       | UChar.Lu -> symbol_Lu
       | UChar.Ll -> symbol_Ll
       | UChar.Lt -> symbol_Lt
       | UChar.Lm -> symbol_Lm
       | UChar.Lo -> symbol_Lo
       | UChar.Mn -> symbol_Mn
       | UChar.Mc -> symbol_Mc
       | UChar.Me -> symbol_Me
       | UChar.Nd -> symbol_Nd
       | UChar.Nl -> symbol_Nl
       | UChar.No -> symbol_No
       | UChar.Pc -> symbol_Pc
       | UChar.Pd -> symbol_Pd
       | UChar.Ps -> symbol_Ps
       | UChar.Pe -> symbol_Pe
       | UChar.Pi -> symbol_Pi
       | UChar.Pf -> symbol_Pf
       | UChar.Po -> symbol_Po
       | UChar.Sm -> symbol_Sm
       | UChar.Sc -> symbol_Sc
       | UChar.Sk -> symbol_Sk
       | UChar.So -> symbol_So
       | UChar.Zs -> symbol_Zs
       | UChar.Zl -> symbol_Zl
       | UChar.Zp -> symbol_Zp
       | UChar.Cc -> symbol_Cc
       | UChar.Cf -> symbol_Cf
       | UChar.Cs -> symbol_Cs
       | UChar.Co -> symbol_Co
       | UChar.Cn -> symbol_Cn)
    "char_category"

let prim_to_symbol x =
  let str = evaluate_char_list "to_symbol" x in
  Symbol (string_to_symbol (Array.of_list str))

let prim_generate_symbol _ =
  Symbol (alloc_symbol ())

(* initialisation *)

let bind_primitive scope name v =
  Scope.add_global scope (string_to_symbol (UString.uc_string_of_ascii name)) v

let bind_bin_op_l scope name pri v =
  let sym = string_to_symbol (UString.uc_string_of_ascii name) in
  Scope.add_bin_op scope pri Lexer.Left sym;
  Scope.add_global scope sym v

let bind_bin_op_n scope name pri v =
  let sym = string_to_symbol (UString.uc_string_of_ascii name) in
  Scope.add_bin_op scope pri Lexer.NonA sym;
  Scope.add_global scope sym v

let bind_bin_op_r scope name pri v =
  let sym = string_to_symbol (UString.uc_string_of_ascii name) in
  Scope.add_bin_op scope pri Lexer.Right sym;
  Scope.add_global scope sym v

let bind_pre_op scope name v =
  let sym = string_to_symbol (UString.uc_string_of_ascii name) in
  Scope.add_pre_op scope sym;
  Scope.add_global scope sym v

let bind_post_op scope name v =
  let sym = string_to_symbol (UString.uc_string_of_ascii name) in
  Scope.add_post_op scope sym;
  Scope.add_global  scope sym v

let initial_scope () =
  let scope = Scope.create () in
  let add name v = bind_primitive scope name v in
  let add1 name f = bind_primitive scope name (Primitive1 f) in
  let add2 name f = bind_primitive scope name (Primitive2 f) in

  add1 "error"    prim_error;

  add1 "is_unbound"  prim_is_unbound;
  add1 "is_bool"     prim_is_bool;
  add1 "is_number"   prim_is_number;
  add1 "is_char"     prim_is_char;
  add1 "is_symbol"   prim_is_symbol;
  add1 "is_function" prim_is_function;
  add1 "is_list"     prim_is_list;
  add1 "is_tuple"    prim_is_tuple;

  add2 "||"       prim_or;
  add2 "&&"       prim_and;
  add1 "not"      prim_not;

  add2 "=="       prim_eq;
  add2 "<>"       prim_neq;
  add2 ">"        prim_gt;
  add2 "<"        prim_lt;
  add2 ">="       prim_ge;
  add2 "<="       prim_le;
  add2 "min"      prim_min;
  add2 "max"      prim_max;

  add2 "+"        Evaluate.prim_add;
  add2 "-"        Evaluate.prim_sub;
  add2 "*"        Evaluate.prim_mul;
  add2 "/"        Evaluate.prim_div;
  add2 "^"        prim_pow;
  add2 "quot"     prim_quot;
  add2 "mod"      prim_mod;
  add1 "~"        prim_negate;
  add1 "abs"      prim_abs;

  add1 "round"    prim_round;
  add1 "truncate" prim_truncate;
  add1 "ceiling"  prim_ceiling;
  add1 "floor"    prim_floor;
  add2 "land"     prim_land;
  add2 "lor"      prim_lor;
  add2 "lxor"     prim_lxor;
  add2 "lsr"      prim_lsr;
  add2 "lsl"      prim_lsl;

  add  "pi"  (Number (num_of_float pi));

  add1 "sqrt"     prim_sqrt;
  add1 "exp"      prim_exp;
  add1 "log"      prim_log;
  add1 "sin"      prim_sin;
  add1 "cos"      prim_cos;
  add1 "tan"      prim_tan;
  add1 "arcsin"   prim_arcsin;
  add1 "arccos"   prim_arccos;
  add1 "arctan"   prim_arctan;
  add1 "sind"     prim_sind;
  add1 "cosd"     prim_cosd;
  add1 "tand"     prim_tand;
  add1 "arcsind"  prim_arcsind;
  add1 "arccosd"  prim_arccosd;
  add1 "arctand"  prim_arctand;
  add1 "sinh"     prim_sinh;
  add1 "cosh"     prim_cosh;
  add1 "tanh"     prim_tanh;
  add1 "arcsinh"  prim_arcsinh;
  add1 "arccosh"  prim_arccosh;
  add1 "arctanh"  prim_arctanh;

  add1 "length"        prim_length;
  add1 "to_string"     prim_to_string;
  add1 "format_string" prim_format_string;
  add2 "sort_strings"  prim_sort_strings;
  add1 "to_list"       prim_to_list;
  add1 "to_tuple"      prim_to_tuple;
  add1 "dir"           prim_dir;
  add1 "angle"         prim_angle;
  add2 "rotate"        prim_rotate;
  add  "add_to_dict"   (PrimitiveN (3, prim_add_to_dict));

  add1 "char_is_letter"     prim_is_letter;
  add1 "char_is_mark"       prim_is_mark;
  add1 "char_is_number"     prim_is_number;
  add1 "char_is_punct"      prim_is_punct;
  add1 "char_is_symbol"     prim_is_symbol;
  add1 "char_is_separator"  prim_is_separator;
  add1 "char_is_control"    prim_is_control;
  add1 "char_is_space"      prim_is_space;
  add1 "to_upper"           prim_to_upper;
  add1 "to_lower"           prim_to_lower;
  add1 "to_title"           prim_to_title;
  add1 "char_name"          prim_char_name;
  add1 "char_category"      prim_char_category;

  add1 "to_symbol"       prim_to_symbol;
  add1 "generate_symbol" prim_generate_symbol;

  let scale x =
    Function ([], 1,
      [|BVariable (0, 0);
        BConst (Number x);
        BConst (Primitive2 Evaluate.prim_mul);
        BApply 2;
        BReturn|]) in

  bind_post_op scope "pt" (scale num_one);
  bind_post_op scope "bp" (scale (num_of_int 7227 // num_of_int 7200));
  bind_post_op scope "cc" (scale (num_of_int 14856 // num_of_int 1157));
  bind_post_op scope "cm" (scale (num_of_int 7227 // num_of_int 254));
  bind_post_op scope "dd" (scale (num_of_int 1238 // num_of_int 1157));
  bind_post_op scope "in" (scale (num_of_int 7227 // num_of_int 100));
  bind_post_op scope "mm" (scale (num_of_int 7227 // num_of_int 2540));
  bind_post_op scope "pc" (scale (num_of_int 12));
  bind_post_op scope "sp" (scale (num_of_int 1 // num_of_int 65536));

  add1 "make_path"               Path.make_path;
  add2 "close_path"              Path.close_path;
  add2 "path_add_point"          Path.add_point;
  add2 "path_add_in_dir"         Path.add_in_dir;
  add2 "path_add_in_angle"       Path.add_in_angle;
  add2 "path_add_in_curl"        Path.add_in_curl;
  add2 "path_add_in_tension"     Path.add_in_tension;
  add2 "path_add_out_dir"        Path.add_out_dir;
  add2 "path_add_out_angle"      Path.add_out_angle;
  add2 "path_add_out_curl"       Path.add_out_curl;
  add2 "path_add_out_tension"    Path.add_out_tension;
  add  "path_add_control_points" (PrimitiveN (3, (function [p1; p2; spec] -> Path.add_control_points p1 p2 spec | _ -> assert false)));

  scope
