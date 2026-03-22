
open XNum
open Runtime
open Unicode
open Logging
open Dim
open Engine
open CharCode

(* simple parsing routines ************************************************************************)

(*
  |match_substring <stream> <str> <i>| tests whether the given string appears at position <i> in <stream>.
*)
let rec match_substring stream str i =
  match str with
  | [] -> true
  | c :: cs ->
      if c = UCStream.get_char stream i then
        match_substring stream cs (i + 1)
      else
        false

(*
  |newline_to_par <stream>| checks whether the first character of <stream> is a newline and replaces it
  by the string |\par|.
*)

let newline_to_par stream =
  if cat_code (UCStream.next_char stream) = Newline then
    UCStream.insert_string stream (UString.uc_string_of_ascii "\\par")
  else ()

(*
  |skip_spaces <stream>| removes all initial spaces from <stream>.
*)

let rec skip_spaces stream =
  while cat_code (UCStream.next_char stream) = Space do
    UCStream.remove stream 1
  done

(*
  |skip_blanks <stream>| removes all initial spaces from <stream>. If two newlines are encountered
  the second one is replaced by the string |\par|.
*)

let rec skip_blanks stream =
  match cat_code (UCStream.next_char stream) with
  | Space ->
      UCStream.remove stream 1;
      skip_blanks stream
  | Newline ->
      UCStream.remove stream 1;
      skip_spaces stream;
      newline_to_par stream
  | _ -> ()

(*
  |skip_comment <stream>| removes all characters from <stream> until the first newlines is found.
*)
let rec skip_comment stream =
  if cat_code (UCStream.pop stream) = Newline then (
    skip_spaces stream;
    newline_to_par stream
  ) else
    skip_comment stream

(* parsing tokens and arguments *)

(*
  |is_command_sequence <str>| checks whether <str> consists of one backslash followed by letters.
  |is_token <str>| checks whether <str> consists of exactly one TeX token.
*)
let is_command_sequence str =
  match str with
  | [] -> false
  | [_] -> false
  | c :: cs -> c = escape_char && List.for_all (fun x -> cat_code x = Letter) cs

let is_token str =
  match str with
  | [] -> false
  | [_] -> true
  | [c; _] -> c = escape_char
  | c :: cs -> c = escape_char && List.for_all (fun x -> cat_code x = Letter) cs

(* |read_token_tail <stream>| reads the rest of a \TeX-token after the initial backslash is already read. *)
let read_token_tail stream =
  let rec read_command_sequence () =
    let c = UCStream.next_char stream in
    if cat_code c = Letter then (
      UCStream.remove stream 1;
      c :: read_command_sequence ()
    ) else (
      skip_blanks stream;
      []
    )
  in
  let char = UCStream.pop stream in
  if cat_code char = Letter then
    escape_char :: char :: read_command_sequence ()
  else
    [escape_char; char]

(* |read_token <stream>| reads a whole \TeX-token. *)

let read_token stream =
  let char = UCStream.pop stream in
  match cat_code char with
  | EOF -> []
  | Escape -> read_token_tail stream
  | _ -> [char]

(* |peek_token <stream>| reads a whole \TeX-token without removing it from <stream>. *)

let peek_token stream =
  let rec read_command_sequence i () =
    let c = UCStream.get_char stream i in
    if cat_code c = Letter then
      c :: read_command_sequence (i + 1) ()
    else
      []
  in
  let char = UCStream.next_char stream in
  if char = -1 then
    []
  else if cat_code char = Escape then (
    let c2 = UCStream.get_char stream 1 in
    if cat_code c2 = Letter then
      char :: c2 :: read_command_sequence 2 ()
    else
      [char; c2]
  ) else
    [char]

(* |read_group <prefix> <stream>| reads a group.
   It is assumed that the opening brace was already removed from the stream.
   The result is appended in reversed order to <prefix>.
*)
let read_group prefix stream =
  let rec iter nest prefix =
    let c = UCStream.pop stream in
    match cat_code c with
    | BeginGroup -> iter (nest + 1) (c :: prefix)
    | EndGroup ->
        if nest = 0 then
          prefix
        else
          iter (nest - 1) (c :: prefix)
    | _ -> iter nest (c :: prefix)
  in
  iter 0 prefix

(* |read_argument| reads the next token or group. *)

let read_argument stream =
  skip_blanks stream;
  if cat_code (UCStream.next_char stream) = BeginGroup then (
    let _ = UCStream.pop stream in
    List.rev (read_group [] stream)
  ) else
    read_token stream

(*
  |read_optional <stream> <default>| reads an optional argument. If none is present <default> is returned.
*)
let read_optional stream default =
  let rec read_group_internal nest =
    let c = UCStream.pop stream in
    match cat_code c with
    | BeginOptional -> c :: read_group_internal (nest + 1)
    | EndOptional ->
        if nest = 0 then
          []
        else
          c :: read_group_internal (nest - 1)
    | _ -> c :: read_group_internal nest
  in
  skip_blanks stream;
  if cat_code (UCStream.next_char stream) = BeginOptional then (
    let _ = UCStream.pop stream in
    read_group_internal 0
  ) else
    default

(* |read_bool <stream>| checks whether the next character is a star. *)
let read_bool stream =
  skip_blanks stream;
  if UCStream.next_char stream = 42 then (
    let _ = UCStream.pop stream in
    true
  ) else
    false

(* |read_true_false <stream>| checks whether the next word is "true" or "false". *)

let read_true_false stream =
  skip_blanks stream;
  if UCStream.match_prefix stream [116; 114; 117; 101] then (
    UCStream.remove stream 4;
    true
  ) else if UCStream.match_prefix stream [102; 97; 108; 115; 101] then (
    UCStream.remove stream 5;
    false
  ) else
    false

(* |read_list <stream>| parses a list separated by commas or semicolons. *)
let rec read_list stream =
  let rec get_next result =
    let rec strip_blanks str =
      match str with
      | [] -> []
      | c :: cs -> (
          match cat_code c with
          | Space | Newline -> strip_blanks cs
          | _ -> str)
    in
    match UCStream.pop stream with
    | -1 | 44 | 59 -> List.rev (strip_blanks result)
    | c -> get_next (c :: result)
  in
  let l = Tools.ListBuilder.make () in
  let rec iter () =
    skip_blanks stream;
    if UCStream.next_char stream = -1 then
      Tools.ListBuilder.get l
    else (
      (match get_next [] with
      | [] -> ()
      | v -> Tools.ListBuilder.add l v);
      iter ())
  in
  ignore (iter ());
  Tools.ListBuilder.get l

(* |read_keyword <stream>| reads a keyword of a key-value list. *)
let read_keyword stream =
  let rec iter () =
    let c = UCStream.next_char stream in
    match cat_code c with
    | Letter | Other -> (
        match c with
        | 44 | 59 | 61 -> [] (* ,  = *)
        | _ ->
            let _ = UCStream.pop stream in
            c :: iter ())
    | _ -> []
  in
  iter ()

(* |read_key_val_list <stream>| reads a list of key-value pairs. *)

let rec read_key_val_list stream =
  let rec read_value () =
    let v = Tools.ListBuilder.make () in
    let blanks = Tools.ListBuilder.make () in
    let add_char c =
      Tools.ListBuilder.append v blanks;
      Tools.ListBuilder.add v c
    in
    let rec read_nested nest =
      let c = UCStream.pop stream in
      match c with
      | -1 -> Tools.ListBuilder.get v
      | 44 | 59 ->
          if nest <= 0 then
            Tools.ListBuilder.get v
          else (
            add_char c;
            read_nested nest)
      | 123 ->
          add_char c;
          read_nested (nest + 1)
      | 125 ->
          if nest <= 0 then
            Tools.ListBuilder.get v
          else (
            add_char c;
            read_nested (nest - 1))
      | _ ->
          (match cat_code c with
          | Space | Newline -> Tools.ListBuilder.add blanks c
          | _ -> add_char c);
          read_nested nest
    in
    skip_blanks stream;
    if UCStream.next_char stream = 123 then (
      let _ = UCStream.pop stream in
      let first_part = read_group [] stream in
      let rest = Tools.ListBuilder.make () in
      let rec read_blanks () =
        let c = UCStream.pop stream in
        match cat_code c with
        | Space | Newline ->
            Tools.ListBuilder.add rest c;
            read_blanks ()
        | _ -> (
            match c with
            | -1 | 44 | 59 | 125 -> List.rev first_part (* , ; } *)
            | _ ->
                let tail = read_nested 0 in
                Tools.ListBuilder.add_list rest tail;
                (* Intent here seems to be constructing a wrapped version {first_part}rest *)
                [123] @ List.rev first_part @ [125] @ (Tools.ListBuilder.get rest)
            )
      in
      read_blanks ()
    ) else
      read_nested 0
  in
  let rec iter dict =
    skip_blanks stream;
    match cat_code (UCStream.next_char stream) with
    | Comment ->
        skip_comment stream;
        iter dict
    | EOF -> dict
    | _ -> (
        let key = read_keyword stream in
        skip_blanks stream;
        if UCStream.next_char stream = 61 then (
          let _ = UCStream.pop stream in
          let v = read_value () in
          iter (Unicode.DynUCTrie.add_list key (Some v) dict)
        ) else (
          let rec skip_rest () =
            match UCStream.pop stream with
            | 44 | 59 | -1 -> iter (Unicode.DynUCTrie.add_list key None dict)
            | _ -> skip_rest ()
          in
          skip_rest ()))
  in
  iter Unicode.DynUCTrie.empty

(*** parsing numbers and dimensions *****************************************************)

let read_digit stream base =
  let c = UCStream.next_char stream in
  if c >= 48 && c <= 57 then
    if c < 48 + base then (
      let _ = UCStream.remove stream 1 in
      c - 48
    ) else
      -1
  else if base > 10 then
    if c >= 65 && c < 55 + base then (
      let _ = UCStream.remove stream 1 in
      c - 55
    ) else if c >= 97 && c < 87 + base then (
      let _ = UCStream.remove stream 1 in
      c - 87
    ) else
      -1
  else
    -1

let read_unsigned_int stream =
  let base =
    if UCStream.next_char stream = 48 then
      match UCStream.get_char stream 1 with
      | 120 | 88 ->
          UCStream.remove stream 2;
          16
      | 111 | 79 ->
          UCStream.remove stream 2;
          8
      | 98 | 66 ->
          UCStream.remove stream 2;
          2
      | _ -> 10
    else
      10
  in
  let rec iter n =
    let d = read_digit stream base in
    if d > -1 then
      iter (base * n + d)
    else
      (n, base)
  in
  iter 0

let read_int stream =
  if UCStream.next_char stream = 45 then (
    let _ = UCStream.remove stream 1 in
    let n, b = read_unsigned_int stream in
    (-n, b)
  ) else
    read_unsigned_int stream

let rec read_number stream =
  if UCStream.next_char stream = 45 then (
    let _ = UCStream.remove stream 1 in
    minus_num (read_number stream)
  ) else (
    let n, base = read_int stream in
    let rec read_fraction x r =
      let d = read_digit stream base in
      if d >= 0 then
        read_fraction (x +/ (r */ num_of_int d)) (r // num_of_int base)
      else
        x
    in
    match UCStream.next_char stream with
    | 44 | 46 ->
        let _ = UCStream.pop stream in
        num_of_int n +/ read_fraction num_zero (num_of_int 1 // num_of_int base)
    | _ -> num_of_int n)

(*
  |conv_dimen <number> <$c_1$> <$c_2$>| converts a number in units <c_1c_2> to either points, em, ex, or mu units.
  |read_skip| reads a fixed dimension (e.g., |-12pt|), |read_skip_with_order| additionally allows orders (e.g.,
  |3.4fill|), and |read_dim| reads an entire dimension (e.g., |-4.5cm plus 1fill minus 12pt|).
*)
let conv_dimen x c1 c2 =
  match (c1, c2) with
  | 98, 112 -> Some (Evaluate.const_pt (x */ (num_of_int 7227 // num_of_int 7200))) (* bp *)
  | 99, 99 -> Some (Evaluate.const_pt (x */ (num_of_int 14856 // num_of_int 1157))) (* cc *)
  | 99, 109 -> Some (Evaluate.const_pt (x */ (num_of_int 7227 // num_of_int 254))) (* cm *)
  | 100, 100 -> Some (Evaluate.const_pt (x */ (num_of_int 1238 // num_of_int 1157))) (* dd *)
  | 101, 109 -> Some (Evaluate.const_em x) (* em *)
  | 101, 120 -> Some (Evaluate.const_ex x) (* ex *)
  | 105, 110 -> Some (Evaluate.const_pt (x */ (num_of_int 7227 // num_of_int 100))) (* in *)
  | 109, 109 -> Some (Evaluate.const_pt (x */ (num_of_int 7227 // num_of_int 2540))) (* mm *)
  | 109, 117 -> Some (Evaluate.const_mu x) (* mu *)
  | 112, 99 -> Some (Evaluate.const_pt (x */ num_of_int 12)) (* pc *)
  | 112, 116 -> Some (Evaluate.const_pt x) (* pt *)
  | 115, 112 -> Some (Evaluate.const_pt (x // num_of_int 65536)) (* sp *)
  | _ -> None

let read_skip_or_number stream =
  let x = read_number stream in
  let c1 = UCStream.get_char stream 0 in
  let c2 = UCStream.get_char stream 1 in
  match conv_dimen x c1 c2 with
  | None -> `Number x
  | Some y ->
      UCStream.remove stream 2;
      `Skip y

let number_to_skip loc n =
  log_warn loc "Unit expected! Assuming points.";
  Evaluate.const_pt n

let read_skip stream =
  match read_skip_or_number stream with
  | `Skip s -> s
  | `Number x -> number_to_skip (UCStream.location stream) x

let read_skip_with_order stream =
  let x = read_number stream in
  let c1 = UCStream.get_char stream 0 in
  let c2 = UCStream.get_char stream 1 in
  if c1 = 102 && c2 = 105 then (
    (* fi... *)
    UCStream.remove stream 2;
    let rec iter ord =
      if UCStream.next_char stream = 108 then (
        (* l *)
        UCStream.remove stream 1;
        iter (ord + 1)
      ) else
        ((fun _ -> x), ord)
    in
    iter 0
  ) else
    match conv_dimen x c1 c2 with
    | None -> (number_to_skip (UCStream.location stream) x, 0)
    | Some y ->
        UCStream.remove stream 2;
        (y, 0)

let read_dim_or_number stream =
  let read_plus_minus key =
    let rec find_key i =
      if cat_code (UCStream.get_char stream i) = Space then
        find_key (i + 1)
      else if match_substring stream key i then
        let rec find_skip i =
          if cat_code (UCStream.get_char stream i) = Space then
            find_skip (i + 1)
          else (
            let _ = UCStream.remove stream i in
            read_skip_with_order stream)
        in
        find_skip (i + List.length key)
      else
        ((fun _ -> num_zero), 0)
    in
    find_key 0
  in
  match read_skip_or_number stream with
  | `Number x -> `Number x
  | `Skip base ->
      let st_f, st_o = read_plus_minus [112; 108; 117; 115] in
      let sh_f, sh_o = read_plus_minus [109; 105; 110; 117; 115] in
      `Dim
        (fun e ->
          {
            d_base = base e;
            d_stretch_factor = st_f e;
            d_stretch_order = st_o;
            d_shrink_factor = sh_f e;
            d_shrink_order = sh_o;
          })

let number_to_dim loc n =
  log_warn loc "Unit expected! Assuming points.";
  fun _ -> fixed_dim n

let read_dim stream =
  match read_dim_or_number stream with
  | `Dim d -> d
  | `Number x -> number_to_dim (UCStream.location stream) x

(*** parsing arithmetic expressions *****************************************************)

type 'a expr =
  | Atom of 'a
  | Scalar of num
  | Add of 'a expr * 'a expr
  | Sub of 'a expr * 'a expr
  | Mul of 'a expr * 'a expr
  | Div of 'a expr * 'a expr

let make_expression x = Atom x

let add_expr conv x y =
  match (x, y) with
  | Scalar s, Scalar t -> Scalar (s +/ t)
  | Scalar s, _ -> Add (Atom (conv s), y)
  | _, Scalar s -> Add (x, Atom (conv s))
  | _ -> Add (x, y)

let sub_expr conv x y =
  match (x, y) with
  | Scalar s, Scalar t -> Scalar (s -/ t)
  | Scalar s, _ -> Sub (Atom (conv s), y)
  | _, Scalar s -> Sub (x, Atom (conv s))
  | _ -> Sub (x, y)

let mul_expr loc x y =
  match (x, y) with
  | Scalar s, Scalar t -> Scalar (s */ t)
  | Scalar _, _ -> Mul (x, y)
  | _, Scalar _ -> Mul (y, x)
  | _ ->
      log_warn loc "undefined multiplication! Ignoring the second factor.";
      x

let div_expr loc x y =
  match (x, y) with
  | Scalar s, Scalar t -> Scalar (s // t)
  | _, Scalar _ -> Div (x, y)
  | _ ->
      log_warn loc "undefined division! Ignoring the second value.";
      x

let evaluate_expression expr conv atom add sub mul =
  let rec eval expr =
    match expr with
    | Atom x -> atom x
    | Scalar x -> atom (conv x)
    | Add (x, y) -> add (eval x) (eval y)
    | Sub (x, y) -> sub (eval x) (eval y)
    | Mul (x, y) -> (
        match x with
        | Scalar s -> mul s (eval y)
        | _ -> assert false)
    | Div (x, y) -> (
        match y with
        | Scalar s -> mul (num_one // s) (eval x)
        | _ -> assert false)
  in
  eval expr

(*
  |read_expression <read-atom> <conv> <stream>| reads an arithmetic expression from <stream> where atomic
  expressions are recognised by <read-atom>. <conv> is used in case of a type error to convert a number
  into the type in question.
*)

let rec read_expression read_atom conv stream =
  let rec read_summands result =
    skip_blanks stream;
    match UCStream.get_char stream 0 with
    | 43 ->
        UCStream.remove stream 1;
        let expr = read_summand read_atom conv stream in
        read_summands ((true, expr) :: result)
    | 45 ->
        UCStream.remove stream 1;
        let expr = read_summand read_atom conv stream in
        read_summands ((false, expr) :: result)
    | _ -> result
  in
  let loc = UCStream.location stream in
  let first = read_summand read_atom conv stream in
  let rest = read_summands [] in
  let rec iter first rest =
    match rest with
    | [] -> first
    | (true, e) :: es -> iter (add_expr (conv loc) first e) es
    | (false, e) :: es -> iter (sub_expr (conv loc) first e) es
  in
  iter first (List.rev rest)

and read_summand read_atom conv stream =
  let rec read_factors result =
    skip_blanks stream;
    match UCStream.get_char stream 0 with
    | 42 ->
        UCStream.remove stream 1;
        let expr = read_simple_expr read_atom conv stream in
        read_factors ((true, expr) :: result)
    | 47 ->
        UCStream.remove stream 1;
        let expr = read_simple_expr read_atom conv stream in
        read_factors ((false, expr) :: result)
    | _ -> result
  in
  let loc = UCStream.location stream in
  let first = read_simple_expr read_atom conv stream in
  let rest = read_factors [] in
  let rec iter first rest =
    match rest with
    | [] -> first
    | (true, e) :: es -> iter (mul_expr loc first e) es
    | (false, e) :: es -> iter (div_expr loc first e) es
  in
  iter first (List.rev rest)

and read_simple_expr read_atom conv stream =
  let read_paren brace =
    UCStream.remove stream 1;
    (* skip opening parenthesis *)
    let expr = read_expression read_atom conv stream in
    skip_blanks stream;
    if UCStream.get_char stream 0 = brace then
      UCStream.remove stream 1
    else (
      log_warn (UCStream.location stream) "missing ";
      log_uc_list
        [
          brace;
          UCStream.get_char stream 0;
          UCStream.get_char stream 1;
          UCStream.get_char stream 2;
          UCStream.get_char stream 3;
        ]);
    expr
  in
  skip_blanks stream;
  match UCStream.get_char stream 0 with
  | -1 -> raise (Failure "parse error")
  | 123 -> read_paren 125 (* { *)
  | 40 -> read_paren 41 (* ( *)
  | _ -> read_atom stream

let read_num_atom stream = Scalar (read_number stream)

let read_num_expression stream =
  match read_expression read_num_atom (fun _ x -> x) stream with
  | Scalar x -> x
  | _ -> assert false

let read_simple_num_expression stream =
  match read_simple_expr read_num_atom (fun _ x -> x) stream with
  | Scalar x -> x
  | _ -> assert false

let read_skip_atom stream =
  match read_skip_or_number stream with
  | `Skip s -> Atom s
  | `Number n -> Scalar n

let read_skip_expression stream =
  match read_expression read_skip_atom number_to_skip stream with
  | Scalar s -> number_to_skip (UCStream.location stream) s
  | expr ->
      fun env ->
        evaluate_expression expr
          (number_to_skip (UCStream.location stream))
          (fun s -> s env)
          add_num sub_num mult_num

let read_simple_skip_expression stream =
  match read_simple_expr read_skip_atom number_to_skip stream with
  | Scalar s -> number_to_skip (UCStream.location stream) s
  | expr ->
      fun env ->
        evaluate_expression expr
          (number_to_skip (UCStream.location stream))
          (fun s -> s env)
          add_num sub_num mult_num

let read_dim_atom stream =
  match read_dim_or_number stream with
  | `Dim d -> Atom d
  | `Number n -> Scalar n

let read_dim_expression stream =
  match read_expression read_dim_atom number_to_dim stream with
  | Scalar s -> fun _ -> fixed_dim s
  | expr ->
      fun env ->
        evaluate_expression expr
          (number_to_dim (UCStream.location stream))
          (fun d -> d env)
          dim_add dim_sub dim_mult

let read_simple_dim_expression stream =
  match read_simple_expr read_dim_atom number_to_dim stream with
  | Scalar s -> fun _ -> fixed_dim s
  | expr ->
      fun env ->
        evaluate_expression expr
          (number_to_dim (UCStream.location stream))
          (fun d -> d env)
          dim_add dim_sub dim_mult

(*** parsing lists and ranges ***********************************************************)

let read_range stream =
  skip_spaces stream;
  let n1 =
    if UCStream.next_char stream = 45 then (* - *)
      num_zero
    else
      read_simple_num_expression stream
  in
  skip_spaces stream;
  if UCStream.next_char stream = 45 then (
    (* - *)
    UCStream.remove stream 1;
    skip_spaces stream;
    let n2 =
      if UCStream.next_char stream = -1 then
        num_zero
      else
        read_simple_num_expression stream
    in
    skip_spaces stream;
    (n1, n2)
  ) else
    (n1, n1)

(*** conversion functions ***************************************************************)
let str_to_stream str =
  let stream = UCStream.of_list str in
  skip_spaces stream;
  stream

let str_to_bool str = read_true_false (str_to_stream str)
let str_to_uint str = fst (read_unsigned_int (str_to_stream str))
let str_to_int str = fst (read_int (str_to_stream str))
let str_to_num str = read_number (str_to_stream str)
let str_to_skip str = read_skip (str_to_stream str)
let str_to_dim str = read_dim (str_to_stream str)
let str_expr_to_num str = read_num_expression (str_to_stream str)
let str_expr_to_skip str = read_skip_expression (str_to_stream str)
let str_expr_to_dim str = read_dim_expression (str_to_stream str)
let str_to_list str = read_list (str_to_stream str)
let str_to_key_val str = read_key_val_list (str_to_stream str)
let str_expr_to_num_expr = str_expr_to_num
