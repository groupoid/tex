
open UTypes

(* locations *)

type location = string * int * int

type internal_location = {
  filename : string;
  mutable line : int;
  mutable column : int;
  fixed : bool;
}

let create_location filename fixed = {
  filename = filename;
  line = 0;
  column = 0;
  fixed = fixed;
}

let make_internal_location (name, line, col) fixed = {
  filename = name;
  line = line;
  column = col;
  fixed = fixed;
}

let duplicate_location loc = {
  filename = loc.filename;
  line = loc.line;
  column = loc.column;
  fixed = loc.fixed;
}

let fix_location loc =
  if loc.fixed then loc
  else { loc with fixed = true }

let inc_line loc =
  if not loc.fixed then (
    loc.line <- loc.line + 1;
    loc.column <- 0
  )

let inc_column loc =
  if not loc.fixed then
    loc.column <- loc.column + 1

let update_location loc char =
  if char = 10 then inc_line loc
  else inc_column loc

(* streams *)

type simple_stream = {
  mutable chars : uc_list;
  loc   : internal_location;
}

let duplicate_simple_stream str = {
  chars = str.chars;
  loc = duplicate_location str.loc;
}

type istream = {
  mutable current : simple_stream;
  mutable stack   : simple_stream list;
}

let create () = {
  stack = [];
  current = {
    chars = [];
    loc = create_location "" true;
  };
}

let of_list s = {
  stack = [];
  current = {
    chars = s;
    loc = create_location "" true;
  };
}

let to_list is =
  List.concat (is.current.chars :: List.map (fun s -> s.chars) is.stack)

let of_string s  = of_list (Array.to_list s)
let to_string is = Array.of_list (to_list is)

let assign is1 is2 =
  is1.current <- duplicate_simple_stream is2.current;
  is1.stack   <- List.map duplicate_simple_stream is2.stack

let exchange is1 is2 =
  let temp = {
    current = is1.current;
    stack   = is1.stack;
  } in
  assign is1 is2;
  assign is2 temp

let duplicate is = {
  current = duplicate_simple_stream is.current;
  stack   = List.map duplicate_simple_stream is.stack;
}

let location is = (is.current.loc.filename, is.current.loc.line, is.current.loc.column)

let set_location is loc fixed =
  is.current <- { is.current with loc = make_internal_location loc fixed }

let get_char is pos =
  let rec iter pos chars stack =
    match chars with
    | c :: cs ->
        if pos = 0 then c
        else iter (pos - 1) cs stack
    | [] ->
        match stack with
        | [] -> -1
        | s :: ss -> iter pos s.chars ss
  in
  iter pos is.current.chars is.stack

let eof is = (get_char is 0 < 0)

let rec next_char is =
  match is.current.chars with
  | c :: _ -> c
  | [] ->
      match is.stack with
      | [] -> -1
      | s :: ss ->
          is.current <- s;
          is.stack <- ss;
          next_char is

let take is num =
  let rec iter n chars stack =
    if n <= 0 then []
    else match chars with
    | c :: cs -> c :: iter (n-1) cs stack
    | [] ->
        match stack with
        | s :: ss -> iter n s.chars ss
        | [] -> []
  in
  iter num is.current.chars is.stack

let rec pop is =
  match is.current.chars with
  | c :: cs ->
      is.current.chars <- cs;
      update_location is.current.loc c;
      c
  | [] ->
      match is.stack with
      | [] -> -1
      | s :: ss ->
          is.current <- s;
          is.stack <- ss;
          pop is

let remove is num =
  let rec iter n =
    if n <= 0 || pop is < 0 then ()
    else iter (n-1)
  in
  iter num

let clear is =
  let rec iter last stack =
    match stack with
    | s :: ss -> iter s ss
    | [] ->
        is.current <- last;
        is.stack <- [];
        while next_char is >= 0 do () done
  in
  iter is.current is.stack

let match_prefix is str =
  let rec iter str chars stack =
    match str with
    | [] -> true
    | c :: cs ->
        match chars with
        | x :: xs ->
            if c = x then iter cs xs stack
            else false
        | [] ->
            match stack with
            | [] -> false
            | s :: ss -> iter str s.chars ss
  in
  iter str is.current.chars is.stack

(* creating streams *)

let insert_list is str =
  is.stack <- is.current :: is.stack;
  is.current <- {
    chars = str;
    loc = fix_location is.current.loc;
  }

let insert_string is str = insert_list is (Array.to_list str)

let insert_stream is stream =
  if stream.current.loc.filename = "" then (
    let loc = fix_location is.current.loc in
    is.stack <- List.map (fun s -> { s with loc = loc }) stream.stack @ (is.current :: is.stack);
    is.current <- { stream.current with loc = loc }
  ) else (
    is.stack <- stream.stack @ (is.current :: is.stack);
    is.current <- stream.current
  )

let include_file is name =
  let buf = Tools.ListBuilder.make () in
  try
    let s = Tools.IO.open_in name in
    is.stack <- is.current :: is.stack;
    let rec iter_stream s =
      if Tools.IO.eof s then Tools.ListBuilder.get buf
      else (
        let c = Tools.IO.read_utf8_char s in
        if c >= 0 then Tools.ListBuilder.add buf c;
        iter_stream s
      )
    in
    is.current <- {
      chars = iter_stream s;
      loc = create_location name false;
    };
    Tools.IO.free s
  with _ -> ()

let of_file name =
  let stream = create () in
  include_file stream name;
  stream