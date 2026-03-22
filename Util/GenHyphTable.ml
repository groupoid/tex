
open Runtime
open Unicode
open Types

(* parsing *)
let skip_comment stream =
  while UCStream.pop stream <> 10 do
    ()
  done

let rec skip_blanks stream =
  let c = UCStream.next_char stream in
  if 0 <= c && c <= 32 then begin
    ignore (UCStream.pop stream);
    skip_blanks stream
  end else if c = 37 then       (* % *)
    skip_comment stream
  else
    ()

let read_next_word stream =
  skip_blanks stream;
  let rec iter_internal () =
    let c = UCStream.pop stream in
    if c <= 32 || c = 37 then []
    else c :: iter_internal () in
  iter_internal ()

let parse_file stream =
  let section_header word = match (UString.to_string word) with
    | "substitute:" -> `Classes
    | "patterns:"   -> `Patterns
    | "exceptions:" -> `Exceptions
    | "language:"   -> `Language
    | _             -> `None in
  let classes    = Tools.ListBuilder.make () in
  let patterns   = Tools.ListBuilder.make () in
  let exceptions = Tools.ListBuilder.make () in
  let rec read_classes language =
    let w = read_next_word stream in
    if w = [] then
      (language, Tools.ListBuilder.get classes, Tools.ListBuilder.get patterns, Tools.ListBuilder.get exceptions)
    else
      match section_header w with
      | `Classes    -> read_classes    language
      | `Patterns   -> read_patterns   language
      | `Exceptions -> read_exceptions language
      | `Language   -> read_language   language
      | `None       -> Tools.ListBuilder.add classes w; read_classes language
  and read_patterns language =
    let w = read_next_word stream in
    if w = [] then
      (language, Tools.ListBuilder.get classes, Tools.ListBuilder.get patterns, Tools.ListBuilder.get exceptions)
    else
      match section_header w with
      | `Classes    -> read_classes    language
      | `Patterns   -> read_patterns   language
      | `Exceptions -> read_exceptions language
      | `Language   -> read_language   language
      | `None       -> Tools.ListBuilder.add patterns w; read_patterns language
  and read_exceptions language =
    let w = read_next_word stream in
    if w = [] then
      (language, Tools.ListBuilder.get classes, Tools.ListBuilder.get patterns, Tools.ListBuilder.get exceptions)
    else
      match section_header w with
      | `Classes    -> read_classes    language
      | `Patterns   -> read_patterns   language
      | `Exceptions -> read_exceptions language
      | `Language   -> read_language   language
      | `None       -> Tools.ListBuilder.add exceptions w; read_exceptions language
  and read_language _language =
    let w = read_next_word stream in
    if w = [] then
      (_language, Tools.ListBuilder.get classes, Tools.ListBuilder.get patterns, Tools.ListBuilder.get exceptions)
    else
      match section_header w with
      | `Classes    -> read_classes    _language
      | `Patterns   -> read_patterns   _language
      | `Exceptions -> read_exceptions _language
      | `Language   -> read_language   _language
      | `None       -> read_language   w in
  let rec find_section_internal () =
    match section_header (read_next_word stream) with
    | `Classes    -> read_classes    []
    | `Patterns   -> read_patterns   []
    | `Exceptions -> read_exceptions []
    | `Language   -> read_language   []
    | `None       -> find_section_internal () in
  find_section_internal ()

let empty_map = Charmap.create 0

let make_charmap classes =
  let map = Charmap.copy empty_map in
  let rec iter_internal n _classes = match _classes with
    | []      -> map
    | c::cs -> List.iter (fun x -> Charmap.set map x n) c; iter_internal (n+1) cs in
  iter_internal 1 classes

let parse_pattern class_table pattern =
  let rec iter_internal p ps ns = match p with
    | []      -> (List.rev ps, List.rev ns)
    | c::cs -> if c >= 48 && c <= 57 then iter_internal cs ps (c - 48 :: List.tl ns)
               else iter_internal cs (Charmap.lookup class_table c :: ps) (0 :: ns) in
  iter_internal pattern [] [0]

let force_break = 11
let force_none  = 10

let parse_exception class_table except =
  let rec iter_internal exc es hs = match exc with
    | []      -> (List.rev (0 :: es), List.rev (force_none :: hs))
    | c::cs -> if c = 45 then iter_internal cs es (force_break :: List.tl hs)
               else iter_internal cs (Charmap.lookup class_table c :: es) (force_none :: hs) in
  iter_internal except [0] [force_none; force_none]

let build_trie max_code entries =
  let rec merge_internal i e_list = match e_list with
    | []                      -> []
    | (_, _, _, p, e) :: es ->
        List.rev_map (fun (str, ns) -> (Array.of_list (i :: str), Array.of_list ns)) p
        @ List.rev_map (fun (str, ns) -> (Array.of_list (i :: str), Array.of_list ns)) e
        @ merge_internal (i + 1) es in
  Trie.build max_code (merge_internal 0 entries)

let build_table data =
  let parse_entry (l, c, p, e) =
    let cmap = make_charmap c in
    (List.length c, Array.of_list l, cmap, List.rev_map (parse_pattern cmap) p, List.rev_map (parse_exception cmap) e) in
  let entries  = List.rev_map parse_entry data in
  let max_code = List.fold_left (fun x (y,_,_,_,_) -> max x y) 0 entries in
  let trie = build_trie max_code entries in
  let rec iter_internal i e_list = match e_list with
    | []                      -> []
      | (_, l, c, _, _) :: xs ->
        (l, { Hyphenation.ht_char_classes  = c;
              Hyphenation.ht_pattern_trie  = trie;
              Hyphenation.ht_pattern_start = Runtime.Trie.get_offset trie i;
              Hyphenation.ht_left_hyphen_min = 2;
              Hyphenation.ht_right_hyphen_min = 2;
              Hyphenation.ht_exception_trie = Runtime.Trie.empty () }) :: iter_internal (i+1) xs in
  (trie, iter_internal 0 entries)

(* output routines *)
let print_list p l =
  print_string "[";
  let rec iter_internal l_in = match l_in with
    | []  -> print_string "]"
    | [x] -> p x; print_string "]"
    | x::xs -> p x; print_string "; "; iter_internal xs in
  iter_internal l

let print_array p newline a =
  let len = Array.length a in
  print_string "[|";
  if len > 1 then
    for i = 0 to len - 2 do
      p a.(i); if newline then print_string ";\n" else print_string "; "
    done;
  if len > 0 then p a.(len - 1);
  print_string "|]"

let dump_trie trie p =
  print_string "{ t_tree = ";
  print_array print_int false trie.Trie.t_tree;
  print_string "; t_data = ";
  print_array (print_array p false) true trie.Trie.t_data;
  print_string "; t_data_len = ";
  print_int trie.Trie.t_data_len;
  print_string "};\n"

let dump_charmap cmap =
  let is_empty cmap_in n =
    let rec iter_internal i =
      if i = 256 * (n + 1) then true
      else if Charmap.lookup cmap_in i <> 0 then false
      else iter_internal (i + 1) in
    iter_internal (256 * n) in
  print_string "Unicode.Charmap.build [|";
  for n = 0 to 255 do
    if is_empty cmap n then print_string "empty_map"
    else begin
      let a = Array.init 256 (fun i -> Charmap.lookup cmap (256 * n + i)) in
      print_array print_int false a
    end;
    if n < 255 then print_string ";\n" else print_string "\n"
  done;
  print_string "|]"

let dump_table (lang, ht) =
  print_string "(";
  print_array print_int false lang;
  print_string ",\n";
  print_string "{ ht_char_classes = ";
  dump_charmap ht.Hyphenation.ht_char_classes;
  print_string ";\n  ht_pattern_trie  = main_trie";
  print_string ";\n  ht_pattern_start = ";
  print_int ht.Hyphenation.ht_pattern_start;
  print_string ";\n  ht_left_hyphen_min = 2; ht_right_hyphen_min = 2; ht_exception_trie = Trie.empty ()\n})\n"

let output_table (trie, tables) =
  print_endline "open Runtime";
  print_endline "open Trie";
  print_endline "open Hyphenation";
  print_endline "let main_trie =";
  dump_trie trie print_int;
  print_endline "";
  print_endline "let empty_map =";
  print_array print_int false (Array.make 256 0);
  print_endline ";";
  print_endline "let tables =";
  print_list dump_table tables;
  print_endline ";"

let main () =
  UString.set_string_format `UTF8;
  let rec iter_internal i =
    if i >= Array.length Sys.argv then []
    else begin
      let stream = UCStream.of_file Sys.argv.(i) in
      let x = parse_file stream in
      x :: iter_internal (i + 1)
    end in
  output_table (build_table (iter_internal 1))

let () = main ()
