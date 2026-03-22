
open Tools.XNum

let split line =
  let tokens = Tools.ListBuilder.make () in
  let rec iter last pos =
    if pos >= String.length line then
      begin
        Tools.ListBuilder.add tokens (String.sub line last (pos - last));
        Tools.ListBuilder.get tokens
      end
    else if line.[pos] = ';' then
      begin
        Tools.ListBuilder.add tokens (String.sub line last (pos - last));
        iter (pos + 1) (pos + 1)
      end
    else iter last (pos + 1)
  in
  iter 0 0

let parse_num str =
  if str = "" then (-1, -1)
  else
    try
      let i = String.index str '/' in
      (int_of_string (String.sub str 0 i),
       int_of_string (String.sub str (i + 1) (String.length str - i - 1)))
    with Not_found -> (int_of_string str, 1)

let read_table () =
  let read () = try read_line () with End_of_file -> "" in
  let int_of_hex str = Scanf.sscanf str "%x" (fun x -> x) in
  let data = Tools.ListBuilder.make () in
  let rec iter () =
    let line = read () in
    if line = "" then Tools.ListBuilder.get data
    else
      match split line with
      | [code; name; cat; comb; bidi; decomp; num1; num2; num3; mirrored; _;
         _comment; uc; lc; tc] ->
          let (n3, n4) = parse_num num3 in
          Tools.ListBuilder.add data
            (int_of_hex code, name, cat, int_of_string comb, bidi, decomp,
             ((if num1 = "" then -1 else int_of_string num1),
              (if num2 = "" then -1 else int_of_string num2), n3, n4),
             (if mirrored = "Y" then true else false), _comment,
             ((if uc = "" then -1 else int_of_hex uc),
              (if lc = "" then -1 else int_of_hex lc),
              (if tc = "" then -1 else int_of_hex tc)));
          iter ()
      | _ -> invalid_arg "wrong file format"
  in
  iter ()

let print_tables name ctype print_val is_default default table =
  let rec iter n main_table table =
    if n >= 0x1100 then
      begin
        print_string "static ";
        print_string ctype;
        print_string " ";
        print_string name;
        print_string "_xxxx[256] = {\n";
        for _ = 0 to 0xfe do print_string default; print_string ",\n" done;
        print_string default;
        print_string "};\n";
        print_string "static ";
        print_string ctype;
        print_string "* ";
        print_string name;
        print_string "_table[] = {";
        print_string (String.sub main_table 0 (String.length main_table - 1));
        print_string "};\n"
      end
    else
      let (defs, rest) =
        List.partition
          (fun (code, _, _, _, _, _, _, _, _, _) -> code < (n + 1) * 0x100)
          table
      in
      if List.for_all is_default defs then
        iter (n + 1) (main_table ^ " " ^ name ^ "_xxxx,") rest
      else
        begin
          Printf.printf "static %s %s_%04x[256] =\n{\n" ctype name n;
          let rec print_entries i defs =
            if i >= 0x100 then
              begin
                print_endline "};";
                iter (n + 1) (main_table ^ Printf.sprintf " %s_%04x," name n)
                  rest
              end
            else
              match defs with
              | [] ->
                  print_string default;
                  if i < 0xff then print_string ",\n";
                  print_entries (i + 1) defs
              | (c, _, _, _, _, _, _, _, _, _ as d) :: ds ->
                  if c = 0x100 * n + i then
                    begin
                      print_val d;
                      if i < 0xff then print_string ",\n";
                      print_entries (i + 1) ds
                    end
                  else
                    begin
                      print_string default;
                      if i < 0xff then print_string ",\n";
                      print_entries (i + 1) defs
                    end
          in
          print_entries 0 defs
        end
  in
  iter 0 "" table

let compile_tables () =
  let table = read_table () in
  print_string "static const char name_reserved[] = \"<reserved>\"\n";
  print_tables "name" "const char *"
    (fun (_, name, _, _, _, _, _, _, _, _) ->
       print_string ("\"" ^ name ^ "\""))
    (fun (_, name, _, _, _, _, _, _, _, _) -> name = "<reserved>")
    "name_reserved" table;
  print_tables "category" "unsigned char"
    (fun (_, _, cat, _, _, _, _, _, _, _) -> print_string cat)
    (fun (_, _, cat, _, _, _, _, _, _, _) -> cat = "Cn") "Cn" table;
  print_tables "combining_class" "unsigned char"
    (fun (_, _, _, comb, _, _, _, _, _, _) -> print_int comb)
    (fun (_, _, _, comb, _, _, _, _, _, _) -> comb = 0) "0" table;
  print_tables "bidi" "unsigned char"
    (fun (_, _, _, _, bidi, _, _, _, _, _) -> print_string bidi)
    (fun (_, _, _, _, bidi, _, _, _, _, _) -> bidi = "L") "L" table;
  print_tables "decomposition" "const char *"
    (fun (_, _, _, _, _, decomp, _, _, _, _) ->
       print_string ("\"" ^ decomp ^ "\""))
    (fun (_, _, _, _, _, decomp, _, _, _, _) -> String.length decomp = 0)
    "\"\"" table;
  print_tables "number_1" "signed char"
    (fun (_, _, _, _, _, _, (n, _, _, _), _, _, _) -> Printf.printf "%d" n)
    (fun (_, _, _, _, _, _, (n, _, _, _), _, _, _) -> n = -1) "-1" table;
  print_tables "number_2" "signed char"
    (fun (_, _, _, _, _, _, (_, n, _, _), _, _, _) -> Printf.printf "%d" n)
    (fun (_, _, _, _, _, _, (_, n, _, _), _, _, _) -> n = -1) "-1" table;
  print_tables "number_3" "int"
    (fun (_, _, _, _, _, _, (_, _, n, _), _, _, _) -> Printf.printf "%d" n)
    (fun (_, _, _, _, _, _, (_, _, n, _), _, _, _) -> n = -1) "-1" table;
  print_tables "number_4" "signed char"
    (fun (_, _, _, _, _, _, (_, _, _, n), _, _, _) -> Printf.printf "%d" n)
    (fun (_, _, _, _, _, _, (_, _, _, n), _, _, _) -> n = -1) "-1" table;
  print_tables "mirrored" "unsigned char"
    (fun (_, _, _, _, _, _, _, mirrored, _, _) ->
       if mirrored then print_string "1" else print_string "0")
    (fun (_, _, _, _, _, _, _, mirrored, _, _) -> mirrored = false) "0" table;
  print_tables "comment" "const char *"
    (fun (_, _, _, _, _, _, _, _, comment, _) ->
       print_string ("\"" ^ comment ^ "\""))
    (fun (_, _, _, _, _, _, _, _, comment, _) -> String.length comment = 0)
    "\"\"" table;
  print_tables "uppercase" "int"
    (fun (_, _, _, _, _, _, _, _, _, (uc, _, _)) -> Printf.printf "%d" uc)
    (fun (_, _, _, _, _, _, _, _, _, (uc, _, _)) -> uc = -1) "-1" table;
  print_tables "lowercase" "int"
    (fun (_, _, _, _, _, _, _, _, _, (_, lc, _)) -> Printf.printf "%d" lc)
    (fun (_, _, _, _, _, _, _, _, _, (_, lc, _)) -> lc = -1) "-1" table;
  print_tables "titlecase" "int"
    (fun (_, _, _, _, _, _, _, _, _, (_, _, tc)) -> Printf.printf "%d" tc)
    (fun (_, _, _, _, _, _, _, _, _, (_, _, tc)) -> tc = -1) "-1" table

let () = compile_tables ()
