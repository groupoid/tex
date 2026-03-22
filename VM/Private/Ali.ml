
(* open Tools.XNum *)

open UTypes
open Unicode
open Types
(* open Unicode.UTypes *)
(* open Unicode.SymbolTable *)

module UString = Unicode.UString

let main () = 
  UString.set_string_format `UTF8;

  let debug = Array.fold_left (fun d arg -> d || arg = "--debug") false Sys.argv in
  if debug then
    Evaluate.tracing_bytecode := true
  else ();

  print_string "This is ali, version 0.1.\nIf you need help, please type \"quit\".\n";
  flush stdout;

  try 
    let scope = Primitives.initial_scope () in
    for i = 1 to Array.length Sys.argv - 1 do
      if Sys.argv.(i) <> "--debug" then begin
        print_string "Loading ";
        print_string Sys.argv.(i);
        print_string "...";
        flush stdout;

        if debug then begin
          let code = Compile.compile_declarations scope (Unicode.UCStream.of_file Sys.argv.(i)) in
          Evaluate.print_bytecode 3 code;
          flush stdout;
          ignore (Evaluate.execute code [ref Unbound])
        end else
          Machine.execute_declarations scope (Unicode.UCStream.of_file Sys.argv.(i));

        print_string " done\n"
      end
    done;

    let rec iter () = 
      print_string "> ";
      flush stdout;
      let expr = read_line () in
      if expr = "quit" then
        ()
      else begin
        (try 
           let result =
             if debug then begin
               let code = Compile.compile_expression scope (Unicode.UCStream.of_list (UString.of_string expr)) in
               Evaluate.print_bytecode 3 code;
               flush stdout;
               let x = Evaluate.execute code [] in
               !x
             end else
               Machine.evaluate_expression scope (Unicode.UCStream.of_list (UString.of_string expr)) 
           in
           Evaluate.print_partial 5 result;
           print_newline ()
         with
         | Syntax_error ((f, l, c), err) ->
             Printf.printf "\n%s:%d:%d syntax error: %s\n" f l c (UString.to_string (Array.to_list err))
         | Runtime_error err -> 
             Printf.printf "\nruntime error: %s\n" (UString.to_string (Array.to_list err)));
        iter ()
      end
    in iter ()
  with
  | Syntax_error ((f, l, c), err) ->
      Printf.printf "\n%s:%d:%d syntax error: %s\n" f l c (UString.to_string (Array.to_list err))
  | Runtime_error err -> 
      Printf.printf "\nruntime error: %s\n" (UString.to_string (Array.to_list err));

  flush stderr

let () = main ()
