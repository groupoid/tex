
open Runtime
open Logging
open Engine

module Job = Engine.Job

let print_help () =
  print_string "This is tex, version 0.3.21.\n\n";
  print_string "USAGE: tex [options] <input-file>\n\n";
  print_string "Supported options are:\n\n";
  print_string "--format=<fmt>    where <fmt> is \"dvi\", \"xdvi\", \"ps\", \"pdf\", or \"svg\"\n";
  print_string "--src-specials    enables the generation of source specials\n";
  print_string "--help            print this message and exists\n"

let rec process_options ((fmt, src_spec, file) as opt) args =
  match args with
  | [] ->
      (match file with
       | None ->
           print_string "No input file specified!\n\n";
           print_help ();
           None
       | Some f -> Some (fmt, src_spec, f))
  | arg :: args ->
      let len = String.length arg in
      if len = 0 then
        process_options opt args
      else if arg.[0] = '-' && len > 1 then
        let v = if arg.[1] = '-' then
                  String.sub arg 2 (len - 2)
                else
                  String.sub arg 1 (len - 1) in
        let len = String.length v in
        if v = "help" then
          (print_help ();
           None)
        else if len >= 6 && String.sub v 0 6 = "format" then
          if len = 6 || v.[6] <> '=' then
            (print_string "Unknown option!\n\n";
             print_help ();
             None)
          else
            (match String.lowercase_ascii (String.sub v 7 (len - 7)) with
             | "dvi"  -> process_options (Job.DVI, src_spec, file) args
             | "xdvi" -> process_options (Job.XDVI, src_spec, file) args
             | "pdf"  -> process_options (Job.PDF, src_spec, file) args
             | "ps"   -> process_options (Job.PS, src_spec, file) args
             | "svg"  -> process_options (Job.SVG, src_spec, file) args
             | _      ->
                 print_string ("File format `" ^ v ^ "' not supported!\n\n");
                 print_help ();
                 None)
        else if len >= 12 && String.sub v 0 12 = "src-specials" then
          process_options (fmt, true, file) args
        else
          (print_string "Unknown option!\n\n";
           print_help ();
           None)
      else
        (match file with
         | None   -> process_options (fmt, src_spec, Some arg) args
         | Some _ ->
             print_string "More than one input file given!\n\n";
             print_help ();
             None)

let main () =
  match process_options (Job.PDF, false, None) (List.tl (Array.to_list Sys.argv)) with
  | None -> ()
  | Some (fmt, src_spec, file) ->
      (try
         let _ = Unix.stat file in
         let job = Job.create file fmt src_spec in
         Markup.Run.initialise job;
         let (nodes, ps) = Markup.Run.parse_file job job.Job.input_file in
         let pages = Engine.Evaluate.evaluate nodes in
         Markup.ALParseState.call_at_exit ps;
         Engine.Output.output_pages job pages
       with
       | Unix.Unix_error (err, _, _) ->
           log_string Sys.argv.(0);
           log_string ": ";
           log_string (Unix.error_message err);
           log_string "\n";
           exit 1)

let () =
  try
    main ()
  with
  | e ->
      print_string ("uncaught exception: " ^ (Printexc.to_string e) ^ "!\n");
      flush stdout;
      raise e
