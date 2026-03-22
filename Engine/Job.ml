
open Runtime
open Runtime.Logging

type output_format = DVI | XDVI | PDF | PS | SVG

type job = {
  time : Unix.tm;
  argv : string array;
  output_format : output_format;
  source_specials : bool;
  jobname : string;
  input_file : string;
  output_file : string;
  src_special_file : string;
  log_file : string;
  src_special_stream : Tools.IO.iorstream;
}

let empty = {
  time = Unix.localtime (Unix.time ());
  argv = Sys.argv;
  output_format = PDF;
  source_specials = false;
  jobname = "";
  input_file = "";
  output_file = "";
  src_special_file = "";
  log_file = "";
  src_special_stream = Tools.IO.make_buffer_stream 10;
}

let create name fmt src_spec =
  let basename =
    try String.sub name 0 (String.rindex name '.')
    with Not_found -> name
  in
  (* KPathSea.init Sys.argv.(0) !FontMetric.default_bitmap_resolution !FontMetric.default_mf_mode; *)
  (* FreeType.ft_init_freetype (); *)
  log_open (basename ^ ".log");
  {
    time = Unix.localtime (Unix.time ());
    argv = Sys.argv;
    output_format = fmt;
    source_specials = src_spec;
    jobname = basename;
    input_file = name;
    output_file = (match fmt with
                   | DVI  -> basename ^ ".dvi"
                   | XDVI -> basename ^ ".xdvi"
                   | PDF  -> basename ^ ".pdf"
                   | PS   -> basename ^ ".ps"
                   | SVG  -> basename ^ ".svg");
    src_special_file   = basename ^ ".pdfsync";
    log_file           = basename ^ ".log";
    src_special_stream = if fmt <> PDF || not src_spec then
                           Tools.IO.make_buffer_stream 10
                         else (
                            let os = Tools.IO.open_out (basename ^ ".pdfsync") in
                            Tools.IO.write_string os basename;
                            Tools.IO.write_string os "\nversion 0\n";
                            Tools.IO.coerce_o_ior os
                         );
  }
