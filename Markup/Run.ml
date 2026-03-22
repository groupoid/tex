
open Runtime
open Engine

(* hooks for loaded modules *)
let hook_parse_state = ref []
let hook_initialise  = ref []

let register_parse_state_hook h = 
  hook_parse_state := h :: !hook_parse_state

let register_init_hook h = 
  hook_initialise := h :: !hook_initialise

let call_parse_state_hooks ps = 
  List.iter (fun h -> h ps) !hook_parse_state

let call_init_hooks () = 
  List.iter (fun h -> h ()) !hook_initialise

(* routines to start ant *)

let initialise job = 
  Mode.init_source_specials job;
  ignore (Fonts.initialise_font_table ());
  call_init_hooks ()

let parse_document ps = 
  Primitives.initialise ps;
  call_parse_state_hooks ps;
  ParseState.run_parser ps `Preamble

let parse_file job name = 
  let ps = ParseState.create job in
  UCStream.include_file ps.ParseState.input_stream name;
  (parse_document ps, ps)

let parse_string job str = 
  let ps = ParseState.create job in
  UCStream.insert_string ps.ParseState.input_stream str;
  (parse_document ps, ps)