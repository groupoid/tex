
open XNum
open Runtime
open Unicode
open Types
open Logging
(* open Typesetting *)
open Engine

let tracing_stacks = ref false
let tracing_macros = ref false
let tracing_input  = ref false

type mode = [ 
|  `Preamble
|  `Galley
|  `Paragraph
|  `Math
|  `HBox
|  `LRBox
|  `RLBox
|  `VBox
|  `Table
]


type parse_state =
{
  job               : Job.job;
  input_stream      : UCStream.istream;
  parse_stack       : (mode * Node.node Tools.ListBuilder.builder) Stack.t;
  mutable default_char_cmd  : command;
  mutable command_table     : command Unicode.DynUCTrie.t;
  mutable pattern_table     : command Unicode.DynUCTrie.t;
  mutable saved_commands    : command list Unicode.DynUCTrie.t;
  mutable saved_patterns    : command list Unicode.DynUCTrie.t;
  mutable environment_table : (command * command) Unicode.DynUCTrie.t;
  environment_stack : (uc_list * uc_list list) Stack.t;
  mutable math_codes        : (Box.math_code * (int * int) * (uc_char * uc_char)) Charmap.charmap;
  mutable al_scope          : Machine.scope;
  mutable global_variables  : Types.partial_value SymbolTable.SymbolMap.t;
  mutable counter_table     : Counter.counter_table;
  mutable old_references    : uc_string Unicode.DynUCTrie.t;
  mutable references        : uc_string Unicode.DynUCTrie.t
}
and command =
{
  execute : parse_state -> unit;
  expand  : parse_state -> uc_list -> uc_list
}

let mode_to_string mode = match mode with
| `Preamble  -> "preamble"
| `Galley    -> "galley"
| `Paragraph -> "paragraph"
| `Math      -> "math"
| `HBox      -> "hbox"
| `LRBox     -> "lrbox"
| `RLBox     -> "rlbox"
| `VBox      -> "vbox"
| `Table     -> "table"

(* |create ()| creates an empty parse state. *)

let create job =
{
  job               = job;
  input_stream      = UCStream.create ();
  parse_stack       = Stack.create ();
  default_char_cmd  = { execute = (fun _ -> ()); expand = (fun _ _ -> []) };
  command_table     = Unicode.DynUCTrie.empty;
  pattern_table     = Unicode.DynUCTrie.empty;
  saved_commands    = Unicode.DynUCTrie.empty;
  saved_patterns    = Unicode.DynUCTrie.empty;
  environment_table = Unicode.DynUCTrie.empty;
  environment_stack = Stack.create ();
  math_codes        = Charmap.create (MathTypes.NoMath, (0, 0), (0, 0));
  al_scope          = Machine.make_scope ();
  global_variables  = SymbolTable.SymbolMap.empty;
  counter_table     = Counter.empty_table;
  old_references    = Unicode.DynUCTrie.empty;
  references        = Unicode.DynUCTrie.empty
}

(*
  |duplicate <parse-state>| creates a new parse-state where the definitions of commands, environments,
  and counters are copied from <parse-state>.
*)
let duplicate ps =
{
  job               = ps.job;
  input_stream      = UCStream.create ();
  parse_stack       = Stack.create ();
  default_char_cmd  = ps.default_char_cmd;
  command_table     = ps.command_table;
  pattern_table     = ps.pattern_table;
  saved_commands    = ps.saved_commands;
  saved_patterns    = ps.saved_patterns;
  environment_table = ps.environment_table;
  environment_stack = Stack.create ();
  math_codes        = Charmap.copy ps.math_codes;
  al_scope          = ps.al_scope;
  global_variables  = ps.global_variables;
  counter_table     = ps.counter_table;
  references        = ps.references;
  old_references    = ps.old_references
}

(* |set_stream <ps> <stream>| replaces the input-stream of <ps>. *)
let set_stream ps stream =
  UCStream.assign ps.input_stream stream

let location ps =
  UCStream.location ps.input_stream

(*
  The parse stack contains a list of partially constructed lists of nodes. Each such list has an
  associated mode.

  |open_node_list <parse-state> <mode>| puts an empty node-list with the respective mode on the parse-stack.
  |close_node_list <parse-state> <mode>| removes the first element of the parse-stack.
  |add_node <parse-state> <node>| adds a node to the first element of the stack.
*)

let open_node_list ps mode =
  if !tracing_stacks then
    log_string ("\n#S: mode (" ^ mode_to_string mode)
  else ();
  Stack.push (mode, Tools.ListBuilder.make ()) ps.parse_stack

let close_node_list ps mode =
  try
    let (m,n) = Stack.top ps.parse_stack in
    if m = mode then (
      if !tracing_stacks then
        log_string ("\n#S: mode " ^ mode_to_string mode ^ ")");
      ignore (Stack.pop ps.parse_stack);
      Tools.ListBuilder.get n
    ) else (
      log_warn (location ps)
        ("Mode " ^ mode_to_string mode ^ " expected in " ^ mode_to_string m ^ " mode!");
      []
    )
  with Stack.Empty ->
    log_warn (location ps) "There is no node open!";
    []

let add_node ps node =
  try
    let (_,n) = Stack.top ps.parse_stack in
    Tools.ListBuilder.add n node
  with Stack.Empty ->
    log_warn (location ps) "There is no node open!"

let current_mode ps =
  try
    let (m,_) = Stack.top ps.parse_stack in
    m
  with Stack.Empty -> `Preamble

(* Character commands *)

(* To each character is associated a command which is invoked everytime the character is read. *)

let set_default_char_cmd ps cmd =
  ps.default_char_cmd <- cmd

(* commands *)

(*
  |define_command <parse-state> <name> <cmd>| binds the function <cmd> to the name <name>.
  |lookup_command <parse-state> <name>| looks up the definition of <name>.
*)
let define_command ps name cmd =
  ps.command_table <- Unicode.DynUCTrie.add_list name cmd ps.command_table

let define_pattern ps name cmd =
  ps.pattern_table <- Unicode.DynUCTrie.add_list name cmd ps.pattern_table

let lookup_command ps name =
  Unicode.DynUCTrie.lookup_list name ps.command_table

let lookup_pattern_prefix ps =
  Unicode.DynUCTrie.lookup_prefix_stream ps.input_stream ps.pattern_table

let save_command ps name =
  match Unicode.DynUCTrie.lookup_list name ps.command_table with
  | None ->
      ps.saved_commands <- Unicode.DynUCTrie.add_list name [] ps.saved_commands
  | Some cmd ->
      let old_list = match Unicode.DynUCTrie.lookup_list name ps.saved_commands with
      | None   -> []
      | Some l -> l
      in
      ps.saved_commands <- Unicode.DynUCTrie.add_list name (cmd :: old_list) ps.saved_commands

let restore_command ps name =
  match Unicode.DynUCTrie.lookup_list name ps.saved_commands with
  | None ->
      log_warn (location ps) "Command ";
      log_uc_list name;
      log_string " undefined!"
  | Some [] ->
      ps.command_table  <- Unicode.DynUCTrie.remove_list name ps.command_table;
      ps.saved_commands <- Unicode.DynUCTrie.remove_list name ps.saved_commands
  | Some (c::cs) ->
      ps.command_table  <- Unicode.DynUCTrie.add_list name c  ps.command_table;
      ps.saved_commands <- Unicode.DynUCTrie.add_list name cs ps.saved_commands

let save_pattern ps name =
  match Unicode.DynUCTrie.lookup_list name ps.pattern_table with
  | None ->
      ps.saved_patterns <- Unicode.DynUCTrie.add_list name [] ps.saved_patterns
  | Some pat ->
      let old_list = match Unicode.DynUCTrie.lookup_list name ps.saved_patterns with
      | None   -> []
      | Some l -> l
      in
      ps.saved_patterns <- Unicode.DynUCTrie.add_list name (pat :: old_list) ps.saved_patterns

let restore_pattern ps name =
  match Unicode.DynUCTrie.lookup_list name ps.saved_patterns with
  | None ->
      log_warn (location ps) "Pattern ";
      log_uc_list name;
      log_string " undefined!"
  | Some [] ->
      ps.pattern_table  <- Unicode.DynUCTrie.remove_list name ps.pattern_table;
      ps.saved_patterns <- Unicode.DynUCTrie.remove_list name ps.saved_patterns
  | Some (c::cs) ->
      ps.pattern_table  <- Unicode.DynUCTrie.add_list name c  ps.pattern_table;
      ps.saved_patterns <- Unicode.DynUCTrie.add_list name cs ps.saved_patterns

(* environemnts *)
let push_env ps name args =
  if !tracing_stacks then (
    log_string "\n#S: env (";
    log_uc_list name
  ) else ();
  Stack.push (name, args) ps.environment_stack

let pop_env ps =
  try
    let (name, args) = Stack.pop ps.environment_stack in
    if !tracing_stacks then (
      log_string "\n#S: env ";
      log_uc_list name;
      log_string ")"
    ) else ();
    (name, args)
  with Stack.Empty ->
    log_error (location ps) "there is no environment open!";
    ([], [])

let set_env_args ps args =
  try
    let (name, _) = Stack.pop ps.environment_stack in
    Stack.push (name, args) ps.environment_stack
  with Stack.Empty ->
    log_error (location ps) "there is no environment open!"

let top_env ps =
  try
    Stack.top ps.environment_stack
  with Stack.Empty ->
    log_error (location ps) "there is no environment open!";
    ([], [])

let lookup_env ps name =
  try
    Some (Unicode.DynUCTrie.find_list name ps.environment_table)
  with Not_found -> None

(* |define_env <name> <begin-cmd> <end-cmd> defines the environment <name>. *)

let define_env ps name begin_cmd end_cmd =
  ps.environment_table <- Unicode.DynUCTrie.add_list name (begin_cmd, end_cmd) ps.environment_table

(* math codes *)

let set_math_code_table ps table =
  ps.math_codes <- table

let set_math_code ps char code small_family small_glyph large_family large_glyph =
  Charmap.set
    ps.math_codes
    char
    (code, (small_family, large_family), (small_glyph, large_glyph))

let get_math_code ps char =
  match Charmap.lookup ps.math_codes char with
  | (Runtime.MathTypes.NoMath, _, _) ->
      (Runtime.MathTypes.NoMath, (1, 1), (char, char))  (* |NoMath| is the same as |Ordinary| but it indicates *)
                                          (* that |char| is a character, not a glyph number.     *)
  | code -> code

(* counters *)

let new_counter ps name val_ super =
  ps.counter_table <-
    Counter.new_counter
      (location ps)
      ps.counter_table
      name
      val_
      super

let get_counter ps name =
  Counter.get_counter
    (location ps)
    ps.counter_table
    name

let set_counter ps name val_ =
  ps.counter_table <-
    Counter.set_counter
      (location ps)
      ps.counter_table
      name
      val_

(* unique names *)

let unique_name_counter = ref 0

let gen_unique_name () =
  incr unique_name_counter;
  UString.of_ascii " $uid " @ Format.num_to_arabic 10 (num_of_int !unique_name_counter)

(* references *)
let add_reference ps name str =
  ps.references <- Unicode.DynUCTrie.add_list name str ps.references

let reference_exists ps name =
  Unicode.DynUCTrie.mem_list name ps.references

let lookup_reference ps name =
  try
    let ref_ = Unicode.DynUCTrie.find_list name ps.references in
    ref_
  with Not_found ->
    try
      let ref_ = Unicode.DynUCTrie.find_list name ps.old_references in
      ref_
    with Not_found ->
      log_warn (location ps) "Unknown reference `";
      log_uc_list name;
      log_string "'!";
      [||]

let iter_references ps f =
  Unicode.DynUCTrie.iter f ps.references

let store_old_references ps =
  ps.old_references <- ps.references

let compare_references ps =
  (* compare old and new value *)
  let cmp name str differ =
    differ || (try Unicode.DynUCTrie.find_string name ps.old_references <> str with Not_found -> true)
  in
  (* where any references deleted? *)
  let find name _ differ =
    differ || (not (Unicode.DynUCTrie.mem_string name ps.references))
  in
  Unicode.DynUCTrie.fold cmp  ps.references     false
  || Unicode.DynUCTrie.fold find ps.old_references false

let write_references ps name =
  let oc = open_out_bin name in
  output_string oc "\\ALcommand{do\n";
  let print_ref name str =
    output_string oc "  ps_add_reference \"";
    output_string oc (UString.to_string (Array.to_list name));
    output_string oc "\" \"";
    output_string oc (UString.to_string (Array.to_list str));
    output_string oc "\";\n"
  in
  iter_references ps print_ref;
  output_string oc "  ps_store_old_references;\nend}\n";
  close_out oc

(* main loop of the parser *)

(*
  |execute_next_char <parse_state>| reads the next input character and executes the corresponding
  character-command.
*)
let execute_next_char ps =
  if !tracing_input then (
    log_string "\n#I: ";
    log_uc_list (UCStream.take ps.input_stream 5)
  ) else ();

  (* First, check whether the input starts with a defined pattern. *)
  match lookup_pattern_prefix ps with
  | Some cmd ->
      cmd.execute ps;
      true
  | None ->
      (* Otherwise, execute the default character command. *)
      if UCStream.eof ps.input_stream then
        false
      else
        let cmd = ps.default_char_cmd in
        cmd.execute ps;
        true

(*
  |run_parser <parse-state> <mode>| calls |execute_next_char| unit the input stream is empty.
*)
let run_parser ps mode =
  open_node_list ps mode;
  while execute_next_char ps do
    ()
  done;
  close_node_list ps mode

(* |execute_stream <parse-state> <stream>| runs the parser on the contents of <stream>. *)
let execute_stream ps stream =
  UCStream.exchange ps.input_stream stream;
  while execute_next_char ps do
    ()
  done;
  UCStream.exchange ps.input_stream stream

(*
  |execute_argument <parse_state> <mode>| reads the next token or group, and executes it.
*)
let execute_argument ps =
  let rec execute_group nest =
    let c = UCStream.next_char ps.input_stream in
    if c < 0 then
      ()
    else match CharCode.cat_code c with
    | CharCode.BeginGroup ->
        ignore (execute_next_char ps);
        execute_group (nest + 1)
    | CharCode.EndGroup ->
        if nest = 0 then
          ()
        else (
          ignore (execute_next_char ps);
          execute_group (nest - 1)
        )
    | _ ->
        ignore (execute_next_char ps);
        execute_group nest
  in
  if UCStream.next_char ps.input_stream = CharCode.begin_group_char then (
    UCStream.remove ps.input_stream 1;
    execute_group 0;
    UCStream.remove ps.input_stream 1;
  ) else
    ignore (execute_next_char ps)

(*
  |execute_argument_in_mode <parse_state> <mode>| opens a new node of mode <mode> and calls
  |execute_argument|.
*)
let execute_argument_in_mode ps mode =
  open_node_list ps mode;
  execute_argument ps;
  close_node_list ps mode

let execute_string_in_mode ps str mode =
  set_stream ps (UCStream.of_list str);
  run_parser ps mode