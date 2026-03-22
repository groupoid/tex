
open Runtime
open Unicode
open Unicode.UTypes
open Types
(* open Typesetting *)

val tracing_stacks : bool ref
val tracing_macros : bool ref
val tracing_input : bool ref

type mode = [ 
  | `Preamble
  | `Galley
  | `Paragraph
  | `Math
  | `HBox
  | `LRBox
  | `RLBox
  | `VBox
  | `Table 
]

type parse_state =
  { job : Job.job;
    input_stream : UCStream.istream;
    parse_stack : (mode * Node.node Tools.ListBuilder.builder) Stack.t;
    mutable default_char_cmd : command;
    mutable command_table : command Unicode.DynUCTrie.t;
    mutable pattern_table : command Unicode.DynUCTrie.t;
    mutable saved_commands : command list Unicode.DynUCTrie.t;
    mutable saved_patterns : command list Unicode.DynUCTrie.t;
    mutable environment_table : (command * command) Unicode.DynUCTrie.t;
    environment_stack : (uc_list * uc_list list) Stack.t;
    mutable math_codes :
      (Box.math_code * (int * int) * (uc_char * uc_char)) Charmap.charmap;
    mutable al_scope : Vm.Machine.scope;
    mutable global_variables : Vm_types.Types.partial_value SymbolTable.SymbolMap.t;
    mutable counter_table : Counter.counter_table;
    mutable old_references : uc_string Unicode.DynUCTrie.t;
    mutable references : uc_string Unicode.DynUCTrie.t }
and command =
  { execute : parse_state -> unit;
    expand : parse_state -> uc_list -> uc_list }

val mode_to_string : mode -> string

val create : Job.job -> parse_state
val duplicate : parse_state -> parse_state
val set_stream : parse_state -> UCStream.istream -> unit
val location : parse_state -> UCStream.location

val open_node_list : parse_state -> mode -> unit
val close_node_list : parse_state -> mode -> Node.node list
val add_node : parse_state -> Node.node -> unit
val current_mode : parse_state -> mode

val set_default_char_cmd : parse_state -> command -> unit

val define_command : parse_state -> uc_list -> command -> unit
val define_pattern : parse_state -> uc_list -> command -> unit
val lookup_command : parse_state -> uc_list -> command option
val save_command : parse_state -> uc_list -> unit
val restore_command : parse_state -> uc_list -> unit
val save_pattern : parse_state -> uc_list -> unit
val restore_pattern : parse_state -> uc_list -> unit

val push_env : parse_state -> uc_list -> uc_list list -> unit
val pop_env : parse_state -> uc_list * uc_list list
val set_env_args : parse_state -> uc_list list -> unit
val top_env : parse_state -> uc_list * uc_list list

val lookup_env : parse_state -> uc_list -> (command * command) option
val define_env : parse_state -> uc_list -> command -> command -> unit

val set_math_code_table :
  parse_state ->
    (Box.math_code * (int * int) * (uc_char * uc_char)) Charmap.charmap ->
    unit
val set_math_code :
  parse_state -> uc_char -> Box.math_code -> int -> uc_char -> int ->
    uc_char -> unit
val get_math_code :
  parse_state -> uc_char -> Box.math_code * (int * int) * (uc_char * uc_char)

val new_counter : parse_state -> uc_string -> int -> uc_string option -> unit
val get_counter : parse_state -> uc_string -> int
val set_counter : parse_state -> uc_string -> int -> unit

val gen_unique_name : unit -> uc_list

val add_reference : parse_state -> uc_list -> uc_string -> unit
val reference_exists : parse_state -> uc_list -> bool
val lookup_reference : parse_state -> uc_list -> uc_string
val iter_references : parse_state -> (uc_string -> uc_string -> unit) -> unit
val store_old_references : parse_state -> unit
val compare_references : parse_state -> bool
val write_references : parse_state -> string -> unit

val execute_next_char : parse_state -> bool
val execute_stream : parse_state -> UCStream.istream -> unit
val execute_argument : parse_state -> unit
val execute_argument_in_mode : parse_state -> mode -> Node.node list
val run_parser : parse_state -> mode -> Node.node list

val execute_string_in_mode : parse_state -> uc_list -> mode -> Node.node list
