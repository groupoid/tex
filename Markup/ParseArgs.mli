
open XNum
open Unicode.UTypes
open Runtime
open Engine
open ParseState

val arg_expanded : parse_state -> uc_list
val arg_execute : parse_state -> mode -> Node.node list
val arg_num : parse_state -> num
val arg_int : parse_state -> int
val arg_skip : parse_state -> Environment.skip_arg
val arg_key_val : parse_state -> uc_list option Unicode.DynUCTrie.t
val arg_dim : parse_state -> Environment.dim_arg
val opt_expanded : parse_state -> uc_list -> uc_list
val opt_key_val : parse_state -> uc_list option Unicode.DynUCTrie.t
val opt_int : parse_state -> int -> int
val arg_TeX_dim : parse_state -> Environment.dim_arg

