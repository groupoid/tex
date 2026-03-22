open XNum
open Unicode.UTypes
(* open Typesetting *)

val match_substring   : UCStream.istream -> uc_list -> int -> bool
val newline_to_par    : UCStream.istream -> unit
val skip_spaces       : UCStream.istream -> unit
val skip_blanks       : UCStream.istream -> unit
val skip_comment      : UCStream.istream -> unit
val read_token        : UCStream.istream -> uc_list
val read_token_tail   : UCStream.istream -> uc_list
val peek_token        : UCStream.istream -> uc_list
val read_argument     : UCStream.istream -> uc_list
val read_optional     : UCStream.istream -> uc_list -> uc_list
val read_bool         : UCStream.istream -> bool
val read_keyword      : UCStream.istream -> uc_list
val read_key_val_list : UCStream.istream -> uc_list option Unicode.DynUCTrie.t

val is_command_sequence : uc_list -> bool
val is_token            : uc_list -> bool

type 'a expr

val make_expression      : 'a -> 'a expr
val evaluate_expression  : 'a expr -> (num -> 'a) -> ('a -> 'b) ->
                             ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) ->
                             (num -> 'b -> 'b) -> 'b
val read_expression      : (UCStream.istream -> 'a expr) -> (UCStream.location -> num -> 'a) ->
                             UCStream.istream -> 'a expr

val read_unsigned_int    : UCStream.istream -> (int * int)
val read_int             : UCStream.istream -> (int * int)
val read_number          : UCStream.istream -> num
val read_skip            : UCStream.istream -> Environment.skip_arg
val read_skip_with_order : UCStream.istream -> (Environment.skip_arg * int)
val read_dim             : UCStream.istream -> Environment.dim_arg

val read_num_expression         : UCStream.istream -> num
val read_skip_expression        : UCStream.istream -> Environment.skip_arg
val read_dim_expression         : UCStream.istream -> Environment.dim_arg
val read_simple_num_expression  : UCStream.istream -> num
val read_simple_skip_expression : UCStream.istream -> Environment.skip_arg
val read_simple_dim_expression  : UCStream.istream -> Environment.dim_arg

val read_range                  : UCStream.istream -> (num * num)

val str_to_bool      : uc_list -> bool
val str_to_uint      : uc_list -> int
val str_to_int       : uc_list -> int
val str_to_num       : uc_list -> num
val str_to_skip      : uc_list -> Environment.skip_arg
val str_to_dim       : uc_list -> Environment.dim_arg
val str_expr_to_num  : uc_list -> num
val str_expr_to_num_expr : uc_list -> num
val str_expr_to_skip : uc_list -> Environment.skip_arg
val str_expr_to_dim  : uc_list -> Environment.dim_arg
val str_to_list      : uc_list -> uc_list list
val str_to_key_val   : uc_list -> uc_list option Unicode.DynUCTrie.t
