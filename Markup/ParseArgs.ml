
open XNum
open Runtime
open ParseState

let arg_expanded ps =
  Macro.expand_string ps (Parser.read_argument ps.input_stream)
let arg_execute ps mode = execute_argument_in_mode ps mode
let arg_num ps = Parser.str_expr_to_num (arg_expanded ps)
let arg_int ps = int_of_num (integer_num (arg_num ps))
let arg_skip ps = Parser.str_expr_to_skip (arg_expanded ps)
let arg_key_val ps = Parser.str_to_key_val (arg_expanded ps)
let arg_dim ps = Parser.str_expr_to_dim (arg_expanded ps)
let opt_expanded ps default =
  Macro.expand_string ps (Parser.read_optional ps.input_stream default)
let opt_key_val ps =
  Parser.str_to_key_val (Parser.read_optional ps.input_stream [])
let opt_int ps default =
  match opt_expanded ps [] with
    [] -> default
  | str -> int_of_num (integer_num (Parser.str_expr_to_num str))

(*
  |arg_TeX_dim| reads either a dim expression enclosed in braces or a simple dim expression
   without braces. This is used for compatibility wiht TeX commands like \vskip, \hskip, \kern.
*)

let arg_TeX_dim ps =
  Parser.skip_blanks ps.input_stream;
  match CharCode.cat_code (UCStream.next_char ps.input_stream) with
    CharCode.BeginGroup | CharCode.Escape -> arg_dim ps
  | _ -> Parser.read_simple_dim_expression ps.input_stream

