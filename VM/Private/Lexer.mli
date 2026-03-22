
open Tools.XNum
open Unicode
open UTypes
open Unicode.UTypes
open Unicode.SymbolTable

type assoc = Left | NonA | Right 

type token_class = 
| EOF
| LID of symbol
| UID of symbol
| NUMBER of num
| CHARACTER of int
| STRING of int list
| BINOP of symbol * int * assoc
| PREOP of symbol
| POSTOP of symbol
| PARENOPEN
| PARENCLOSE
| BRACKETOPEN
| BRACKETCLOSE
| BRACEOPEN
| BRACECLOSE
| APOSTROPHE
| QUOTE
| COMMA
| COLON
| SEMICOLON
| UNDERSCORE
| EQUAL
| COLON_EQUAL
| BAR
| AMPERSAND
| PERIOD
| DO
| IF
| THEN
| ELSE
| ELSEIF
| END
| FORCE
| BEGIN
| MATCH
| WITH
| LOCAL
| WHERE
| INFIX of assoc
| PREFIX
| POSTFIX


type lexer

val initial_symbol_table : unit -> (uc_string, token_class) Hashtbl.t
val make_lexer           : (uc_string, token_class) Hashtbl.t -> Unicode.UCStream.istream -> lexer
val set_stream           : lexer -> Unicode.UCStream.istream -> unit
val read_token           : lexer -> token_class
val restore_token        : lexer -> token_class -> unit
val add_bin_op           : lexer -> int -> assoc -> symbol -> unit
val add_pre_op           : lexer -> symbol -> unit
val add_post_op          : lexer -> symbol -> unit
val token_to_string      : token_class -> uc_string
val syntax_error         : lexer -> string -> 'a
val syntax_error_uc      : lexer -> uc_string -> 'a
