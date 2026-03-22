
open Tools.XNum
open Unicode.UTypes
open Vm_types.Types
open Unicode
open UTypes
open Unicode.SymbolTable

module UString = Unicode.UString
module UChar   = Unicode.UChar

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

type lexer =
{
  mutable input    : Unicode.UCStream.istream;
  mutable tokens   : token_class list;
  keywords : (uc_string, token_class) Hashtbl.t
}

let syntax_error_uc lexer msg =
  raise (Syntax_error (Unicode.location lexer.input, msg))

let syntax_error lexer msg = syntax_error_uc lexer (UString.uc_string_of_ascii msg)

type char_class = LetterLC | LetterUC | Digit | Symbol | Whitespace | Special of token_class

let category_to_class cat = match cat with
  | UChar.Ll | UChar.Lm | UChar.Lo -> LetterLC
  | UChar.Lu | UChar.Lt            -> LetterUC
  | UChar.Mn | UChar.Mc | UChar.Me
  | UChar.Zs | UChar.Zl | UChar.Zp
  | UChar.Cc | UChar.Cf | UChar.Cs
  | UChar.Co | UChar.Cn            -> Whitespace
  | UChar.Nd | UChar.Nl | UChar.No -> Digit
  | UChar.Pc | UChar.Pd | UChar.Ps
  | UChar.Pe | UChar.Pi | UChar.Pf
  | UChar.Po | UChar.Sm | UChar.Sc
  | UChar.Sk | UChar.So            -> Symbol

let special_cases = Array.init 128 (fun c -> category_to_class (UChar.category c))

let () =
  special_cases.(0x22) <- Special QUOTE;
  special_cases.(0x27) <- Special APOSTROPHE;
  special_cases.(0x28) <- Special PARENOPEN;
  special_cases.(0x29) <- Special PARENCLOSE;
  special_cases.(0x2c) <- Special COMMA;
  special_cases.(0x3b) <- Special SEMICOLON;
  special_cases.(0x5b) <- Special BRACKETOPEN;
  special_cases.(0x5d) <- Special BRACKETCLOSE;
  special_cases.(0x5f) <- Special UNDERSCORE;
  special_cases.(0x7b) <- Special BRACEOPEN;
  special_cases.(0x7d) <- Special BRACECLOSE

let char_class c =
  if c < 128 then
    special_cases.(c)
  else
    category_to_class (UChar.category c)

let rec read_id istream =
  let c = Unicode.UCStream.next_char istream in
  if c < 0 then
    []
  else match char_class c with
  | LetterLC | LetterUC -> read_id_alpha istream
  | Digit               -> read_id_num   istream
  | Symbol              -> read_id_sym   istream
  | _                   -> []

and read_id_alpha istream =
  let c = Unicode.UCStream.next_char istream in
  if c < 0 then
    []
  else match char_class c with
  | LetterLC | LetterUC ->
      let _ = Unicode.UCStream.pop istream in
      c :: read_id_alpha istream
  | Special UNDERSCORE ->
      let _ = Unicode.UCStream.pop istream in
      c :: read_id istream
  | _ -> []

and read_id_sym istream =
  let c = Unicode.UCStream.next_char istream in
  if c < 0 then
    []
  else match char_class c with
  | Symbol ->
      let _ = Unicode.UCStream.pop istream in
      c :: read_id_sym istream
  | Special UNDERSCORE ->
      let _ = Unicode.UCStream.pop istream in
      c :: read_id istream
  | _ -> []

and read_id_num istream =
  let c = Unicode.UCStream.next_char istream in
  if c < 0 then
    []
  else match char_class c with
  | Digit ->
      let _ = Unicode.UCStream.pop istream in
      c :: read_id_num istream
  | Special UNDERSCORE ->
      let _ = Unicode.UCStream.pop istream in
      c :: read_id istream
  | _ -> []

let read_symbol istream =
  let c = Unicode.UCStream.next_char istream in
  if c < 0 then
    []
  else match char_class c with
  | Symbol ->
      let _ = Unicode.UCStream.pop istream in
      c :: read_id_sym istream
  | _ -> []

let rec read_digit base istream =
  let c = Unicode.UCStream.next_char istream in
  if c < 48 then
    -1
  else if c = 95 then begin (* _ *)
    let _ = Unicode.UCStream.pop istream in
    read_digit base istream
  end
  else if base <= 10 then begin
    if c < 48 + base then begin
      let _ = Unicode.UCStream.pop istream in
      c - 48
    end
    else
      -1
  end
  else begin
    if c < 58 then begin
      let _ = Unicode.UCStream.pop istream in
      c - 48
    end
    else if 97 <= c then begin
      if c < 87 + base then begin
        let _ = Unicode.UCStream.pop istream in
        c - 87
      end
      else
        -1
    end
    else if 65 <= c && c < 55 + base then begin
      let _ = Unicode.UCStream.pop istream in
      c - 55
    end
    else
      -1
  end

let read_natural istream first_char =
  let select_base istream first_char =
    if first_char <> 48 then 10
    else match Unicode.UCStream.next_char istream with
    | 98  -> let _ = Unicode.UCStream.pop istream in 2    (* b *)
    | 111 -> let _ = Unicode.UCStream.pop istream in 8    (* o *)
    | 120 -> let _ = Unicode.UCStream.pop istream in 16   (* x *)
    | _   -> 10
  in
  let base = select_base istream first_char in
  let b = num_of_int base in
  let rec iter x =
    let d = read_digit base istream in
    if d < 0 then (x, base)
    else iter (b */ x +/ num_of_int d)
  in
  iter (num_of_int (first_char - 48))

let read_fraction base x istream =
  let r = num_one // num_of_int base in
  let rec iter x y =
    let d = read_digit base istream in
    if d < 0 then x
    else iter (x +/ y */ num_of_int d) (y */ r)
  in
  iter x r

let read_number istream first_char =
  let (int_part, base) = read_natural istream first_char in
  let c = Unicode.UCStream.next_char istream in
  if c < 0 then int_part
  else if c = 46 then begin (* . *)
    let _ = Unicode.UCStream.pop istream in
    read_fraction base int_part istream
  end
  else if c = 47 (* / *)
       && char_class (Unicode.UCStream.get_char istream 1) = Digit then begin
    let _ = Unicode.UCStream.pop istream in
    let (denom, _) = read_natural istream (Unicode.UCStream.pop istream) in
    int_part // denom
  end
  else
    int_part

let read_character istream =
  let c = Unicode.UCStream.pop istream in
  if c <> 92 then c (* \ *)
  else begin
    match Unicode.UCStream.pop istream with
    | 98  -> 7   (* b *)
    | 101 -> 27  (* e *)
    | 102 -> 12  (* f *)
    | 110 -> 10  (* n *)
    | 114 -> 13  (* r *)
    | 116 -> 9   (* t *)
    | 117 -> 
        let d1 = read_digit 16 istream in
        let d2 = read_digit 16 istream in
        let d3 = read_digit 16 istream in
        let d4 = read_digit 16 istream in
        if d1 >= 0 && d2 >= 0 && d3 >= 0 && d4 >= 0 then
          0x1000 * d1 + 0x100 * d2 + 0x10 * d3 + d4
        else 0
    | 120 -> 
        let d1 = read_digit 16 istream in
        let d2 = read_digit 16 istream in
        if d1 >= 0 && d2 >= 0 then
          0x10 * d1 + d2
        else 0
    | c when c >= 48 && c <= 57 -> 
        let d2 = Unicode.UCStream.get_char istream 0 in
        if d2 >= 48 && d2 <= 57 then
          let d3 = Unicode.UCStream.get_char istream 1 in
          if d3 >= 48 && d3 <= 57 then begin
            let _ = Unicode.UCStream.remove istream 2 in
            100 * (c - 48) + 10 * (d2 - 48) + d3 - 48
          end else begin
            let _ = Unicode.UCStream.remove istream 1 in
            10 * (c - 48) + d2 - 48
          end
        else c - 48
    | c -> c
  end

let read_char_constant lexer =
  let c = read_character lexer.input in
  if Unicode.UCStream.pop lexer.input <> 39 then syntax_error lexer "' expected"
  else c

let read_string_constant lexer =
  let str = Tools.ListBuilder.make () in
  let rec iter () = match Unicode.UCStream.next_char lexer.input with
    | 34 -> (* quote *)
        let _ = Unicode.UCStream.pop lexer.input in
        Tools.ListBuilder.get str
    | -1 -> syntax_error lexer "undelimited string constant"
    | _ -> 
        Tools.ListBuilder.add str (read_character lexer.input);
        iter ()
  in
  iter ()

let keywords =
  let t = Hashtbl.create 97 in
  let add_token str token = 
    let ustr = UString.uc_string_of_ascii str in
    Hashtbl.add t ustr token
  in
  let add_bin_op str pri ass = 
    let ustr = UString.uc_string_of_ascii str in
    Hashtbl.add t ustr (BINOP (string_to_symbol ustr, pri, ass))
  in
  let add_pre_op str = 
    let ustr = UString.uc_string_of_ascii str in
    Hashtbl.add t ustr (PREOP (string_to_symbol ustr))
  in
  add_token "do"     DO;
  add_token "if"     IF;
  add_token "then"   THEN;
  add_token "else"   ELSE;
  add_token "elseif" ELSEIF;
  add_token "end"    END;
  add_token "force"  FORCE;
  add_token "begin"  BEGIN;
  add_token "match"  MATCH;
  add_token "with"   WITH;
  add_token "local"  LOCAL;
  add_token "where"  WHERE;
  add_token "="      EQUAL;
  add_token ":="     COLON_EQUAL;
  add_token "|"      BAR;
  add_token "&"      AMPERSAND;
  add_token "."      PERIOD;
  add_token ":"      COLON;
  add_token ";"      SEMICOLON;
  add_token "declare_infix_left"  (INFIX Left);
  add_token "declare_infix_non"   (INFIX NonA);
  add_token "declare_infix_right" (INFIX Right);
  add_token "declare_prefix"      PREFIX;
  add_token "declare_postfix"     POSTFIX;
  add_bin_op "land" 5 Left;
  add_bin_op "lor"  5 Left;
  add_bin_op "lxor" 5 Left;
  add_bin_op "lsr"  5 Left;
  add_bin_op "lsl"  5 Left;
  add_bin_op "mod"  7 Left;
  add_bin_op "||"   2 Right;
  add_bin_op "&&"   3 Right;
  add_bin_op "=="   4 NonA;
  add_bin_op "<>"   4 NonA;
  add_bin_op ">"    4 NonA;
  add_bin_op "<"    4 NonA;
  add_bin_op ">="   4 NonA;
  add_bin_op "<="   4 NonA;
  add_bin_op "+"    6 Left;
  add_bin_op "-"    6 Left;
  add_bin_op "*"    7 Left;
  add_bin_op "/"    7 Left;
  add_bin_op "^"    8 Left;
  add_pre_op "~";
  t

let initial_symbol_table () = Hashtbl.copy keywords

let rec skip_line_comment istream =
  let c = Unicode.UCStream.pop istream in
  if c < 0 || c = 10 || c = 13 then ()
  else skip_line_comment istream

let rec skip_comment istream nest = match Unicode.UCStream.pop istream with
  | -1 -> ()
  | 59 -> (* ; *)
      if Unicode.UCStream.next_char istream = 93 then begin (* ] *)
        let _ = Unicode.UCStream.remove istream 1 in
        if nest > 0 then skip_comment istream (nest - 1) else ()
      end else skip_comment istream nest
  | 91 -> (* [ *)
      if Unicode.UCStream.next_char istream = 59 then (* ; *)
        skip_comment istream (nest + 1)
      else skip_comment istream nest
  | _ -> skip_comment istream nest

let make_lexer keyword_table stream =
{
  input    = stream;
  tokens   = [];
  keywords = keyword_table
}

let set_stream lexer stream =
  lexer.input <- stream

let rec read_token lexer = match lexer.tokens with
  | t :: ts -> 
      lexer.tokens <- ts;
      t
  | [] -> 
      let c = Unicode.UCStream.pop lexer.input in
      if c < 0 then EOF
      else match char_class c with
      | Whitespace -> read_token lexer
      | LetterLC   -> 
          let lid = Array.of_list (c :: read_id_alpha lexer.input) in
          (try Hashtbl.find lexer.keywords lid
           with Not_found -> LID (string_to_symbol lid))
      | LetterUC   -> UID (string_to_symbol (Array.of_list (c :: read_id_alpha lexer.input)))
      | Symbol     -> 
          let sym = Array.of_list (c :: read_symbol lexer.input) in
          (try Hashtbl.find lexer.keywords sym
           with Not_found -> LID (string_to_symbol sym))
      | Digit     -> NUMBER (read_number lexer.input c)
      | Special s -> match s with
        | APOSTROPHE -> CHARACTER (read_char_constant lexer)
        | QUOTE      -> STRING (read_string_constant lexer)
        | SEMICOLON  -> 
            if Unicode.UCStream.next_char lexer.input = 59 then begin (* ;; *)
              skip_line_comment lexer.input;
              read_token lexer
            end else s
        | BRACKETOPEN -> 
            if Unicode.UCStream.get_char lexer.input 0 = 59 then begin (* [; *)
              skip_comment lexer.input 0;
              read_token lexer
            end else s
        | _ -> s

let restore_token lexer tok =
  lexer.tokens <- tok :: lexer.tokens

let add_bin_op lexer pri assoc sym =
  let str = symbol_to_string sym in
  Hashtbl.add lexer.keywords str (BINOP (sym, pri, assoc))

let add_pre_op lexer sym =
  let str = symbol_to_string sym in
  Hashtbl.add lexer.keywords str (PREOP sym)

let add_post_op lexer sym =
  let str = symbol_to_string sym in
  Hashtbl.add lexer.keywords str (POSTOP sym)

let token_to_string tok = match tok with
  | EOF           -> UString.uc_string_of_ascii "end-of-file"
  | LID _         -> UString.uc_string_of_ascii "lowercase identifier"
  | UID _         -> UString.uc_string_of_ascii "uppercase identifier"
  | NUMBER _      -> UString.uc_string_of_ascii "number"
  | CHARACTER c   -> Array.of_list [39; c; 39]
  | STRING str    -> Array.of_list (34 :: str @ [34])
  | BINOP (sym, _, _) -> symbol_to_string sym
  | PREOP sym     -> symbol_to_string sym
  | POSTOP sym    -> symbol_to_string sym
  | PARENOPEN     -> UString.uc_string_of_ascii "("
  | PARENCLOSE    -> UString.uc_string_of_ascii ")"
  | BRACKETOPEN   -> UString.uc_string_of_ascii "["
  | BRACKETCLOSE  -> UString.uc_string_of_ascii "]"
  | BRACEOPEN     -> UString.uc_string_of_ascii "{"
  | BRACECLOSE    -> UString.uc_string_of_ascii "}"
  | APOSTROPHE    -> UString.uc_string_of_ascii "'"
  | QUOTE         -> UString.uc_string_of_ascii "\""
  | COMMA         -> UString.uc_string_of_ascii ","
  | COLON         -> UString.uc_string_of_ascii ":"
  | SEMICOLON     -> UString.uc_string_of_ascii ";"
  | UNDERSCORE    -> UString.uc_string_of_ascii "_"
  | EQUAL        -> UString.uc_string_of_ascii "="
  | COLON_EQUAL  -> UString.uc_string_of_ascii ":="
  | BAR           -> UString.uc_string_of_ascii "|"
  | AMPERSAND     -> UString.uc_string_of_ascii "&"
  | PERIOD        -> UString.uc_string_of_ascii "."
  | DO            -> UString.uc_string_of_ascii "do"
  | IF            -> UString.uc_string_of_ascii "if"
  | THEN          -> UString.uc_string_of_ascii "then"
  | ELSE          -> UString.uc_string_of_ascii "else"
  | ELSEIF        -> UString.uc_string_of_ascii "elseif"
  | END           -> UString.uc_string_of_ascii "end"
  | FORCE         -> UString.uc_string_of_ascii "force"
  | BEGIN         -> UString.uc_string_of_ascii "begin"
  | MATCH         -> UString.uc_string_of_ascii "match"
  | WITH          -> UString.uc_string_of_ascii "with"
  | LOCAL        -> UString.uc_string_of_ascii "local"
  | WHERE         -> UString.uc_string_of_ascii "where"
  | INFIX Left    -> UString.uc_string_of_ascii "declare_infix_left"
  | INFIX NonA    -> UString.uc_string_of_ascii "declare_infix_non"
  | INFIX Right   -> UString.uc_string_of_ascii "declare_infix_right"
  | PREFIX        -> UString.uc_string_of_ascii "declare_prefix"
  | POSTFIX       -> UString.uc_string_of_ascii "declare_postfix"
