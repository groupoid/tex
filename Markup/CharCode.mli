
open Unicode.UTypes

type cat_code =
    Letter
  | Newline
  | Space
  | Escape
  | BeginGroup
  | EndGroup
  | BeginOptional
  | EndOptional
  | Macro
  | Comment
  | Other
  | EOF

val macro_char : uc_char
val comment_char : uc_char
val escape_char : uc_char
val begin_optional_char : uc_char
val end_optional_char : uc_char
val begin_group_char : uc_char
val end_group_char : uc_char

val cat_code : uc_char -> cat_code

