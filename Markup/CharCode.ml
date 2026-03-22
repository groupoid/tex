
open Runtime
open Unicode
open Types
open Box

(* catcodes and math-codes *)

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

let macro_char = 35  (* # *)
let comment_char = 37  (* % *)
let begin_optional_char = 91  (* [ *)
let escape_char = 92  (* \ *)
let end_optional_char = 93  (* ] *)
let begin_group_char = 123  (* { *)
let end_group_char = 125  (* } *)

let default_cat_codes_xx = 
  [| Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other |]

let default_cat_codes_00 = 
  [| Space; Space; Space; Space; Space; Space; Space; Space; Space; Space;
     Newline; Space; Space; Space; Space; Space; Space; Space; Space; Space;
     Space; Space; Space; Space; Space; Space; Space; Space; Space; Space;
     Space; Space; Space; Other; Other; Macro; Other; Comment; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; BeginOptional; Escape; EndOptional;
     Other; Other; Other; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; BeginGroup; Other; EndGroup; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Other; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Other; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter |]

let default_cat_codes_01 = 
  [| Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Letter; Letter; Letter; Letter; Letter;
     Letter |]

let default_cat_codes_03 = 
  [| Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Letter; Other; Letter; Letter; Letter; Other;
     Letter; Other; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Other; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter; Letter;
     Letter; Letter; Letter; Letter; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other |]

let default_cat_codes_1e = 
  [| Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Letter; Letter;
     Letter; Letter; Letter; Letter; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Letter; Letter; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other |]

let default_cat_codes_fb = 
  [| Letter; Letter; Letter; Letter; Letter; Letter; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other; Other; Other; Other;
     Other; Other; Other; Other; Other; Other; Other |]

let cat_code_table =
  Charmap.build
    [| default_cat_codes_00; default_cat_codes_01; default_cat_codes_xx;
       default_cat_codes_03; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_1e; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_fb;
       default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
       default_cat_codes_xx |]

let cat_code char =
  if char < 0 then EOF else Charmap.lookup cat_code_table char
