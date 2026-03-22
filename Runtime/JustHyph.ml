
open Unicode.UTypes
open RuntimeGlobals
open Dim
open Tools.XNum
open Substitute

let is_real = function
  | `Glyph _ | `Box _ | `Math _ | `Char _ -> true
  | _ -> false

type ('box, 'cmd) glyph_item = [
  | `Glyph of (GlyphMetric.glyph_desc * FontMetric.font_metric)
  | `Kern of Dim.dim
  | `Penalty of int
  | `Glue of (Tools.XNum.num * bool * bool)
  | `Math of (MathTypes.math_code * 'box)
  | `Box of 'box
  | `Command of 'cmd
]

type ('box, 'cmd) extended_glyph_item = [
  | ('box, 'cmd) glyph_item
  | `Break of (Tools.XNum.num * bool * 'box array * 'box array * 'box array)
]

type hyphen_params = {
  hp_left_hyphen_min  : int;
  hp_right_hyphen_min : int;
  hp_hyphen_table     : Hyphenation.hyphen_table;
  hp_hyphen_penalty   : int;
  hp_ex_hyphen_penalty: int;
  hp_script_lang      : int;
}

type ('box, 'cmd) char_item =
  | Char of uc_char
  | Item of ('box, 'cmd) extended_glyph_item

let hyphenate params str = Hyphenation.hyphenate params.hp_hyphen_table str

let convert_to_glyphs font _composer word =
  let rec iter = function
    | [] -> []
    | Char c :: rest ->
        `Glyph (FontMetric.get_glyph font c, font) :: iter rest
    | Item i :: rest -> i :: iter rest
  in
  iter word

let convert_to_glyphs_and_add_breaks params font composer word =
  convert_to_glyphs font composer word (* Simplified for now *)

let strip_composer (i : ('box, 'cmd) extended_glyph_item) = i
let add_lig_kern _ (items : ('box, 'cmd) extended_glyph_item list) = items
let add_lig_kern_iterative_list _ _ (l : ('box, 'cmd) extended_glyph_item list) = (l, List.length l, [])
let add_lig_kern_iterative_array _ (prefix : ('box, 'cmd) extended_glyph_item list) _ _ _ = (prefix, 0)

let postfix_with_list l1 l2 = Array.of_list (l1 @ l2)
