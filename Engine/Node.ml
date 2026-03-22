
open Tools.XNum
open Runtime
open Unicode.UTypes
open Dim
(* open Typesetting *)
open Box
open Environment

type area_contents_arg = [
  | `Galley of uc_string * skip_arg * skip_arg * skip_arg * skip_arg
  | `Float of FloatVertical.vert_alignment * skip_arg * skip_arg * dim_arg
  | `Footnote of
       node list * skip_arg * skip_arg * skip_arg * line_param_modifier *
         par_param_modifier * line_break_param_modifier *
         hyphen_param_modifier * space_param_modifier * math_param_modifier
  | `Direct of page_info -> (num * num) -> node list
]
and node =
    Nodes of node list
  | Command of Unicode.UCStream.location * env_cmd
  | CommandBox of Unicode.UCStream.location * box_cmd
  | GfxCommand of Unicode.UCStream.location * (dim_arg, Box.box) Graphic.graphic_command
  | NewGalley of Unicode.UCStream.location * uc_string * skip_arg
  | NewLayout of Unicode.UCStream.location * uc_string * skip_arg * skip_arg
  | NewArea of
      Unicode.UCStream.location * uc_string * skip_arg * skip_arg * skip_arg *
        skip_arg * skip_arg * skip_arg * area_contents_arg
  | ShipOut of Unicode.UCStream.location * uc_string * uc_string * int
  | AddToGalley of Unicode.UCStream.location * uc_string * node list
  | PutGalleyInVBox of Unicode.UCStream.location * bool * uc_string
  | ModifyGalleyGlue of
      Unicode.UCStream.location * (environment -> Box.box list -> Box.box list)
  | Paragraph of Unicode.UCStream.location * node list
  | BeginGroup of Unicode.UCStream.location
  | EndGroup of Unicode.UCStream.location
  | Float of Unicode.UCStream.location * uc_string * node list
  | Glyph of Unicode.UCStream.location * int
  | Letter of Unicode.UCStream.location * uc_char
  | Space of Unicode.UCStream.location
  | Glue of Unicode.UCStream.location * dim_arg * dim_arg * bool * bool
  | Break of
      Unicode.UCStream.location * num option * bool * node list * node list *
        node list
  | Rule of Unicode.UCStream.location * dim_arg * dim_arg * dim_arg
  | Image of
      Unicode.UCStream.location * string * LoadImage.format * skip_arg * skip_arg
  | Accent of Unicode.UCStream.location * uc_char * node list
  | HBox of Unicode.UCStream.location * [ `LR | `RL | `Default ] * node list
  | HBoxTo of
      Unicode.UCStream.location * [ `LR | `RL | `Default ] * skip_arg * node list
  | HBoxSpread of
      Unicode.UCStream.location * [ `LR | `RL | `Default ] * skip_arg * node list
  | VBox of Unicode.UCStream.location * node list
  | VBoxTo of Unicode.UCStream.location * skip_arg * node list
  | VBoxSpread of Unicode.UCStream.location * skip_arg * node list
  | Phantom of Unicode.UCStream.location * bool * bool * node list
  | HLeaders of Unicode.UCStream.location * dim_arg * node list
  | VInsert of Unicode.UCStream.location * bool * node list
  | Table of Unicode.UCStream.location * node list
  | TableEntry of Unicode.UCStream.location * int * int * int * int * int * node list
  | Math of Unicode.UCStream.location * node list
  | MathCode of Unicode.UCStream.location * MathTypes.math_code * node list
  | MathChar of
      Unicode.UCStream.location * (MathTypes.math_code * (int * int) * (uc_char * uc_char))
  | SubScript of Unicode.UCStream.location * node list
  | SuperScript of Unicode.UCStream.location * node list
  | Fraction of
      Unicode.UCStream.location * node list * node list * node * node * skip_arg
  | Underline of Unicode.UCStream.location * node list
  | Overline of Unicode.UCStream.location * node list
  | MathAccent of Unicode.UCStream.location * int * uc_char * node list
  | Root of Unicode.UCStream.location * int * uc_char * int * uc_char * node list
  | LeftRight of Unicode.UCStream.location * node list list
  | MathStyle of Unicode.UCStream.location * MathLayout.math_style
  | IndexPosition of Unicode.UCStream.location * Runtime.MathTypes.index_position
