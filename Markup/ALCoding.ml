
open XNum
open Runtime
open Logging

(* open Typesetting *)
open Engine
open ParseState
open Vm_types

module UString   = Unicode.UString
module SymbolMap = Unicode.SymbolTable.SymbolMap

(* symbols *)
let decl_sym str = Machine.string_to_symbol (UString.uc_string_of_ascii str)

let sym_Accent               = decl_sym "Accent"
let sym_AddToGalley          = decl_sym "AddToGalley"
let sym_AdjDemerits          = decl_sym "AdjDemerits"
let sym_Adjustments          = decl_sym "Adjustments"
let sym_Alignment            = decl_sym "Alignment"
let sym_AutoLigatures        = decl_sym "AutoLigatures"
let sym_Base                 = decl_sym "Base"
let sym_BaselineSkip         = decl_sym "BaselineSkip"
let sym_BeginGroup           = decl_sym "BeginGroup"
let sym_Bevel                = decl_sym "Bevel"
let sym_BinOp                = decl_sym "BinOp"
let sym_BinOpPenalty         = decl_sym "BinOpPenalty"
let sym_Bitmap               = decl_sym "Bitmap"
let sym_Bmp                  = decl_sym "Bmp"
let sym_Bool                 = decl_sym "Bool"
let sym_BorderKern           = decl_sym "BorderKern"
let sym_Bottom               = decl_sym "Bottom"
let sym_BottomSkip           = decl_sym "BottomSkip"
let sym_Break                = decl_sym "Break"
let sym_Butt                 = decl_sym "Butt"
let sym_Circle               = decl_sym "Circle"
let sym_Clip                 = decl_sym "Clip"
let sym_Close                = decl_sym "Close"
let sym_ClubPenalty          = decl_sym "ClubPenalty"
let sym_CMYK                 = decl_sym "CMYK"
let sym_Command              = decl_sym "Command"
let sym_CommandBox           = decl_sym "CommandBox"
let sym_CrampedDisplay       = decl_sym "CrampedDisplay"
let sym_CrampedScript        = decl_sym "CrampedScript"
let sym_CrampedScript2       = decl_sym "CrampedScriptScript"
let sym_CrampedText          = decl_sym "CrampedText"
let sym_Default              = decl_sym "Default"
let sym_DelimiterFactor      = decl_sym "DelimiterFactor"
let sym_DelimiterShortfall   = decl_sym "DelimiterShortfall"
let sym_Direct               = decl_sym "Direct"
let sym_Display              = decl_sym "Display"
let sym_DoubleHyphenDemerits = decl_sym "DoubleHyphenDemerits"
let sym_Dpi                  = decl_sym "Dpi"
let sym_EmergencyStretch     = decl_sym "EmergencyStretch"
let sym_Encoding             = decl_sym "Encoding"
let sym_EndGroup             = decl_sym "EndGroup"
let sym_ExHyphenPenalty      = decl_sym "ExHyphenPenalty"
let sym_Family               = decl_sym "Family"
let sym_Fill                 = decl_sym "Fill"
let sym_FinalHyphenDemerits  = decl_sym "FinalHyphenDemerits"
let sym_Fixed                = decl_sym "Fixed"
let sym_Float                = decl_sym "Float"
let sym_FloatSep             = decl_sym "FloatSep"
let sym_Footnote             = decl_sym "Footnote"
let sym_Fraction             = decl_sym "Fraction"
let sym_Galley               = decl_sym "Galley"
let sym_GfxCommand           = decl_sym "GfxCommand"
let sym_Glyph                = decl_sym "Glyph"
let sym_Glue                 = decl_sym "Glue"
let sym_Grey                 = decl_sym "Grey"
let sym_GridSize             = decl_sym "GridSize"
let sym_HBox                 = decl_sym "HBox"
let sym_HBoxSpread           = decl_sym "HBoxSpread"
let sym_HBoxTo               = decl_sym "HBoxTo"
let sym_Height               = decl_sym "Height"
let sym_HLeaders             = decl_sym "HLeaders"
let sym_Hyphen               = decl_sym "Hyphen"
let sym_HyphenGlyph          = decl_sym "HyphenGlyph"
let sym_HyphenParams         = decl_sym "HyphenParams"
let sym_HyphenPenalty        = decl_sym "HyphenPenalty"
let sym_HyphenTable          = decl_sym "HyphenTable"
let sym_Image                = decl_sym "Image"
let sym_IndexPosition        = decl_sym "IndexPosition"
let sym_Inner                = decl_sym "Inner"
let sym_Kern                 = decl_sym "Kern"
let sym_Leading              = decl_sym "Leading"
let sym_Left                 = decl_sym "Left"
let sym_AddToGalley          = decl_sym "AddToGalley"
let sym_AdjDemerits          = decl_sym "AdjDemerits"
let sym_Adjustments          = decl_sym "Adjustments"
let sym_Alignment            = decl_sym "Alignment"
let sym_AutoLigatures        = decl_sym "AutoLigatures"
let sym_Base                 = decl_sym "Base"
let sym_BaselineSkip         = decl_sym "BaselineSkip"
let sym_BeginGroup           = decl_sym "BeginGroup"
let sym_Bevel                = decl_sym "Bevel"
let sym_BinOp                = decl_sym "BinOp"
let sym_BinOpPenalty         = decl_sym "BinOpPenalty"
let sym_Bitmap               = decl_sym "Bitmap"
let sym_Bmp                  = decl_sym "Bmp"
let sym_Bool                 = decl_sym "Bool"
let sym_BorderKern           = decl_sym "BorderKern"
let sym_Bottom               = decl_sym "Bottom"
let sym_BottomSkip           = decl_sym "BottomSkip"
let sym_Break                = decl_sym "Break"
let sym_Butt                 = decl_sym "Butt"
let sym_Circle               = decl_sym "Circle"
let sym_Clip                 = decl_sym "Clip"
let sym_Close                = decl_sym "Close"
let sym_ClubPenalty          = decl_sym "ClubPenalty"
let sym_CMYK                 = decl_sym "CMYK"
let sym_Command              = decl_sym "Command"
let sym_CommandBox           = decl_sym "CommandBox"
let sym_CrampedDisplay       = decl_sym "CrampedDisplay"
let sym_CrampedScript        = decl_sym "CrampedScript"
let sym_CrampedScript2       = decl_sym "CrampedScriptScript"
let sym_CrampedText          = decl_sym "CrampedText"
let sym_Default              = decl_sym "Default"
let sym_DelimiterFactor      = decl_sym "DelimiterFactor"
let sym_DelimiterShortfall   = decl_sym "DelimiterShortfall"
let sym_Direct               = decl_sym "Direct"
let sym_Display              = decl_sym "Display"
let sym_DoubleHyphenDemerits = decl_sym "DoubleHyphenDemerits"
let sym_Dpi                  = decl_sym "Dpi"
let sym_EmergencyStretch     = decl_sym "EmergencyStretch"
let sym_Encoding             = decl_sym "Encoding"
let sym_EndGroup             = decl_sym "EndGroup"
let sym_ExHyphenPenalty      = decl_sym "ExHyphenPenalty"
let sym_Family               = decl_sym "Family"
let sym_Fill                 = decl_sym "Fill"
let sym_FinalHyphenDemerits  = decl_sym "FinalHyphenDemerits"
let sym_Fixed                = decl_sym "Fixed"
let sym_Float                = decl_sym "Float"
let sym_FloatSep             = decl_sym "FloatSep"
let sym_Footnote             = decl_sym "Footnote"
let sym_Fraction             = decl_sym "Fraction"
let sym_Galley               = decl_sym "Galley"
let sym_GfxCommand           = decl_sym "GfxCommand"
let sym_Glyph                = decl_sym "Glyph"
let sym_Glue                 = decl_sym "Glue"
let sym_Grey                 = decl_sym "Grey"
let sym_GridSize             = decl_sym "GridSize"
let sym_HBox                 = decl_sym "HBox"
let sym_HBoxSpread           = decl_sym "HBoxSpread"
let sym_HBoxTo               = decl_sym "HBoxTo"
let sym_Height               = decl_sym "Height"
let sym_HLeaders             = decl_sym "HLeaders"
let sym_Hyphen               = decl_sym "Hyphen"
let sym_HyphenGlyph          = decl_sym "HyphenGlyph"
let sym_HyphenParams         = decl_sym "HyphenParams"
let sym_HyphenPenalty        = decl_sym "HyphenPenalty"
let sym_HyphenTable          = decl_sym "HyphenTable"
let sym_Image                = decl_sym "Image"
let sym_IndexPosition        = decl_sym "IndexPosition"
let sym_Inner                = decl_sym "Inner"
let sym_Kern                 = decl_sym "Kern"
let sym_Leading              = decl_sym "Leading"
let sym_Left                 = decl_sym "Left"
let sym_LeftHyphenMin        = decl_sym "LeftHyphenMin"
let sym_LeftRight            = decl_sym "LeftRight"
let sym_LeftSkip             = decl_sym "LeftSkip"
let sym_Letter               = decl_sym "Letter"
let sym_LetterSpacing        = decl_sym "LetterSpacing"
let sym_Ligature             = decl_sym "Ligature"
let sym_LineBreakParams      = decl_sym "LineBreakParams"
let sym_LineParams           = decl_sym "LineParams"
let sym_LinePenalty          = decl_sym "LinePenalty"
let sym_LineSkip             = decl_sym "LineSkip"
let sym_LineSkipLimit        = decl_sym "LineSkipLimit"
let sym_Looseness            = decl_sym "Looseness"
let sym_Space                = decl_sym "Space"
let sym_SubScript            = decl_sym "SubScript"
let sym_SuperScript          = decl_sym "SuperScript"
let sym_Table                = decl_sym "Table"
let sym_LR                   = decl_sym "LR"
let sym_LRBox                = decl_sym "LRBox"
let sym_Mandantory           = decl_sym "Mandantory"
let sym_Math                 = decl_sym "Math"
let sym_MathAccent           = decl_sym "MathAccent"
let sym_MathChar             = decl_sym "MathChar"
let sym_MathCode             = decl_sym "MathCode"
let sym_MathFamily           = decl_sym "MathFamily"
let sym_MathParams           = decl_sym "MathParams"
let sym_MathStyle            = decl_sym "MathStyle"
let sym_Measure              = decl_sym "Measure"
let sym_MedMathSkip          = decl_sym "MedMathSkip"
let sym_MinSize              = decl_sym "MinSize"
let sym_Miter                = decl_sym "Miter"
let sym_ModifyGalleyGlue     = decl_sym "ModifyGalleyGlue"
let sym_Name                 = decl_sym "Name"
let sym_NewArea              = decl_sym "NewArea"
let sym_NewGalley            = decl_sym "NewGalley"
let sym_NewLayout            = decl_sym "NewLayout"
let sym_NewMarks             = decl_sym "NewMarks"
let sym_NoMath               = decl_sym "NoMath"
let sym_None                 = decl_sym "None"
let sym_NullDelimiterSpace   = decl_sym "NullDelimiterSpace"
let sym_OldMarks             = decl_sym "OldMarks"
let sym_Open                 = decl_sym "Open"
let sym_Operator             = decl_sym "Operator"
let sym_Optional             = decl_sym "Optional"
let sym_Ordinary             = decl_sym "Ordinary"
let sym_Overline             = decl_sym "Overline"
let sym_PageNo               = decl_sym "PageNo"
let sym_Paragraph            = decl_sym "Paragraph"
let sym_ParIndent            = decl_sym "ParIndent"
let sym_ParFillSkip          = decl_sym "ParFillSkip"
let sym_ParParams            = decl_sym "ParParams"
let sym_ParShape             = decl_sym "ParShape"
let sym_ParSkip              = decl_sym "ParSkip"
let sym_PDF                  = decl_sym "PDF"
let sym_Phantom              = decl_sym "Phantom"
let sym_PostProcessLine      = decl_sym "PostProcessLine"
let sym_PostScript           = decl_sym "PostScript"
let sym_Preamble             = decl_sym "Preamble"
let sym_PreTolerance         = decl_sym "PreTolerance"
let sym_Punct                = decl_sym "Punct"
let sym_PutBox               = decl_sym "PutBox"
let sym_PutGalleyInVBox      = decl_sym "PutGalleyInVBox"
let sym_Register             = decl_sym "Register"
let sym_Relation             = decl_sym "Relation"
let sym_RelPenalty           = decl_sym "RelPenalty"
let sym_RGB                  = decl_sym "RGB"
let sym_Right                = decl_sym "Right"
let sym_RightHyphenMin       = decl_sym "RightHyphenMin"
let sym_RightSkip            = decl_sym "RightSkip"
let sym_RiverDemerits        = decl_sym "RiverDemerits"
let sym_RiverThreshold       = decl_sym "RiverThreshold"
let sym_RL                  = decl_sym "RL"
let sym_RLBox                = decl_sym "RLBox"
let sym_Root                 = decl_sym "Root"
let sym_Round                = decl_sym "Round"
let sym_Rule                 = decl_sym "Rule"
let sym_Scale                = decl_sym "Scale"
let sym_Script               = decl_sym "Script"
let sym_Script2              = decl_sym "ScriptScript"
let sym_ScriptLang           = decl_sym "ScriptLang"
let sym_ScriptSpace          = decl_sym "ScriptSpace"
let sym_ScriptSize           = decl_sym "ScriptSize"
let sym_Script2Size          = decl_sym "ScriptScriptSize"
let sym_Separator            = decl_sym "Separator"
let sym_Series               = decl_sym "Series"
let sym_SetAlpha             = decl_sym "SetAlpha"
let sym_SetBgColour          = decl_sym "SetBgColour"
let sym_SetColour            = decl_sym "SetColour"
let sym_SetFont              = decl_sym "SetFont"
let sym_SetLineCap           = decl_sym "SetLineCap"
let sym_SetLineJoin          = decl_sym "SetLineJoin"
let sym_SetLineWidth         = decl_sym "SetLineWidth"
let sym_SetMiterLimit        = decl_sym "SetMiterLimit"
let sym_Shape                = decl_sym "Shape"
let sym_ShipOut              = decl_sym "ShipOut"
let sym_Shrink               = decl_sym "Shrink"
let sym_ShrinkFactor         = decl_sym "ShrinkFactor"
let sym_ShrinkOrder          = decl_sym "ShrinkOrder"
let sym_SimpleBreaking       = decl_sym "SimpleBreaking"
let sym_Size                 = decl_sym "Size"
let sym_Skew                 = decl_sym "Skew"
let sym_SkewGlyph            = decl_sym "SkewGlyph"
let sym_Skyline              = decl_sym "Skyline"
let sym_Text                 = decl_sym "Text"
let sym_VBox                 = decl_sym "VBox"
let sym_VBoxTo               = decl_sym "VBoxTo"
let sym_VBoxSpread           = decl_sym "VBoxSpread"
let sym_Table                = decl_sym "Table"
let sym_VLeaders             = decl_sym "VLeaders"
let sym_SpaceSkip            = decl_sym "SpaceSkip"
let sym_Square               = decl_sym "Square"
let sym_Stretch              = decl_sym "Stretch"
let sym_StretchFactor        = decl_sym "StretchFactor"
let sym_StretchOrder         = decl_sym "StretchOrder"
let sym_Stroke               = decl_sym "Stroke"
let sym_TableEntry           = decl_sym "TableEntry"
let sym_TeX                  = decl_sym "TeX"
let sym_TextSize             = decl_sym "TextSize"
let sym_ThickMathSkip        = decl_sym "ThickMathSkip"
let sym_ThinMathSkip         = decl_sym "ThinMathSkip"
let sym_Tolerance            = decl_sym "Tolerance"
let sym_Top                  = decl_sym "Top"
let sym_TopSkip              = decl_sym "TopSkip"
let sym_Underline            = decl_sym "Underline"
let sym_Vert                 = decl_sym "Vert"
let sym_VictorianSpacing     = decl_sym "VictorianSpacing"
let sym_VInsert              = decl_sym "VInsert"
let sym_WidowPenalty         = decl_sym "WidowPenalty"
let sym_Width                = decl_sym "Width"
let sym_XSpaceSkip           = decl_sym "XSpaceSkip"
let sym_Space                = decl_sym "Space"
let sym_SpaceFactor          = decl_sym "SpaceFactor"
let sym_SpaceParams          = decl_sym "SpaceParams"
let sym_Ordinary             = decl_sym "Ordinary"
let sym_Overline             = decl_sym "Overline"
let sym_PageNo               = decl_sym "PageNo"
let sym_Paragraph            = decl_sym "Paragraph"
let sym_ParIndent            = decl_sym "ParIndent"
let sym_ParFillSkip          = decl_sym "ParFillSkip"
let sym_ParParams            = decl_sym "ParParams"
let sym_ParShape             = decl_sym "ParShape"
let sym_ParSkip              = decl_sym "ParSkip"
let sym_PDF                  = decl_sym "PDF"
let sym_Phantom              = decl_sym "Phantom"
let sym_PostProcessLine      = decl_sym "PostProcessLine"
let sym_PostScript           = decl_sym "PostScript"
let sym_Preamble             = decl_sym "Preamble"
let sym_PreTolerance         = decl_sym "PreTolerance"
let sym_Punct                = decl_sym "Punct"
let sym_PutBox               = decl_sym "PutBox"
let sym_PutGalleyInVBox      = decl_sym "PutGalleyInVBox"
let sym_Register             = decl_sym "Register"
let sym_Relation             = decl_sym "Relation"
let sym_RelPenalty           = decl_sym "RelPenalty"
let sym_RGB                  = decl_sym "RGB"
let sym_Right                = decl_sym "Right"
let sym_RightHyphenMin       = decl_sym "RightHyphenMin"
let sym_RightSkip            = decl_sym "RightSkip"
let sym_RiverDemerits        = decl_sym "RiverDemerits"
let sym_RiverThreshold       = decl_sym "RiverThreshold"
let sym_RL                   = decl_sym "RL"
let sym_RLBox                = decl_sym "RLBox"
let sym_Root                 = decl_sym "Root"
let sym_Round                = decl_sym "Round"
let sym_Rule                 = decl_sym "Rule"
let sym_Scale                = decl_sym "Scale"
let sym_Script               = decl_sym "Script"
let sym_ScriptLang           = decl_sym "ScriptLang"
let sym_Script2              = decl_sym "ScriptScript"
let sym_ScriptSpace          = decl_sym "ScriptSpace"
let sym_ScriptSize           = decl_sym "ScriptSize"
let sym_Script2Size          = decl_sym "ScriptScriptSize"
let sym_Separator            = decl_sym "Separator"
let sym_Series               = decl_sym "Series"
let sym_SetAlpha             = decl_sym "SetAlpha"
let sym_SetBgColour          = decl_sym "SetBgColour"
let sym_SetColour            = decl_sym "SetColour"
let sym_SetFont              = decl_sym "SetFont"
let sym_SetLineCap           = decl_sym "SetLineCap"
let sym_SetLineJoin          = decl_sym "SetLineJoin"
let sym_SetLineWidth         = decl_sym "SetLineWidth"
let sym_SetMiterLimit        = decl_sym "SetMiterLimit"
let sym_Shape                = decl_sym "Shape"
let sym_ShipOut              = decl_sym "ShipOut"
let sym_Shrink               = decl_sym "Shrink"
let sym_ShrinkFactor         = decl_sym "ShrinkFactor"
let sym_ShrinkOrder          = decl_sym "ShrinkOrder"
let sym_SimpleBreaking       = decl_sym "SimpleBreaking"
let sym_Size                 = decl_sym "Size"
let sym_Skew                 = decl_sym "Skew"
let sym_SkewGlyph            = decl_sym "SkewGlyph"
let sym_Skyline              = decl_sym "Skyline"
let sym_Space                = decl_sym "Space"
let sym_SpaceFactor          = decl_sym "SpaceFactor"
let sym_SpaceParams          = decl_sym "SpaceParams"
let sym_SpaceSkip            = decl_sym "SpaceSkip"
let sym_Square               = decl_sym "Square"
let sym_Stretch              = decl_sym "Stretch"
let sym_StretchFactor        = decl_sym "StretchFactor"
let sym_StretchOrder         = decl_sym "StretchOrder"
let sym_Stroke               = decl_sym "Stroke"
let sym_SubScript            = decl_sym "SubScript"
let sym_SuperScript          = decl_sym "SuperScript"
let sym_Table                = decl_sym "Table"
let sym_TableEntry           = decl_sym "TableEntry"
let sym_TeX                  = decl_sym "TeX"
let sym_Text                 = decl_sym "Text"
let sym_TextSize             = decl_sym "TextSize"
let sym_ThickMathSkip        = decl_sym "ThickMathSkip"
let sym_ThinMathSkip         = decl_sym "ThinMathSkip"
let sym_Tolerance            = decl_sym "Tolerance"
let sym_Top                  = decl_sym "Top"
let sym_TopSkip              = decl_sym "TopSkip"
let sym_Underline            = decl_sym "Underline"
let sym_VBox                 = decl_sym "VBox"
let sym_VBoxSpread           = decl_sym "VBoxSpread"
let sym_VBoxTo               = decl_sym "VBoxTo"
let sym_VLeaders             = decl_sym "VLeaders"
let sym_Vert                 = decl_sym "Vert"
let sym_VictorianSpacing     = decl_sym "VictorianSpacing"
let sym_VInsert              = decl_sym "VInsert"
let sym_WidowPenalty         = decl_sym "WidowPenalty"
let sym_Width                = decl_sym "Width"
let sym_XSpaceSkip           = decl_sym "XSpaceSkip"

let decode_symbol _name v = match !v with Types.Symbol s -> s | _ -> 0
let decode_int name v = Tools.XNum.int_of_num (Machine.decode_num name v)
let decode_bool _name v = match !v with Types.Bool b -> b | _ -> false
let decode_char _name v = match !v with Types.Char c -> c | _ -> 0
let decode_option name f v = match !v with Types.Symbol s when s = sym_None -> None | _ -> Some (f name v)
let decode_uc_string name v = Array.of_list (Machine.decode_string name v)
let decode_tuple name v = match !v with Types.Tuple a -> a | _ -> Types.runtime_error (name ^ ": tuple expected")

let decode_dict name d = match !d with
  | Types.Dictionary dict -> dict
  | _ -> Types.runtime_error (name ^ ": dictionary expected")

let lookup decode dict sym =
  try Some (decode (SymbolMap.find sym dict))
  with Not_found -> None

let lookup_string name dict sym = lookup (fun v -> Array.of_list (Machine.decode_string name v)) dict sym
let lookup_bool _name dict sym = lookup (fun v -> match !v with Types.Bool b -> b | _ -> false) dict sym
let lookup_int name dict sym = lookup (fun v -> Tools.XNum.int_of_num (Machine.decode_num name v)) dict sym
let lookup_num name dict sym = lookup (Machine.decode_num name) dict sym
let lookup_symbol _name dict sym = lookup (fun v -> match !v with Types.Symbol s -> s | _ -> 0) dict sym
let lookup_dict name dict sym = lookup (decode_dict name) dict sym
let lookup_tuple _name dict sym = lookup (fun v -> match !v with Types.Tuple t -> t | _ -> [||]) dict sym
let lookup_list name dict sym = lookup (Machine.decode_list name) dict sym

let decode_opaque name f _opaque_name v =
  match !v with
  | Types.Opaque o -> f o
  | _ -> Types.runtime_error (name ^ ": opaque expected")

let decode_math_style _name s = match !s with
  | Types.Symbol s when s = sym_Display        -> MathLayout.Display
  | Types.Symbol s when s = sym_CrampedDisplay -> MathLayout.CrampedDisplay
  | Types.Symbol s when s = sym_Text           -> MathLayout.Text
  | Types.Symbol s when s = sym_CrampedText    -> MathLayout.CrampedText
  | Types.Symbol s when s = sym_Script         -> MathLayout.Script
  | Types.Symbol s when s = sym_CrampedScript  -> MathLayout.CrampedScript
  | Types.Symbol s when s = sym_Script2        -> MathLayout.Script2
  | Types.Symbol s when s = sym_CrampedScript2 -> MathLayout.CrampedScript2
  | _ -> Types.runtime_error "invalid math style"

let decode_mode name m = match !m with
  | Types.Symbol s ->
      if s = sym_Preamble then `Preamble
      else if s = sym_Galley then `Galley
      else if s = sym_Paragraph then `Paragraph
      else if s = sym_Math then `Math
      else if s = sym_HBox then `HBox
      else if s = sym_LRBox then `LRBox
      else if s = sym_RLBox then `RLBox
      else if s = sym_VBox then `VBox
      else if s = sym_Table then `Table
      else Types.runtime_error (name ^ ": invalid mode")
  | _ -> Types.runtime_error (name ^ ": invalid mode")

let decode_hbox_dir name d = match !d with
  | Types.Symbol s ->
      if s = sym_LR then `LR
      else if s = sym_RL then `RL
      else if s = sym_Default then `Default
      else Types.runtime_error (name ^ ": invalid direction")
  | _ -> Types.runtime_error (name ^ ": invalid direction")

let encode_colour col = match col with
  | `Grey g           -> Types.Tuple [| ref (Types.Symbol sym_Grey); ref (Types.Number g) |]
  | `RGB (r, g, b)    -> Types.Tuple [| ref (Types.Symbol sym_RGB); ref (Types.Number r); ref (Types.Number g); ref (Types.Number b) |]
  | `CMYK (c, m, y, k) -> Types.Tuple [| ref (Types.Symbol sym_CMYK); ref (Types.Number c); ref (Types.Number m); ref (Types.Number y); ref (Types.Number k) |]

let decode_colour name col = match !col with
  | Types.Tuple xs when Array.length xs > 0 ->
      (match !(xs.(0)) with
      | Types.Symbol s ->
          if s = sym_Grey && Array.length xs = 2 then `Grey (Machine.decode_num name xs.(1))
          else if s = sym_RGB && Array.length xs = 4 then
            `RGB (Machine.decode_num name xs.(1), Machine.decode_num name xs.(2), Machine.decode_num name xs.(3))
          else if s = sym_CMYK && Array.length xs = 5 then
            `CMYK (Machine.decode_num name xs.(1), Machine.decode_num name xs.(2), Machine.decode_num name xs.(3), Machine.decode_num name xs.(4))
          else Types.runtime_error (name ^ ": colour expected")
      | _ -> Types.runtime_error (name ^ ": colour expected"))
  | _ -> Types.runtime_error (name ^ ": colour expected")

let encode_math_style s = match s with
  | MathLayout.Display        -> Types.Symbol sym_Display
  | MathLayout.CrampedDisplay -> Types.Symbol sym_CrampedDisplay
  | MathLayout.Text           -> Types.Symbol sym_Text
  | MathLayout.CrampedText    -> Types.Symbol sym_CrampedText
  | MathLayout.Script         -> Types.Symbol sym_Script
  | MathLayout.CrampedScript  -> Types.Symbol sym_CrampedScript
  | MathLayout.Script2        -> Types.Symbol sym_Script2
  | MathLayout.CrampedScript2 -> Types.Symbol sym_CrampedScript2

let encode_mode m = match m with
  | `Preamble  -> Types.Symbol sym_Preamble
  | `Galley    -> Types.Symbol sym_Galley
  | `Paragraph -> Types.Symbol sym_Paragraph
  | `Math      -> Types.Symbol sym_Math
  | `HBox      -> Types.Symbol sym_HBox
  | `LRBox     -> Types.Symbol sym_LRBox
  | `RLBox     -> Types.Symbol sym_RLBox
  | `VBox      -> Types.Symbol sym_VBox
  | `Table     -> Types.Symbol sym_Table

let encode_hbox_dir d = match d with
  | `LR      -> Types.Symbol sym_LR
  | `RL      -> Types.Symbol sym_RL
  | `Default -> Types.Symbol sym_Default

let encode_location loc = match loc with (f, l, c) ->
  Types.Tuple [| ref (Types.Tuple (Array.map (fun c -> ref (Types.Char c)) (Unicode.UString.uc_string_of_ascii f))); ref (Types.Number (Tools.XNum.num_of_int l)); ref (Types.Number (Tools.XNum.num_of_int c)) |]

let decode_location _name loc = match !loc with
  | Types.Tuple [| f; l; c |] -> (match !f, !l, !c with
      | Types.Tuple fs, Types.Number ln, Types.Number cn -> 
          let fs_chars = Array.map (fun c_ref -> match !c_ref with Types.Char c -> c | _ -> 0) fs in
          (Unicode.UString.uc_string_to_ascii fs_chars, Tools.XNum.int_of_num ln, Tools.XNum.int_of_num cn)
      | _ -> ("", 0, 0))
  | _ -> ("", 0, 0)

let encode_index_position p = match p with
  | Runtime.MathTypes.LeftIndex  -> Types.Symbol sym_Left
  | Runtime.MathTypes.RightIndex -> Types.Symbol sym_Right
  | Runtime.MathTypes.VertIndex  -> Types.Symbol sym_Vert

let decode_index_position name p = match !p with
  | Types.Symbol s ->
      if s = sym_Left then Runtime.MathTypes.LeftIndex
      else if s = sym_Right then Runtime.MathTypes.RightIndex
      else if s = sym_Vert then Runtime.MathTypes.VertIndex
      else Types.runtime_error (name ^ ": index position expected")
  | _ -> Types.runtime_error (name ^ ": index position expected")

let encode_math_code c = match c with
  | Runtime.MathTypes.NoMath      -> Types.Symbol sym_NoMath
  | Runtime.MathTypes.Ordinary    -> Types.Symbol sym_Ordinary
  | Runtime.MathTypes.BinOp       -> Types.Symbol sym_BinOp
  | Runtime.MathTypes.Relation    -> Types.Symbol sym_Relation
  | Runtime.MathTypes.Operator    -> Types.Symbol sym_Operator
  | Runtime.MathTypes.Punct       -> Types.Symbol sym_Punct
  | Runtime.MathTypes.Open        -> Types.Symbol sym_Open
  | Runtime.MathTypes.Close       -> Types.Symbol sym_Close
  | Runtime.MathTypes.Inner       -> Types.Symbol sym_Inner
  | Runtime.MathTypes.SubScript   -> Types.Symbol sym_SubScript
  | Runtime.MathTypes.SuperScript -> Types.Symbol sym_SuperScript
  | Runtime.MathTypes.IndexPosition p ->
      let v = Array.make 2 (ref Types.Unbound) in
      v.(0) <- ref (Types.Symbol sym_IndexPosition);
      v.(1) <- ref (encode_index_position p);
      Types.Tuple v

let decode_math_code name c = match !c with
  | Types.Symbol s ->
      if s = sym_NoMath then Runtime.MathTypes.NoMath
      else if s = sym_Ordinary then Runtime.MathTypes.Ordinary
      else if s = sym_BinOp then Runtime.MathTypes.BinOp
      else if s = sym_Relation then Runtime.MathTypes.Relation
      else if s = sym_Operator then Runtime.MathTypes.Operator
      else if s = sym_Punct then Runtime.MathTypes.Punct
      else if s = sym_Open then Runtime.MathTypes.Open
      else if s = sym_Close then Runtime.MathTypes.Close
      else if s = sym_Inner then Runtime.MathTypes.Inner
      else if s = sym_SubScript then Runtime.MathTypes.SubScript
      else if s = sym_SuperScript then Runtime.MathTypes.SuperScript
      else Types.runtime_error (name ^ ": math code expected")
  | Types.Tuple [| s; p |] ->
      if decode_symbol name s = sym_IndexPosition then
        Runtime.MathTypes.IndexPosition (decode_index_position name p)
      else Types.runtime_error (name ^ ": math code expected")
  | _ -> Types.runtime_error (name ^ ": math code expected")
