
open XNum
open Unicode.UTypes
open Unicode.SymbolTable
open Runtime

open Types

val sym_Accent : symbol
val sym_AddToGalley : symbol
val sym_AdjDemerits : symbol
val sym_Adjustments : symbol
val sym_Alignment : symbol
val sym_AutoLigatures : symbol
val sym_Base : symbol
val sym_BaselineSkip : symbol
val sym_BeginGroup : symbol
val sym_Bevel : symbol
val sym_BinOp : symbol
val sym_BinOpPenalty : symbol
val sym_Bitmap : symbol
val sym_Bmp : symbol
val sym_Bool : symbol
val sym_BorderKern : symbol
val sym_Bottom : symbol
val sym_BottomSkip : symbol
val sym_Break : symbol
val sym_Butt : symbol
val sym_Circle : symbol
val sym_Clip : symbol
val sym_Close : symbol
val sym_ClubPenalty : symbol
val sym_CMYK : symbol
val sym_Command : symbol
val sym_CommandBox : symbol
val sym_CrampedDisplay : symbol
val sym_CrampedScript : symbol
val sym_CrampedScript2 : symbol
val sym_CrampedText : symbol
val sym_Default : symbol
val sym_DelimiterFactor : symbol
val sym_DelimiterShortfall : symbol
val sym_Direct : symbol
val sym_Display : symbol
val sym_DoubleHyphenDemerits : symbol
val sym_Dpi : symbol
val sym_EmergencyStretch : symbol
val sym_Encoding : symbol
val sym_EndGroup : symbol
val sym_ExHyphenPenalty : symbol
val sym_Family : symbol
val sym_Fill : symbol
val sym_FinalHyphenDemerits : symbol
val sym_Fixed : symbol
val sym_Float : symbol
val sym_FloatSep : symbol
val sym_Footnote : symbol
val sym_Fraction : symbol
val sym_Galley : symbol
val sym_GfxCommand : symbol
val sym_Glyph : symbol
val sym_Glue : symbol
val sym_Grey : symbol
val sym_GridSize : symbol
val sym_HBox : symbol
val sym_HBoxSpread : symbol
val sym_HBoxTo : symbol
val sym_Height : symbol
val sym_HLeaders : symbol
val sym_Hyphen : symbol
val sym_HyphenGlyph : symbol
val sym_HyphenParams : symbol
val sym_HyphenPenalty : symbol
val sym_HyphenTable : symbol
val sym_Image : symbol
val sym_IndexPosition : symbol
val sym_Inner : symbol
val sym_Kern : symbol
val sym_Leading : symbol
val sym_Left : symbol
val sym_LeftHyphenMin : symbol
val sym_LeftRight : symbol
val sym_LeftSkip : symbol
val sym_Letter : symbol
val sym_LetterSpacing : symbol
val sym_Ligature : symbol
val sym_LineBreakParams : symbol
val sym_LineParams : symbol
val sym_LinePenalty : symbol
val sym_LineSkip : symbol
val sym_LineSkipLimit : symbol
val sym_Looseness : symbol
val sym_LR : symbol
val sym_LRBox : symbol
val sym_Mandantory : symbol
val sym_Math : symbol
val sym_MathAccent : symbol
val sym_MathChar : symbol
val sym_MathCode : symbol
val sym_MathFamily : symbol
val sym_MathParams : symbol
val sym_MathStyle : symbol
val sym_Measure : symbol
val sym_MedMathSkip : symbol
val sym_MinSize : symbol
val sym_Miter : symbol
val sym_ModifyGalleyGlue : symbol
val sym_Name : symbol
val sym_NewArea : symbol
val sym_NewGalley : symbol
val sym_NewLayout : symbol
val sym_NewMarks : symbol
val sym_NoMath : symbol
val sym_None : symbol
val sym_NullDelimiterSpace : symbol
val sym_OldMarks : symbol
val sym_Open : symbol
val sym_Operator : symbol
val sym_Optional : symbol
val sym_Ordinary : symbol
val sym_Overline : symbol
val sym_PageNo : symbol
val sym_Paragraph : symbol
val sym_ParIndent : symbol
val sym_ParFillSkip : symbol
val sym_ParParams : symbol
val sym_ParShape : symbol
val sym_ParSkip : symbol
val sym_PDF : symbol
val sym_Phantom : symbol
val sym_PostProcessLine : symbol
val sym_PostScript : symbol
val sym_Preamble : symbol
val sym_PreTolerance : symbol
val sym_Punct : symbol
val sym_PutBox : symbol
val sym_PutGalleyInVBox : symbol
val sym_Register : symbol
val sym_Relation : symbol
val sym_RelPenalty : symbol
val sym_RGB : symbol
val sym_Right : symbol
val sym_RightHyphenMin : symbol
val sym_RightSkip : symbol
val sym_RiverDemerits : symbol
val sym_RiverThreshold : symbol
val sym_RL: symbol
val sym_RLBox : symbol
val sym_Root : symbol
val sym_Round : symbol
val sym_Rule : symbol
val sym_Scale : symbol
val sym_Script : symbol
val sym_ScriptLang : symbol
val sym_Script2 : symbol
val sym_ScriptSpace : symbol
val sym_ScriptSize : symbol
val sym_Script2Size : symbol
val sym_Separator : symbol
val sym_Series : symbol
val sym_SetAlpha : symbol
val sym_SetBgColour : symbol
val sym_SetColour : symbol
val sym_SetFont : symbol
val sym_SetLineCap : symbol
val sym_SetLineJoin : symbol
val sym_SetLineWidth : symbol
val sym_SetMiterLimit : symbol
val sym_Shape : symbol
val sym_ShipOut : symbol
val sym_Shrink : symbol
val sym_ShrinkFactor : symbol
val sym_ShrinkOrder : symbol
val sym_SimpleBreaking : symbol
val sym_Size : symbol
val sym_Skew : symbol
val sym_SkewGlyph : symbol
val sym_Skyline : symbol
val sym_Space : symbol
val sym_SpaceFactor : symbol
val sym_SpaceParams : symbol
val sym_SpaceSkip : symbol
val sym_Square : symbol
val sym_Stretch : symbol
val sym_StretchFactor : symbol
val sym_StretchOrder : symbol
val sym_Stroke : symbol
val sym_SubScript : symbol
val sym_SuperScript : symbol
val sym_Table : symbol
val sym_TableEntry : symbol
val sym_TeX : symbol
val sym_Text : symbol
val sym_TextSize : symbol
val sym_ThickMathSkip : symbol
val sym_ThinMathSkip : symbol
val sym_Tolerance : symbol
val sym_Top : symbol
val sym_TopSkip : symbol
val sym_Underline : symbol
val sym_VBox : symbol
val sym_VBoxSpread : symbol
val sym_VBoxTo : symbol
val sym_Vert : symbol
val sym_VictorianSpacing : symbol
val sym_VInsert : symbol
val sym_WidowPenalty : symbol
val sym_Width : symbol
val sym_XSpaceSkip : symbol

val decode_symbol : string -> unknown -> symbol
val decode_int : string -> unknown -> int
val decode_bool : string -> unknown -> bool
val decode_char : string -> unknown -> int
val decode_option :
  string -> (string -> unknown -> 'a) -> unknown -> 'a option
val decode_uc_string : string -> unknown -> uc_string
val decode_tuple : string -> unknown -> unknown array
val decode_dict : string -> unknown -> unknown SymbolMap.t

val lookup : (unknown -> 'a) -> unknown SymbolMap.t -> symbol -> 'a option

val lookup_string :
  string -> unknown SymbolMap.t -> symbol -> uc_string option
val lookup_bool : string -> unknown SymbolMap.t -> symbol -> bool option
val lookup_int : string -> unknown SymbolMap.t -> symbol -> int option
val lookup_num : string -> unknown SymbolMap.t -> symbol -> num option
val lookup_symbol : string -> unknown SymbolMap.t -> symbol -> symbol option
val lookup_dict :
  string -> unknown SymbolMap.t -> symbol -> unknown SymbolMap.t option
val lookup_tuple :
  string -> unknown SymbolMap.t -> symbol -> unknown array option
val lookup_list :
  string -> unknown SymbolMap.t -> symbol -> unknown list option

val decode_opaque :
  string -> (unknown Opaque.opaque -> 'a) -> string -> unknown -> 'a

val encode_location : UCStream.location -> partial_value
val encode_index_position : Box.index_position -> partial_value
val encode_math_code : Box.math_code -> partial_value
val encode_math_style : MathLayout.math_style -> partial_value
val encode_mode : ParseState.mode -> partial_value
val encode_hbox_dir : [ `LR | `RL| `Default ] -> partial_value
val encode_colour : Graphic.colour -> partial_value

val decode_location : string -> unknown -> UCStream.location
val decode_index_position :
  string -> unknown -> Box.index_position
val decode_math_code : string -> unknown -> Box.math_code
val decode_math_style : string -> unknown -> MathLayout.math_style
val decode_mode : string -> unknown -> ParseState.mode
val decode_hbox_dir : string -> unknown -> [ `LR | `RL| `Default ]
val decode_colour : string -> unknown -> Graphic.colour

