open Runtime
open Vm_types
open Unicode.UTypes
open Runtime

val encode_gfx_cmd :
  (Environment.dim_arg, Box.box) Graphic.graphic_command -> Types.partial_value
val decode_gfx_cmd :
  string -> Types.unknown -> (Environment.dim_arg, Box.box) Graphic.graphic_command

val encode_line_cap : Runtime.Graphic.line_cap -> Types.partial_value
val encode_line_join : Runtime.Graphic.line_join -> Types.partial_value
val decode_line_cap : string -> Types.unknown -> Runtime.Graphic.line_cap
val decode_line_join : string -> Types.unknown -> Runtime.Graphic.line_join
