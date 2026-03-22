
type 'a bezier = 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a

type 'a path = 'a bezier list
type path_cmd = [ `Stroke | `Fill | `Clip ]
type line_cap = [ `Butt | `Circle | `Square ]
type line_join = [ `Miter | `Round | `Bevel ]

type colour = [ `RGB of (Tools.XNum.num * Tools.XNum.num * Tools.XNum.num) | `Grey of Tools.XNum.num | `CMYK of (Tools.XNum.num * Tools.XNum.num * Tools.XNum.num * Tools.XNum.num) ]

type ('dim, 'box) graphic_command =
  | SetColour of colour
  | SetBgColour of colour
  | SetAlpha of Tools.XNum.num
  | Draw of path_cmd * 'dim path
  | Fill of path_cmd * 'dim path
  | Clip of 'dim path
  | PutBox of 'dim * 'dim * 'box * int option
  | SetLineWidth of 'dim
  | SetLineCap of line_cap
  | SetLineJoin of line_join
  | SetMiterLimit of Tools.XNum.num

val get_bounding_box : string -> (int * int * int * int) option
val get_postscript_size : string -> (Tools.XNum.num * Tools.XNum.num)
val get_bmp_size : string -> (Tools.XNum.num * Tools.XNum.num)

val command_to_string : ('a, 'b) graphic_command -> string

val compare_colour : colour -> colour -> int
