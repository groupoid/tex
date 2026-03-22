
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

let command_to_string _ = "gfx-command"
let get_bounding_box _ = None
let get_postscript_size _ = (Tools.XNum.num_zero, Tools.XNum.num_zero)
let get_postscript_size _ = (Tools.XNum.num_zero, Tools.XNum.num_zero)
let get_bmp_size _ = (Tools.XNum.num_zero, Tools.XNum.num_zero)

let dummy () = ()

let compare_colour = Stdlib.compare
