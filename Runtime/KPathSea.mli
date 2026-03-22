
type file_type = [ `PK | `Source | `TeX | `TFM| `Type1 | `TrueType | `OpenType ]

val init : string -> int -> string -> unit
val find_file : string -> file_type -> bool -> string
val find_glyph : string -> int -> string
