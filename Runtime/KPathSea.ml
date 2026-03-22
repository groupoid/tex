
type file_type = [ `PK | `Source | `TeX | `TFM| `Type1 | `TrueType | `OpenType ]

let init _ _ _ = ()

let find_file name _ _ =
  if Sys.file_exists name then name
  else name

let find_glyph name _ = name
