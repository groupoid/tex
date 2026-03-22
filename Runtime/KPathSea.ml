open Unicode.UTypes

type file_type = [ `PK | `Source | `TeX | `TFM| `Type1 | `TrueType | `OpenType ]

let init _ _ _ = ()

let kpsewhich_cache = Hashtbl.create 16

let find_file name _ _ =
  try
    Hashtbl.find kpsewhich_cache name
  with Not_found ->
    let cmd = "kpsewhich " ^ (Filename.quote name) in
    let ic = Unix.open_process_in cmd in
    let path =
      try
        let res = input_line ic in
        let _ = Unix.close_process_in ic in
        String.trim res
      with End_of_file ->
        let _ = Unix.close_process_in ic in
        name
    in
    if path <> "" && path <> name then begin
      Hashtbl.add kpsewhich_cache name path;
      path
    end else
      name

let find_file_of_type _ name = find_file name `TFM false

let find_glyph name _ = name
