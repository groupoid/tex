
open Tools.XNum

type 'a pdf_file

type pdf_obj =
  | Bool of bool
  | Int of int
  | Float of float
  | Num of num
  | String of string
  | Name of string
  | Array of pdf_obj array
  | Dict of (string * pdf_obj) list
  | Stream of (pdf_obj * string)
  | Ref of int

val open_pdf : string -> 'a pdf_file
val close_pdf : 'a pdf_file -> unit

val add_obj : 'a pdf_file -> pdf_obj -> int
val add_obj_at : 'a pdf_file -> int -> pdf_obj -> unit
val get_next_obj_index : 'a pdf_file -> int

val write_pdf : 'a pdf_file -> unit
