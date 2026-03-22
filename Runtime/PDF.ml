
open Tools.XNum

type pdf_dictionary = (string * pdf_obj) list

and pdf_obj =
  | Bool of bool
  | Int of int
  | Float of float
  | Num of num
  | String of string
  | Name of string
  | Array of pdf_obj array
  | Dict of pdf_dictionary
  | Stream of (pdf_obj * string)
  | Ref of int

type 'a pdf_file = {
  mutable objects : pdf_obj array;
  mutable next_obj : int;
}

let open_pdf _ = { objects = [||]; next_obj = 0 }
let close_pdf _ = ()
let add_obj f o = 0
let add_obj_at f i o = ()
let get_next_obj_index f = f.next_obj
let write_pdf f = ()
