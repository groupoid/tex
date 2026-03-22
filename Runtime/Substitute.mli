
open Tools.XNum

type substitute_item =
  | Margin
  | LetterSpace of num
  | Font of string
  | Lig of int
  | Kern of num

val substitute : string -> (int * substitute_item) list -> string

val first_real_item : 'a list -> int -> int -> 'a option
val last_real_item  : 'a list -> int -> int -> 'a option
