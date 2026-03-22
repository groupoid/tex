
type uc_char = int
type uc_list = uc_char list
type uc_string = uc_char array

type location = (string * int * int)

module OrderedChar =
  struct
    type t = uc_char
    let compare : uc_char -> uc_char -> int = compare
  end

module DynamicCharMap = Map.Make (OrderedChar)

