
open Tools.XNum

type substitute_item =
  | Margin
  | LetterSpace of num
  | Font of string
  | Lig of int
  | Kern of num

let substitute (s:string) (_:(int * substitute_item) list) = s

let is_real x = match (Obj.magic x : [> `Glyph of 'a]) with
  | `Glyph _ | `Box _ | `Math _ | `Char _ -> true
  | _ -> false

let first_real_item items start stop =
  let rec iter i l =
    match l with
    | [] -> None
    | x :: xs ->
        if i > stop then None
        else if i >= start then
          if is_real x then Some x else iter (i + 1) xs
        else iter (i + 1) xs
  in
  iter 0 items

let last_real_item items start stop =
  let rec iter i last l =
    match l with
    | [] -> last
    | x :: xs ->
        if i > stop then last
        else if i >= start then
          if is_real x then iter (i + 1) (Some x) xs else iter (i + 1) last xs
        else iter (i + 1) last xs
  in
  iter 0 None items
