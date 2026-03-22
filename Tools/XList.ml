
include List

let rec repeat n x =
  if n <= 0 then []
  else x :: repeat (n - 1) x

let rec span p l =
  match l with
  | [] -> [], []
  | x :: xs ->
      if p x then
        let (l1, l2) = span p xs in
        (x :: l1, l2)
      else
        ([], l)

let from_array = Array.to_list

let append_sub_array arr start stop tail =
  let rec iter i acc =
    if i < start then acc
    else iter (i - 1) (arr.(i) :: acc)
  in
  iter stop tail
let from_sub_array arr start stop = append_sub_array arr start stop []
