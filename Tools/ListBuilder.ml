
type 'a builder = 'a list ref

let empty () = ref []
let make () = ref []
let add b x = b := x :: !b
let add_list b l = b := List.rev l @ !b
let build b = List.rev !b
let get b = List.rev !b
let is_empty b = !b = []
let append b1 b2 = b1 := !b2 @ !b1
