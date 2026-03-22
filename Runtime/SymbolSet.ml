
open Unicode.UTypes

module ST = Map.Make(struct type t = uc_string let compare = compare end)

type 'a trie = 'a ST.t

let empty = ST.empty
let add t s v = ST.add s v t
let find t s = ST.find s t
let mem t s = ST.mem s t
let iter f t = ST.iter f t
let fold f t acc = ST.fold f t acc
