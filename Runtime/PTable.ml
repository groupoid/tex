
open Unicode.UTypes

type 'a table = {
  current : 'a;
  key : uc_string;
  table : 'a Unicode.DynUCTrie.t;
}

let empty s v = { current = v; key = s; table = Unicode.DynUCTrie.add_string s v Unicode.DynUCTrie.empty }
let is_empty t = Unicode.DynUCTrie.is_empty t.table
let key t = t.key
let table t = t.table
let select t s =
  try { t with current = Unicode.DynUCTrie.find_string s t.table; key = s }
  with Not_found -> raise Not_found
let add t s v = { current = v; key = s; table = Unicode.DynUCTrie.add_string s v t.table }
let get t s = Unicode.DynUCTrie.find_string s t.table
let set t v = { t with current = v }
let current t = t.current
let sync t = { t with table = Unicode.DynUCTrie.add_string t.key t.current t.table }
let map f t = { current = f t.current; key = t.key; table = Unicode.DynUCTrie.map f t.table }
let mapi f t = { current = f t.key t.current; key = t.key; table = Unicode.DynUCTrie.mapi f t.table }
let iter f t = Unicode.DynUCTrie.iter f t.table
let fold f t acc = Unicode.DynUCTrie.fold f t.table acc
let update t tab = { t with table = tab }
