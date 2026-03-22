
open Unicode.UTypes

type 'a trie = {
  t_tree : int array;
  t_data : 'a array;
  mutable t_data_len : int;
}

let empty () = { t_tree = [||]; t_data = [||]; t_data_len = 0 }

let get_offset trie i = trie.t_tree.(i)
let get_char trie i = trie.t_tree.(i + 1)
let get_data trie i =
  let off = trie.t_tree.(i + 2) in
  if off < 0 then None else Some trie.t_data.(off)

let rec lookup_internal trie node s i len =
  if i = len then get_data trie node
  else
    let c = s.(i) in
    let off = trie.t_tree.(node) in
    if off = 0 then None
    else
      let rec find j =
        if j >= off + trie.t_tree.(node + 1) then None
        else if trie.t_tree.(j + 1) = c then lookup_internal trie trie.t_tree.(j) s (i + 1) len
        else find (j + 2)
      in find off

let lookup trie node s = lookup_internal trie node s 0 (Array.length s)

let lookup_prefix trie node s pos =
  lookup_internal trie node s pos (Array.length s)

let rec lookup_list_internal trie node l =
  match l with
  | [] -> get_data trie node
  | c :: cs ->
      let off = trie.t_tree.(node) in
      if off = 0 then None
      else
        let rec find j =
          if j >= off + trie.t_tree.(node + 1) then None
          else if trie.t_tree.(j + 1) = c then lookup_list_internal trie trie.t_tree.(j) cs
          else find (j + 2)
        in find off

let lookup_list trie node l = lookup_list_internal trie node l

let lookup_prefix_list trie node l = lookup_list_internal trie node l

let build _ _ = { t_tree = [||]; t_data = [||]; t_data_len = 0 }
