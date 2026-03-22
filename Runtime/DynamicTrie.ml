
open Unicode.UTypes

type 'a t = {
  children : 'a t DynamicCharMap.t;
  data     : 'a option;
}

let empty = {
  children = DynamicCharMap.empty;
  data     = None;
}

let is_empty trie = (trie.data = None) && DynamicCharMap.is_empty trie.children

let prefix trie char =
  try DynamicCharMap.find char trie.children
  with Not_found -> empty

let root_value trie = trie.data

let rec depth trie =
  DynamicCharMap.fold (fun _ child d -> max d (1 + depth child)) trie.children 0

let lookup_string str trie =
  let rec iter i trie =
    if i >= Array.length str then trie.data
    else
      try
        let next = DynamicCharMap.find str.(i) trie.children in
        iter (i+1) next
      with Not_found -> None
  in iter 0 trie

let rec lookup_list str trie =
  match str with
  | [] -> trie.data
  | c :: cs ->
      try
        let next = DynamicCharMap.find c trie.children in
        lookup_list cs next
      with Not_found -> None

let find_string str trie =
  match lookup_string str trie with
  | Some x -> x
  | None -> raise Not_found

let mem_string str trie =
  match lookup_string str trie with
  | Some _ -> true
  | None -> false

let find_list str trie =
  match lookup_list str trie with
  | Some x -> x
  | None -> raise Not_found

let mem_list str trie =
  match lookup_list str trie with
  | Some _ -> true
  | None -> false

let generic_lookup f input trie =
  let final_trie = f prefix trie input in
  final_trie.data

let generic_lookup_prefix f input trie =
  let (_, final_trie) = f (fun (best, current) char ->
    let next = prefix current char in
    if next.data <> None then (next, next) else (best, next)
  ) (empty, trie) input in
  final_trie.data

let lookup_prefix_string str trie =
  let rec iter i best current =
    if i >= Array.length str then best.data
    else
      let next = prefix current str.(i) in
      let best = if next.data <> None then next else best in
      iter (i+1) best next
  in iter 0 empty trie

let lookup_prefix_list str trie =
  let rec iter l best current =
    match l with
    | [] -> best.data
    | c :: cs ->
        let next = prefix current c in
        let best = if next.data <> None then next else best in
        iter cs best next
  in iter str empty trie

let lookup_prefix_stream stream trie =
  let rec iter best current =
    let c = Unicode.UCStream.next_char stream in
    if c < 0 then best.data
    else
        let next = prefix current c in
        if is_empty next then best.data
        else (
          ignore (Unicode.UCStream.pop stream);
          let best = if next.data <> None then next else best in
          iter best next
        )
  in iter empty trie

let add_string str data trie =
  let rec iter i trie =
    if i >= Array.length str then { trie with data = Some data }
    else
      let next = try DynamicCharMap.find str.(i) trie.children with Not_found -> empty in
      { trie with children = DynamicCharMap.add str.(i) (iter (i+1) next) trie.children }
  in iter 0 trie

let rec add_list str data trie =
  match str with
  | [] -> { trie with data = Some data }
  | c :: cs ->
      let next = try DynamicCharMap.find c trie.children with Not_found -> empty in
      { trie with children = DynamicCharMap.add c (add_list cs data next) trie.children }

let rec remove_string str trie =
  let rec iter i trie =
    if i >= Array.length str then { trie with data = None }
    else
      try
        let next = DynamicCharMap.find str.(i) trie.children in
        { trie with children = DynamicCharMap.add str.(i) (iter (i+1) next) trie.children }
      with Not_found -> trie
  in iter 0 trie

let rec remove_list str trie =
  match str with
  | [] -> { trie with data = None }
  | c :: cs ->
      try
        let next = DynamicCharMap.find c trie.children in
        { trie with children = DynamicCharMap.add c (remove_list cs next) trie.children }
      with Not_found -> trie

let rec merge t1 t2 =
  {
    data = (match t2.data with Some _ -> t2.data | None -> t1.data);
    children = DynamicCharMap.fold (fun char child acc ->
      let other = try DynamicCharMap.find char acc with Not_found -> empty in
      DynamicCharMap.add char (merge other child) acc
    ) t2.children t1.children
  }

let rec map f trie =
  {
    data = (match trie.data with Some x -> Some (f x) | None -> None);
    children = DynamicCharMap.map (map f) trie.children
  }

let mapi f trie =
  let rec iter path trie =
    {
      data = (match trie.data with Some x -> Some (f (Array.of_list (List.rev path)) x) | None -> None);
      children = DynamicCharMap.mapi (fun c child -> iter (c :: path) child) trie.children
    }
  in iter [] trie

let iter f trie =
  let rec walk path trie =
    (match trie.data with Some x -> f (Array.of_list (List.rev path)) x | None -> ());
    DynamicCharMap.iter (fun c child -> walk (c :: path) child) trie.children
  in walk [] trie

let fold f trie acc =
  let rec walk path trie acc =
    let acc = match trie.data with Some x -> f (Array.of_list (List.rev path)) x acc | None -> acc in
    DynamicCharMap.fold (fun c child acc -> walk (c :: path) child acc) trie.children acc
  in walk [] trie acc
