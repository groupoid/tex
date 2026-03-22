module IntMap = Map.Make(Int)

type 'a charmap = {
  mutable map : 'a IntMap.t;
  default : 'a option;
}

let empty () = { map = IntMap.empty; default = None }
let create d = { map = IntMap.empty; default = Some d }
let copy c = { map = c.map; default = c.default }
let add c k v = { c with map = IntMap.add k v c.map }
let set c k v = c.map <- IntMap.add k v c.map
let find c k =
  try IntMap.find k c.map
  with Not_found ->
    match c.default with
    | Some d -> d
    | None -> raise Not_found

let lookup = find
let mem c k = IntMap.mem k c.map
let iter f c = IntMap.iter f c.map
let fold f c a = IntMap.fold f c.map a

let build blocks =
  (* Use the first element of the first block as a tentative default if needed,
     or just stick to no default if not specified.
     However, standard charmaps often want 0 or similar.
  *)
  let c = empty () in
  Array.iteri (fun i block ->
    Array.iteri (fun j v ->
      set c (i * 256 + j) v
    ) block
  ) blocks;
  c

let iter_classes f c =
  let tbl = Hashtbl.create 16 in
  IntMap.iter (fun k v ->
    let l = try Hashtbl.find tbl v with Not_found -> [] in
    Hashtbl.replace tbl v (k :: l)
  ) c.map;
  Hashtbl.iter f tbl
