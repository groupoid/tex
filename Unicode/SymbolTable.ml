
open UTypes

type symbol = int

(* tables for mappings string -> int and int -> string *)
let symbol_table = Hashtbl.create 500
let name_table   = ref (Array.make 500 [||])
let table_size   = ref 1

(*
  We add the empty string as symbol since, for symbols obtained directly by |alloc_symbol|,
  |symbol_to_string| returns the empty string. Consequently, |string_to_symbol []| will not
  be equal to such a symbol. This ensures that these symbols cannot be faked.
*)

let () = Hashtbl.add symbol_table [||] 0

let alloc_symbol () =
  let n = !table_size in
  table_size := n + 1;

  (* increase <name_table> if necessary *)
  if Array.length !name_table <= n then
    name_table :=
      Array.init (2 * Array.length !name_table)
                 (fun i -> if i < Array.length !name_table then
                             (!name_table).(i)
                           else
                             [||])
  else ();
  n

let add_symbol (str : uc_string) =
  let n = alloc_symbol () in
  (* add string to <symbol_table> and <name_table> *)
  Hashtbl.add symbol_table str n;
  (!name_table).(n) <- str;
  n

let string_to_symbol str =
  try
    Hashtbl.find symbol_table str
  with
  | Not_found -> add_symbol str

let symbol_to_string sym = (!name_table).(sym)

module SymbolMap = Map.Make(struct
  type t = int
  let compare (a : int) (b : int) = compare a b
end)

module SymbolSet = Set.Make(struct
  type t = int
  let compare (a : int) (b : int) = compare a b
end)

(*
  Returns an associative list of all elements of <map>. The current implementation
  of Map ensures that the resulting list is sorted in ascending order.
  HACK: We rely on this fact.
*)

let map_to_list map = SymbolMap.fold (fun k v l -> (k, v) :: l) map []
