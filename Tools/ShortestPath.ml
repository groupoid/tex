
open XNum

type ('graph, 'aux, 'solution) update_solution =
  'graph -> 'solution list -> 'aux -> int -> int -> 'solution list ->
    'solution list * 'aux option

type 'graph is_forced = 'graph -> int -> bool
type ('graph, 'solution, 'result) compute_path =
  'graph -> 'solution list array -> 'result

let find_shortest_path _ _ _ _ _ _ _ = failwith "Not implemented"

let shortest_path _ _ = []
