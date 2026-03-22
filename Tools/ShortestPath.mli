
open XNum

type ('graph, 'aux, 'solution) update_solution =
  'graph -> 'solution list -> 'aux -> int -> int -> 'solution list ->
    'solution list * 'aux option
type 'graph is_forced = 'graph -> int -> bool
type ('graph, 'solution, 'result) compute_path =
  'graph -> 'solution list array -> 'result

val find_shortest_path :
  ('graph, 'aux, 'solution) update_solution -> 'graph is_forced ->
    ('graph, 'solution, 'result) compute_path -> 'solution -> 'aux ->
    'graph -> int -> 'result

