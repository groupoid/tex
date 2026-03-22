type 'a charmap

val empty : unit -> 'a charmap
val create : 'a -> 'a charmap
val copy : 'a charmap -> 'a charmap
val add : 'a charmap -> int -> 'a -> 'a charmap
val set : 'a charmap -> int -> 'a -> unit
val find : 'a charmap -> int -> 'a
val lookup : 'a charmap -> int -> 'a
val mem : 'a charmap -> int -> bool
val iter : (int -> 'a -> unit) -> 'a charmap -> unit
val fold : (int -> 'a -> 'b -> 'b) -> 'a charmap -> 'b -> 'b
val build : 'a array array -> 'a charmap

val iter_classes : ('a -> int list -> unit) -> 'a charmap -> unit
