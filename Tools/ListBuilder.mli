
type 'a builder

val empty : unit -> 'a builder
val make : unit -> 'a builder
val add : 'a builder -> 'a -> unit
val add_list : 'a builder -> 'a list -> unit
val build : 'a builder -> 'a list
val get : 'a builder -> 'a list
val is_empty : 'a builder -> bool
val append   : 'a builder -> 'a builder -> unit
