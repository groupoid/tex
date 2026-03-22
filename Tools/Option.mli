
val maybe : 'b -> ('a -> 'b) -> 'a option -> 'b
val is_some : 'a option -> bool
val is_none : 'a option -> bool
val from_some : 'a option -> 'a
val from_option : 'a -> 'a option -> 'a
val compare : 'a -> 'a option -> bool
val compareq : 'a -> 'a option -> bool
val to_list : 'a option -> 'a list
val from_list : 'a list -> 'a option
val concat : 'a option list -> 'a list
val map : ('a -> 'b option) -> 'a list -> 'b list
