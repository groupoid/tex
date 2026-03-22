
exception Type_error

type 'a opaque

val declare_type : string -> ('b -> 'a -> 'a) -> ('b -> 'b -> bool) -> ('b -> 'b -> bool) ->
                     (('b -> 'a opaque) * ('a opaque -> 'b))
val type_name    : 'a opaque -> string
val same_type    : 'a opaque -> 'a opaque -> bool
val apply        : 'a opaque -> 'a -> 'a
val compare      : 'a opaque -> 'a opaque -> bool
val unify        : 'a opaque -> 'a opaque -> bool
