
open Tools.XNum
open Runtime
open Dim
open Box

val calc_vert_dimensions : box list -> dim * xdim * dim
val calc_height : box list -> dim * dim

val layout_scaled : num * int -> box list -> box
val to_top : box -> box

val make : box list -> box
val make_to : num -> box list -> box
val make_scaled : num -> box list -> box
val make_spread : num -> box list -> box
val make_top : box list -> box
val make_top_to : num -> box list -> box
val make_top_scaled : num -> box list -> box
val make_top_spread : num -> box list -> box

