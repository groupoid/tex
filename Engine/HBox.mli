
open Tools.XNum
open Runtime
open Dim
open Box

type direction = LR | RL

val dimensions : box list -> dim * dim * dim
val calc_xwidth : box list -> xdim
val calc_width : box list -> dim
val calc_width_and_glue : box list -> xdim * xdim list

val make : direction -> box list -> box
val make_to : direction -> num -> box list -> box
val make_scaled : direction -> num -> box list -> box
val make_spread : direction -> num -> box list -> box

