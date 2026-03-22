
open Tools.XNum
open Runtime
open Unicode.UTypes
(* open Typesetting *)
open Environment

val tracing_engine : bool ref

val const_pt : num -> skip_arg
val const_em : num -> skip_arg
val const_ex : num -> skip_arg
val const_mu : num -> skip_arg
val const_fixed_dim : skip_arg -> dim_arg

val eval_node_list :
  environment -> Builder.builder_interface -> Node.node list -> environment
val evaluate : Node.node list -> FontMetric.page list

