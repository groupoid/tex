
open Runtime

open Types
(* open Typesetting *)
open Engine
open Environment
open Node

val encode_box_cmd : Box.box_cmd -> partial_value
val decode_box_cmd : string -> unknown -> Box.box_cmd

val encode_area_contents : area_contents_arg -> partial_value
val decode_area_contents : string -> unknown -> area_contents_arg

val encode_glue_function :
  (environment -> Box.box list -> Box.box list) -> partial_value
val decode_glue_function :
  string -> unknown -> environment -> Box.box list -> Box.box list

val encode_node : node -> partial_value
val encode_node_list : node list -> partial_value
val decode_node : string -> unknown -> node
val decode_node_list : string -> unknown -> node list

