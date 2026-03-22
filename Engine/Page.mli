
open Tools.XNum
open Runtime
open Unicode.UTypes
open Logging
open Dim
open Box

type page =
  { p_width : num;
    p_height : num;
    p_used : unit Tools.PlanePartition.map;
    p_boxes : (num * num * box) list }

type area_shape =
  { as_pos_x : num;
    as_pos_y : num;
    as_width : num;
    as_height : num;
    as_top : num;
    as_bottom : num }

val new_page : num -> num -> page
val allocate_rect : page -> num -> num -> num -> num -> page
val put_box : page -> num -> num -> box -> page
val put_box_on_page : page -> num -> num -> box -> page

val find_place_in_area_top :
  page -> area_shape -> dim -> xdim -> dim -> (num * (num * int)) option
val find_place_in_area_bottom :
  page -> area_shape -> dim -> xdim -> dim -> (num * (num * int)) option
val find_place_in_area_left :
  page -> area_shape -> dim -> (num * (num * int)) option
val find_place_in_area_right :
  page -> area_shape -> dim -> (num * (num * int)) option

val area_free_vert : page -> area_shape -> (num * num) list
val area_free_horiz : page -> area_shape -> (num * num) list
