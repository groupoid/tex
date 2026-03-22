
open Tools.XNum
open Runtime
open Unicode.UTypes
open Logging
open Dim
open Runtime.Substitute
open FontMetric
open Box
open Page

type area =
  { ar_name : uc_string;
    ar_shape : area_shape;
    ar_contents : area_contents_function }

and page_update = (page * area_finaliser * page_state)

and area_contents_function =
  page -> area_shape -> (Box.extended_glyph_item list) list -> page_state -> page_update option

and page_layout =
  { pl_width : num;
    pl_height : num;
    pl_areas : area array }

and area_finaliser = page -> page

and page_state =
  { ps_page_no : int;
    ps_old_marks : (uc_string Unicode.DynUCTrie.t);
    ps_new_marks : (uc_string * uc_string) list;
    ps_galleys : (box list * Galley.galley) Unicode.DynUCTrie.t;
    ps_layouts : page_layout Unicode.DynUCTrie.t;
    ps_next_layout : page_layout;
    ps_new_floats : floating list;
    ps_badness : num;
    ps_finished : bool }

type page_run_state

val tracing_page_layout : bool ref

val simple_page_update : page -> page_state -> page_update option

val get_page_info : page -> page_state -> page_info

val new_page_run_state : int -> num -> Galley.galley Unicode.DynUCTrie.t ->
                           page_layout Unicode.DynUCTrie.t -> page_run_state
val page_no : page_run_state -> int
val get_galley_table : page_run_state -> Galley.galley Unicode.DynUCTrie.t

val layout_one_sided : page_layout -> int -> page_layout
val layout_two_sided : page_layout -> page_layout -> int -> page_layout

val layout_run_of_pages : (int -> page_layout) -> (page_run_state -> bool) -> page_run_state ->
                            (FontMetric.page list * page_run_state)

val abort_when_done : page_run_state -> bool
val abort_on_page : int -> page_run_state -> bool
