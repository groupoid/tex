
open Box

type 'a table_entry =
  { te_left : int;
    te_right : int;
    te_top : int;
    te_baseline : int;
    te_bottom : int;
    te_contents : 'a }

val make :
  int -> int -> box list table_entry list -> Galley.line_params -> box
