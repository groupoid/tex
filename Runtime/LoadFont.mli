
open Tools.XNum
open Unicode.UTypes
open FontMetric

(* |load_font <name> <params>| tries to load the given font. *)

val load_font : string -> font_load_params -> font_metric

