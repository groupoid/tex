
open Unicode.UTypes

type glyph =
  { g_glyph : int;
    g_width : int;
    g_height : int;
    g_depth : int;
    g_hdpp : float;
    g_vdpp : float;
    g_min_x : int;
    g_min_y : int;
    g_max_x : int;
    g_max_y : int;
    g_bitmap : Bitmap.bitmap }

val empty_glyph : glyph

val make : int -> int -> int -> int -> float -> float -> (int * int) -> (int * int) -> glyph

val point : glyph -> int -> int -> bool
val set_point : glyph -> int -> int -> unit
val unset_point : glyph -> int -> int -> unit
val set_line : glyph -> int -> int -> int -> unit
val unset_line : glyph -> int -> int -> int -> unit
val copy_line : glyph -> int -> int -> unit
val print : Tools.IO_Base.ostream -> glyph -> string -> string -> string -> unit
val print_to_string : glyph -> Unicode.UTypes.uc_list -> Unicode.UTypes.uc_list -> Unicode.UTypes.uc_list -> Unicode.UTypes.uc_string
