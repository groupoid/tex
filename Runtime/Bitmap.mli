
type bitmap = {
  bm_width : int;
  bm_height : int;
  bm_bytes_per_row : int;
  mutable bm_data : bytes;
}

val make : int -> int -> bitmap

val point : bitmap -> int -> int -> bool
val set_point : bitmap -> int -> int -> unit
val unset_point : bitmap -> int -> int -> unit

val set_line : bitmap -> int -> int -> int -> unit
val unset_line : bitmap -> int -> int -> int -> unit

val copy_line : bitmap -> int -> int -> unit

val print : Tools.IO.ostream -> bitmap -> string -> string -> string -> unit
