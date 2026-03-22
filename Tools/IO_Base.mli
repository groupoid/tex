
type -'a io

type io_r   = [`IO_R]
type io_w   = [`IO_W]
type io_s   = [`IO_S]
type io_rs  = [io_r | io_s]
type io_ws  = [io_w | io_s]
type io_rw  = [io_r | io_w]
type io_rws = [io_r | io_w | io_s]

type istream   = io_r io
type irstream  = io_rs io
type ostream   = io_w io
type orstream  = io_ws io
type iostream  = io_rw io
type iorstream = io_rws io

val io_make : (unit -> unit) -> (unit -> char) -> (int -> string) -> (unit -> bool) -> (char -> unit) -> (string -> unit) -> (unit -> int) -> (unit -> int) -> (unit -> int) -> (int -> unit) -> 'a io
val io_make_read : (unit -> unit) -> (unit -> char) -> (int -> string) -> (unit -> bool) -> istream
val io_make_write : (unit -> unit) -> (char -> unit) -> (string -> unit) -> (unit -> int) -> ostream
val io_make_read_seek : (unit -> unit) -> (unit -> char) -> (int -> string) -> (unit -> bool) -> (unit -> int) -> (unit -> int) -> (int -> unit) -> irstream
val io_make_write_seek : (unit -> unit) -> (char -> unit) -> (string -> unit) -> (unit -> int) -> (unit -> int) -> (unit -> int) -> (int -> unit) -> orstream
val io_make_read_write : (unit -> unit) -> (unit -> char) -> (int -> string) -> (unit -> bool) -> (char -> unit) -> (string -> unit) -> (unit -> int) -> iostream

val io_free          : 'a io -> unit
val io_read_char     : [> io_r] io -> char
val io_read_string   : [> io_r] io -> int -> string
val io_eof           : [> io_r] io -> bool
val io_write_char    : [> io_w] io -> char -> unit
val io_write_string  : [> io_w] io -> string -> unit
val io_bytes_written : [> io_w] io -> int
val io_size          : [> io_s] io -> int
val io_pos           : [> io_s] io -> int
val io_seek          : [> io_s] io -> int -> unit

val io_coerce_i   : 'a io -> istream
val io_coerce_o   : 'a io -> ostream
val io_coerce_o_ior : ostream -> iorstream
val io_coerce_ir  : 'a io -> irstream
val io_coerce_or  : 'a io -> orstream
val io_coerce_io  : 'a io -> iostream
val io_coerce_ior : 'a io -> iorstream
