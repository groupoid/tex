
open IO_Base

type istream = IO_Base.istream
type irstream = IO_Base.irstream
type ostream = IO_Base.ostream
type orstream = IO_Base.orstream
type iostream = IO_Base.iostream
type iorstream = IO_Base.iorstream

val open_in : string -> istream
val open_out : string -> ostream
val close_in : istream -> unit
val close_out : ostream -> unit
val eof : istream -> bool
val to_channel : [> io_r] io -> out_channel -> unit
val consume_stream : [> io_r] io -> (string -> unit) -> unit
val to_string : [> io_r] io -> string

val coerce_i   : istream -> istream
val coerce_o   : 'a io -> ostream
val coerce_ir  : irstream -> irstream
val coerce_or  : orstream -> orstream
val coerce_io  : iostream -> iostream
val coerce_ior : iorstream -> iorstream
val coerce_ir_i : irstream -> istream
val coerce_o_ior : ostream -> iorstream
val coerce_ior_ir : iorstream -> irstream

val make_buffer_stream : int -> iorstream
val make_string_stream : string -> irstream

val read_char : [> io_r] io -> char
val read_string : [> io_r] io -> int -> string
val read_utf8_char : [> io_r] io -> int
val write_char : [> io_w] io -> char -> unit
val write_string : [> io_w] io -> string -> unit
val write_utf8_char : [> io_w] io -> int -> unit
val printf : [> io_w] io -> ('a, unit, string, unit) format4 -> 'a

val pos : irstream -> int
val size : irstream -> int
val seek : irstream -> int -> unit
val skip : irstream -> int -> unit

val read_byte : [> io_r] io -> int
val read_be_u8 : [> io_r] io -> int
val read_be_u16 : [> io_r] io -> int
val read_be_u24 : [> io_r] io -> int
val read_be_u32 : [> io_r] io -> XNum.num
val read_be_i8 : [> io_r] io -> int
val read_be_i16 : [> io_r] io -> int
val read_be_i24 : [> io_r] io -> int
val read_be_i32 : [> io_r] io -> XNum.num

val write_byte : [> io_w] io -> int -> unit
val write_be_u8 : [> io_w] io -> int -> unit
val write_be_u16 : [> io_w] io -> int -> unit
val write_be_u24 : [> io_w] io -> int -> unit
val write_be_u32 : [> io_w] io -> XNum.num -> unit
val write_be_i8 : [> io_w] io -> int -> unit
val write_be_i16 : [> io_w] io -> int -> unit
val write_be_i24 : [> io_w] io -> int -> unit
val write_be_i32 : [> io_w] io -> XNum.num -> unit

val to_string : [> io_r] io -> string
val free : 'a io -> unit
val bytes_written : ostream -> int
