
open XNum
open IO_Base

let num_0x100       = num_of_int 0x100
let num_0x10000     = num_of_int 0x10000
let num_0x1000000   = num_of_int 0x1000000
let num_0x100000000 = mult_num num_0x10000 num_0x10000
let num_0x80000000  = div_num num_0x100000000 (num_of_int 2)

type istream   = IO_Base.istream
type irstream  = IO_Base.irstream
type ostream   = IO_Base.ostream
type orstream  = IO_Base.orstream
type iostream  = IO_Base.iostream
type iorstream = IO_Base.iorstream

let coerce_i   = io_coerce_i
let coerce_o   = io_coerce_o
let coerce_ir  = io_coerce_ir
let coerce_or  = io_coerce_or
let coerce_io  = io_coerce_io
let coerce_ior = io_coerce_ior
let coerce_o_ior = io_coerce_o_ior

let size           = io_size
let pos            = io_pos
let seek           = io_seek
let bytes_written  = io_bytes_written
let eof            = io_eof
let free           = io_free
let read_char      = io_read_char
let read_string    = io_read_string
let write_char     = io_write_char
let write_string   = io_write_string

let skip cs off    = ignore (io_read_string cs off)

let read_byte cs =
  if io_eof cs then -1
  else int_of_char (io_read_char cs)

let peek_char cs off =
  let p = io_pos cs in
  io_seek cs (p + off);
  let chr = io_read_char cs in
  io_seek cs p;
  chr

let write_byte cs x = io_write_char cs (Char.chr (x land 0xff))

let printf cs = Printf.ksprintf (write_string cs)

let read_be_u8 cs = read_byte cs
let read_be_u16 cs =
  let x = read_byte cs in
  let y = read_byte cs in
  0x100 * x + y

let read_be_u24 cs =
  let x = read_byte cs in
  let y = read_byte cs in
  let z = read_byte cs in
  0x10000 * x + 0x100 * y + z

let read_be_u32 cs =
  let x = read_be_u16 cs in
  let y = read_be_u16 cs in
  add_num (mult_num (num_of_int 0x10000) (num_of_int x)) (num_of_int y)

let read_be_i8 cs =
  let x = read_be_u8 cs in
  if x > 0x7f then x - 0x100 else x

let read_be_i16 cs =
  let x = read_be_u16 cs in
  if x > 0x7fff then x - 0x10000 else x

let read_be_i24 cs =
  let x = read_be_u24 cs in
  if x > 0x7fffff then x - 0x1000000 else x

let read_be_i32 cs =
  let x = read_be_u32 cs in
  if ge_num x num_0x80000000 then sub_num x num_0x100000000 else x

let write_be_u8 cs x = write_byte cs x
let write_be_u16 cs x =
  write_byte cs ((x lsr 8) land 0xff);
  write_byte cs (x land 0xff)
let write_be_u24 cs x =
  write_byte cs ((x lsr 16) land 0xff);
  write_byte cs ((x lsr 8) land 0xff);
  write_byte cs (x land 0xff)

let write_be_u32 cs n =
  let y = mod_num n (num_of_int 0x10000) in
  let x = quo_num (sub_num n y) (num_of_int 0x10000) in
  let u = int_of_num x in
  let v = int_of_num y in
  write_be_u16 cs u;
  write_be_u16 cs v

let write_be_i8 cs x = write_be_u8 cs x
let write_be_i16 cs x = write_be_u16 cs x
let write_be_i24 cs x = write_be_u24 cs x
let write_be_i32 cs n = write_be_u32 cs n

let read_utf8_char cs =
  let c = read_byte cs in
  if c < 0x80 then c
  else if c < 0xe0 then
    let c2 = read_byte cs in
    ((c land 0x1f) lsl 6) lor (c2 land 0x3f)
  else
    let c2 = read_byte cs in
    let c3 = read_byte cs in
    ((c land 0x0f) lsl 12) lor ((c2 land 0x3f) lsl 6) lor (c3 land 0x3f)

let write_utf8_char cs x =
  if x < 0x80 then write_byte cs x
  else if x < 0x800 then (
    write_byte cs (0xc0 lor (x lsr 6));
    write_byte cs (0x80 lor (x land 0x3f))
  ) else (
    write_byte cs (0xe0 lor (x lsr 12));
    write_byte cs (0x80 lor ((x lsr 6) land 0x3f));
    write_byte cs (0x80 lor (x land 0x3f))
  )

let open_in filename =
  let ic = open_in_bin filename in
  let eof = ref false in
  io_make_read
    (fun () -> Stdlib.close_in ic)
    (fun () -> try input_char ic with End_of_file -> eof := true; '\000')
    (fun len -> really_input_string ic len)
    (fun () -> !eof)

let open_out filename =
  let oc = open_out_bin filename in
  io_make_write
    (fun () -> Stdlib.close_out oc)
    (fun c -> output_char oc c)
    (fun s -> output_string oc s)
    (fun () -> pos_out oc)

let close_in = io_free
let close_out = io_free

let make_buffer_stream size =
  let buf = Buffer.create size in
  let pos = ref 0 in
  let fr () = () in
  let rc () = if !pos >= Buffer.length buf then '\000' else (let c = Buffer.nth buf !pos in pos := !pos + 1; c) in
  let rs len = let s = Buffer.sub buf !pos len in pos := !pos + len; s in
  let eof () = !pos >= Buffer.length buf in
  let wc c = Buffer.add_char buf c; pos := Buffer.length buf in
  let ws s = Buffer.add_string buf s; pos := Buffer.length buf in
  let bw () = Buffer.length buf in
  let sz () = Buffer.length buf in
  let ps () = !pos in
  let sk p = pos := max 0 (min p (Buffer.length buf)) in
  io_make fr rc rs eof wc ws bw sz ps sk

let make_string_stream s =
  let pos = ref 0 in
  let fr () = () in
  let rc () = if !pos >= String.length s then '\000' else (let c = s.[!pos] in pos := !pos + 1; c) in
  let rs len = let r = String.sub s !pos len in pos := !pos + len; r in
  let eof () = !pos >= String.length s in
  let sz () = String.length s in
  let ps () = !pos in
  let sk p = pos := max 0 (min p (String.length s)) in
  io_make_read_seek fr rc rs eof sz ps sk

let consume_stream is f =
  while not (io_eof is) do
    f (io_read_string is 1024)
  done

let skip is n = io_seek is (io_pos is + n)
let read_byte = read_be_u8
let coerce_ir_i (is : irstream) = (is :> istream)
let coerce_io_i (is : iostream) = (is :> istream)
let coerce_ior_i (is : iorstream) = (is :> istream)
let coerce_is_i (is : istream) = (is :> istream)
let coerce_ior_ir (is : iorstream) = (is :> irstream)
let to_channel is oc = consume_stream is (output_string oc)
let to_string is = let b = Buffer.create 1024 in consume_stream is (Buffer.add_string b); Buffer.contents b
let from_string s = make_string_stream s
let free (is : 'a io) = io_free is
