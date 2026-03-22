type -'a io = {
  io_free          : unit -> unit;
  io_read_char     : unit -> char;
  io_read_string   : int -> string;
  io_eof           : unit -> bool;
  io_write_char    : char -> unit;
  io_write_string  : string -> unit;
  io_bytes_written : unit -> int;
  io_size          : unit -> int;
  io_pos           : unit -> int;
  io_seek          : int -> unit;
}

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

let not_implemented _ = assert false

let io_make fr rc rs eof wc ws bw sz pos sk = {
  io_free          = fr;
  io_read_char     = rc;
  io_read_string   = rs;
  io_eof           = eof;
  io_write_char    = wc;
  io_write_string  = ws;
  io_bytes_written = bw;
  io_size          = sz;
  io_pos           = pos;
  io_seek          = sk;
}

let io_make_read fr rc rs eof = {
  io_free          = fr;
  io_read_char     = rc;
  io_read_string   = rs;
  io_eof           = eof;
  io_write_char    = not_implemented;
  io_write_string  = not_implemented;
  io_bytes_written = not_implemented;
  io_size          = not_implemented;
  io_pos           = not_implemented;
  io_seek          = not_implemented;
}

let io_make_write fr wc ws bw = {
  io_free          = fr;
  io_read_char     = not_implemented;
  io_read_string   = not_implemented;
  io_eof           = not_implemented;
  io_write_char    = wc;
  io_write_string  = ws;
  io_bytes_written = bw;
  io_size          = not_implemented;
  io_pos           = not_implemented;
  io_seek          = not_implemented;
}

let io_make_read_seek fr rc rs eof sz pos sk = {
  io_free          = fr;
  io_read_char     = rc;
  io_read_string   = rs;
  io_eof           = eof;
  io_write_char    = not_implemented;
  io_write_string  = not_implemented;
  io_bytes_written = not_implemented;
  io_size          = sz;
  io_pos           = pos;
  io_seek          = sk;
}

let io_make_write_seek fr wc ws bw sz pos sk = {
  io_free          = fr;
  io_read_char     = not_implemented;
  io_read_string   = not_implemented;
  io_eof           = not_implemented;
  io_write_char    = wc;
  io_write_string  = ws;
  io_bytes_written = bw;
  io_size          = sz;
  io_pos           = pos;
  io_seek          = sk;
}

let io_make_read_write fr rc rs eof wc ws bw = {
  io_free          = fr;
  io_read_char     = rc;
  io_read_string   = rs;
  io_eof           = eof;
  io_write_char    = wc;
  io_write_string  = ws;
  io_bytes_written = bw;
  io_size          = not_implemented;
  io_pos           = not_implemented;
  io_seek          = not_implemented;
}

let io_free          (cs : 'a io)     = cs.io_free          ()
let io_read_char     (cs : [> io_r] io)     = cs.io_read_char     ()
let io_read_string   (cs : [> io_r] io) len = cs.io_read_string  len
let io_eof           (cs : [> io_r] io)     = cs.io_eof           ()
let io_write_char    (cs : [> io_w] io) chr = cs.io_write_char   chr
let io_write_string  (cs : [> io_w] io) str = cs.io_write_string str
let io_bytes_written (cs : [> io_w] io)     = cs.io_bytes_written ()
let io_size          (cs : [> io_s] io)     = cs.io_size          ()
let io_pos           (cs : [> io_s] io)     = cs.io_pos           ()
let io_seek          (cs : [> io_s] io) off = cs.io_seek         off

let io_coerce_i   cs = (cs :> istream)
let io_coerce_o   cs = (cs :> ostream)
let io_coerce_o_ior (os : ostream) =
  ({ io_free = os.io_free;
    io_read_char = (fun () -> failwith "io_read_char on ostream");
    io_read_string = (fun _ -> failwith "io_read_string on ostream");
    io_eof = (fun () -> true);
    io_write_char = os.io_write_char;
    io_write_string = os.io_write_string;
    io_bytes_written = os.io_bytes_written;
    io_size = os.io_size;
    io_pos = os.io_pos;
    io_seek = os.io_seek } : iorstream)
let io_coerce_ir  cs = (cs :> irstream)
let io_coerce_or  cs = (cs :> orstream)
let io_coerce_io  cs = (cs :> iostream)
let io_coerce_ior cs = (cs :> iorstream)
