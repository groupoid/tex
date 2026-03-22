

open UTypes

type location = (string * int * int)

type istream

val create        : unit -> istream
val of_list       : uc_list -> istream
val to_list       : istream -> uc_list
val of_string     : uc_string -> istream
val to_string     : istream -> uc_string
val of_file       : string -> istream
val assign        : istream -> istream -> unit
val exchange      : istream -> istream -> unit
val duplicate     : istream -> istream
val eof           : istream -> bool
val location      : istream -> location
val set_location  : istream -> location -> bool -> unit
val get_char      : istream -> int -> uc_char
val next_char     : istream -> uc_char
val take          : istream -> int -> uc_list
val remove        : istream -> int -> unit
val pop           : istream -> uc_char
val clear         : istream -> unit
val match_prefix  : istream -> uc_list -> bool
val insert_list   : istream -> uc_list -> unit
val insert_string : istream -> uc_string -> unit
val insert_stream : istream -> istream -> unit
val include_file  : istream -> string -> unit

