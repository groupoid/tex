
open UTypes

val read_uc_char : Tools.IO.istream -> uc_char
val write_uc_char : Tools.IO.ostream -> uc_char -> unit
val to_unicode : int list -> uc_list
val from_unicode : uc_list -> int list
val string_to_bytes : string -> int list
val bytes_to_string : int list -> string
val of_ascii : string -> uc_list
val to_ascii : uc_list -> string
val uc_string_of_ascii : string -> uc_string
val uc_string_to_ascii : uc_string -> string
val of_string : string -> uc_list
val to_string : uc_list -> string
val append : uc_string -> uc_string -> uc_string
val set_string_format : [> `ASCII | `Latin1 | `UTF8 | `Unicode ] -> unit

