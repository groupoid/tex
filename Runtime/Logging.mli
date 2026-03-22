
open Tools.XNum
open Unicode.UTypes

val log_open : string -> unit
val log_string : string -> unit
val log_uc_list : uc_list -> unit
val log_uc_string : uc_string -> unit
val log_int : int -> unit
val log_num : num -> unit
val log_info : Unicode.UCStream.location -> string -> unit
val log_warn : Unicode.UCStream.location -> string -> unit
val log_error : Unicode.UCStream.location -> string -> unit

