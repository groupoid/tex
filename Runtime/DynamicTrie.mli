
open Unicode.UTypes

type 'a t

val empty                 : 'a t
val is_empty              : 'a t -> bool
val prefix                : 'a t -> uc_char -> 'a t
val root_value            : 'a t -> 'a option
val depth                 : 'a t -> int

val find_string           : uc_string -> 'a t -> 'a
val mem_string            : uc_string -> 'a t -> bool
val find_list             : uc_list -> 'a t -> 'a
val mem_list              : uc_list -> 'a t -> bool
val generic_lookup        : (('a t -> uc_char -> 'a t) -> 'a t -> 'b -> 'a t) ->
                              'b -> 'a t -> 'a option
val generic_lookup_prefix : ((( 'a t * 'a t) -> uc_char -> ('a t * 'a t)) ->
                               ( 'a t * 'a t) -> 'b -> ('a t * 'a t)) ->
                              'b -> 'a t -> 'a option
val lookup_string         : uc_string -> 'a t -> 'a option
val lookup_list           : uc_list -> 'a t -> 'a option
val lookup_prefix_string  : uc_string -> 'a t -> 'a option
val lookup_prefix_list    : uc_list -> 'a t -> 'a option
val lookup_prefix_stream  : Unicode.UCStream.istream -> 'a t -> 'a option
val add_string            : uc_string -> 'a -> 'a t -> 'a t
val remove_string         : uc_string -> 'a t -> 'a t
val add_list              : uc_list -> 'a -> 'a t -> 'a t
val remove_list           : uc_list -> 'a t -> 'a t
val merge                 : 'a t -> 'a t -> 'a t
val map                   : ('a -> 'b) -> 'a t -> 'b t
val mapi                  : (uc_string -> 'a -> 'b) -> 'a t -> 'b t
val iter                  : (uc_string -> 'a -> unit) -> 'a t -> unit
val fold                  : (uc_string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
