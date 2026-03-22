
type tag = int32
module TagMap : Map.S with type key = tag

val compose_tag : int -> int -> tag
val make_tag : string -> tag
val make_tag_uc : int array -> tag
val tag_to_string : tag -> string

val empty_tag    : tag
val latn_tag     : tag
val dflt_tag     : tag
val size_tag     : tag

val ttcf_tag : tag
val true_tag : tag
val otto_tag : tag
val cff_tag  : tag
val cmap_tag : tag
val glyf_tag : tag
val gdef_tag : tag
val gpos_tag : tag
val gsub_tag : tag
val head_tag : tag
val hhea_tag : tag
val hmtx_tag : tag
val kern_tag : tag
val loca_tag : tag
val maxp_tag : tag
val name_tag : tag
val post_tag : tag
val os2_tag  : tag
