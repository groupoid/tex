
type format = [ `PNG | `JPG | `PDF | `PostScript | `Bmp | `Other ]

type bitmap = {
  bm_width : int;
  bm_height : int;
  bm_h_orig : int;
  bm_v_orig : int;
  bm_data : string;
}

type image = [ `Bitmap of bitmap | `PDF of (int * string) ]

val load_image : string -> image
val get_bounding_box : string -> (int * int * int * int) option
