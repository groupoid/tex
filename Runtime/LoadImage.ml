
type format = [ `PNG | `JPG | `PDF | `PostScript | `Bmp | `Other ]

type bitmap = {
  bm_width : int;
  bm_height : int;
  bm_h_orig : int;
  bm_v_orig : int;
  bm_data : string;
}

type image = [ `Bitmap of bitmap | `PDF of (int * string) ]

let load_image _ = failwith "CamlImages dependency removed"
let get_bounding_box _ = None
