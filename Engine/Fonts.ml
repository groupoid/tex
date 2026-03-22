
open Tools.XNum
open Unicode
open Unicode.UTypes
open Runtime
open Runtime.Substitute
open FontMetric
open Logging
open Dim
(* open Typesetting *)
open Box

type font_definition = {
  fd_name         : uc_string;
  fd_family       : uc_string;
  fd_series       : uc_string;
  fd_shape        : uc_string;
  fd_min_size     : num;
  fd_max_size     : num;
  mutable fd_loaded_sizes : (num * font_metric) list;
  fd_data         : font_load_params;
}

type font = {
  f_font_def : font_definition;
  f_metric   : font_metric;
  f_size     : num;
}

type font_table = font_definition list Unicode.DynUCTrie.t

(* table of fonts *)
let get_font_list font_table family =
  try
    Unicode.DynUCTrie.find_string family font_table
  with
  | Not_found -> []

let add_font font_table font_def =
  let rec add_entry font_list = match font_list with
  | []      -> [font_def]
  | f::fs -> if f.fd_name     =  font_def.fd_name     &&
                  f.fd_family   =  font_def.fd_family   &&
                  f.fd_series   =  font_def.fd_series   &&
                  f.fd_shape    =  font_def.fd_shape    &&
                  f.fd_min_size =/ font_def.fd_min_size &&
                  f.fd_max_size =/ font_def.fd_max_size then
                 font_list
               else
                 f :: add_entry fs
  in
  let font_list = get_font_list font_table font_def.fd_family in
  let fam_str = UString.to_string (Array.to_list font_def.fd_family) in
  let len = List.length font_list in
  Printf.printf "add_font: Adding %s to family %s (current length: %d)\n%!" (UString.to_string (Array.to_list font_def.fd_name)) fam_str len;
  Unicode.DynUCTrie.add_string
      font_def.fd_family
      (add_entry font_list)
      font_table

let load_font fd size =
  (* |fd.fd_data.flp_size| does not contain the real size, but a scale factor. *)
  let params =
    {
      (fd.fd_data)
      with
      flp_size = fd.fd_data.flp_size */ size
    } in
  let font = LoadFont.load_font (UString.to_string (Array.to_list fd.fd_name)) params in
  let rec add sizes = match sizes with
  | []        -> [(size, font)]
  | (s, _) as entry :: ss -> if s </ size then
                   entry :: add ss
                 else if s >/ size then
                   (size, font) :: sizes
                 else
                   sizes
  in
  fd.fd_loaded_sizes <- add fd.fd_loaded_sizes;
  {
    f_font_def = fd;
    f_metric   = font;
    f_size     = size
  }

let get_font font_table family series shape size =
  let rec choose_font fonts = match fonts with
  | []      -> None
  | f::fs ->
      if f.fd_series <> series then
        choose_font fs
      else if f.fd_shape <> shape then
        choose_font fs
      else
        if f.fd_min_size <=/ size &&
           (size </ f.fd_max_size || f.fd_max_size =/ num_zero ||
            (size =/ f.fd_max_size && f.fd_max_size =/ f.fd_min_size)) then
          Some f
        else
          choose_font fs
  in
  let load fd size =
    try
      Some (load_font fd size)
    with
    | e ->
        Printf.printf "\nException in load_font %s: %s\n%!" (UString.to_string (Array.to_list fd.fd_name)) (Printexc.to_string e);
        log_warn ("",0,0) "Cannot load font file `";
        log_uc_string fd.fd_name;
        log_string "'!";
        None
  in
  let font_l = get_font_list font_table family in
  Printf.printf "get_font lookup: count=%d\n%!" (List.length font_l);
  List.iter (fun f ->
      let fam = UString.to_string (Array.to_list f.fd_family) in
      let ser = UString.to_string (Array.to_list f.fd_series) in
      let sha = UString.to_string (Array.to_list f.fd_shape) in
      let ms = Tools.XNum.string_of_num f.fd_min_size in
      let xs = Tools.XNum.string_of_num f.fd_max_size in
      Printf.printf "  table entry: %s/%s/%s [%s .. %s]\n%!" fam ser sha ms xs
  ) font_l;
  match choose_font font_l with
  | None    -> None
  | Some fd ->
      let rec iter sizes = match sizes with
      | []            -> load fd size
      | (s,f) :: ss -> if s </ size then
                           iter ss
                         else if s =/ size then
                           Some {
                                   f_font_def = fd;
                                   f_metric   = f;
                                   f_size     = size
                                 }
                         else
                           load fd size
      in
      iter fd.fd_loaded_sizes

let declare_font font_table name family series shape size params =
  add_font
    font_table
    {
      fd_name         = name;
      fd_family       = family;
      fd_series       = series;
      fd_shape        = shape;
      fd_min_size     = fst size;
      fd_max_size     = snd size;
      fd_loaded_sizes = [];
      fd_data         = params
    }

(* |initialise_font_table ()| declares some base fonts. *)
let initialise_font_table () =
  declare_font
           Unicode.DynUCTrie.empty
           [||] [||] [||] [||]
           (num_zero, num_of_int 10000)
           empty_load_params

(* virtual fonts *)
let make_virtual_font name size glyphs italic lig_kern params =
  let pi = {
             pi_page_no   = 0;
             pi_width     = num_zero;
             pi_height    = num_zero;
             pi_depth     = num_zero;
             pi_top_margin= num_zero;
             pi_bottom_margin=num_zero;
             pi_left_margin=num_zero;
             pi_right_margin=num_zero;
           } in
  let vg = Array.init
             (Array.length glyphs)
             (fun i ->
               let open Runtime.FontVirtual in
               {
                 vg_width  = glyphs.(i).b_width.d_base;
                 vg_height = glyphs.(i).b_height.d_base;
                 vg_depth  = glyphs.(i).b_depth.d_base;
                  vg_italic = italic.(i);
                  vg_commands = [| Box.draw_box pi num_zero num_zero glyphs.(i) |]
                }) in

  FontVirtual.make_virtual_font name size vg lig_kern params
