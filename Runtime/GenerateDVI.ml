open Tools.XNum
open Unicode.UTypes
open Logging
open Graphic
open Dim
open Substitute
open GlyphMetric
open FontMetric
open LoadImage

type dvi_format = DVI | XDVI 

type 'a state = {
  mutable os : Tools.IO.ostream;
  format : dvi_format;
  mutable data : 'a;
}

type draw_info = {
  pos_h        : num;
  pos_v        : num;
  current_font : font_metric;
  mutable loaded_fonts : (font_metric * int) list;
  stack_depth  : int;
}

type page_info = {
  page_lengths : int list;
  font_defs    : (font_metric * int) list;
  max_width    : num;
  max_height   : num;
  max_stack    : int;
}

type draw_state = draw_info state
type page_state = page_info state

let clear_stack state =
  state.data <- { state.data with stack_depth = 0 }

let rat_to_fixed r = round_num (r */ num_of_int 0x10000)
let float_to_fixed r = num_of_int (int_of_float (r *. 65536.0))
let pt_to_bp x = float_of_num (num_of_ints 7200 7227 */ x)

let write_rat os r = Tools.IO.write_be_i32 os (rat_to_fixed r)

let write_special os string =
  let len = String.length string in
  if len < 0x100 then (
    Tools.IO.write_be_u8  os 239;
    Tools.IO.write_be_u8  os len;
    Tools.IO.write_string os string
  ) else if len < 0x10000 then (
    Tools.IO.write_be_u8  os 240;
    Tools.IO.write_be_u16 os len;
    Tools.IO.write_string os string
  ) else (
    Tools.IO.write_be_u8  os 242;
    Tools.IO.write_be_u32 os (num_of_int len);
    Tools.IO.write_string os string

  )

let write_preamble state comment =
  Tools.IO.write_be_u8 state.os 247;
  (match state.format with
   | DVI  -> Tools.IO.write_be_u8 state.os 2
   | XDVI -> Tools.IO.write_be_u8 state.os 5);
  Tools.IO.write_be_u32 state.os (num_of_int 25400000);
  Tools.IO.write_be_u32 state.os (num_of_int 473628672);
  Tools.IO.write_be_u32 state.os (num_of_int 1000);
  Tools.IO.write_be_u8  state.os (String.length comment);
  Tools.IO.write_string state.os comment



let load_tex_font state font idx =
  if idx < 0x100 then begin
    Tools.IO.write_be_u8 state.os 243;
    Tools.IO.write_be_u8 state.os idx
  end else if idx < 0x1000 then begin
    Tools.IO.write_be_u8  state.os 244;
    Tools.IO.write_be_u16 state.os idx
  end else begin
    Tools.IO.write_be_u8  state.os 245;
    Tools.IO.write_be_u32 state.os (num_of_int idx)
  end;
  Tools.IO.write_be_u32 state.os (num_of_int (Int32.to_int font.fm_checksum));
  write_rat       state.os font.fm_size;
  write_rat       state.os font.fm_design_size;
  Tools.IO.write_be_u8  state.os 0;

  Tools.IO.write_be_u8  state.os 0 (* Skip name for now *)


let load_native_font state font idx =
  Tools.IO.write_be_u8  state.os 252;
  Tools.IO.write_be_u32 state.os (num_of_int idx);
  write_rat       state.os font.fm_size;

  Tools.IO.write_be_u16 state.os 2;
  Tools.IO.write_be_u8  state.os (Array.length font.fm_name + 2);
  Tools.IO.write_be_u8  state.os 0;
  Tools.IO.write_be_u8  state.os 0;
  Tools.IO.write_string state.os "[";
  Tools.IO.write_string state.os (Unicode.UString.to_string (Array.to_list font.fm_name));
  Tools.IO.write_string state.os "]"


let load_font state font idx =
  if state.format = XDVI && (match font.fm_type with `TrueType | `OpenType | `Type1 -> true | _ -> false) then
    load_native_font state font idx
  else
    load_tex_font state font idx

let rec write_font_defs state font_defs =
  match font_defs with
  | [] -> ()
  | (f, i) :: fs ->
      load_font state f i;
      write_font_defs state fs

let write_postamble state =
  let pos = Tools.IO.bytes_written state.os in
  Tools.IO.write_be_u8  state.os 248;
  Tools.IO.write_be_u32 state.os (num_of_int (pos - List.hd state.data.page_lengths));
  Tools.IO.write_be_u32 state.os (num_of_int 25400000);
  Tools.IO.write_be_u32 state.os (num_of_int 473628672);
  Tools.IO.write_be_u32 state.os (num_of_int 1000);
  write_rat       state.os (state.data.max_height -/ inch);
  write_rat       state.os (state.data.max_width -/ inch);
  Tools.IO.write_be_u16 state.os state.data.max_stack;
  Tools.IO.write_be_u16 state.os (List.length state.data.page_lengths);
  write_font_defs state state.data.font_defs;
  Tools.IO.write_be_u8  state.os 249;
  Tools.IO.write_be_u32 state.os (num_of_int pos);
  (match state.format with
   | DVI  -> Tools.IO.write_be_u8 state.os 2
   | XDVI -> Tools.IO.write_be_u8 state.os 5);
  Tools.IO.write_be_u8  state.os 223;
  Tools.IO.write_be_u8  state.os 223;
  Tools.IO.write_be_u8  state.os 223;
  Tools.IO.write_be_u8  state.os 223


let move_right state x =
  if num_of_int (-0x80) <=/ x && num_of_int 0x7f >=/ x then (
    Tools.IO.write_be_u8 state.os 143;
    Tools.IO.write_be_i8 state.os (int_of_num x)
  ) else if num_of_int (-0x8000) <=/ x && num_of_int 0x7fff >=/ x then (
    Tools.IO.write_be_u8  state.os 144;
    Tools.IO.write_be_i16 state.os (int_of_num x)
  ) else (
    Tools.IO.write_be_u8  state.os 146;
    Tools.IO.write_be_i32 state.os x
  )

let move_down state x =
  if num_of_int (-0x80) <=/ x && num_of_int 0x7f >=/ x then (
    Tools.IO.write_be_u8 state.os 157;
    Tools.IO.write_be_i8 state.os (int_of_num x)
  ) else if num_of_int (-0x8000) <=/ x && num_of_int 0x7fff >=/ x then (
    Tools.IO.write_be_u8  state.os 158;
    Tools.IO.write_be_i16 state.os (int_of_num x)
  ) else (
    Tools.IO.write_be_u8  state.os 160;
    Tools.IO.write_be_i32 state.os x
  )

let rec write_boxes box box_h box_v state =
  match box with
  | Empty -> clear_stack state
  | SimpleGlyph (g, f) -> write_boxes_char g f box_h box_v state
  | Rule (w, h) -> write_boxes_rule w h box_h box_v state
  | Image (w, h, f, fmt) -> write_boxes_image w h f fmt box_h box_v state
  | Group bs -> write_boxes_group bs box_h box_v state
  | Command cmd ->
      match cmd with
      | `DVI_Special str ->
          write_special state.os str;
          clear_stack state

and write_boxes_char glyph font box_h box_v state =
  if state.format = XDVI then
    Printf.printf "[GenerateDVI] write_boxes_char called: font=%s\n%!"
      (Unicode.UString.uc_string_to_ascii font.fm_name);
  let char = match glyph with
    | `Char c       -> c
    | `GlyphIndex i -> i
    | `Simple i     -> i
    | `GlyphName _  -> 0
    | `Undef        -> 0
    | `Border _     -> 0
    | `Accent (i, _) -> i
    | `Sequence _   -> 0
    | `Extendable _ -> 0
  in
  let rec choose_font font loaded_fonts =
    if not (List.mem_assq font loaded_fonts) then
      let idx = List.length loaded_fonts in
      load_font state font idx;
      choose_font font ((font, idx) :: loaded_fonts)
    else
      let idx = List.assq font loaded_fonts in
      if idx < 64 then Tools.IO.write_be_u8 state.os (idx + 171)
      else if idx < 0x100 then (
        Tools.IO.write_be_u8 state.os 235;
        Tools.IO.write_be_u8 state.os idx
      ) else if idx < 0x10000 then (
        Tools.IO.write_be_u8  state.os 236;
        Tools.IO.write_be_u16 state.os idx
      ) else (
        Tools.IO.write_be_u8  state.os 238;
        Tools.IO.write_be_u32 state.os (num_of_int idx)
      )
  in
  let glyph_width g f = (get_glyph_metric f g).gm_width in
  let delta_h = rat_to_fixed (glyph_width glyph font) in
  if state.format = XDVI then
    Printf.printf "[GenerateDVI] write_boxes_char: format=XDVI, font_type=%s\n%!"
      (match font.fm_type with `OpenType -> "OpenType" | `TrueType -> "TrueType" | `Type1 -> "Type1" | `TFM -> "TFM" | `Virtual -> "Virtual" | `PK -> "PK");

  if state.data.current_font != font then (
    let new_loaded_fonts = if List.mem_assq font state.data.loaded_fonts then
                             state.data.loaded_fonts
                           else
                             ((font, List.length state.data.loaded_fonts) :: state.data.loaded_fonts) in
    choose_font font state.data.loaded_fonts;
    state.data <- { state.data with current_font = font; loaded_fonts = new_loaded_fonts };
    write_boxes_char glyph font box_h box_v state
  ) else if state.format = XDVI && (match font.fm_type with `TrueType | `OpenType | `Type1 -> true | _ -> false) then (
    Tools.IO.write_be_u8 state.os 254;
    Tools.IO.write_be_u32 state.os delta_h;
    Tools.IO.write_be_u16 state.os 1;
    Tools.IO.write_be_u32 state.os (num_of_int 0);
    Tools.IO.write_be_u16 state.os char;
    state.data <- { state.data with pos_h = state.data.pos_h +/ delta_h; stack_depth = 0 }
  ) else if char < 0x80 then (
    Tools.IO.write_be_u8 state.os char;
    state.data <- { state.data with pos_h = state.data.pos_h +/ delta_h; stack_depth = 0 }
  ) else if char < 0x100 then (
    Tools.IO.write_be_u8 state.os 128;
    Tools.IO.write_be_u8 state.os char;
    state.data <- { state.data with pos_h = state.data.pos_h +/ delta_h; stack_depth = 0 }
  ) else if char < 0x10000 then (
    Tools.IO.write_be_u8  state.os 129;
    Tools.IO.write_be_u16 state.os char;
    state.data <- { state.data with pos_h = state.data.pos_h +/ delta_h; stack_depth = 0 }
  ) else (
    Tools.IO.write_be_u8  state.os 131;
    Tools.IO.write_be_u32 state.os (num_of_int char);
    state.data <- { state.data with pos_h = state.data.pos_h +/ delta_h; stack_depth = 0 }

  )

and write_boxes_rule width height _box_h _box_v state =
  Tools.IO.write_be_u8 state.os 137;
  write_rat      state.os height;
  write_rat      state.os width;
  clear_stack state

and write_boxes_image width height file_name fmt _box_h _box_v state =
  (match fmt with
   | `PostScript ->
       let w = pt_to_bp width in
       let h = pt_to_bp height in
       let (x0, y0, x1, y1) = match get_bounding_box file_name with
         | Some (x0, y0, x1, y1) -> (float_of_int x0, float_of_int y0, float_of_int x1, float_of_int y1)
         | None                  -> (0.0, 0.0, w, h) in
       move_right state (float_to_fixed (~-. x0));
       move_down  state (float_to_fixed y0);
       write_special state.os
         (Printf.sprintf "PSfile=\"%s\" llx=%f lly=%f urx=%f ury=%f rwi=%f rhi=%f"
                         file_name x0 y0 x1 y1 w h)
   | `Bmp ->
       write_special state.os
         (Printf.sprintf "em: graph %s, %f pt, %f pt"
                         file_name (float_of_num width) (float_of_num height))
   | _ -> log_string "\nWarning: Unsupported image format!");
  clear_stack state


and write_boxes_group bs box_h box_v state =
  let write_box bh bv box state =
    let delta_h = rat_to_fixed (bh +/ box_h) -/ state.data.pos_h in
    let delta_v = rat_to_fixed (bv +/ box_v) -/ state.data.pos_v in
    if delta_h <>/ num_zero then move_right state delta_h;
    if delta_v <>/ num_zero then move_down state (minus_num delta_v);
    state.data <- { state.data with pos_h = state.data.pos_h +/ delta_h; pos_v = state.data.pos_v +/ delta_v };

    let old_stack_depth = state.data.stack_depth in

    write_boxes box (box_h +/ bh) (box_v +/ bv) state;
    state.data <- { state.data with stack_depth = max old_stack_depth state.data.stack_depth }
  in
  let write_path path_cmd path state =
    let str = Tools.IO.make_buffer_stream 1024 in
    let rec draw_path cur_x cur_y path =
      match path with
      | [] -> (match path_cmd with
               | `Stroke -> Tools.IO.write_string str " stroke"
               | `Fill   -> Tools.IO.write_string str " fill"
               | `Clip   -> Tools.IO.write_string str " clip")
      | (ax,ay,bx,by,cx,cy,dx,dy) :: ps ->
          if ax <>/ cur_x || ay <>/ cur_y then
            Tools.IO.write_string str (Printf.sprintf " %f %f moveto" (pt_to_bp ax) (pt_to_bp ay));


          Tools.IO.write_string str (Printf.sprintf " %f %f %f %f %f %f curveto" (pt_to_bp bx) (pt_to_bp by) (pt_to_bp cx) (pt_to_bp cy) (pt_to_bp dx) (pt_to_bp dy));

          draw_path dx dy ps
    in
    match path with
    | [] -> ()
    | (ax,_,_,_,_,_,_,_) :: _ ->
        Tools.IO.write_string str "\" newpath";
        draw_path (ax -/ num_one) num_zero path;
        write_special state.os (Tools.IO.to_string str)
  in
  let reset_colour state = write_special state.os "color pop" in
  let set_colour colour_changed col state =
    if colour_changed then reset_colour state;
    let str n = string_of_float (float_of_num n) in
    let color_spec = match col with
      | `Grey x       -> "gray " ^ str x
      | `RGB (r, g, b)    -> "rgb " ^ str r ^ " " ^ str g ^ " " ^ str b
      | `CMYK (c, m, y, k) -> "cmyk " ^ str c ^ " " ^ str m ^ " " ^ str y ^ " " ^ str k
 in
    write_special state.os ("color push " ^ color_spec)
  in
  let set_alpha _col _state = log_string "Warning: The DVI driver does not support transparency!" in
  let set_line_width w state = write_special state.os ("\" " ^ string_of_float (pt_to_bp w) ^ " setlinewidth") in
  let set_line_cap c state =
    let cap = match c with | `Butt -> "0" | `Circle -> "1" | `Square -> "2" in
    write_special state.os ("\" " ^ cap ^ " setlinecap") in
  let set_line_join j state =
    let join = match j with | `Miter -> "0" | `Round -> "1" | `Bevel -> "2" in
    write_special state.os ("\" " ^ join ^ " setlinejoin") in
  let set_miter_limit l state = write_special state.os ("\" " ^ string_of_float (float_of_num l) ^ " setmiterlimit") in
  
  Tools.IO.write_be_u8 state.os 141;
  let old_pos_h = state.data.pos_h in
  let old_pos_v = state.data.pos_v in
  clear_stack state;
  let rec iter_gfx colour_changed gfx_cmds =
    match gfx_cmds with
    | [] -> if colour_changed then reset_colour state
    | c :: cs -> (match c with
        | PutBox (x, y, b, _) -> write_box x y b state; iter_gfx colour_changed cs
        | Draw (_, p) -> write_path `Stroke p state; iter_gfx colour_changed cs
        | Fill (_, p) -> write_path `Fill p state; iter_gfx colour_changed cs
        | Clip p -> write_path `Clip p state; iter_gfx colour_changed cs
        | SetColour col -> set_colour colour_changed col state; iter_gfx true cs
        | SetAlpha a -> set_alpha a state; iter_gfx colour_changed cs
        | SetBgColour _ -> assert false
        | SetLineWidth  w -> set_line_width  w state; iter_gfx colour_changed cs
        | SetLineCap    c -> set_line_cap    c state; iter_gfx colour_changed cs
        | SetLineJoin   j -> set_line_join   j state; iter_gfx colour_changed cs
        | SetMiterLimit l -> set_miter_limit l state; iter_gfx colour_changed cs)
  in
  iter_gfx false bs;
  Tools.IO.write_be_u8 state.os 142;
  state.data <- { state.data with pos_h = old_pos_h; pos_v = old_pos_v; stack_depth = state.data.stack_depth + 1 }

let rec write_pages state pages =
  let start_pos = Tools.IO.bytes_written state.os in
  match pages with
  | [] -> ()
  | p :: ps ->
      Tools.IO.write_be_u8  state.os 139;
      Tools.IO.write_be_u32 state.os (num_of_int p.p_number);
      for _i = 1 to 9 do Tools.IO.write_be_u32 state.os num_zero done;

      (match state.data.page_lengths with
       | []     ->  Tools.IO.write_be_i32 state.os (num_of_int (-1))
       | l :: _ ->  Tools.IO.write_be_i32 state.os (num_of_int (start_pos - l)));
      
      let draw_state = { state with
        data = {
          pos_h        = rat_to_fixed inch;
          pos_v        = rat_to_fixed inch;
          current_font = empty_font;
          loaded_fonts = state.data.font_defs;
          stack_depth  = 0;
        }
      } in
      write_boxes (Group [PutBox (minus_num inch, inch, p.p_contents, None)]) inch inch draw_state;

      state.os <- draw_state.os;
      Tools.IO.write_be_u8 state.os 140;
      state.data <- {
        page_lengths = (Tools.IO.bytes_written state.os - start_pos) :: state.data.page_lengths;
        font_defs    = draw_state.data.loaded_fonts;
        max_width    = max state.data.max_width  p.p_width;
        max_height   = max state.data.max_height p.p_height;
        max_stack    = max state.data.max_stack  draw_state.data.stack_depth;
      };
      write_pages state ps


let write_file format name comment pages =
  Printf.printf "[GenerateDVI] write_file called with format=%s\n%!"
    (match format with DVI -> "DVI" | XDVI -> "XDVI");
  let state = {
    os     = Tools.IO.open_out name;
    format = format;
    data   = {
      page_lengths = [];
      font_defs    = [];
      max_width    = num_zero;
      max_height   = num_zero;
      max_stack    = 0;
    }
  } in
  write_preamble  state comment;
  write_pages     state pages;
  write_postamble state;
  Tools.IO.close_out state.os

let write_dvi_file name comment pages = write_file DVI name comment pages
let write_xdvi_file name comment pages = write_file XDVI name comment pages
