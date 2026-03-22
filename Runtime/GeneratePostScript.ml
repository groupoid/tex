
open Tools.XNum
open Logging
open FontMetric

type font_ref = {
  font : font_metric;
  first_glyph_index : int;
  mutable used_glyphs : int;
  mutable used_ps_fonts : (string * int) list;
  glyph_map : int array;
}

type state = { 
  preamble : Tools.IO.iorstream;
  pages    : Tools.IO.iorstream;
  mutable fonts    : font_ref list;
}

let new_state () = {
  preamble = Tools.IO.make_buffer_stream 0x1000;
  pages    = Tools.IO.make_buffer_stream 0x10000;
  fonts    = [];
}

let pt_to_bp x = float_of_num (num_of_ints 7200 7227 */ x)

let write_postscript_file name _comment _pages =
  let state = new_state () in
  let oc = open_out_bin name in
  Tools.IO.to_channel (Tools.IO.coerce_ior_ir state.preamble) oc;
  Tools.IO.to_channel (Tools.IO.coerce_ior_ir state.pages) oc;
  close_out oc
