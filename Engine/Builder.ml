
open Tools.XNum
open Unicode.UTypes
open Runtime
open Runtime.Substitute
open GlyphMetric
open FontMetric
open Dim
open Box
open JustHyph

type builder_interface =
{
  add_char  : uc_char -> unit;
  add_break : num -> bool -> (box, box) char_item list
                          -> (box, box) char_item list
                          -> (box, box) char_item list -> unit;
  add_kern  : dim -> dim -> unit;
  add_box   : box -> unit;
  add_cmd   : box -> unit;

  set_font          : font_metric -> glyph_composer -> unit;
  set_hyphen_params : JustHyph.hyphen_params -> unit
}

type 'a builder = (builder_interface * (unit -> 'a))

let add_char  builder chr                      = builder.add_char  chr
let add_break builder penalty hyph pre post no = builder.add_break penalty hyph pre post no
let add_kern  builder x y                      = builder.add_kern  x y
let add_box   builder box                      = builder.add_box   box
let add_cmd   builder cmd                      = builder.add_cmd   cmd
let set_font  builder font composer            = builder.set_font  font composer
let set_hyphen_params builder hyphen_params    = builder.set_hyphen_params hyphen_params

let add_char_list builder chars = List.iter (fun c -> add_char builder c) chars
let add_box_list  builder boxes = List.iter (add_box builder) boxes
let add_cmd_list  builder boxes = List.iter (add_cmd builder) boxes

(* A builder which throws away all boxes. *)
let void_builder =
{
  add_char  = (fun _ -> ());
  add_break = (fun _ _ _ _ _ -> ());
  add_kern  = (fun _ _ -> ());
  add_box   = (fun _ -> ());
  add_cmd   = (fun _ -> ());
  set_font  = (fun _ _ -> ());
  set_hyphen_params = (fun _ -> ())
}

(* A simple builder which is just a wrapper around Tools.ListBuilder. *)
type simple_builder_state =
{
  boxes              : box Tools.ListBuilder.builder;
  mutable font       : font_metric;
  mutable composer   : glyph_composer
}

let sb_add_char sb chr =
  Tools.ListBuilder.add sb.boxes (new_char_box chr sb.font)

let sb_add_break sb p h pre post no =
  Tools.ListBuilder.add sb.boxes
    (new_break_box
      p h
      (List.map extended_item_to_box (JustHyph.convert_to_glyphs sb.font sb.composer pre))
      (List.map extended_item_to_box (JustHyph.convert_to_glyphs sb.font sb.composer post))
      (List.map extended_item_to_box (JustHyph.convert_to_glyphs sb.font sb.composer no)))

let sb_add_kern sb x y =
  Tools.ListBuilder.add sb.boxes (new_kern_box x y)

let sb_add_box sb box =
  Tools.ListBuilder.add sb.boxes box

let sb_set_font sb font composer =
  sb.font     <- font;
  sb.composer <- composer

let sb_set_hyphen_params _ = ()

let sb_get sb () =
  Tools.ListBuilder.get sb.boxes

let simple_builder font composer =
  let b =
    {
      boxes    = Tools.ListBuilder.make ();
      font     = font;
      composer = composer
    }
  in
  ({
      add_char  = sb_add_char  b;
      add_break = sb_add_break b;
      add_kern  = sb_add_kern  b;
      add_box   = sb_add_box   b;
      add_cmd   = sb_add_box   b;
      set_font  = sb_set_font  b;
      set_hyphen_params = sb_set_hyphen_params
    },
    (sb_get b)
  )
