
open Tools.XNum
open Unicode.UTypes
open Runtime
open Dim
open Runtime.Substitute
open GlyphMetric
open FontMetric
open Box

let discard_glue (items : Box.extended_glyph_item list) =
  let rec iter cmds items = match items with
    | []      -> (List.rev cmds, [])
    | (i : Box.extended_glyph_item) :: is -> match i with
      | `Glyph _              -> (List.rev cmds, items)
      | `Kern _               -> iter cmds is
      | `Break (_, _, _, _, [||]) -> iter cmds is   (* only break boxes where no-break is empty *)
      | `Break _              -> (List.rev cmds, items)
      | `Command c            -> iter (i :: cmds) is
      | `Box b                -> begin match b.b_contents with
          | GlueBox (_, disc)      -> if disc then iter cmds is else (List.rev cmds, items)
          | BreakBox (_, _, _, _, []) -> iter cmds is   (* only break boxes where no-break is empty *)
          | EmptyBox            -> iter cmds is
          | _                   -> if is_real_box b then (List.rev cmds, items) else iter (i :: cmds) is
        end
      | _ -> (List.rev cmds, items)
  in
  iter [] items

let discard_glue_array from_pos to_pos (items : Box.extended_glyph_item array) =
  let rec iter cmds pos =
    if pos > to_pos then
      (List.rev cmds, pos)
    else match (items.(pos) : Box.extended_glyph_item) with
      | `Glyph _              -> (List.rev cmds, pos)
      | `Kern _               -> iter cmds (pos+1)
      | `Break (_, _, _, _, [||]) -> iter cmds (pos+1)   (* only break boxes where no-break is empty *)
      | `Break _              -> (List.rev cmds, pos)
      | `Command c            -> iter (items.(pos) :: cmds) (pos+1)
      | `Box b                -> begin match b.b_contents with
          | GlueBox (_, disc)      -> if disc then iter cmds (pos+1) else (List.rev cmds, pos)
          | BreakBox (_, _, _, _, []) -> iter cmds (pos+1)   (* only break boxes where no-break is empty *)
          | EmptyBox            -> iter cmds (pos+1)
          | _                   -> if is_real_box b then (List.rev cmds, pos) else iter (items.(pos) :: cmds) (pos+1)
        end
      | _ -> (List.rev cmds, pos)
  in
  iter [] from_pos

let rec box_add_lig_kern boxes =
  let insert_lig lig_char keep_first keep_second b1 b2 bs =
    match (b1.b_contents, b2.b_contents) with
    | (CharBox (_, font), CharBox (_, _)) ->
        (if keep_first  then [b1] else [])
      @ [new_glyph_box (`Char lig_char) font]
      @ (if keep_second then [b2] else [])
      @ bs
    | _ -> assert false
  in
  let collect_commands boxes =
    let cmds = Tools.ListBuilder.make () in
    let rec iter boxes = match boxes with
      | []      -> (Tools.ListBuilder.get cmds, [])
      | b :: bs ->
          if is_real_box b then
            (Tools.ListBuilder.get cmds, boxes)
          else (
            Tools.ListBuilder.add cmds b;
            iter bs
          )
    in
    iter boxes
  in
  let process_lig_kern b1 boxes = match collect_commands boxes with
    | (cmds, []) -> b1 :: cmds
    | (cmds, (b2 :: rest as bs)) -> match (b1.b_contents, b2.b_contents) with
        | (CharBox (`Char c1, f1), CharBox (`Char c2, f2)) ->
            if f1 == f2 then
              match Runtime.FontMetric.get_lig_kern f1 c1 c2 with
              | `NoLigKern -> b1 :: (cmds @ box_add_lig_kern bs)
              | `Kern k    -> b1 :: (new_kern_box (fixed_dim k) (fixed_dim num_zero) :: (cmds @ box_add_lig_kern boxes))
              | `Ligature (c, s, k1, k2) ->
                  let new_boxes = insert_lig c k1 k2 b1 b2 rest in
                  Tools.XList.take s new_boxes @ box_add_lig_kern (Tools.XList.drop s new_boxes)
            else
              b1 :: (cmds @ box_add_lig_kern bs)
        | _ -> b1 :: (cmds @ box_add_lig_kern bs)
  in
  match boxes with
  | []      -> []
  | b :: bs -> if is_char_box b then process_lig_kern b bs else b :: box_add_lig_kern bs

type ('result, 'cmd) composer =
{
  result        : 'result Tools.ListBuilder.builder;
  current       : (box, 'cmd) JustHyph.char_item Tools.ListBuilder.builder;
  end_word      : ('result, 'cmd) composer -> unit;
  process_box   : ('result, 'cmd) composer -> box -> 'result;
  mutable font  : font_metric;
  mutable composer_fn : glyph_composer;
  mutable hyphen_params : JustHyph.hyphen_params
}

let set_font composer font glyph_composer =
  if composer.composer_fn != glyph_composer || composer.font != font then (
    composer.end_word composer;
    composer.font <- font;
    composer.composer_fn <- glyph_composer
  ) else ()

let set_hyphen_params composer hyphen_params =
  composer.hyphen_params <- hyphen_params

let add_char composer chr =
  Tools.ListBuilder.add composer.current (JustHyph.Char chr)

let add_break composer p h pre post no =
  let cvt x = List.map extended_item_to_box (JustHyph.add_lig_kern true (JustHyph.convert_to_glyphs composer.font composer.composer_fn x)) in
  Tools.ListBuilder.add composer.current (JustHyph.Item (`Break (p, h, Array.of_list (cvt pre), Array.of_list (cvt post), Array.of_list (cvt no))))

let ignore_break _ _ _ _ _ _ = ()

let add_kern composer x y =
  Tools.ListBuilder.add composer.current (JustHyph.Item (`Kern x))

let add_cmd composer cmd =
  Tools.ListBuilder.add composer.current (JustHyph.Item (`Command cmd))

let add_box composer box =
  composer.end_word composer;
  Tools.ListBuilder.add composer.result (composer.process_box composer box)

let get_contents composer () =
  composer.end_word composer;
  Tools.ListBuilder.get composer.result

let process_box_to_box _ box = box
let process_box_to_item _ box = `Box box
let process_box_to_char_item _ box = JustHyph.Item (`Box box)

let end_word_just_hyph composer =
  match Tools.ListBuilder.get composer.current with
  | []   -> ()
  | word ->
      let items = JustHyph.convert_to_glyphs_and_add_breaks composer.hyphen_params composer.font composer.composer_fn word in
      Tools.ListBuilder.add_list
        composer.result
        (List.map extended_item_to_box (JustHyph.add_lig_kern true items))

let end_word_ligature composer =
  match Tools.ListBuilder.get composer.current with
  | []   -> ()
  | word ->
      Tools.ListBuilder.add_list
        composer.result
        (List.map extended_item_to_box
          (JustHyph.add_lig_kern false
            (JustHyph.convert_to_glyphs composer.font composer.composer_fn word)))

let end_word_hyph_only composer =
  match Tools.ListBuilder.get composer.current with
  | []   -> ()
  | word ->
      let items = JustHyph.convert_to_glyphs_and_add_breaks composer.hyphen_params composer.font composer.composer_fn word in
      Tools.ListBuilder.add_list composer.result items

let end_word_char_item composer =
  Tools.ListBuilder.add_list composer.result (Tools.ListBuilder.get composer.current)

let end_word_glyph_item composer = match Tools.ListBuilder.get composer.current with
  | []   -> ()
  | word ->
      Tools.ListBuilder.add_list
        composer.result
        (JustHyph.convert_to_glyphs composer.font composer.composer_fn word)

let make_composer add_break composer =
  ({
     Builder.add_char          = add_char composer;
     Builder.add_break         = add_break composer;
     Builder.add_kern          = add_kern composer;
     Builder.add_box           = add_box composer;
     Builder.add_cmd           = add_cmd composer;
     Builder.set_font          = set_font composer;
     Builder.set_hyphen_params = set_hyphen_params composer
   },
   (get_contents composer)
  )

let just_hyph_builder font composer_fn params =
  make_composer add_break
    {
      result        = Tools.ListBuilder.make ();
      current       = Tools.ListBuilder.make ();
      end_word      = end_word_just_hyph;
      process_box   = process_box_to_box;
      font          = font;
      composer_fn   = composer_fn;
      hyphen_params = params
    }

let ligature_builder font composer_fn params =
  make_composer add_break
    {
      result        = Tools.ListBuilder.make ();
      current       = Tools.ListBuilder.make ();
      end_word      = end_word_ligature;
      process_box   = process_box_to_box;
      font          = font;
      composer_fn   = composer_fn;
      hyphen_params = params
    }

let hyph_only_builder font composer_fn params =
  make_composer add_break
    {
      result        = Tools.ListBuilder.make ();
      current       = Tools.ListBuilder.make ();
      end_word      = end_word_hyph_only;
      process_box   = process_box_to_item;
      font          = font;
      composer_fn   = composer_fn;
      hyphen_params = params
    }

let char_item_builder font composer_fn params =
  make_composer add_break
    {
      result        = Tools.ListBuilder.make ();
      current       = Tools.ListBuilder.make ();
      end_word      = end_word_char_item;
      process_box   = process_box_to_char_item;
      font          = font;
      composer_fn   = composer_fn;
      hyphen_params = params
    }

let glyph_item_builder font composer_fn params =
  make_composer add_break
    {
      result        = Tools.ListBuilder.make ();
      current       = Tools.ListBuilder.make ();
      end_word      = end_word_glyph_item;
      process_box   = process_box_to_item;
      font          = font;
      composer_fn   = composer_fn;
      hyphen_params = params
    }
