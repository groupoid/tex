
open XNum
open Runtime
open Logging
(* open Typesetting *)
open Engine
open ParseState

let pdf_spec_num        = ref 0
let pdf_page            = ref (-1)
let pdf_src_spec_stream = ref (IO.coerce_o (IO.make_buffer_stream 10))
let src_spec_enabled    = ref false

let init_source_specials job =
  pdf_spec_num        := 0;
  pdf_page            := -1;
  src_spec_enabled    := job.Job.source_specials;
  pdf_src_spec_stream := IO.coerce_o job.Job.src_special_stream

let insert_source_special ps =
  let ((file, line, col) as loc) = location ps in
  if file <> "" then (
    let dvi_spec = Printf.sprintf "src:%d:%d %s" line col file in
    let pdf_spec pi (x,y) =
      let x_pos = int_of_float (float_of_num x *. 65536.0) in
      let y_pos = int_of_float (float_of_num y *. 65536.0) in
      if pi.Box.pi_page_no <> !pdf_page then (
        IO.printf !pdf_src_spec_stream "s %d\n" pi.Box.pi_page_no;
        pdf_page := pi.Box.pi_page_no
      ) else ();
      IO.printf !pdf_src_spec_stream "(%s\nl %d %d\np %d %d %d\n)\n" file !pdf_spec_num line !pdf_spec_num x_pos y_pos;

      pdf_spec_num := !pdf_spec_num + 1
    in
    add_node ps (Node.CommandBox (loc, `Special (`DVI_Special dvi_spec)));
    add_node ps (Node.CommandBox (loc, `PageCmd (Box.CallPageFunction pdf_spec)))
  ) else ()

(* |begin_paragraph| starts a new paragraph, |end_paragraph| adds the current paragraph to the page. *)
let begin_paragraph ps =
  open_node_list ps `Paragraph;
  if !src_spec_enabled then
    insert_source_special ps
  else ()

let end_paragraph ps =
  match close_node_list ps `Paragraph with
  | []    -> ()
  | nodes -> add_node ps (Node.Paragraph (location ps, nodes))

(* |begin_math| starts a math formula, |end_math| adds it to the current paragraph. *)
let begin_math ps =
  open_node_list ps `Math

let end_math ps =
  match close_node_list ps `Math with
  | []    -> ()
  | nodes -> add_node ps (Node.Math (location ps, nodes))

let begin_hbox ps =
  open_node_list ps `HBox

let end_hbox ps =
  match close_node_list ps `HBox with
  | []    -> ()
  | nodes -> add_node ps (Node.HBox (location ps, `Default, nodes))

let end_lrbox ps =
  match close_node_list ps `LRBox with
  | []    -> ()
  | nodes -> add_node ps (Node.HBox (location ps, `LR, nodes))

let end_rlbox ps =
  match close_node_list ps `RLBox with
  | []    -> ()
  | nodes -> add_node ps (Node.HBox (location ps, `RL, nodes))

(* |ensure_par_mode| enters paragraph mode if the system is in document mode. *)
let ensure_par_mode ps = match current_mode ps with
  | `Galley                    -> begin_paragraph ps
  | `VBox                      -> begin_hbox ps
  | `Paragraph | `Math | `HBox | `LRBox | `RLBox -> ()
  | m                          -> log_error (location ps)
                                    ("You can't start a paragraph in "
                                   ^ ParseState.mode_to_string m
                                   ^ " mode!")

let leave_par_mode ps = match current_mode ps with
  | `Preamble
  | `Galley
  | `VBox      -> ()
  | `Paragraph -> end_paragraph ps
  | `Math      -> 
      end_math ps;
      end_paragraph ps
  | `HBox      -> end_hbox  ps
  | `LRBox     -> end_lrbox ps
  | `RLBox     -> end_rlbox ps
  | m          -> log_error (location ps)
                     ("Mode "
                    ^ ParseState.mode_to_string m
                    ^ " at end of paragraph!")

(* |set_mode <mode>| sets the current mode and returns |true| on success. *)
let set_mode ps mode =
  let set_mode_par ps = match current_mode ps with
    | `Galley    ->
        begin_paragraph ps;
        true
    | `Paragraph -> true
    | `Math      ->
        end_math ps;
        true
    | _          -> false
  in
  let set_mode_math ps = match current_mode ps with
    | `Galley    ->
        begin_paragraph ps;
        begin_math ps;
        true
    | `Paragraph ->
        begin_math ps;
        true
    | `Math      -> true
    | `HBox | `LRBox | `RLBox ->
        begin_math ps;
        true
    | _          -> false
  in
  match mode with
  | `Galley    ->
      leave_par_mode ps;
      true
  | `Paragraph -> set_mode_par ps
  | `Math      -> set_mode_math ps
  | _          -> false
