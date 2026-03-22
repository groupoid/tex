open XNum
open Unicode.Types
open Runtime
open FontMetric
open Logging
open Dim
open Typesetting
open Box
open Environment

module UString = Unicode.UString

let tracing_engine = ref false

(* evaluation of nodes *)

let const_pt x _env = x

let const_em x env =
  x */ (current_font_metric env).parameter.quad

let const_ex x env =
  x */ (current_font_metric env).parameter.x_height

let const_mu x env =
  MathLayout.math_units_to_points
    (MathLayout.get_font_params (current_math_font_params env) (current_math_style env))
    x

let const_fixed_dim skip env = fixed_dim (skip env)

(* |get_location <node>| returns the location stored in <node>. *)

let rec get_location node = match node with
| Node.Nodes []                    -> ("", 0, 0)
| Node.Nodes (n :: _)              -> get_location n
| Node.Command (loc, _)               -> loc
| Node.CommandBox (loc, _)            -> loc
| Node.GfxCommand (loc, _)            -> loc
| Node.NewGalley (loc, _, _)           -> loc
| Node.NewLayout (loc, _, _, _)         -> loc
| Node.NewArea (loc, _, _, _, _, _, _, _, _) -> loc
| Node.ShipOut (loc, _, _, _)           -> loc
| Node.AddToGalley (loc, _, _)         -> loc
| Node.PutGalleyInVBox (loc, _, _)     -> loc
| Node.ModifyGalleyGlue (loc, _)      -> loc
| Node.Paragraph (loc, _)             -> loc
| Node.BeginGroup loc              -> loc
| Node.EndGroup loc                -> loc
| Node.Float (loc, _, _)               -> loc
| Node.Glyph (loc, _)                 -> loc
| Node.Letter (loc, _)                -> loc
| Node.Space loc                   -> loc
| Node.Glue (loc, _, _, _, _)            -> loc
| Node.Break (loc, _, _, _, _, _)         -> loc
| Node.Rule (loc, _, _, _)              -> loc
| Node.Image (loc, _, _, _, _)           -> loc
| Node.Accent (loc, _, _)              -> loc
| Node.HBox (loc, _, _)                -> loc
| Node.HBoxTo (loc, _, _, _)            -> loc
| Node.HBoxSpread (loc, _, _, _)        -> loc
| Node.VBox (loc, _)                  -> loc
| Node.VBoxTo (loc, _, _, _)            -> loc
| Node.VBoxSpread (loc, _, _, _)        -> loc
| Node.Phantom (loc, _, _, _)           -> loc
| Node.HLeaders (loc, _, _)            -> loc
| Node.VInsert (loc, _, _)             -> loc
| Node.Table (loc, _, _, _)             -> loc
| Node.TableRow (loc, _, _, _)          -> loc
| Node.TableCell (loc, _, _, _, _)         -> loc
| Node.LineBreak loc               -> loc
| Node.DiscretionaryBreak loc      -> loc
| Node.GfxState (loc, _)              -> loc
| Node.SimpleMath (loc, _, _)           -> loc
| Node.MathBox (loc, _, _, _)           -> loc
| Node.Leaders (loc, _, _, _, _)         -> loc
| Node.Math loc                  -> loc
| Node.Page loc                  -> loc
| Node.TableLayout (loc, _, _)       -> loc
| Node.TableEntry (loc, _, _, _, _, _, _)  -> loc
| Node.MathCode (loc, _, _)            -> loc
| Node.MathChar (loc, _)     -> loc
| Node.SubScript (loc, _)             -> loc
| Node.SuperScript (loc, _)           -> loc
| Node.Fraction (loc, _, _, _, _, _)      -> loc
| Node.Underline (loc, _)             -> loc
| Node.Overline (loc, _)              -> loc
| Node.MathAccent (loc, _, _, _)        -> loc
| Node.Root (loc, _, _, _, _, _)      -> loc
| Node.LeftRight (loc, _)            -> loc
| Node.MathStyle (loc, _)             -> loc
| Node.IndexPosition (loc, p)         -> loc


(*
  |eval_node <env> <node>| evaluates <node> and returns a pair consisting of the updated environment
  and a list of boxes to be inserted in the parent node.
*)

let rec eval_node env builder node = try
  match node with Node.Nodes n                     -> eval_node_list env builder n
  | Node.Command (loc, cmd)             -> ev_command env builder loc cmd
  | Node.CommandBox (loc, c)            -> ev_command_box env builder loc c
  | Node.GfxCommand (loc, c)            -> ev_gfx_command env builder loc c
  | Node.NewGalley (loc, n, m)           -> ev_new_galley env builder loc n m
  | Node.NewLayout (loc, n, w, h)         -> ev_new_layout env builder loc n w h
  | Node.NewArea (loc, n, x, y, w, h, t, b, c) -> ev_new_area env builder loc n x y w h t b c
  | Node.ShipOut (loc, e, o, n)           -> ev_shipout_pages env builder loc e o n
  | Node.AddToGalley (loc, g, n)         -> ev_add_to_galley env builder loc g n
  | Node.PutGalleyInVBox (loc, a, n)     -> ev_put_galley_in_vbox env builder loc a n
  | Node.ModifyGalleyGlue (loc, f)      -> ev_modify_galley_glue env builder loc f
  | Node.Paragraph (loc, b)             -> ev_paragraph env builder loc b
  | Node.BeginGroup loc              -> ev_begin_group env builder loc
  | Node.EndGroup loc                -> ev_end_group env builder loc
  | Node.Float (loc, n, b)               -> ev_float env builder loc n b
  | Node.Glyph (loc, g)                 -> ev_glyph env builder loc g
  | Node.Letter (loc, char)             -> ev_letter env builder loc char
  | Node.Space loc                   -> ev_space env builder loc
  | Node.Glue (loc, w, h, i, d)            -> ev_glue env builder loc w h i d
  | Node.Break (loc, p, h, pre, post, no)   -> ev_break env builder loc p h pre post no
  | Node.Rule (loc, w, h, d)              -> ev_rule env builder loc w h d
  | Node.Image (loc, f, fmt, w, h)         -> ev_image env builder loc f fmt w h
  | Node.Accent (loc, a, c)              -> ev_accent env builder loc a c
  | Node.HBox (loc, d, b)                -> ev_hbox env builder loc d b
  | Node.HBoxTo (loc, d, w, b)            -> ev_hbox_to env builder loc d w b
  | Node.HBoxSpread (loc, d, a, b)        -> ev_hbox_spread env builder loc d a b
  | Node.VBox (loc, b)                  -> ev_vbox env builder loc b
  | Node.VBoxTo (loc, h, b)              -> ev_vbox_to env builder loc h b
  | Node.VBoxSpread (loc, a, b)          -> ev_vbox_spread env builder loc a b
  | Node.Phantom (loc, h, v, n)           -> ev_phantom env builder loc h v n
  | Node.HLeaders (loc, w, n)            -> ev_hleaders env builder loc w n
  | Node.VInsert (loc, b, ns)            -> ev_vinsert env builder loc b ns
  | Node.Table (loc, n)                 -> ev_table env builder loc n
  | Node.TableEntry (loc, _, _, _, _, _, _)  -> ev_table_entry env builder loc
  | Node.Math (loc, n)                  -> ev_math env builder loc n
  | Node.MathCode (loc, c, n)            -> ev_math_code env builder loc c n
  | Node.MathChar (loc, (cd, f, c))     -> ev_math_char env builder loc cd f c
  | Node.SubScript (loc, n)             -> ev_sub_script env builder loc n
  | Node.SuperScript (loc, n)           -> ev_super_script env builder loc n
  | Node.Fraction (loc, n, d, l, r, t)      -> ev_fraction env builder loc n d l r t
  | Node.Underline (loc, n)             -> ev_underline env builder loc n
  | Node.Overline (loc, n)              -> ev_overline env builder loc n
  | Node.MathAccent (loc, f, c, n)        -> ev_math_accent env builder loc f c n
  | Node.Root (loc, sf, sc, lf, lc, n)      -> ev_root env builder loc sf sc lf lc n
  | Node.LeftRight (loc, ns)            -> ev_left_right env builder loc ns
  | Node.MathStyle (loc, s)             -> ev_math_style env builder loc s
  | Node.IndexPosition (loc, p)         -> ev_index_position env builder loc p

with
| VM.Types.Syntax_error (loc, msg) -> begin
    log_warn loc (UString.to_string (Array.to_list msg));

    env
  end
| VM.Types.Runtime_error msg    -> begin
    log_warn (get_location node) (UString.to_string (Array.to_list msg));

    env
  end


and eval_node_list env builder nodes = match nodes with
| []      -> env
| n::ns -> begin
    let e1 = eval_node      env builder n in
    let e2 = eval_node_list e1  builder ns in

    e2
  end


and eval_grouped_list env builder nodes = begin
  let e  = eval_node_list (save_environment env) builder nodes in
  let e2 = restore_environment e in

  Builder.add_cmd_list builder (adjust_graphics_state e e2);

  e2
end

and ev_command env builder loc cmd = begin
  if !tracing_engine then
    log_string "\n#E: command"
  else ();

  let e = cmd loc env in

  Builder.set_font builder (current_font_metric e) (current_composer e);
  Builder.set_hyphen_params builder (Galley.current_hyphen_params (current_galley e));
  Builder.add_cmd_list builder (adjust_graphics_state env e);

  e
end

and ev_command_box env builder _loc cmd = begin
  if !tracing_engine then begin
    log_string "\n#E: command-box ";

    begin match cmd with
    | `ParCmd  c -> begin match c with
      | Box.VInsert (_, _)       -> log_string "vinsert"
      | Box.CallParFunction _ -> log_string "call-par-function"
      end
    | `PageCmd c -> begin match c with
      | Box.SetNextLayout l -> begin
          log_string "set-next-layout ";
          log_uc_string l
        end
      | Box.SetMark (m, v) -> begin
          log_string "set-mark ";
          log_uc_string m;
          log_string " = ";
          log_uc_string v
        end
      | Box.CallPageFunction _ -> begin
          log_string "call-page-function"
        end
      | Box.Float (n, _) -> begin
          log_string "float";
          log_uc_string n
        end
      end
    | `GfxCmd c  -> log_string (Graphic.command_to_string c)
    | `Special _ -> log_string "special"
    end
  end
  else ();

  Builder.add_cmd builder (new_command_box cmd);

  env
end

and ev_gfx_command env builder _loc cmd = begin
  if !tracing_engine then begin
    log_string "\n#E: gfx-command ";
    log_string (Graphic.command_to_string cmd)
  end
  else ();

  let conv path =
    List.map
      (fun (ax,ay,bx,by,cx,cy,dx,dy) ->
        (ax env, ay env, bx env, by env,
         cx env, cy env, dx env, dy env))
      path in
  let c = match cmd with
 Graphic.PutBox (x, y, b)    -> Graphic.PutBox (x env, y env, b)
  | Graphic.Draw (pc, p)       -> Graphic.Draw (pc, conv p)
  | Graphic.SetColour     c -> Graphic.SetColour     c
  | Graphic.SetBgColour   c -> Graphic.SetBgColour   c
  | Graphic.SetAlpha      a -> Graphic.SetAlpha      a
  | Graphic.SetLineWidth  w -> Graphic.SetLineWidth  w
  | Graphic.SetLineCap    c -> Graphic.SetLineCap    c
  | Graphic.SetLineJoin   j -> Graphic.SetLineJoin   j
  | Graphic.SetMiterLimit l -> Graphic.SetMiterLimit l
  in

  Builder.add_cmd builder (new_command_box (`GfxCmd c));
  env
end

and ev_begin_group env _builder _loc = begin
  if !tracing_engine then
    log_string "\n#E: begin group"
  else ();

  save_environment env
end

and ev_end_group env builder _loc = begin
  if !tracing_engine then
    log_string "\n#E: end group"
  else ();

  let e = restore_environment env in

  Builder.set_font builder (current_font_metric e) (current_composer e);
  Builder.set_hyphen_params builder (Galley.current_hyphen_params (current_galley e));
  Builder.add_cmd_list builder (adjust_graphics_state env e);
  e
end

(* layout *)

and ev_new_galley env _builder loc name measure = begin
  if !tracing_engine then begin
    log_string "\n#E: new-galley ";
    log_uc_string name;
  end
  else ();

  new_galley name (measure env) loc env
end

and ev_new_layout env _builder loc name width height = begin
  if !tracing_engine then begin
    log_string "\n#E: new-layout ";
    log_uc_string name;
  end
  else ();

  new_page_layout name (width env) (height env) loc env
end

and ev_new_area env _builder loc name x y width height max_top max_bot contents = begin
  if !tracing_engine then
    log_string "\n#E: new-area"
  else ();

  let c_fun    = match contents with
 `Galley (name, t, b, m, g) ->
                     AreaGalley.contents_from_galley
                       {
                         AreaGalley.galley      = name;
                         AreaGalley.top_skip    = t env;
                         AreaGalley.bottom_skip = b env;
                         AreaGalley.min_size    = m env;
                         AreaGalley.grid_size   = g env
                       }
                 | `Float (a, t, b, f) ->
                     FloatVertical.layout
                       {
                         FloatVertical.alignment   = a;
                         FloatVertical.top_skip    = t env;
                         FloatVertical.bottom_skip = b env;
                         FloatVertical.float_sep   = f env
                       }
                 | `Footnote (sep, t, b, g, l, p, lb, h, s, m) ->
                     let galley = current_galley env in
                     Footnote.layout
                       {
                         Footnote.separator = begin
                             let (b, get) = Builder.simple_builder
                                              (current_font_metric env)
                                              (current_composer    env) in
                             let _        = eval_node_list env b sep in
                             VBox.make (get ())
                           end;
                         Footnote.top_skip          = t env;
                         Footnote.bottom_skip       = b env;
                         Footnote.grid_size         = g env;
                         Footnote.line_params       = modify_line_params l env
                                                        (Galley.line_params galley);
                         Footnote.par_params        = modify_par_params p env
                                                        (Galley.par_params galley);
                         Footnote.line_break_params = modify_line_break_params lb env
                                                        (Galley.line_break_params galley);
                         Footnote.hyphen_params     = modify_hyphen_params h loc
                                                        (Galley.hyphen_params galley);
                         Footnote.space_params      = modify_space_params s env
                                                        (Galley.space_params galley);
                         Footnote.math_params       = modify_math_params m env
                                                        (Galley.math_params galley)
                       }
                 | `Direct f ->
                     (fun page area _ ps -> begin
                        let (b, get) = Builder.simple_builder
                                         (current_font_metric env)
                                         (current_composer    env) in
                        let _        = eval_node_list env b
                                         (f (PageLayout.get_page_info page ps)
                                            (area.Page.as_pos_x,
                                             page.Page.p_height -/ area.Page.as_pos_y)) in
                        let boxes    = get () in

                        PageLayout.simple_page_update
                          (Page.put_box_on_page
                                page
                                area.Page.as_pos_x
                                area.Page.as_pos_y
                                (VBox.make boxes))
                          ps
                      end)
                 in
  let page_layout = current_page_layout env in
  let areas       = page_layout.PageLayout.pl_areas in
  let area_shape  =
    {
      Page.as_pos_x  = x       env;
                       (* internally we use a right handed coordinate system  *)
      Page.as_pos_y  = page_layout.PageLayout.pl_height -/ y env;
      Page.as_width  = width   env;
      Page.as_height = height  env;
      Page.as_top    = max_top env;
      Page.as_bottom = max_bot env
    } in
  let new_area =
    {
      PageLayout.ar_name     = name;
      PageLayout.ar_shape    = area_shape;
      PageLayout.ar_contents = c_fun
    } in
  let new_areas = Array.init
                    (Array.length areas + 1)
                    (fun i -> if i < Array.length areas then
                                areas.(i)
                              else
                                new_area) in
  set_page_layout
    {
      (current_page_layout env)

      with

      PageLayout.pl_areas = new_areas
    }
    loc
    env
end

and ev_float env builder _loc name boxes = begin
  if !tracing_engine then begin
    log_string "\n#E: float ";
    log_uc_string name
  end
  else ();

  let (b, get) = Compose.hyph_only_builder
                  (current_font_metric env)
                  (current_composer    env)
                  (Galley.hyphen_params (current_galley env)) in
  let _        = eval_node_list env b boxes in
  let items    = get () in

  Builder.add_cmd builder (new_command_box (`PageCmd (Box.Float (name, items))));
  env
end

(* paragraphs *)

and ev_paragraph env _builder loc boxes = begin
  if !tracing_engine then
    log_string "\n#E: (paragraph"
  else ();

  let (b, get) =
    Compose.hyph_only_builder
      (current_font_metric env)
      (current_composer    env)
      (Galley.hyphen_params (current_galley env)) in
  let new_env = eval_node_list (set_space_factor env num_one) b boxes in
  let bs      = get () in

  if !tracing_engine then begin
    log_string "\n#E: added to galley `";
    log_uc_string (PTable.key (galley_table new_env));
    log_string "')"
  end
  else ();

  let g1 = begin match bs with
           | [] -> current_galley new_env
           | _  -> Galley.add_paragraph (current_galley new_env) loc bs
           end in
  let g2 = List.fold_left
             Galley.add_glue
             g1
             (adjust_graphics_state env new_env) in

  set_galley
    (Galley.reset_params g2)
    loc new_env
end

and ev_add_to_galley env builder loc galley nodes = begin
  if !tracing_engine then begin
    log_string "\n#E: (add-to-galley `";
    log_uc_string galley;
    log_string "'"
  end
  else ();

  let rec iter env nodes = begin match nodes with
  | [] -> begin
      if !tracing_engine then
        log_string "\n#E: )"
      else ();

      let e = restore_environment env in

      Builder.add_cmd_list builder (adjust_graphics_state env e);
      e
    end
  | (n :: ns) -> begin
      let (b, get) = Builder.simple_builder
                       (current_font_metric env)
                       (current_composer    env) in
      let e        = eval_node env b n in
      let g        = List.fold_left
                       Galley.add_glue
                       (current_galley e)
                       (get ()) in

      iter (set_galley g loc e) ns
    end
  end in
  iter (select_galley galley loc (save_environment env)) nodes
end

and ev_put_galley_in_vbox env builder _loc top_align name = begin
  if top_align then
    Builder.add_box builder
      (Galley.put_in_vtop (PTable.get (galley_table env) name))
  else
    Builder.add_box builder
      (Galley.put_in_vbox (PTable.get (galley_table env) name));

  env
end

and ev_modify_galley_glue env _builder loc f = begin
  set_galley (Galley.modify_glue (current_galley env) (f env)) loc env
end

and ev_shipout_pages env _builder loc even odd number = begin
  if !tracing_engine then begin
    log_string "\n#E: shipout ";
    log_int number;
  end
  else ();

  let e = sync_tables env in

  try
    let even_layout = DynUCTrie.find_string even (PTable.table (page_layout_table e)) in

    try

      let odd_layout  = DynUCTrie.find_string odd (PTable.table (page_layout_table e)) in
      let abort       = if number <= 0 then PageLayout.abort_when_done else PageLayout.abort_on_page (current_page_number e + number) in

      let (pages, rs) = PageLayout.layout_run_of_pages
                          (PageLayout.layout_two_sided even_layout odd_layout) abort
                          (PageLayout.new_page_run_state
                            (current_page_number e)
                            (current_float_misplacement_demerits e)
                            (PTable.table (galley_table e))
                            (PTable.table (page_layout_table e))) in

      let x = add_pages (PageLayout.page_no rs) pages loc (set_galley_table e (PTable.update (galley_table e) (PageLayout.get_galley_table rs))) in

      log_string "\n#E: pages added.";

      x
    with Not_found -> env
  with Not_found -> env
end

(* letters and spaces *)

and ev_glyph env builder _loc glyph = begin
  if !tracing_engine then begin
    log_string "\n#E: glyph ";
    log_int glyph
  end
  else ();

  let font = current_font_metric env in

  let ((gw, gh, gd), _it) =
    FontMetric.get_glyph_metrics font glyph in
  Builder.add_box builder
    (new_glyph_box (FontMetric.index_to_glyph font glyph) font);
  env
end

and ev_letter env builder loc char = begin
  if !tracing_engine then begin
    log_string "\n#E: letter ";
    log_int char
  end
  else ();

  Builder.add_char builder char;

  adjust_space_factor char loc env
end

and ev_space env builder _loc = begin
  if !tracing_engine then
    log_string "\n#E: space"
  else ();

  let add_blank factor width =
    Builder.add_box builder
      (new_glue_box
        {
          (width)

          with

          d_stretch_factor = width.d_stretch_factor */ factor;
          d_shrink_factor  = width.d_shrink_factor  // factor
        }
        dim_zero
        False True) in

  let space_params = Galley.current_space_params (current_galley env) in

  if space_params.Galley.space_factor </ num_of_int 2 then
    begin match space_params.Galley.space_skip with
|  Some s -> add_blank space_params.Galley.space_factor s
    | None   -> add_blank space_params.Galley.space_factor
                          (FontMetric.space_glue (current_font_metric env))
    end
  else begin match space_params.Galley.xspace_skip with
  | Some s -> add_blank num_one s
  | None   -> begin
        let fm = current_font_metric env in

        begin match space_params.Galley.space_skip with
        | Some s -> add_blank
                      space_params.Galley.space_factor
                      (dim_add s (fixed_dim fm.parameter.extra_space))
        | None   -> add_blank
                      space_params.Galley.space_factor
                      (FontMetric.xspace_glue fm)
        end
      end
    end;

  env
end

and ev_glue env builder _loc width height implicit discard = begin
  let w = width  env in
  let h = height env in

  if !tracing_engine then begin
    log_string "\n#E: glue ";
    log_dim w;
    log_string " x ";
    log_dim h
  end
  else ();

  if implicit then
    Builder.add_cmd builder (new_glue_box w h implicit discard)
  else
    Builder.add_box builder (new_glue_box w h implicit discard);
  env
end

and ev_break env builder _loc penalty hyph pre_break post_break no_break = begin
  let p = match penalty with
  | None   -> if hyph then
                (Galley.current_hyphen_params (current_galley env)).JustHyph.hyphen_penalty
              else
                num_zero
  | Some x -> x
  in

  if !tracing_engine then begin
    log_string "\n#E: break";

    if p <>/ num_zero then begin
      log_string " ";
      log_num p
    end
    else ()
  end
  else ();

  let (b_pre, get_pre) = Compose.char_item_builder
                           (current_font_metric env)
                           (current_composer    env)
                           (Galley.hyphen_params (current_galley env)) in
  let e_pre            = eval_node_list env b_pre pre_break in
  let pre              = get_pre () in

  let (b_post, get_post) = Compose.char_item_builder
                             (current_font_metric e_pre)
                             (current_composer    e_pre)
                             (Galley.hyphen_params (current_galley e_pre)) in
  let e_post             = eval_node_list e_pre b_post post_break in
  let post               = get_post () in

  let (b_no, get_no) = Compose.char_item_builder
                         (current_font_metric e_post)
                         (current_composer    e_post)
                         (Galley.hyphen_params (current_galley e_post)) in
  let e_no           = eval_node_list e_post b_no no_break in
  let no             = get_no () in

  Builder.add_break builder p hyph pre post no;
  env
end

and ev_rule env builder _loc width height depth = begin
  let w = width  env in
  let h = height env in
  let d = depth  env in

  if !tracing_engine then begin
    log_string "\n#E: rule ";
    log_dim w;
    log_string " x ";
    log_dim h;
    log_string "+";
    log_dim d
  end
  else ();

  Builder.add_box builder (new_rule_box w h d);
  env
end

and ev_image env builder _loc file fmt width height = begin
  let w = width  env in
  let h = height env in

  if !tracing_engine then begin
    log_string "\n#E: image ";
    log_string file;
  end
  else ();

  Builder.add_box builder
    (new_image_box w h file fmt);
  env
end

and ev_accent env builder loc acc chr = begin
  if !tracing_engine then begin
    log_string "\n#E: accent ";
    log_int acc
  end
  else ();

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env) in
  let e        = eval_grouped_list env b chr in
  let bs       = get () in
  let font     = current_font_metric env in

  begin match bs with
  | [] -> Builder.add_char builder acc
  | [ { b_contents = CharBox (c, f) } ] -> begin
      Builder.add_box builder
        (Glyph.attach_accent font (FontMetric.index_to_glyph font acc) f c)
    end
  | ( { b_contents = CharBox (c, f) } :: _ ) -> begin
      log_warn loc "Additional characters ignored!";

      Builder.add_box builder
        (Glyph.attach_accent font (FontMetric.index_to_glyph font acc) f c)
    end
  | _ -> log_warn loc "Not a character!"
  end;

  e
end

(* boxes *)

and ev_hbox env builder _loc dir boxes = begin
  if !tracing_engine then
    log_string "\n#E: hbox"
  else ();

  let d  = match dir with
  | `LR      -> HBox.LR
  | `RL      -> HBox.RL
  | `Default -> HBox.LR (* FIX: take let from par-param *)
  in

  let (composer, get) =
    Compose.ligature_builder
      (current_font_metric env)
      (current_composer    env)
      (Galley.hyphen_params (current_galley env)) in
  let e  = eval_grouped_list env composer boxes in
  let bs = get () in

  Builder.add_box builder (HBox.make d bs);
  e
end

and ev_hbox_to env builder _loc dir width boxes = begin
  let w = width env in

  if !tracing_engine then begin
    log_string "\n#E: hbox-to ";
    log_num w
  end
  else ();

  let d  = match dir with
  | `LR      -> HBox.LR
  | `RL      -> HBox.RL
  | `Default -> HBox.LR (* FIX: take let from par-param *)
  in

  let (composer, get) =
    Compose.ligature_builder
      (current_font_metric env)
      (current_composer    env)
      (Galley.hyphen_params (current_galley env)) in
  let e        = eval_grouped_list env composer boxes in
  let bs       = get () in

  Builder.add_box builder (HBox.make_to d w bs);
  e
end

and ev_hbox_spread env builder _loc dir amount boxes = begin
  let a = amount env in

  if !tracing_engine then begin
    log_string "\n#E: hbox-spread ";
    log_num a
  end
  else ();

  let d  = match dir with
  | `LR      -> HBox.LR
  | `RL      -> HBox.RL
  | `Default -> HBox.LR (* FIX: take let from par-param *)
  in

  let (composer, get) =
    Compose.ligature_builder
      (current_font_metric env)
      (current_composer    env)
      (Galley.hyphen_params (current_galley env)) in
  let e        = eval_grouped_list env composer boxes in
  let bs       = get () in

  Builder.add_box builder (HBox.make_spread d a bs);
  e
end

and ev_vbox env builder _loc boxes = begin
  if !tracing_engine then
    log_string "\n#E: vbox"
  else ();

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env) in
  let e        = eval_grouped_list env b boxes in
  let bs       = get () in

  Builder.add_box builder (VBox.make bs);
  e
end

and ev_vbox_to env builder _loc height boxes = begin
  let h = height env in

  if !tracing_engine then begin
    log_string "\n#E: vbox-to ";
    log_num h
  end
  else ();

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env) in
  let e        = eval_grouped_list env b boxes in
  let bs       = get () in

  Builder.add_box builder (VBox.make_to h bs);
  e
end

and ev_vbox_spread env builder _loc amount boxes = begin
  let a = amount env in

  if !tracing_engine then begin
    log_string "\n#E: vbox-spread ";
    log_num a
  end
  else ();

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env) in
  let e        = eval_grouped_list env b boxes in
  let bs       = get () in

  Builder.add_box builder (VBox.make_spread a bs);
  e
end

and ev_phantom env builder _loc horiz vert nodes = begin
  let (composer, get) =
    Compose.ligature_builder
      (current_font_metric env)
      (current_composer    env)
      (Galley.hyphen_params (current_galley env)) in
  let e        = eval_grouped_list env composer nodes in
  let boxes    = get () in

  if horiz then begin
    if vert then
      Builder.add_box builder (make_phantom  (HBox.make HBox.LR boxes))
    else
      Builder.add_box builder (make_hphantom (HBox.make HBox.LR boxes))
  end
  else begin
    if vert then
      Builder.add_box builder (make_vphantom (HBox.make HBox.LR boxes))
    else
      ()
  end;

  e
end

and ev_hleaders env builder _loc width nodes = begin
  let (composer, get) =
    Compose.ligature_builder
      (current_font_metric env)
      (current_composer    env)
      (Galley.hyphen_params (current_galley env)) in
  let e        = eval_grouped_list env composer nodes in
  let boxes    = get () in
  let box      = HBox.make HBox.LR boxes;

  let f _pi (x, _y) b = begin
      let n = (floor_num ((x +/ b.b_width.d_base) // box.b_width.d_base));

      iter ((n -/ num_one) */ box.b_width.d_base) []

      where rec iter z cmds = begin
        if z </ x then
          cmds
        else
          iter
            (z -/ box.b_width.d_base)
            (Graphic.PutBox (fixed_dim (z -/ x)) dim_zero box
             :: cmds)
      end
    end;

  Builder.add_box builder
    (new_proc_box (width env) box.b_height box.b_depth f);
  e
end

and ev_vinsert env builder _loc below nodes = begin
  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env);
  let e        = eval_grouped_list env b nodes;
  let boxes    = get ();

  Builder.add_cmd builder
    (new_command_box (`ParCmd (Box.VInsert below boxes)));
  e
end

and ev_table_entry env _builder loc = begin
  log_warn loc "Ignoring table entry outside table!";

  env
end

and ev_table env builder _loc nodes = begin
  let rec eval_entries env nodes = match nodes with] -> (env, [])
  | (Node.TableEntry _loc l r t bl b c :: ns) -> begin
      let (compose, get) =
        Compose.ligature_builder
          (current_font_metric env)
          (current_composer    env)
          (Galley.hyphen_params (current_galley env));
      let e1        = eval_grouped_list env compose c;
      let boxes     = get ();
      let (e2, tes) = eval_entries e1 ns;

      let entry =
        {
          Table.te_left     = l;
          Table.te_right    = r;
          Table.te_top      = t;
          Table.te_baseline = bl;
          Table.te_bottom   = b;
          Table.te_contents = boxes
        end;

      (e2, (entry :: tes))
    end
  | (n :: ns) -> begin
      let e = eval_node env Builder.void_builder n;

      eval_entries e ns
    end
  ];

  let (e, tes)     = eval_entries env nodes;
  let (cols, rows) = List.fold_left
                       (fun (c,r) te ->
                         (max c (te.Table.te_right+1), max r (te.Table.te_bottom+1)))
                       (0,0)
                       tes;
  let line_params  = Galley.current_line_params (current_galley e);

  Builder.add_box builder (Table.make cols rows tes line_params);
  e
end

(* math *)

and ev_math env builder _loc nodes = begin
  if !tracing_engine then
    log_string "\n#E: math"
  else ();

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env);
  let e        = eval_node_list
                   (set_math_style env MathLayout.Text)
                   b nodes;
  let body     = get ();

  Builder.add_box_list builder
    (MathLayout.layout
      (current_math_style e)
      body
      (current_math_font_params e)
      (Galley.current_math_params (current_galley e)));

  set_space_factor e num_one
end

and ev_math_code env builder _loc code nodes = begin
  if !tracing_engine then
    log_string "\n#E: math-code"
  else ();

  let get_box body = match body with]  -> new_glue_box dim_zero dim_zero False False
  | [b] -> MathLayout.remove_math_box b
  | _   -> HBox.make HBox.LR (Compose.box_add_lig_kern body)
  ];

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env);
  let e        = eval_node_list env b nodes;
  let body     = get ();

  Builder.add_box builder
    (new_math_box
      code
      (get_box
        (MathLayout.layout
          (current_math_style e)
          body
          (current_math_font_params e)
          (Galley.current_math_params (current_galley e)))));
   e
end

and ev_math_char env builder _loc code (f,_) (c,_) = begin
  if !tracing_engine then begin
    log_string "\n#E: math-char ";
    log_int f;
    log_string ", ";
    log_int c
  end
  else ();

  let font = get_math_font env (current_math_style env) f;

  match code with
 Box.Operator -> Builder.add_box builder
                      (MathLayout.make_operator
                        (current_math_style env)
                        (FontMetric.index_to_glyph font c)
                        font
                        (current_math_font_params env))
  | Box.NoMath   -> Builder.add_box builder
                      (new_math_box code
                        (new_char_box c font))
  | _            -> Builder.add_box builder
                      (new_math_box code
                        (new_glyph_box (FontMetric.index_to_glyph font c) font))
  ];

  env
end

and ev_sub_script env builder _loc nodes = begin
  if !tracing_engine then
    log_string "\n#E: sub-script"
  else ();

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env);
  let e        = eval_grouped_list
                   (set_math_style env  (MathLayout.sub_style (current_math_style env)))
                   b
                   nodes;
  let script = get ();

  if script = [] then
    ()
  else
    Builder.add_box builder
      (new_math_box
        Box.SubScript
        (HBox.make HBox.LR
          (Compose.box_add_lig_kern
            (MathLayout.layout
              (current_math_style e)
              script
              (current_math_font_params e)
              (Galley.current_math_params (current_galley e))))));
  env
end

and ev_super_script env builder _loc nodes = begin
  if !tracing_engine then
    log_string "\n#E: super-script"
  else ();

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env);
  let e        = eval_node_list
                   (set_math_style env (MathLayout.super_style (current_math_style env)))
                   b
                   nodes;
  let script   = get ();

  if script = [] then
    ()
  else
    Builder.add_box builder
      (new_math_box
        Box.SuperScript
        (HBox.make HBox.LR
          (Compose.box_add_lig_kern
            (MathLayout.layout
              (current_math_style e)
              script
              (current_math_font_params e)
              (Galley.current_math_params (current_galley e))))));
  env
end

and ev_underline env builder _loc nodes = begin
  if !tracing_engine then
    log_string "\n#E: underline"
  else ();

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env);
  let e        = eval_grouped_list env b nodes;
  let body     = get ();

  Builder.add_box builder
    (MathLayout.make_underline
      (current_math_style env)
      body
      (current_math_font_params env)
      (Galley.current_math_params (current_galley env)));

  e
end

and ev_overline env builder _loc nodes = begin
  if !tracing_engine then
    log_string "\n#E: overline"
  else ();

  let style    = MathLayout.cramped_style (current_math_style env);
  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env);
  let e        = eval_grouped_list (set_math_style env style) b nodes;
  let body     = get ();

  Builder.add_box builder
    (MathLayout.make_overline
      style
      body
      (current_math_font_params env)
      (Galley.current_math_params (current_galley env)));
  e
end

and ev_math_accent env builder _loc family char nodes = begin
  if !tracing_engine then
    log_string "\n#E: math-accent"
  else ();

  let style    = MathLayout.cramped_style (current_math_style env);
  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env);
  let e        = eval_grouped_list (set_math_style env style) b nodes;
  let body     = get ();
  let font     = get_math_font env style family;

  Builder.add_box builder
    (MathLayout.make_accent
      style char font
      body
      (current_math_font_params env)
      (Galley.current_math_params (current_galley env)));

  e
end

(* |family_to_fonts <env> <style> <fam>| returns the list of fonts corresponding to <fam>. *)

and family_to_fonts env style fam = begin
  if fam < 0 then
    []
  else begin
    let text_fam    = get_math_font env MathLayout.Text    fam;
    let script_fam  = get_math_font env MathLayout.Script  fam;
    let script2_fam = get_math_font env MathLayout.Script2 fam;

    match style with
 MathLayout.Display | MathLayout.CrampedDisplay
    | MathLayout.Text    | MathLayout.CrampedText    -> [text_fam]
    | MathLayout.Script  | MathLayout.CrampedScript  -> [script_fam; text_fam]
    | MathLayout.Script2 | MathLayout.CrampedScript2 -> [script2_fam; script_fam; text_fam]
    end
  end
end

and ev_root env builder _loc small_fam small_chr large_fam large_chr nodes = begin
  if !tracing_engine then begin
    log_string "\n#E: root ";
    log_int small_fam;
    log_string " ";
    log_int small_chr;
    log_string " ";
    log_int large_fam;
    log_string " ";
    log_int large_chr
  end
  else ();

  let style = MathLayout.cramped_style (current_math_style env);

  let small_fonts = family_to_fonts env (current_math_style env) small_fam;
  let large_fonts = family_to_fonts env (current_math_style env) large_fam;

  let (b, get)    = Builder.simple_builder
                      (current_font_metric env)
                      (current_composer    env);
  let e           = eval_grouped_list (set_math_style env style) b nodes;
  let body        = get ();

  Builder.add_box builder
    (MathLayout.make_root
      (current_math_style env)
      (HBox.make HBox.LR
        (Compose.box_add_lig_kern
          (MathLayout.layout
            style body
            (current_math_font_params env)
            (Galley.current_math_params (current_galley env)))
        )
      )
      (small_chr, small_fonts, large_chr, large_fonts)
      (current_math_font_params env)
      (Galley.current_math_params (current_galley env)));

  e
end

(*
  |node_to_delim_spec <env> (<f1>, <f2>) (<c1>, <c2>) <style>| converts a node of type |MathChar| to
  a delimiter-specification.
*)

and node_to_delim_spec env (f1, f2) (c1, c2) style = begin
  let small_fonts = family_to_fonts env style f1;
  let large_fonts = if c1 <> c2 || f1 <> f2 then
                      family_to_fonts env style f2
                    else
                      [];

  (c1, small_fonts, c2, large_fonts)
end

and ev_left_right env builder loc nodes = begin
  let get_delim node = match node withNode.MathChar _ (_, f, c)] -> begin
        if !tracing_engine then begin
          log_string "\n#E: delim (";
          log_int (fst f);
          log_string ", ";
          log_int (fst c);
          log_string "; ";
          log_int (snd f);
          log_string ", ";
          log_int (snd c);
          log_string ")";
        end
        else ();

        node_to_delim_spec env f c (current_math_style env)
      end
    | _ -> begin log_warn loc "illegal delimiter!"; raise (Failure "") end
    ];

  let delims = ListBuilder.make ();
  let bodies = ListBuilder.make ();

  try
    iter env nodes
  with | Failure _ -> env

  where rec iter env nodes = match nodes with] -> begin
      log_warn loc "missing delimiter!";

      env
    end
  | [r] -> begin
      ListBuilder.add delims (get_delim r);

      Builder.add_box builder
        (MathLayout.attach_delimiters
          (current_math_style env)
          (ListBuilder.get delims)
          (ListBuilder.get bodies)
          (current_math_font_params env)
          (Galley.current_math_params (current_galley env)));
      env
    end
  | (d;n::ns) -> begin
      ListBuilder.add delims (get_delim d);

      let (b, get) = Builder.simple_builder
                       (current_font_metric env)
                       (current_composer    env) in
      let e        = eval_grouped_list env b n in
      let bs       = get () in

      ListBuilder.add bodies bs;

      iter e ns
    end
  end
end

and ev_fraction env builder loc num_nodes denom_nodes left right thick = begin
  if !tracing_engine then
    log_string "\n#E: fraction"
  else ();

  let (b, get) = Builder.simple_builder
                   (current_font_metric env)
                   (current_composer    env);
  let e1       = eval_grouped_list
                   (set_math_style env
                     (MathLayout.numerator_style   (current_math_style env)))
                   b
                   num_nodes;
  let num      = get ();
  let e2       = eval_grouped_list
                   (set_math_style e1
                     (MathLayout.denominator_style (current_math_style env)))
                   b
                   denom_nodes;
  let denom    = get ();

  match (left, right) with
  [ (Node.MathChar _ (_, fl, cl), Node.MathChar _ (_, fr, cr)) -> begin
      Builder.add_box builder
        (MathLayout.make_fraction
          (current_math_style env)
          num
          denom
          (node_to_delim_spec env fl cl (current_math_style env))
          (node_to_delim_spec env fr cr (current_math_style env))
          (thick env)
          (current_math_font_params env)
          (Galley.current_math_params (current_galley env)));

      (set_math_style e2 (current_math_style env))
    end
  | _ -> begin
      log_warn loc "Illegal delimiter!";

      env
    end
  end
end

and ev_math_style env _builder _loc s = begin
  set_math_style env s
end

and ev_index_position env builder _loc p = begin
  Builder.add_cmd builder (new_math_box (IndexPosition p) empty_box);

  env
end;

(* |evaluate <ast>| evaluates the <ast>. *)

let evaluate ast = begin
  let env = eval_node_list (initialise_environment ()) Builder.void_builder ast;

  get_pages env
end;

