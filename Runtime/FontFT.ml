
open Tools.XNum

open Unicode
open Substitute
open GlyphMetric
open FontMetric
open FreeType
open OpenType

let ft_kerning face scale c1 c2 =
  let (x,y)  = ft_get_kerning face c1 c2 ft_kerning_unscaled in
  if x <> 0 then
    `Kern (scale */ num_of_int x) 
  else
    `NoLigKern

let make_glyph_metric params extra_kern scale glyph_idx glyph =
  let (width, height, h_off, v_off, adv)  = glyph_metrics glyph in
  let left_bound  = max 0 (~-h_off)  in
  let right_bound = max 0 (h_off + width - adv)  in
  let l = scale */ num_of_int left_bound in
  let r = scale */ num_of_int right_bound in
  let user_kern_info = try
    let ki = List.assoc glyph_idx extra_kern in
    {
      ki_after_space    = params.flp_size */ ki.ki_after_space;
      ki_before_space   = params.flp_size */ ki.ki_before_space;
      ki_after_margin   = params.flp_size */ ki.ki_after_margin;
      ki_before_margin  = params.flp_size */ ki.ki_before_margin;
      ki_after_foreign  = params.flp_size */ ki.ki_after_foreign;
      ki_before_foreign = params.flp_size */ ki.ki_before_foreign
    }
  with Not_found -> GlyphMetric.zero_kern_info in

  (* If these values are zero we do not need to allocate a new structure. *) 
  let kern_info = if left_bound <> 0 || right_bound <> 0 then
    {
      user_kern_info
      with
      ki_after_foreign  = l +/ user_kern_info.ki_after_foreign;
    }
  else
    user_kern_info in
  {
    gm_width      = scale */ num_of_int adv +/ num_two */ params.flp_size */ params.flp_letter_spacing;
    gm_height     = scale */ num_of_int v_off;
    gm_depth      = scale */ num_of_int (height - v_off) ;
    gm_italic     = r;
    gm_extra      = `Normal;
    gm_extra_kern = kern_info
 }

let get_glyph_metric params extra_kern scale face =
  let (_em, _asc, _desc, _height, _ul_pos, _ul_thick)  =
    face_metrics face in
  let num_glyphs = face_num_glyphs face in
  let gm         = Array.make num_glyphs empty_glyph_metric in
  for i = 1 to num_glyphs - 1 do
    ft_load_glyph face i (ft_load_no_hinting + ft_load_no_scale + ft_load_linear_design) ;
    gm.(i - 1)  <- make_glyph_metric params extra_kern scale i (face_glyph face) 
  done;
  gm

let get_glyph_bitmap face fm code =
ft_set_char_size face fm.fm_size !default_bitmap_resolution;
  ft_load_glyph face code ft_load_monochrome;
  let desc = FontMetric.get_glyph fm code in
  let gm = FontMetric.get_glyph_metric fm desc in
  let (x, y, b)  = glyph_to_bitmap (face_glyph face)  in
  let dpp       = num_of_ints 100 7227 */ num_of_int !default_bitmap_resolution in
  let g = GlyphBitmap.make
            code
            (int_of_num (round_num (gm.gm_width  */ dpp) ) ) 
            (int_of_num (round_num (gm.gm_height */ dpp) ) ) 
            (int_of_num (round_num (gm.gm_depth  */ dpp) ) ) 
            (float_of_num dpp) 
            (float_of_num dpp) 
            (x, y - b.Bitmap.bm_height + 1) 
            (x + b.Bitmap.bm_width - 1, y)  in
  { g with GlyphBitmap.g_bitmap = b }

let builtin_encoding face char = match ft_get_char_index face char with
  | 0 -> `Undef
  | g -> `Simple g

let builtin_decoding face glyph =
  let rec lookup g =
    let rec iter (c,g_idx)  =
      if g_idx = 0 then
        []
      else if g_idx = g then
        [c]
      else
        iter (ft_get_next_char face (c,g_idx) ) 
    in iter (ft_get_first_char face) 
  in
  let rec decode g = match g with
    | `Undef              -> []
    | `Border _           -> []
    | `Simple g_idx       -> lookup g_idx
    | `GlyphIndex g_idx   -> lookup g_idx
    | `GlyphName _        -> []
    | `Char c             -> lookup (ft_get_char_index face c)
    | `Accent (a, g_idx)      -> lookup a @ lookup g_idx
    | `Sequence gs        -> List.concat (List.map (fun g -> match g with `Simple i -> lookup i | `GlyphIndex i -> lookup i | `Char i -> lookup (ft_get_char_index face i) | _ -> []) gs) 
    | `Extendable (t, m, b, _)  -> decode t @ decode m @ decode b
  in
  Array.of_list (decode glyph) 

module Composer =
struct

(* table to memorise already computed composers *) 

module SymbolTrie = struct
  type 'a t = 'a Unicode.DynUCTrie.t
  let empty = Unicode.DynUCTrie.empty
  let add key value trie = Unicode.DynUCTrie.add_string key value trie
  let find key trie = Unicode.DynUCTrie.find_string key trie
end
let empty_table = []

let rec add_composer table script lang features composer = match table with
  | []              -> [(script, lang, SymbolTrie.add features composer SymbolTrie.empty) ]
  | (s,l,m)  :: xs -> if s = script && l = lang then
                       (s,l, SymbolTrie.add features composer m)  :: xs
                     else
                       add_composer xs script lang features composer

let rec lookup_composer table script lang features = match table with
  | []              -> raise Not_found
  | (s,l,m)  :: xs -> if s = script && l = lang then
                       SymbolTrie.find features m
                     else
                       lookup_composer xs script lang features

let simple_pair_kerning_cmd k = `PairPositioningCmd (float_of_num k, 0.0) 
let tex_ligature_cmd g k1 k2 = `Ligature (g, k1, k2) (* dummy *) 

let max2_adjustment_depth _ = 0
let lookup2_adjustments _ _ = None
let match_substitution_trie _ _ _ = fun _ -> []

(* translating lookups to adjustments *) 

type lang_sys =
{
  ls_required    : int array;
  ls_tags        : Tag.tag array;
  ls_adjustments : (int array)  array
}

type inner_adj_trie = ([ 
  | `PairPositioningCmd of float * float
  | `ReplaceWithMultipleGlyphsCmd of int * [ `Simple of int ] array
  | `ReplaceWithSingleGlyphCmd of int * [ `Simple of int ]
  | `SinglePositioningCmd of float * float * float
  | `ContextPos of [ `ConstGlyph of [ `Simple of int ] | `ConstKern of float * float | `CopyCommands of int * int ] list list
  | `ContextSubst of [ `ConstGlyph of [ `Simple of int ] | `CopyCommands of int * int ] list list
] * int) SymbolTrie.t

type adjustment_table = [
  | `NoAdjustment
  | `DirectLookup of inner_adj_trie
  | `ClassPairLookup of int * int Tools.Maps.IntMap.t * int Tools.Maps.IntMap.t * ([ `PairPositioningCmd of float * float ] * int) array
]

type adj_table =
{
  at_scripts           : lang_sys Tag.TagMap.t Tag.TagMap.t;
  at_adjustment_tables : adjustment_table array array;
  at_user_adjustments  : adjustment_table array
}

let pos_to_pre_kern scale pos =
  `ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_x_off,
             scale */ num_of_int pos.OTF_Pos_Subst.p_y_off) 

let pos_to_post_kern scale pos =
  `ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_h_adv_off,
             num_zero)

let position_to_adj scale p =
  (`SinglePositioningCmd
    (scale */ num_of_int p.OTF_Pos_Subst.p_x_off, 
     scale */ num_of_int p.OTF_Pos_Subst.p_y_off, 
     scale */ num_of_int p.OTF_Pos_Subst.p_h_adv_off),
   0) 

let kern_to_adj scale p1 p2 =
  (`PairPositioningCmd
    (scale */ num_of_int (p1.OTF_Pos_Subst.p_x_off + p2.OTF_Pos_Subst.p_x_off),
     scale */ num_of_int (p1.OTF_Pos_Subst.p_h_adv_off + p2.OTF_Pos_Subst.p_x_off)),
   0)


let substitution_to_adj g =
  (`ReplaceWithSingleGlyphCmd (1, `Simple g) ,
   0) 

let multi_subst_to_adj glyphs =
  (`ReplaceWithMultipleGlyphsCmd (1, Array.map (fun g -> `Simple g) glyphs) ,
    0) 

let ligature_to_adj n lig =
  (`ReplaceWithSingleGlyphCmd (n, `Simple lig) ,
   0) 

let pos_rule_to_adj scale glyphs rule =
  let lookups = Array.make (Array.length glyphs)  [||] in
  Array.iter
    (fun l ->
        lookups.(l.OTF_Pos_Subst.prr_seq_idx)  <-
          l.OTF_Pos_Subst.prr_lookup.OTF_Pos_Subst.l_commands
    ) 
    rule;

  let adjs = Tools.ListBuilder.make ()  in
  for i = 0 to Array.length glyphs - 1 do
    let cmds = lookups.(i)  in
    let rec iter_cmds k =
      if k >= Array.length cmds then
        Tools.ListBuilder.add adjs [`ConstGlyph (`Simple glyphs.(i) ) ; `CopyCommands (i, i) ]
      else match cmds.(k)  with
          | OTF_Pos_Subst.Position pos ->
              begin try
                let p = Tools.Maps.IntMap.find glyphs.(i)  pos in
                Tools.ListBuilder.add adjs [pos_to_pre_kern scale p;

                                  `ConstGlyph (`Simple glyphs.(i) ) ;
                                  pos_to_post_kern scale p;
                                  `CopyCommands (i, i) ]
          with Not_found -> iter_cmds (k+1) end
      | _ -> iter_cmds (k+1) 
    in iter_cmds 0
  done;

  (`ContextPos (Tools.ListBuilder.get adjs), 0)
 

let subst_rule_to_adj glyphs rule =
  let lookups = Array.make (Array.length glyphs)  [||] in
  Array.iter
    (fun l ->
        lookups.(l.OTF_Pos_Subst.prr_seq_idx)  <-
          l.OTF_Pos_Subst.prr_lookup.OTF_Pos_Subst.l_commands
    ) 
    rule;

  let adjs = Tools.ListBuilder.make ()  in
  for i = 0 to Array.length glyphs - 1 do
    let cmds = lookups.(i)  in
    let rec iter_cmds k =
      if k >= Array.length cmds then
        Tools.ListBuilder.add adjs [`ConstGlyph (`Simple glyphs.(i) ) ; `CopyCommands (i, i) ]
      else match cmds.(k)  with
      | OTF_Pos_Subst.Substitution subst ->
          begin try
            let g2 = Tools.Maps.IntMap.find glyphs.(i)  subst in
            Tools.ListBuilder.add adjs [`ConstGlyph (`Simple g2) ;

                                  `CopyCommands (i, i) ]
          with Not_found -> iter_cmds (k+1) end
      | _ -> iter_cmds (k+1) 
    in iter_cmds 0
  done;

  (`ContextSubst (Tools.ListBuilder.get adjs), 0)


let pos_subst_to_adjustment scale cmd = match cmd with
  | OTF_Pos_Subst.NoCommand    -> `NoAdjustment
  | OTF_Pos_Subst.Position pos ->
      `DirectLookup
        (Tools.Maps.IntMap.fold
          (fun g p trie -> Unicode.DynUCTrie.add_list [g] (position_to_adj scale p)  trie) 
          pos
          Unicode.DynUCTrie.empty) 
  | OTF_Pos_Subst.CursiveAnchors (_entry, _exit)  -> `NoAdjustment (* FIX *) 
  | OTF_Pos_Subst.MarkToBaseAnchors (_, _)
  | OTF_Pos_Subst.MarkToLigAnchors  (_, _)
  | OTF_Pos_Subst.MarkToMarkAnchors (_, _) -> `NoAdjustment
  | OTF_Pos_Subst.Kern kerns ->
      `DirectLookup
        (Tools.Maps.IntMap.fold
          (fun g1 m trie ->
            Tools.Maps.IntMap.fold
              (fun g2 (p1,p2)  trie ->
                Unicode.DynUCTrie.add_list
                  [g1; g2]
                  (kern_to_adj scale p1 p2) 
                  trie) 
              m
              trie) 
          kerns
          Unicode.DynUCTrie.empty) 
  | OTF_Pos_Subst.KernClass (n, classes1, classes2, pos1, pos2)  ->
      `ClassPairLookup (n, classes1, classes2,
        (Array.init (Array.length pos1) 
          (fun i -> kern_to_adj scale pos1.(i)  pos2.(i) ) ) ) 
  | OTF_Pos_Subst.Substitution subst ->
      `DirectLookup
        (Tools.Maps.IntMap.fold
          (fun g s trie -> Unicode.DynUCTrie.add_list [g] (substitution_to_adj s)  trie) 
          subst
          Unicode.DynUCTrie.empty) 
  | OTF_Pos_Subst.Multiple map ->
      `DirectLookup
        (Tools.Maps.IntMap.fold
          (fun g s trie -> Unicode.DynUCTrie.add_list [g] (multi_subst_to_adj s)  trie) 
          map
          Unicode.DynUCTrie.empty) 
  | OTF_Pos_Subst.Alternate _map -> `NoAdjustment (* FIX *) 
  | OTF_Pos_Subst.Ligature ligs ->
      `DirectLookup
        (Unicode.DynUCTrie.mapi
          (fun gs s -> ligature_to_adj (Array.length gs)  s) 
          ligs) 
  | OTF_Pos_Subst.ContextGlyphPos rules ->
      `DirectLookup
        (Unicode.DynUCTrie.mapi
          (fun gs r -> pos_rule_to_adj scale gs r) 
          rules) 
  | OTF_Pos_Subst.ContextGlyphSubst rules ->
      `DirectLookup
        (Unicode.DynUCTrie.mapi
          (fun gs r -> subst_rule_to_adj gs r) 
          rules) 
  | _ -> `NoAdjustment

let lookup_to_adjustment scale lookups =
  Array.map
    (fun l -> Array.map
                (pos_subst_to_adjustment scale) 
                l.OTF_Pos_Subst.l_commands) 
    lookups

let make_adjustment_table ps_table scale user_adjustments =
  let conv_script s = 
    Tag.TagMap.map
      (fun ls ->
          let r = match ls.OTF_Pos_Subst.ls_required with
            | None   -> [||]
            | Some f -> f.OTF_Pos_Subst.f_lookups in
          let t = Array.map (fun f -> f.OTF_Pos_Subst.f_tag)      ls.OTF_Pos_Subst.ls_features in
          let l = Array.map (fun f -> f.OTF_Pos_Subst.f_lookups)  ls.OTF_Pos_Subst.ls_features in
          {
            ls_required    = r;
            ls_tags        = t;
            ls_adjustments = l
          }
        ) 
      s in
  {
    at_scripts           = Tag.TagMap.map conv_script ps_table.OTF_Pos_Subst.t_scripts;
    at_adjustment_tables = lookup_to_adjustment scale ps_table.OTF_Pos_Subst.t_lookups;
    at_user_adjustments  = Array.of_list user_adjustments
  }

let make_simple_adjustment_table face scale user_adjustments =
  let kerns_to_adjustments face scale =
    let last = face_num_glyphs face in
    let rec iter g1 g2 adj =
      if g1 > last then
        adj
      else if g2 > last then
        iter (g1 + 1)  1 adj
      else
        let (k,_)  = ft_get_kerning face g1 g2 ft_kerning_unscaled in
        let new_adj =
          if k <> 0 then
            Unicode.DynUCTrie.add_list
              [g1; g2]
              (simple_pair_kerning_cmd (scale */ num_of_int k) ,
               1) 
              adj
          else
            adj in
        iter g1 (g2 + 1)  new_adj
    in iter 1 1 Unicode.DynUCTrie.empty in

  let n   = List.length user_adjustments in
  let adj = Array.make (n+1)  `NoAdjustment in

  let _ = List.fold_left
    (fun i a -> adj.(i)  <- a; i+1) 
    0
    user_adjustments in

  adj.(n)  <- `DirectLookup (kerns_to_adjustments face scale) ;

  {
    at_scripts           = Tag.TagMap.empty;
    at_adjustment_tables = [||];
    at_user_adjustments  = adj
  }

let get_adjustments adj_table script lang_sys features =
  let mark_feature_adjs used adjs =
    Array.iter
      (fun i -> used.(i)  <- true) 
      adjs in
  let mark_lang_sys_adjs used lang_sys features =
    mark_feature_adjs used lang_sys.ls_required;
    for i = 0 to Array.length lang_sys.ls_tags - 1 do
      if Tag.TagSet.mem lang_sys.ls_tags.(i)  features then
        mark_feature_adjs used lang_sys.ls_adjustments.(i) 
      else () 
    done in

  let num_adjs = Array.length adj_table.at_adjustment_tables in
  let marks    = Array.make num_adjs false in
  let ls = try 
    let s = Tag.TagMap.find script adj_table.at_scripts in
    try
      Tag.TagMap.find lang_sys s
    with Not_found -> Tag.TagMap.find Tag.dflt_tag s
  with Not_found -> { ls_required = [||]; ls_tags = [||]; ls_adjustments = [||] } in

  mark_lang_sys_adjs marks ls features;
  let (_, active)  =
    Array.fold_right
      (fun l (i, lu)  ->
          if marks.(i)  then
            (i-1, l :: lu) 
          else
            (i-1, lu) 
        ) 
      adj_table.at_adjustment_tables
      (num_adjs - 1, [])  in
  match adj_table.at_user_adjustments with
  | [||] -> active
  | _    -> adj_table.at_user_adjustments :: active

let make_matcher memo_table scale get_border_glyph adj_table script features =
  let s_tag = Tag.latn_tag in
  let l_tag = Tag.dflt_tag in
  let trie =
    try
      lookup_composer !memo_table s_tag l_tag features
    with
    Not_found ->
        let f_tags = Array.fold_left
                       (fun set f ->
                         Tag.TagSet.add
                           (Tag.make_tag_uc (SymbolTable.symbol_to_string f) ) 
                           set) 
                       Tag.TagSet.empty
                       features in
        let adj = get_adjustments adj_table s_tag l_tag f_tags in
        let max_depth = max2_adjustment_depth adj in
        let is_empty (n, _)         = (n > max_depth)  in
        let prefix (n, glyphs)  g   = (n+1, g :: glyphs)  in
        let root_value (_, glyphs)  =
          lookup2_adjustments adj (List.rev glyphs)  in
        let trie = (is_empty, prefix, root_value)  in
        memo_table := add_composer !memo_table s_tag l_tag features trie;
        trie in
  let compose fm glyphs = match_substitution_trie get_border_glyph trie (0, []) glyphs in
  let base = FontMetric.get_glyph_composer FontMetric.empty_font (Unicode.UString.uc_string_of_ascii "latn") Unicode.SymbolTable.SymbolSet.empty in
  { base with gc_compose = compose }

let make_simple_matcher face scale get_border_glyph extra_adjustments =
  let max_depth = max2_adjustment_depth extra_adjustments in
  let is_empty (n, _)         = (n > max_depth)  in
  let prefix (n, glyphs)  g   = (n+1, g :: glyphs)  in
  let root_value (_, glyphs)  =
    match lookup2_adjustments extra_adjustments (List.rev glyphs)  with
    | Some cmds -> Some cmds
    | None             ->
      match glyphs with
      | [g2; g1] -> begin match ft_kerning face scale g1 g2 with
          | `NoLigKern          -> None
          | `Kern k             -> Some (simple_pair_kerning_cmd k, 1) 
          | `Ligature (l, s, k1, k2) -> Some (tex_ligature_cmd (`Simple l) k1 k2, s) 
          end
      | _ -> None in
  let trie = (is_empty, prefix, root_value) in
  let compose fm glyphs = match_substitution_trie get_border_glyph trie (0, []) glyphs in
  let base = FontMetric.get_glyph_composer FontMetric.empty_font (Unicode.UString.uc_string_of_ascii "latn") Unicode.SymbolTable.SymbolSet.empty in
  { base with gc_compose = compose }

let get_composer face scale get_border_glyph (pos, subst)  p_table s_table fm scr feat =
  let feat_arr = Array.of_list (Unicode.SymbolTable.SymbolSet.elements feat) in
  if Array.length pos.at_adjustment_tables = 0 && Array.length pos.at_user_adjustments = 0 then
    simple_composer fm (make_matcher s_table scale get_border_glyph subst scr feat_arr) 
  else
    if Array.length subst.at_adjustment_tables = 0 && Array.length subst.at_user_adjustments = 0 then
      simple_composer fm (make_matcher p_table scale get_border_glyph pos scr feat_arr) 
    else
      two_phase_composer fm (make_matcher s_table scale get_border_glyph subst scr feat_arr) 
                            (make_matcher p_table scale get_border_glyph pos   scr feat_arr) 

end


let read_ft file name params =
  let face = ft_new_face file in
  if ft_is_postscript face then begin
    (* look for an afm file *) 
    if String.length file >= 4 then
      ignore (ft_attach_file face (String.sub file 0 (String.length file - 4)  ^ ".afm") ) 
    else () ;
    ignore (ft_attach_file face (file ^ ".afm") )
  end else () ;
  let last_glyph = face_num_glyphs face - 1 in
  let (em, asc, desc, _height, _ul_pos, _ul_thick)  =
    face_metrics face in
  let (enc,dec)  = match params.flp_encoding with
  | [| |] -> (builtin_encoding face,
              builtin_decoding face) 
  | m     -> (Encodings.charmap_encoding (Encodings.fake_encoding m) ,
              Encodings.array_decoding m)  in
  let lookup_char x = match enc x with
  | `Simple g -> g
  | _        -> (-1)  in
  let lookup_name x =
    if ft_has_ps_glyph_names face then
      let rec iter i =
        if i > face_num_glyphs face then
          (-1) 
        else if ft_get_glyph_name face i = x then
          i - 1
        else
          iter (i+1) 
      in iter 1
    else
      (-1)  in
  let glyph_spec_to_index lookup_char lookup_name spec = match spec with
    | `Char c       -> lookup_char c
    | `GlyphName n  -> lookup_name n
    | `Simple i     -> i
    | `GlyphIndex i -> i
    | _             -> (-1)  in
  let adjustment_spec_to_table _ _ trie =
    `DirectLookup (Encodings.GlyphSpecTrie.fold
      (fun gs v acc -> Unicode.DynUCTrie.add_list (Array.to_list gs) (Obj.magic v) acc) 
      trie Unicode.DynUCTrie.empty) in
  let add_border_kern _ _ _ _ _ extra_pos = extra_pos in
  let size         = params.flp_size in
  let scale        = size // num_of_int em in
  let design_size  = scale */ num_of_int (asc - desc)  in
  let extra_kern =
    List.map
      (fun (g,k)  ->
        (glyph_spec_to_index lookup_char lookup_name g, k) ) 
      params.flp_extra_kern in
  let glyph_metric = get_glyph_metric params extra_kern scale face in
  let space_glyph  = ft_get_char_index face 32 in
  let x_glyph      = ft_get_char_index face 102 in
  let m_glyph      = ft_get_char_index face 77 in
  let space        = if space_glyph > 0 then                               (* width of " "  *) 
                        glyph_metric.(space_glyph - 1) .gm_width
                      else
                        size // num_of_int 3 in
  let x_height     = if x_glyph > 0 then                                   (* height of "x" *) 
                        glyph_metric.(x_glyph - 1) .gm_width
                      else
                        size // num_of_int 2 in
  let quad         = if m_glyph > 0 then                                   (* width of "M"  *) 
                        glyph_metric.(m_glyph - 1) .gm_width
                      else
                        size in
  let hyphen_glyph = match params.flp_hyphen_glyph with
                     | `Undef -> builtin_encoding face 45
                     | h     -> h in
  let get_border_glyph b = match b with
  | Margin  -> `Simple (last_glyph + 1) 
  | Space   -> `Simple (last_glyph + 2) 
  | Foreign -> `Simple (last_glyph + 3)  in
  let s_table = ref Composer.empty_table in
  let p_table = ref Composer.empty_table in
  let (font_type, adj_table)  =
    let (ts, pos, subst)  = try
        let ts = read_font_tables file in
        match get_pos_subst ts with
        | (Some p, Some s)  -> (ts, p, s) 
        | (Some p, None)    -> (ts, p, OTF_Pos_Subst.empty_pos_subst) 
        | (None,   Some s)  -> (ts, OTF_Pos_Subst.empty_pos_subst, s) 
        | (None,   None)    -> (ts, OTF_Pos_Subst.empty_pos_subst,
                                        OTF_Pos_Subst.empty_pos_subst) 
      with _ -> (Tag.TagMap.empty,
              OTF_Pos_Subst.empty_pos_subst,
              OTF_Pos_Subst.empty_pos_subst)  in
    let font_type =
      if ft_is_sfnt face then
        if is_cff ts then `OpenType else `TrueType
      else if ft_is_postscript face then `Type1
      else `TFM in
    let extra_pos =
      if Encodings.GlyphSpecTrie.is_empty params.flp_extra_pos then []
      else [ adjustment_spec_to_table lookup_char lookup_name params.flp_extra_pos ] in
    let extra_subst =
      if Encodings.GlyphSpecTrie.is_empty params.flp_extra_subst then []
      else [ adjustment_spec_to_table lookup_char lookup_name params.flp_extra_subst ] in
    let user_pos =
      add_border_kern (last_glyph + 1) (last_glyph + 2) (last_glyph + 3) params.flp_size extra_kern extra_pos in
    let pos_adj =
      if Array.length pos.OTF_Pos_Subst.t_lookups = 0 then
        Composer.make_simple_adjustment_table face scale user_pos
      else
        Composer.make_adjustment_table pos scale user_pos in
    let subst_adj = Composer.make_adjustment_table subst scale extra_subst in
    (font_type, (pos_adj, subst_adj)) in
  let composer = { get = (fun fm s f ->
    Obj.magic (Composer.get_composer face scale get_border_glyph adj_table p_table s_table fm s f)) } in
  let fm_char_metrics = Array.init last_glyph (fun i ->
    let idx = i + 1 in
    let m = glyph_metric.(i) in
    {
      cm_char = idx;
      cm_glyph = `Simple idx;
      cm_width = m.gm_width;
      cm_height = m.gm_height;
      cm_depth = m.gm_depth;
      cm_italic = m.gm_italic;
      cm_top_accent = None;
      cm_bot_accent = None;
      cm_top_left_kern = num_zero;
      cm_top_right_kern = num_zero;
      cm_bot_left_kern = num_zero;
      cm_bot_right_kern = num_zero;
      cm_lig_kern = [];
      cm_extensible = None;
    }) in
  {
    fm_name = name; fm_design_size = design_size; fm_size = size; fm_checksum = 0l; fm_slant = num_zero;
    fm_space = space; fm_space_stretch = space // num_of_int 2; fm_space_shrink = space // num_of_int 3;
    fm_x_height = x_height; fm_quad = quad; fm_extra_space = num_zero;
    fm_num1 = num_zero; fm_num2 = num_zero; fm_num3 = num_zero;
    fm_denom1 = num_zero; fm_denom2 = num_zero;
    fm_sup1 = num_zero; fm_sup2 = num_zero; fm_sup3 = num_zero;
    fm_sub1 = num_zero; fm_sub2 = num_zero;
    fm_sup_drop = num_zero; fm_sub_drop = num_zero;
    fm_delim1 = num_zero; fm_delim2 = num_zero;
    fm_axis_height = num_zero; fm_default_rule_thickness = num_zero;
    fm_big_op_spacing1 = num_zero; fm_big_op_spacing2 = num_zero; fm_big_op_spacing3 = num_zero;
    fm_big_op_spacing4 = num_zero; fm_big_op_spacing5 = num_zero;
    fm_char_metrics = fm_char_metrics;
    fm_hyphen_char = (match hyphen_glyph with `Simple i | `GlyphIndex i -> i | _ -> -1);
    fm_skew_char = -1; fm_skew_glyph = params.flp_skew_glyph; fm_type = font_type; fm_get_composer = composer;
  }
