
open Tools.XNum
open Tools.Maps
module IO = Tools.IO
open Unicode

module Tag =
struct

type tag = int32

let compose_tag high low = begin
  Int32.logor
    (Int32.shift_left (Int32.of_int high) 16)
    (Int32.of_int low)
end

let make_tag str = begin
  if String.length str <> 4 then
    invalid_arg "make_tag: 4 characters expected"
  else begin
    let d0 = int_of_char str.[0] in
    let d1 = int_of_char str.[1] in
    let d2 = int_of_char str.[2] in
    let d3 = int_of_char str.[3] in

    if d0 >= 32 && d0 <= 126
    && d1 >= 32 && d1 <= 126
    && d2 >= 32 && d2 <= 126
    && d3 >= 32 && d3 <= 126 then
      compose_tag ((d0 lsl 8) lor d1)
                  ((d2 lsl 8) lor d3)
    else
      invalid_arg "make_tag: invalid character"
  end end


let make_tag_uc str = begin
  if Array.length str <> 4 then
    invalid_arg "make_tag_uc: 4 characters expected"
  else begin
    let d0 = str.(0) in
    let d1 = str.(1) in
    let d2 = str.(2) in
    let d3 = str.(3) in

    if d0 >= 32 && d0 <= 126
    && d1 >= 32 && d1 <= 126
    && d2 >= 32 && d2 <= 126
    && d3 >= 32 && d3 <= 126 then
      compose_tag ((d0 lsl 8) lor d1)
                  ((d2 lsl 8) lor d3)
    else
      invalid_arg "make_tag_uc: invalid character"
  end end


let tag_to_string tag = begin
  let str = String.make 4 ' ' in

  let x = Int32.to_int (Int32.logand (Int32.shift_right tag 16)
                                     (Int32.of_int 0xffff)) in
  let y = Int32.to_int (Int32.logand tag (Int32.of_int 0xffff)) in

  Bytes.set (Bytes.of_string str) 0 (char_of_int ((x lsr 8) land 0xff));
  Bytes.set (Bytes.of_string str) 1 (char_of_int (x land 0xff));
  Bytes.set (Bytes.of_string str) 2 (char_of_int ((y lsr 8) land 0xff));
  Bytes.set (Bytes.of_string str) 3 (char_of_int (y land 0xff));

  str
end

let empty_tag    = make_tag "    "
let latn_tag     = make_tag "latn"
let dflt_tag     = make_tag "dflt"
let size_tag     = make_tag "size"
(*
let vkrn_tag     = make_tag "vkrn"
*)

let ttcf_tag = make_tag "ttcf"
let true_tag = make_tag "true"
let otto_tag = make_tag "OTTO"
let cff_tag  = make_tag "CFF "
let cmap_tag = make_tag "cmap"
let glyf_tag = make_tag "glyf"
let gdef_tag = make_tag "GDEF"
let gpos_tag = make_tag "GPOS"
let gsub_tag = make_tag "GSUB"
(*
let bdat_tag = make_tag "bdat"
let ebdt_tag = make_tag "EBDT"
let bloc_tag = make_tag "bloc"
let eblc_tag = make_tag "EBLC"
let bhed_tag = make_tag "bhed"
*)
let head_tag = make_tag "head"
let hhea_tag = make_tag "hhea"
let hmtx_tag = make_tag "hmtx"
let kern_tag = make_tag "kern"
let loca_tag = make_tag "loca"
let maxp_tag = make_tag "maxp"
let name_tag = make_tag "name"
let post_tag = make_tag "post"
let os2_tag  = make_tag "OS/2"
(*
let vhea_tag = make_tag "vhea"
let vmtx_tag = make_tag "vmtx"
let vorg_tag = make_tag "VORG"
let acnt_tag = make_tag "acnt"
let feat_tag = make_tag "feat"
let lcar_tag = make_tag "lcar"
let mort_tag = make_tag "mort"
let morx_tag = make_tag "morx"
let opbd_tag = make_tag "opbd"
let prop_tag = make_tag "prop"
*)
let cvt_tag  = make_tag "cvt "
let prep_tag = make_tag "prep"
let fpgm_tag = make_tag "fpgm"
(*
let gvar_tag = make_tag "gvar"
let fvar_tag = make_tag "fvar"
let avar_tag = make_tag "avar"
let cvar_tag = make_tag "cvar"
*)

module OrderedTag = struct type t = tag let compare = (compare : tag -> tag -> int); end
module TagMap = Map.Make(OrderedTag)
module TagSet = Set.Make(OrderedTag)

let init_map num get_key get_value = begin
  let rec iter i map = begin
    if i >= num then
      map
    else
      iter (i+1) (IntMap.add (get_key i) (get_value i) map)
  end in
  iter 0 IntMap.empty
end
end

module Table =
struct

type table = string

let read_u8 table off = int_of_char (table.[off])

let read_u16 table off = begin
  let x = read_u8 table off in
  let y = read_u8 table (off+1) in

  0x100 * x + y
end

let read_u24 table off = begin
  let x = read_u8 table off in
  let y = read_u8 table (off+1) in
  let z = read_u8 table (off+2) in

  0x10000 * x + 0x100 * y + z
end

let read_u32 table off = begin
  let x = read_u16 table off in
  let y = read_u16 table (off+2) in

  Tag.compose_tag x y
end

let read_i8 table off = begin
  let x = read_u8 table off in

  if x > 0x7f then
    x - 0x100
  else
    x
end

let read_i16 table off = begin
  let x = read_u16 table off in

  if x > 0x7fff then
    x - 0x10000
  else
    x
end

let read_u16_array table off len = begin
  Array.init len
    (fun i -> read_u16 table (off + 2 * i))
end

end

module OTF_Pos_Subst =
struct

open Table
open Tag

type feature =
{
  f_tag         : tag;
  f_params      : int;
  f_lookups     : int array;
  f_script_list : tag TagMap.t
}

let flags_right_to_left        = 0x0001
let flags_ignore_base_glyphs   = 0x0002
let flags_ignore_ligatures     = 0x0004
let flags_ignore_marks         = 0x0008
let flags_mark_attachment_type = 0xff00

type lang_sys =
{
  ls_required : feature option;
  ls_features : feature array
}

type positioning =
{
  p_x_off     : int;
  p_y_off     : int;
  p_h_adv_off : int;
  p_v_adv_off : int
}

and pos_rule_record =
{
  prr_seq_idx : int;
  prr_lookup  : lookup
}

and 'a pos_subst_rule =
{
  psr_data    : 'a;
  psr_lookups : pos_rule_record array
}

and pos_subst_command =
  NoCommand
| Position             of positioning IntMap.t
| CursiveAnchors       of (int * int) IntMap.t * (int * int) IntMap.t
| MarkToBaseAnchors    of (int * int * int * int) array * (int * (int * int) array) array
| MarkToLigAnchors     of (int * int * int * int) array * (int * ((int * int) array) array) array
| MarkToMarkAnchors    of (int * int * int * int) array * (int * (int * int) array) array
| Kern                 of ((positioning * positioning) IntMap.t) IntMap.t
| KernClass            of int * int IntMap.t * int IntMap.t * positioning array * positioning array
| Substitution         of int IntMap.t
| Multiple             of (int array) IntMap.t
| Alternate            of (int array) IntMap.t
| Ligature             of int DynUCTrie.t
| ContextGlyphPos      of (pos_rule_record array) DynUCTrie.t
| ContextGlyphSubst    of (pos_rule_record array) DynUCTrie.t
| ContextClassPos      of int IntMap.t * ((int array) pos_subst_rule) array
| ContextClassSubst    of int IntMap.t * ((int array) pos_subst_rule) array
| ContextCoveragePos   of (((int array) array) pos_subst_rule) array
| ContextCoverageSubst of (((int array) array) pos_subst_rule) array
| ChainGlyphPos        of (((int array * int array * int array) pos_subst_rule) array)
| ChainGlyphSubst      of (((int array * int array * int array) pos_subst_rule) array)
| ChainClassPos        of int IntMap.t * int IntMap.t * int IntMap.t *
                          ((int array * int array * int array) pos_subst_rule) array
| ChainClassSubst      of int IntMap.t * int IntMap.t * int IntMap.t *
                          ((int array * int array * int array) pos_subst_rule) array
| ChainCoveragePos     of ((int array) array * (int array) array * (int array) array) pos_subst_rule array
| ChainCoverageSubst   of ((int array) array * (int array) array * (int array) array) pos_subst_rule array
| ReverseSubst         of int array * int array * (int array) array * (int array) array


and lookup =
{
  l_flags    : int;
  l_commands : pos_subst_command array
}

type size_params =
{
  design_size         : int;
  font_style_id       : int;
  font_style_name     : int;
  design_range_bottom : int;
  design_range_top    : int
}

type pos_subst_table =
{
  t_scripts : (lang_sys TagMap.t) TagMap.t;
  t_lookups : lookup array;
  t_size    : size_params option
}

let empty_pos_subst =
{
  t_scripts = TagMap.empty;
  t_lookups = [||];
  t_size    = None
}

let make_pos_subst_table scripts lookups size =
{
  t_scripts = scripts;
  t_lookups = lookups;
  t_size    = size
}

(* access method *)

let get_lookups ps_table script lang_sys features = let mark_feature_lookups lookups feature = begin
    Array.iter
      (fun l -> lookups.(l) <- true)
      feature.f_lookups
  end in
  let mark_lang_sys_lookups lookups lang_sys features = begin
    begin match lang_sys.ls_required with
    | None   -> ()
    | Some f -> mark_feature_lookups lookups f
    end;

    Array.iter
      (fun f -> if TagSet.mem f.f_tag features then
                  mark_feature_lookups lookups f
                else ())
      lang_sys.ls_features
  end in

  let num_lookups = Array.length ps_table.t_lookups in
  let marks       = Array.make num_lookups false in

  let s  = TagMap.find script ps_table.t_scripts in

  let ls = try
             TagMap.find lang_sys s
           with
           Not_found -> TagMap.find dflt_tag s in

  mark_lang_sys_lookups marks ls features;

  let (_, active_lookups) =
    Array.fold_right
      (fun l (i, lu) -> begin
          if marks.(i) then
            (i-1, (l :: lu))
          else
            (i-1, lu)
        end)
      ps_table.t_lookups
      (num_lookups-1, []) in

  active_lookups

(* parsing routines *)

let parse_size_params table table_off =
{
  design_size         = read_u16 table table_off;
  font_style_id       = read_u16 table (table_off+2);
  font_style_name     = read_u16 table (table_off+4);
  design_range_bottom = read_u16 table (table_off+6);
  design_range_top    = read_u16 table (table_off+8)
}

let parse_features table table_off = begin
  let num_features = read_u16 table table_off in

  let read_feature i = begin
    let f_off  = table_off + 6 * i + 2 in
    let tag    = read_u32 table f_off in
    let offset = read_u16 table (f_off + 4) in

    let params      = read_u16       table (table_off + offset) in
    let num_lookups = read_u16       table (table_off + offset + 2) in
    let lookups     = read_u16_array table (table_off + offset + 4) num_lookups in

    {
      f_tag         = tag;
      f_params      = params;
      f_lookups     = lookups;
      f_script_list = TagMap.empty
    }
  end in

  Array.init num_features read_feature
end

let process_lang_sys table table_off (required_script_list, script_list) script lang = begin
  let f = read_u16 table (table_off + 2) in
  let n = read_u16 table (table_off + 4) in

  if f = 0xffff then
    ()
  else
    required_script_list.(f) <- TagMap.add script lang required_script_list.(f);

  for i = 0 to n-1 do
    let f = read_u16 table (table_off + 2 * i + 6) in

    script_list.(f) <- TagMap.add script lang script_list.(f)
  done
end

let parse_scripts table table_off features = begin
  let read_lang_sys table_off = begin
    let f = read_u16 table (table_off + 2) in
    let n = read_u16 table (table_off + 4) in

    {
      ls_required =
          if f = 0xffff then
            None
          else
            Some features.(f);
      ls_features =
          Array.init
            n
            (fun i -> begin
                let f = read_u16 table (table_off + 2 * i + 6) in

                features.(f)
              end)
    }
  end in
  let read_script i scripts = begin
    let script_tag = read_u32 table (table_off + 6 * i + 2) in
    let script_off = read_u16 table (table_off + 6 * i + 6) in

    let off          = table_off + script_off in
    let def_lang_sys = read_u16 table off in
    let num_langs    = read_u16 table (off + 2) in

    let ls =
      if def_lang_sys <> 0 then
        TagMap.add dflt_tag (read_lang_sys (off + def_lang_sys)) TagMap.empty
      else
        TagMap.empty in

    let rec iter i ls = begin
      if i >= num_langs then
        TagMap.add script_tag ls scripts
      else begin
        let t = read_u32 table (off + 6 * i + 4) in
        let o = read_u16 table (off + 6 * i + 8) in

        iter (i+1) (TagMap.add t (read_lang_sys (off + o)) ls)
      end
    end in

    iter 0 ls
  end in

  let num_scripts = read_u16 table table_off in

  let rec iter i scripts = begin
    if i >= num_scripts then
      scripts
    else
      iter (i+1) (read_script i scripts)
  end in

  iter 0 TagMap.empty
end


let get_coverage_table table table_off = begin
  match read_u16 table table_off with
   1 -> begin
      let num = read_u16 table (table_off + 2) in

      read_u16_array table (table_off + 4) num
    end
  | 2 -> begin
      let rel_num = read_u16 table (table_off + 2) in

      let rec iter i num_glyphs intervals = begin
        if i >= rel_num then begin
          let glyphs = Array.make num_glyphs 0 in

          List.iter
            (fun (first, last, ind) ->
                for i = 0 to last - first do
                  glyphs.(ind + i) <- first + i
                done
            )
            intervals;

          glyphs
        end
        else begin
          let first = read_u16 table (table_off + 6 * i + 4) in
          let last  = read_u16 table (table_off + 6 * i + 6) in
          let ind   = read_u16 table (table_off + 6 * i + 8) in

          iter (i+1) (max num_glyphs (ind + last - first + 1))
                     ((first, last, ind) :: intervals)
        end
      end in
      iter 0 0 []
    end
  | _ -> raise (Failure "invalid coverage table")
end

let parse_lookups table table_off read = begin
  let num_lookups    = read_u16 table table_off in
  let lookup_offsets = read_u16_array table (table_off + 2) num_lookups in

  let rec read_lookup l = begin
    let off    = table_off + lookup_offsets.(l) in
    let l_type = read_u16 table off in
    let flags  = read_u16 table (off + 2) in
    let num    = read_u16 table (off + 4) in

    let offsets = read_u16_array table (off + 6) num in

    let commands = Array.init num (fun i -> read read_lookup table (off + offsets.(i)) l_type) in
    {
      l_flags    = flags;
      l_commands = commands
    }
  end in

  Array.init num_lookups read_lookup
end

let read_class_table table table_off = begin
  match read_u16 table table_off with
   1 -> begin
      let start = read_u16 table (table_off + 2) in
      let num   = read_u16 table (table_off + 4) in

      init_map num
        (fun i -> start + i)
        (fun i -> read_u16 table (table_off + 2 * i + 6))
    end
  | 2 -> begin
      let num = read_u16 table (table_off + 2) in

      let rec iter i ct = begin
        if i >= num then
          ct
        else begin
          let first = read_u16 table (table_off + 6 * i + 4) in
          let last  = read_u16 table (table_off + 6 * i + 6) in
          let cls   = read_u16 table (table_off + 6 * i + 8) in

          let rec add k ct = begin
            if k > last then
              iter (i+1) ct
            else
              add (k+1) (IntMap.add k cls ct)
          end in
          add first ct
        end
      end in
      iter 0 IntMap.empty
    end
  | _ -> raise (Failure "unknown class table format")
end

let read_pos_rule_record table table_off read_lookup num = begin
  Array.init num
    (fun i -> { prr_seq_idx =              read_u16 table (table_off + 4 * i);
                prr_lookup  = read_lookup (read_u16 table (table_off + 4 * i + 2))})
end

let read_context_subrule table table_off read_lookup first i = begin
  let off = table_off + read_u16 table (table_off + 2 * i + 2) in

  let num1 = read_u16 table off in
  let num2 = read_u16 table (off + 2) in

  let first_array =
    Array.init num1
      (fun i ->
          if i = 0 then
            first
          else
            read_u16 table (off + 2 * i + 2)
        ) in

  let sl_off = off + 2 * num1 + 2 in

  (first_array,
   read_pos_rule_record table sl_off read_lookup num2)
end

let read_context_1 _read read_lookup table table_off is_gpos = begin
  let read_rule glyphs i = begin
    let off = table_off + read_u16 table (table_off + 2 * i + 6) in

    if off = table_off then
      [| |]
    else begin
      let num_sub = read_u16 table off in

      Array.init num_sub (read_context_subrule table off read_lookup glyphs.(i))
    end
  end in

  let coverage  = read_u16 table (table_off + 2) in
  let num_rules = read_u16 table (table_off + 4) in

  let glyphs = get_coverage_table table (table_off + coverage) in

  let rules = Array.init num_rules (read_rule glyphs) in

  let rec iter i k trie = begin
    if i >= Array.length rules then begin
      if is_gpos then
        ContextGlyphPos   trie
      else
        ContextGlyphSubst trie
    end
    else if k >= Array.length rules.(i) then
      iter (i+1) 0 trie
    else begin
      let (glyphs, lookups) = rules.(i).(k) in

      iter i (k+1) (DynUCTrie.add_string glyphs lookups trie)
    end
  end in
  iter 0 0 DynUCTrie.empty
end


let read_context_2 _read read_lookup table table_off is_gpos = begin
  let read_rule table_off i = begin
    let off = table_off + read_u16 table (table_off + 2 * i) in

    if off = table_off then
      [| |]
    else begin
      let num_sub = read_u16 table off in

      Array.init num_sub (read_context_subrule table off read_lookup i)
    end
  end in

(*  let coverage = read_u16 table (table_off + 2); *)
  let class_off = read_u16 table (table_off + 4) in
  let num_rules = read_u16 table (table_off + 6) in

(*  let glyphs    = get_coverage_table table (table_off + coverage); *)
  let class_table = read_class_table table (table_off + class_off) in

  let rules = Array.init num_rules (read_rule (table_off + 8)) in

  let num_psr =
    Array.fold_left
      (fun n sr -> n + Array.length sr)
      0
      rules in
  let ps_rules = Array.make num_psr { psr_data = [||]; psr_lookups = [||] } in

  let rec iter i k n = begin
    if i >= Array.length rules then begin
      if is_gpos then
        ContextClassPos   (class_table, ps_rules)
      else
        ContextClassSubst (class_table, ps_rules)
    end
    else if k >= Array.length rules.(i) then
      iter (i+1) 0 n
    else begin
      let (classes, lookups) = rules.(i).(k) in

      ps_rules.(n) <-
        {
          psr_data    = classes;
          psr_lookups = lookups
        };

      iter i (k+1) (n+1)
    end
  end in
  iter 0 0 0
end


let read_context_3 _read read_lookup table table_off is_gpos = begin
  let num_glyphs = read_u16 table (table_off + 2) in
  let num_sl     = read_u16 table (table_off + 4) in

  let coverage   = read_u16_array table (table_off + 6) num_glyphs in

  let sl = read_pos_rule_record table (table_off + 2 * num_glyphs + 6) read_lookup num_sl in

  let glyphs =
    Array.map
      (fun off -> get_coverage_table table (table_off + off))
      coverage in

  let ps_rules =
    [| {
         psr_data    = glyphs;
         psr_lookups = sl
       } |] in

  if is_gpos then
    ContextCoveragePos   ps_rules
  else
    ContextCoverageSubst ps_rules
end

let rec read_context read read_lookup table table_off is_gpos = begin
  match read_u16 table table_off with
   1 -> read_context_1 read read_lookup table table_off is_gpos
  | 2 -> read_context_2 read read_lookup table table_off is_gpos
  | 3 -> read_context_3 read read_lookup table table_off is_gpos
  | _ -> NoCommand
end

let read_chaining_subrule table table_off read_lookup first i = begin
  let b_off   = table_off + read_u16 table (table_off + 2 * i + 2) in
  let b_num   = read_u16 table b_off in
  let b_array = read_u16_array table (b_off + 2) b_num in

  let i_off = b_off + 2 * b_num + 2 in
  let i_num = read_u16 table i_off in

  let i_array =
    Array.init i_num
      (fun i ->
          if i = 0 then
            first
          else
            read_u16 table (i_off + 2 * i)
        ) in

  let a_off = i_off + 2 * i_num + 2 in
  let a_num = read_u16 table a_off in

  let a_array = read_u16_array table (a_off + 2) a_num in

  let p_off = a_off + 2 * a_num + 2 in
  let num_p = read_u16 table p_off in

  (b_array, i_array, a_array,
   read_pos_rule_record table (p_off + 2) read_lookup num_p)
end

let read_chaining_1 _read read_lookup table table_off is_gpos = begin
  let read_rule glyphs i = begin
    let off = table_off + read_u16 table (table_off + 2 * i + 6) in

    if off = table_off then
      [| |]
    else begin
      let num_sub = read_u16 table off in

      Array.init num_sub (read_chaining_subrule table off read_lookup glyphs.(i))
    end
  end in

  let coverage  = read_u16 table (table_off + 2) in
  let num_rules = read_u16 table (table_off + 4) in

  let glyphs = get_coverage_table table (table_off + coverage) in

  let rules = Array.init num_rules (read_rule glyphs) in

  let num_psr =
    Array.fold_left
      (fun n sr -> n + Array.length sr)
      0
      rules in
  let ps_rules = Array.make num_psr { psr_data = ([||], [||], [||]); psr_lookups = [||] } in

  let rec iter i k n = begin
    if i >= Array.length rules then begin
      if is_gpos then
        ChainGlyphPos   ps_rules
      else
        ChainGlyphSubst ps_rules
    end
    else if k >= Array.length rules.(i) then
      iter (i+1) 0 n
    else begin
      let (b,g,f, lookups) = rules.(i).(k) in

      ps_rules.(n) <-
        {
          psr_data    = (b,g,f);
          psr_lookups = lookups
        };

      iter i (k+1) (n+1)
    end
  end in
  iter 0 0 0
end


let read_chaining_2 _read read_lookup table table_off is_gpos = begin
  let read_rule table_off i = begin
    let off = table_off + read_u16 table (table_off + 2 * i) in

    if off = table_off then
      [| |]
    else begin
      let num_sub = read_u16 table off in

      Array.init num_sub (read_chaining_subrule table off read_lookup i)
    end
  end in

(*  let coverage  = read_u16 table (table_off +  2); *)
  let b_class_off = read_u16 table (table_off +  4) in
  let g_class_off = read_u16 table (table_off +  6) in
  let f_class_off = read_u16 table (table_off +  8) in
  let num_rules   = read_u16 table (table_off + 10) in

(*  let glyphs  = get_coverage_table table (table_off + coverage); *)
  let b_classes = read_class_table table (table_off + b_class_off) in
  let g_classes = read_class_table table (table_off + g_class_off) in
  let f_classes = read_class_table table (table_off + f_class_off) in

  let rules = Array.init num_rules (read_rule (table_off + 12)) in

  let num_psr =
    Array.fold_left
      (fun n sr -> n + Array.length sr)
      0
      rules in
  let ps_rules = Array.make num_psr { psr_data = ([||], [||], [||]); psr_lookups = [||] } in

  let rec iter i k n = begin
    if i >= Array.length rules then begin
      if is_gpos then
        ChainClassPos   (b_classes, g_classes, f_classes, ps_rules)
      else
        ChainClassSubst (b_classes, g_classes, f_classes, ps_rules)
    end
    else if k >= Array.length rules.(i) then
      iter (i+1) 0 n
    else begin
      let (b,g,f, lookups) = rules.(i).(k) in

      ps_rules.(n) <-
        {
          psr_data    = (b,g,f);
          psr_lookups = lookups
        };

      iter i (k+1) (n+1)
    end
  end in
  iter 0 0 0
end


let read_chaining_3 _read read_lookup table table_off is_gpos = begin
  let num_b      = read_u16 table (table_off + 2) in
  let b_coverage = read_u16_array table (table_off + 4) num_b in
  let n_off      = table_off + 2 * num_b + 4 in
  let num_n      = read_u16 table n_off in
  let n_coverage = read_u16_array table (n_off + 2) num_n in
  let f_off      = n_off + 2 * num_n + 2 in
  let num_f      = read_u16 table f_off in
  let f_coverage = read_u16_array table (f_off + 2) num_f in
  let sl_off     = f_off + 2 * num_f + 2 in
  let num_sl     = read_u16 table sl_off in

  let sl = read_pos_rule_record table (sl_off + 2) read_lookup num_sl in

  let b_glyphs =
    Array.map
      (fun off -> get_coverage_table table (table_off + off))
      b_coverage in
  let n_glyphs =
    Array.map
      (fun off -> get_coverage_table table (table_off + off))
      n_coverage in
  let f_glyphs =
    Array.map
      (fun off -> get_coverage_table table (table_off + off))
      f_coverage in

  let ps_rules =
    [| {
         psr_data    = (b_glyphs, n_glyphs, f_glyphs);
         psr_lookups = sl
       } |] in

  if is_gpos then
    ChainCoveragePos   ps_rules
  else
    ChainCoverageSubst ps_rules
end

let rec read_chaining read read_lookup table table_off is_gpos = begin
  match read_u16 table table_off with
   1 -> read_chaining_1 read read_lookup table table_off is_gpos
  | 2 -> read_chaining_2 read read_lookup table table_off is_gpos
  | 3 -> read_chaining_3 read read_lookup table table_off is_gpos
  | _ -> NoCommand
end

let read_extension read read_lookup table table_off = begin
  let lu_type = read_u16 table (table_off + 2) in
  let offset  = read_u16 table (table_off + 4) in

  read read_lookup table (table_off + offset) lu_type
end

let parse_table table read = begin
  let script_offset  = read_u16 table 4 in
  let feature_offset = read_u16 table 6 in
  let lookup_offset  = read_u16 table 8 in

  let features = parse_features table feature_offset in
  let scripts  = parse_scripts  table script_offset features in
  let lookups  = parse_lookups  table lookup_offset read in

  let rec get_size f = begin
    if f >= Array.length features then
      None
    else if features.(f).f_tag = size_tag then
      Some (parse_size_params table (feature_offset + features.(f).f_params))
    else
      get_size (f+1)
  end in
  let size = get_size 0 in

  make_pos_subst_table scripts lookups size
end

end

module GPOS =
struct

open Table
open Tag
open OTF_Pos_Subst

type value_record =
{
  x_placement : int;
  y_placement : int;
  x_advance   : int;
  y_advance   : int
}

let value_record_len vf = begin
  let bit i = if vf land i <> 0 then 2 else 0 in

    bit 0x01 + bit 0x02 + bit 0x04 + bit 0x08
  + bit 0x10 + bit 0x20 + bit 0x40 + bit 0x80
end

let read_value_record table table_off vf = begin
  let read (t,o) bit = if bit = 0 then ((t,o), 0) else ((t, o+2), read_i16 t o) in

  let (t1, x_placement) = read (table, table_off) (vf land 0x01) in
  let (t2, y_placement) = read t1                 (vf land 0x02) in
  let (t3, x_advance)   = read t2                 (vf land 0x04) in
  let (_,  y_advance)   = read t3                 (vf land 0x08) in

  {
    x_placement = x_placement;
    y_placement = y_placement;
    x_advance   = x_advance;
    y_advance   = y_advance
  }
end


let read_value_records table table_off num vf = begin
  let len = value_record_len vf in

  Array.init num
    (fun i -> read_value_record table (table_off + i * len) vf)
end

let value_record_to_positioning vr =
{
  p_x_off     = vr.x_placement;
  p_y_off     = vr.y_placement;
  p_h_adv_off = vr.x_advance;
  p_v_adv_off = vr.y_advance
}

let gpos_simple_pos table table_off = begin
  let format = read_u16 table table_off in

  if format <> 1 && format <> 2 then
    NoCommand
  else begin
    let coverage     = read_u16 table (table_off + 2) in
    let value_format = read_u16 table (table_off + 4) in

    if value_format land 0xf = 0 then
      NoCommand
    else begin
      let num_records =
        if format = 1 then
          1
        else
          read_u16 table (table_off + 6) in
      let record_off =
        if format = 1 then
          table_off + 6
        else
          table_off + 8 in
      let value_records = read_value_records table record_off num_records value_format in
      let glyphs        = get_coverage_table table (table_off + coverage) in

      Position
        (init_map (Array.length glyphs)
          (fun i -> glyphs.(i))
          (fun i -> value_record_to_positioning
                      value_records.(if format = 1 then 0 else i)))
    end
  end
end


let gpos_kern table table_off = begin
  let format = read_u16 table table_off in

  if format <> 1 && format <> 2 then
    NoCommand
  else begin
    let coverage = read_u16 table (table_off + 2) in
    let vf1      = read_u16 table (table_off + 4) in
    let vf2      = read_u16 table (table_off + 6) in
    let vr1_len  = value_record_len vf1 in
    let vr2_len  = value_record_len vf2 in

    match format with
   | 1 -> begin
        let num = read_u16 table (table_off + 8) in

        let offsets = read_u16_array table (table_off + 10) num in
        let glyphs  = get_coverage_table table (table_off + coverage) in

        let record_len = vr1_len + vr2_len + 2 in

        let num_pairs = Array.map
                          (fun off -> read_u16 table (table_off + off))
                          offsets in

        let read_sub_table i = begin
          let off = table_off + offsets.(i) in

          init_map num_pairs.(i)
            (fun k -> read_u16 table (off + k * record_len + 2))
            (fun k -> begin
                let vr1 = read_value_record table (off + k * record_len + 4)           vf1 in
                let vr2 = read_value_record table (off + k * record_len + vr1_len + 4) vf2 in

                (value_record_to_positioning vr1,
                 value_record_to_positioning vr2)
               end)
        end in

        Kern
          (init_map num
            (fun i -> glyphs.(i))
            read_sub_table)
      end
    | 2 -> begin
        let class1_off = read_u16 table (table_off +  8) in
        let class2_off = read_u16 table (table_off + 10) in
        let class1_num = read_u16 table (table_off + 12) in
        let class2_num = read_u16 table (table_off + 14) in

        let class1 = read_class_table table (table_off + class1_off) in
        let class2 = read_class_table table (table_off + class2_off) in

        let kern_val1 = Array.make (class1_num * class2_num) { p_x_off = 0; p_y_off = 0; p_h_adv_off = 0; p_v_adv_off = 0 } in
        let kern_val2 = Array.make (class1_num * class2_num) { p_x_off = 0; p_y_off = 0; p_h_adv_off = 0; p_v_adv_off = 0 } in

        for i = 0 to class1_num * class2_num - 1 do
          let off = table_off + i * (vr1_len + vr2_len) + 16 in

          kern_val1.(i) <- value_record_to_positioning (read_value_record table off             vf1);
          kern_val2.(i) <- value_record_to_positioning (read_value_record table (off + vr1_len) vf2)
        done;

        KernClass (class2_num, class1, class2, kern_val1, kern_val2)
      end
    | _ -> assert false
  end
end


let gpos_cursive table table_off = begin
  if read_u16 table table_off <> 1 then
    NoCommand
  else begin
    let coverage = read_u16 table (table_off + 2) in
    let num      = read_u16 table (table_off + 4) in

    let offsets =
      Array.init num
        (fun i ->
          (read_u16 table (table_off + 4 * i + 6),
           read_u16 table (table_off + 4 * i + 8))) in

    let glyphs = get_coverage_table table (table_off + coverage) in

    let rec iter i entry_map exit_map = begin
      if i >= num then
        CursiveAnchors (entry_map, exit_map)
      else begin
        let (entry, exit) = offsets.(i) in

        let new_entry =
          if entry = 0 then
            entry_map
          else
            IntMap.add
              glyphs.(i)
              (read_u16 table (table_off + entry + 2),
               read_u16 table (table_off + entry + 4))
             entry_map in
        let new_exit =
          if exit = 0 then
            exit_map
          else
            IntMap.add
              glyphs.(i)
              (read_u16 table (table_off + exit + 2),
               read_u16 table (table_off + exit + 4))
             exit_map in

        iter (i+1) new_entry new_exit
      end
    end in
    iter 0 IntMap.empty IntMap.empty
  end
end


let read_marks table table_off mark_glyphs = begin
  let num = read_u16 table table_off in

  let offsets =
    Array.init num
      (fun i ->
          (read_u16 table (table_off + 4 * i + 2),
           read_u16 table (table_off + 4 * i + 4))) in

  let rec iter i marks = begin
    if i >= num then
      Array.of_list marks
    else begin
      let g = mark_glyphs.(i) in

      let (cls, off) = offsets.(i) in

      if off <> 0 then
        iter (i+1)
             ((g, cls, read_u16 table (table_off + off + 2),
                       read_u16 table (table_off + off + 4))
              :: marks)
      else
        iter (i+1) marks
    end
  end in
  iter 0 []
end


let read_bases table table_off base_glyphs num_classes = begin
  let num = read_u16 table table_off in

  let offsets = read_u16_array table (table_off + 2) num in

  let read_base i = begin
    let anchors =
      Array.init num_classes
        (fun k ->
            let off = offsets.(i * num_classes + k) in

            (read_u16 table (table_off + off + 2),
             read_u16 table (table_off + off + 4))
          ) in

    (base_glyphs.(i), anchors)
  end in

  Array.init num (fun i -> read_base i)
end

let read_ligs table table_off base_glyphs num_classes = begin
  let num = read_u16 table table_off in

  let loffsets = read_u16_array table (table_off + 2) num in

  let read_lig i = begin
    let off = table_off + loffsets.(i) in

    let num_comp = read_u16 table off in
    let aoffsets = read_u16_array table (off + 2) (num_classes * num_comp) in

    let anchors =
      Array.init num_comp
        (fun k -> Array.init num_classes
          (fun c ->
              let aoff = aoffsets.(k * num_classes + c) in

              (read_u16 table (off + aoff + 2),
               read_u16 table (off + aoff + 4))
            )) in

    (base_glyphs.(i), anchors)
  end in

  Array.init num (fun i -> read_lig i)
end

let gpos_mark read_base table table_off = begin
  let mark_coverage = read_u16 table (table_off +  2) in
  let base_coverage = read_u16 table (table_off +  4) in
  let num_classes   = read_u16 table (table_off +  6) in
  let mark_offset   = read_u16 table (table_off +  8) in
  let base_offset   = read_u16 table (table_off + 10) in

  let mark_glyphs = get_coverage_table table (table_off + mark_coverage) in
  let base_glyphs = get_coverage_table table (table_off + base_coverage) in

  let marks = read_marks table (table_off + mark_offset) mark_glyphs in
  let bases = read_base  table (table_off + base_offset) base_glyphs num_classes in

  (marks, bases)
end

let rec read_gpos allow_extension read_lookup table table_off l_type = match l_type with
   1 -> gpos_simple_pos table table_off
| 2 -> gpos_kern table table_off
| 3 -> gpos_cursive table table_off
| 4 -> let (m,b) = gpos_mark read_bases  table table_off in MarkToBaseAnchors (m, b)
| 5 -> let (m,b) = gpos_mark read_ligs   table table_off in MarkToLigAnchors  (m, b)
| 6 -> let (m,b) = gpos_mark read_bases  table table_off in MarkToMarkAnchors (m, b)
| 7 -> read_context  (read_gpos true read_lookup) read_lookup table table_off true
| 8 -> read_chaining (read_gpos true read_lookup) read_lookup table table_off true
| 9 -> if allow_extension then
         read_extension (read_gpos false) read_lookup table table_off
       else
         raise (Failure "corrupt extension table")
| _ -> raise (Failure "invalid gpos table")


let parse_gpos_table table = parse_table table (read_gpos true)

end

module GSUB =
struct

open Table
open Tag
open OTF_Pos_Subst

let gsub_simple_subst table table_off = begin
  let format = read_u16 table table_off in

  if format <> 1 && format <> 2 then
    NoCommand
  else begin
    let coverage    = read_u16 table (table_off + 2) in
    let from_glyphs = get_coverage_table table (table_off + coverage) in

    if format = 1 then begin
      let delta = read_u16 table (table_off + 4) in

      Substitution
        (init_map (Array.length from_glyphs)
          (fun i -> from_glyphs.(i))
          (fun i -> from_glyphs.(i) + delta))
    end
    else begin
      let num = read_u16 table (table_off + 4) in

      let to_glyphs = read_u16_array table (table_off + 6) num in

      Substitution
        (init_map (Array.length from_glyphs)
          (fun i -> from_glyphs.(i))
          (fun i -> to_glyphs.(i)))
    end
  end
end


let gsub_multiple table table_off = begin
  if read_u16 table table_off <> 1 then
    IntMap.empty
  else begin
    let coverage = read_u16 table (table_off + 2) in
    let num_off  = read_u16 table (table_off + 4) in
    let offsets  = read_u16_array table (table_off + 6) num_off in
    let glyphs   = get_coverage_table table (table_off + coverage) in

    init_map (Array.length glyphs)
      (fun i -> glyphs.(i))
      (fun i -> begin
          let off        = table_off + offsets.(i) in
          let num_glyphs = read_u16 table off in
          let to_glyphs  = read_u16_array table (off + 2) num_glyphs in

          to_glyphs
        end)
  end
end


let gsub_ligature table table_off = begin
  let coverage = read_u16 table (table_off + 2) in
  let num      = read_u16 table (table_off + 4) in
  let offsets  = read_u16_array table (table_off + 6) num in
  let glyphs   = get_coverage_table table (table_off + coverage) in

  let rec iter_i i trie = begin
    if i >= num then
      Ligature trie
    else begin
      let off         = table_off + offsets.(i) in
      let num_ligs    = read_u16 table off in
      let lig_offsets = read_u16_array table (off + 2) num_ligs in

      let rec iter_k k trie = begin
        if k >= num_ligs then
          iter_i (i+1) trie
        else begin
          let loff         = off + lig_offsets.(k) in
          let lig          = read_u16 table loff in
          let num_lig_comp = read_u16 table (loff + 2) in

          let lig_glyphs =
            Array.init num_lig_comp
              (fun l -> if l = 0 then
                          glyphs.(i)
                        else
                          read_u16 table (loff + 2 * l + 2)) in

          iter_k (k+1) (DynUCTrie.add_string lig_glyphs lig trie)
        end
      end in
      iter_k 0 trie
    end
  end in
  iter_i 0 DynUCTrie.empty
end

let gsub_reverse_chain table table_off = begin
  if read_u16 table table_off <> 1 then
    NoCommand
  else begin
    let coverage = read_u16 table (table_off + 2) in
    let glyphs   = get_coverage_table table (table_off + coverage) in

    let num_b      = read_u16 table (table_off + 4) in
    let b_coverage = read_u16_array table (table_off + 6) num_b in
    let f_off      = table_off + 2 * num_b + 6 in
    let num_f      = read_u16 table f_off in
    let f_coverage = read_u16_array table (f_off + 2) num_f in
    let s_off      = f_off + 2 * num_f + 2 in
    let num_s      = read_u16 table s_off in
    let s_glyphs   = read_u16_array table (s_off + 2) num_s in

    ReverseSubst
      (glyphs,
       s_glyphs,
       Array.map (fun off -> get_coverage_table table (table_off + off)) b_coverage,
       Array.map (fun off -> get_coverage_table table (table_off + off)) f_coverage)
  end
end


let rec read_gsub allow_extension read_lookup table table_off l_type = match l_type with
   1 -> gsub_simple_subst table table_off
| 2 -> Multiple  (gsub_multiple table table_off)
| 3 -> Alternate (gsub_multiple table table_off)
| 4 -> gsub_ligature                              table table_off
| 5 -> read_context  (read_gsub true) read_lookup table table_off false
| 6 -> read_chaining (read_gsub true) read_lookup table table_off false
| 7 -> if allow_extension then
         read_extension (read_gsub false) read_lookup table table_off
       else
         raise (Failure "corrupt extension table")
| 8 -> gsub_reverse_chain table table_off
| _ -> raise (Failure "invalid gsub table")


let parse_gsub_table table = parse_table table (read_gsub true)

end

type glyph_metric =
{
  g_adv_width : int;
  g_min_x     : int;
  g_min_y     : int;
  g_max_x     : int;
  g_max_y     : int
}

type otf_font =
{
  otf_units_per_em : int;
  otf_glyphs       : glyph_metric array;
  otf_gpos         : OTF_Pos_Subst.pos_subst_table option;
  otf_gsub         : OTF_Pos_Subst.pos_subst_table option
}

module Parse_Simple_Tables =
struct

open Table

let parse_head table = begin
  let units_per_em        = read_u16 table 18 in
  let index_to_loc_format = read_u16 table 50 in

  (units_per_em, index_to_loc_format <> 0)
end

let parse_hhea table = begin
(*  let ascent        = read_u16 table  4;
  let descent       = read_u16 table  6 in
  let leading       = read_u16 table  8;*)
  let num_h_metrics = read_u16 table 34 in

  num_h_metrics
end

let parse_maxp table = begin
  let num_glyphs = read_u16 table 4 in

  num_glyphs
end

let parse_hmtx table num_glyphs num_h_metrics = begin
  let width = Array.make num_glyphs 0 in

  for i = 0 to num_h_metrics - 1 do
    width.(i) <- read_u16 table (4 * i)
  done;

  if num_h_metrics < num_glyphs then
    for i = num_h_metrics to num_glyphs - 1 do
      width.(i) <- width.(num_h_metrics - 1)
    done
  else ();

  width
end

let parse_loca table num_glyphs index_to_loc_format = begin
  if index_to_loc_format then
    Array.init num_glyphs
      (fun i -> Int32.to_int (read_u32 table (4 * i)))
  else
    Array.init num_glyphs
      (fun i -> 2 * read_u16 table (2 * i))
end

let parse_glyf table glyph_locations = begin
  Array.init
    (Array.length glyph_locations)
    (fun i ->
        let off = glyph_locations.(i) in

        (read_u16 table (off+2),
         read_u16 table (off+4),
         read_u16 table (off+6),
         read_u16 table (off+8))
      )
end

end

(*
module CFF =
struct

open Table

let parse_cff table = begin
  if read_u8 table 0 <> 1 then
    raise (Failure "wrong CFF version")
  else begin
    let header_len  = read_u8 table 2 in
    let offset_size = read_u8 table 3 in

    ()
  end


end
*)

module Parse_OTF =
struct

open Tag

let read_tag is = begin
  let x = IO.read_be_u16 is in
  let y = IO.read_be_u16 is in

  compose_tag x y
end

let read_tables is = begin
  let version = read_tag is in

  if version = ttcf_tag then
    raise (Failure "font collections not supported")
  else if version <> Int32.of_int 0x10000 &&
          version <> Int32.of_int 0x20000 &&
          version <> otto_tag then
    raise (Failure "unknown font type")
  else begin
    let num_tables = IO.read_be_u16 is in

    IO.skip is 6;

    let table_list =
      Array.init num_tables
        (fun _ ->
            let tag = read_tag is in

            IO.skip is 4;

            let offset = int_of_num (IO.read_be_u32 is) in
            let length = int_of_num (IO.read_be_u32 is) in

            (tag, offset, length)
          ) in

    let rec iter i tables = begin
      if i >= num_tables then
        tables
      else begin
        let (tag, off, len) = table_list.(i) in

        IO.seek is off;

        iter (i+1) (TagMap.add tag (IO.read_string is len) tables)
      end
    end in
    iter 0 TagMap.empty
  end
end


let parse_cff_font tables num_glyphs units_per_em advance_widths gpos gsub = begin
  let glyphs =
    Array.init num_glyphs
      (fun i ->
          let (x0,y0,x1,y1) = (0,0,0,0) in (*bboxes.(i)*)

          {
            g_adv_width = advance_widths.(i);
            g_min_x     = x0;
            g_min_y     = y0;
            g_max_x     = x1;
            g_max_y     = y1
          }
        ) in

  let font =
  {
    otf_units_per_em = units_per_em;
    otf_glyphs       = glyphs;
    otf_gpos         = gpos;
    otf_gsub         = gsub
  } in

  (* not implemented *)

  font
end

let parse_ttf_font tables num_glyphs units_per_em index_to_loc_format advance_widths gpos gsub = begin
  let loca_table = TagMap.find loca_tag tables in
  let glyf_table = TagMap.find loca_tag tables in

  let locations = Parse_Simple_Tables.parse_loca loca_table num_glyphs index_to_loc_format in
  let bboxes    = Parse_Simple_Tables.parse_glyf glyf_table locations in

  let glyphs =
    Array.init num_glyphs
      (fun i ->
          let (x0,y0,x1,y1) = bboxes.(i) in

          {
            g_adv_width = advance_widths.(i);
            g_min_x     = x0;
            g_min_y     = y0;
            g_max_x     = x1;
            g_max_y     = y1
          }
        ) in

  (* FIX: read kern table, if present *)

  let font =
  {
    otf_units_per_em = units_per_em;
    otf_glyphs       = glyphs;
    otf_gpos         = gpos;
    otf_gsub         = gsub
  } in

  font
end

let read_pos_subst tables = begin
  let gpos_table =
    try
      Some (GPOS.parse_gpos_table (TagMap.find gpos_tag tables))
    with Not_found -> None in
  let gsub_table =
    try
      Some (GSUB.parse_gsub_table (TagMap.find gsub_tag tables))
    with Not_found -> None in

  (gpos_table, gsub_table)
end

(* only return the pos_subst data *)

let parse_pos_subst tables = begin
  try
    read_pos_subst tables
  with
  | Not_found -> (None,None)
end

(* load the whole font *)

let parse_font is = begin
  let tables = read_tables is in

  try begin
    let head_table = TagMap.find head_tag tables in
    let hhea_table = TagMap.find hhea_tag tables in
    let maxp_table = TagMap.find maxp_tag tables in
    let hmtx_table = TagMap.find hmtx_tag tables in

    let (upm, itlf)    = Parse_Simple_Tables.parse_head head_table in
    let num_h_metrics  = Parse_Simple_Tables.parse_hhea hhea_table in
    let num_glyphs     = Parse_Simple_Tables.parse_maxp maxp_table in
    let advance_widths = Parse_Simple_Tables.parse_hmtx hmtx_table num_glyphs num_h_metrics in

    let (gpos, gsub)   = read_pos_subst tables in

    let font =
      if TagMap.mem glyf_tag tables then
        parse_ttf_font tables num_glyphs upm itlf advance_widths gpos gsub
      else
        parse_cff_font tables num_glyphs upm advance_widths gpos gsub in

    font
  end
  with
  | Not_found -> raise (Failure "font lacks required tables")
end

end

module WriteOTF =
struct

open Tag

let check_sum str = begin
  let get i = begin
    if i >= String.length str then
      0
    else
      int_of_char (String.unsafe_get str i)
  end in

  let rec iter pos chk = begin
    if pos >= String.length str then
      chk
    else begin
      let a = get pos in
      let b = get (pos+1) in
      let c = get (pos+2) in
      let d = get (pos+3) in

      let x = compose_tag ((a lsl 8) + b) ((c lsl 8) + d) in

      iter (pos + 4) (Int32.add chk x)
    end
  end in
  iter 0 Int32.zero
end

let write_tag os tag = begin
  let x = Int32.to_int (Int32.logand (Int32.shift_right tag 16) (Int32.of_int 0xffff)) in
  let y = Int32.to_int (Int32.logand tag (Int32.of_int 0xffff)) in

  IO.write_be_u16 os x;
  IO.write_be_u16 os y
end

let write_tables os tables ttf = begin
  (* create preamble *)

  let preamble = IO.make_buffer_stream 0x1000 in

  if ttf then
    write_tag preamble (Int32.of_int 0x10000)
  else
    write_tag preamble otto_tag;

  let num_tables = Array.length tables in
  let rec iter x i = begin
    if 1 lsr i > x then
      i - 1
    else
      iter x (i+1)
  end in
  let num_bits   = iter 0 num_tables in

  IO.write_be_u16 preamble num_tables;
  IO.write_be_u16 preamble (16 * (1 lsl num_bits));
  IO.write_be_u16 preamble num_bits;
  IO.write_be_u16 preamble (16 * num_tables - num_bits);

  let table_offsets = Array.make num_tables 0 in
  let _ = Array.fold_left
            (fun (i,off) (_,table) ->
                let len     = String.length table in
                let new_off = if len land 3 = 0 then
                                off + len
                              else
                                off + len + 4 - (len land 3) in

                table_offsets.(i) <- off;

                (i+1, new_off)
              )
            (0, 12 + 16 * num_tables)
            tables in

  let chk = ref Int32.zero in

  for i = 0 to num_tables - 1 do
    let (tag,tab) = tables.(i) in

    let c = check_sum tab in

    chk := Int32.add !chk c;

    write_tag       preamble tag;
    write_tag       preamble c;
    IO.write_be_u32 preamble (num_of_int table_offsets.(i));
    IO.write_be_u32 preamble (num_of_int (String.length tab))
  done;

  (* write preamble *)

  let p = IO.to_string preamble in

  IO.write_string os p;

  chk := Int32.add !chk (check_sum p);

  (* Fix check sum in head table. *)
  (* We assume that the head table is the first element of <tables>! *)

  let (_, head) = tables.(0) in

  let h = IO.make_buffer_stream (String.length head) in
  IO.write_string h head;
  IO.seek (IO.coerce_ior_ir h) 8;
  write_tag h (Int32.sub (compose_tag 0xb1b0 0xafba) !chk);

  tables.(0) <- (head_tag, IO.to_string h);

  (* write tables *)

  for i = 0 to num_tables - 1 do
    let (_,tab) = tables.(i) in

    IO.write_string os tab;

    (* padding *)

    if String.length tab land 3 <> 0 then
      for n = String.length tab land 3 to 3 do
        IO.write_byte os 0
      done
    else ()
  done
end

let make_head_table font = begin
  (* Just copy the table but set the checksum to 0.*)

  let head = TagMap.find head_tag font in

  let s = IO.make_buffer_stream (String.length head) in
  IO.write_string s head;

  IO.seek (IO.coerce_ior_ir s) 8;
  IO.write_be_u32 s num_zero;

  IO.to_string s
end

let make_hhea_table font encoding = begin
  let hhea = TagMap.find hhea_tag font in

  (* Just change the number of metrics. *)

  let s = IO.make_buffer_stream (String.length hhea) in
  IO.write_string s hhea;

  IO.seek (IO.coerce_ior_ir s) 34;
  IO.write_be_u16 s (Array.length encoding);

  IO.to_string s
end

let make_hmtx_table font encoding = begin
  let hhea = IO.make_string_stream (TagMap.find hhea_tag font) in
  let maxp = IO.make_string_stream (TagMap.find maxp_tag font) in
  let hmtx = IO.make_string_stream (TagMap.find hmtx_tag font) in

  IO.seek hhea 34;
  IO.seek maxp 4;

  let num_metrics = IO.read_be_u16 hhea in
  let num_glyphs  = IO.read_be_u16 maxp in

  let width        = Array.make num_glyphs 0 in
  let side_bearing = Array.make num_glyphs 0 in

  let os = IO.make_buffer_stream (4 * Array.length encoding) in

  for i = 0 to num_metrics - 1 do
    width.(i)        <- IO.read_be_u16 hmtx;
    side_bearing.(i) <- IO.read_be_i16 hmtx
  done;

  for i = num_metrics to num_glyphs - 1 do
    width.(i) <- width.(num_metrics - 1);
    side_bearing.(i) <- IO.read_be_i16 hmtx
  done;

  for i = 0 to Array.length encoding - 1 do
    IO.write_be_u16 os width.(encoding.(i));
    IO.write_be_i16 os side_bearing.(encoding.(i))
  done;

  IO.to_string os
end

let make_maxp_table font encoding = begin
  let maxp = TagMap.find maxp_tag font in

  (* Just change the number of glyphs. *)

  let s = IO.make_buffer_stream (String.length maxp) in
  IO.write_string s maxp;

  IO.seek (IO.coerce_ior_ir s) 4;
  IO.write_be_u16 s (Array.length encoding);

  IO.to_string s
end

let make_cmap_table encoding = begin
  let cmap = IO.make_buffer_stream 0x100 in

  IO.write_be_u16 cmap 0;
  IO.write_be_u16 cmap 1;

  IO.write_be_u16 cmap 1;
  IO.write_be_u16 cmap 0;
  IO.write_be_u32 cmap (num_of_int 12);

  IO.write_be_u16 cmap 0;
  IO.write_be_u16 cmap (6 + 256);
  IO.write_be_u16 cmap 0;

  for i = 0 to 255 do
    if i < Array.length encoding then
      IO.write_be_u8 cmap i
    else
      IO.write_be_u8 cmap 0
  done;

  IO.to_string cmap
end

let make_name_table font = begin
  TagMap.find name_tag font
end

let make_os2_table  font = begin
  TagMap.find os2_tag font
end

let make_post_table font = begin
  let post = IO.make_string_stream (TagMap.find post_tag font) in

  IO.seek post 4;

  let os = IO.make_buffer_stream 0x80 in

  IO.write_be_u16 os 3;
  IO.write_be_u16 os 0;
  IO.write_string os (IO.read_string post 28);

  IO.to_string os
end

let make_cvt_table  font = begin
  TagMap.find cvt_tag font
end

let make_fpgm_table font = begin
  TagMap.find fpgm_tag font
end

let make_prep_table font = begin
  TagMap.find prep_tag font
end

let make_glyf_loca_tables font encoding = begin
  let head = TagMap.find head_tag font in
  let glyf = TagMap.find glyf_tag font in
  let loca = TagMap.find loca_tag font in

  let loca_fmt = (head.[50] <> '\000' || head.[51] <> '\000') in

  let igs = IO.make_string_stream glyf in
  let ils = IO.make_string_stream loca in
  let ogs = IO.make_buffer_stream (String.length glyf) in
  let ols = IO.make_buffer_stream (4 * Array.length encoding) in

  let get_loca = if loca_fmt then
                   (fun n ->
                       IO.seek ils (4 * n);
                       int_of_num (IO.read_be_u32 ils)
                     )
                 else
                   (fun n ->
                       IO.seek ils (2 * n);
                       2 * IO.read_be_u16 ils
                     ) in
  let set_loca = if loca_fmt then
                   (fun off -> IO.write_be_u32 ols (num_of_int off))
                 else
                   (fun off -> IO.write_be_u16 ols (off / 2)) in

  for i = 0 to Array.length encoding - 1 do
    let off  = get_loca encoding.(i) in
    let next = get_loca (encoding.(i) + 1) in

    set_loca (IO.bytes_written (IO.coerce_o ogs));

    IO.seek igs off;
    IO.write_string ogs (IO.read_string igs (next - off))
  done;
  set_loca (IO.bytes_written (IO.coerce_o ogs));

  (IO.to_string ogs, IO.to_string ols)
end

let make_cff_table font encoding = begin
  ""
end

let write_ttf_subset stream font encoding = begin
  let cmap = make_cmap_table encoding in
  let head = make_head_table font in
  let hhea = make_hhea_table font encoding in
  let hmtx = make_hmtx_table font encoding in
  let maxp = make_maxp_table font encoding in
  let name = make_name_table font in
  let os2  = make_os2_table  font in
  let post = make_post_table font in
  let cvt  = make_cvt_table  font in
  let prep = make_prep_table font in
  let fpgm = make_fpgm_table font in

  let (glyf, loca) = make_glyf_loca_tables font encoding in

  write_tables stream
    [|
      (head_tag, head);
      (hhea_tag, hhea);
      (maxp_tag, maxp);
      (os2_tag,  os2);
      (hmtx_tag, hmtx);
      (fpgm_tag, fpgm);
      (prep_tag, prep);
      (cvt_tag,  cvt);
      (loca_tag, loca);
      (glyf_tag, glyf);
      (post_tag, post);
      (name_tag, name);
      (cmap_tag, cmap)
    |]
    true
end

let write_cff_subset stream font encoding = begin
  let cmap = make_cmap_table encoding in
  let head = make_head_table font in
  let hhea = make_hhea_table font encoding in
  let hmtx = make_hmtx_table font encoding in
  let maxp = make_maxp_table font encoding in
  let name = make_name_table font in
  let os2  = make_os2_table  font in
  let post = make_post_table font in
  let cff  = make_cff_table  font encoding in

  write_tables stream
    [|
      (head_tag, head);
      (hhea_tag, hhea);
      (maxp_tag, maxp);
      (os2_tag,  os2);
      (name_tag, name);
      (cmap_tag, cmap);
      (post_tag, post);
      (cff_tag,  cff);
      (hmtx_tag, hmtx)
    |]
    false
end

end

type otf_tables = string Tag.TagMap.t

let load_file name =
  let ic = open_in_bin name in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  IO.make_string_stream s

let read_font_tables name = begin
  Parse_OTF.read_tables (load_file name)
end

let get_pos_subst font = begin
  Parse_OTF.parse_pos_subst font
end

let is_cff font = begin
  Tag.TagMap.mem Tag.cff_tag font
end

let get_cff font = begin
  Tag.TagMap.find Tag.cff_tag font
end

let load_font name = begin
  Parse_OTF.parse_font (load_file name)
end

let read_font name = begin
  Parse_OTF.parse_font (load_file name)
end

let write_subset stream font encoding = begin
  if Tag.TagMap.mem Tag.glyf_tag font then
    WriteOTF.write_ttf_subset stream font encoding
  else
    WriteOTF.write_cff_subset stream font encoding
end

