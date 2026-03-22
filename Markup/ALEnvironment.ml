
open XNum
open Runtime
open Logging

(* open Typesetting *)
open Engine
open ALCoding
open ALDim

module UString   = Unicode.UString
module SymbolMap = Unicode.SymbolTable.SymbolMap

(* Opaque type for environments *)

let apply_env _x x = match !x with
  | Types.Symbol _s ->
      Types.runtime_error "invalid argument"
  | _ -> Types.runtime_error "invalid argument"

let cmp_env e1 e2 = e1 == e2

let (env_wrapper, env_unwrapper) = Opaque.declare_type "environment" apply_env cmp_env cmp_env

let wrap_env env = Types.Opaque (env_wrapper env)

let unwrap_env = decode_opaque "environment" env_unwrapper

let wrap_env_cmd name f loc env =
  wrap_env (f (decode_location name loc) (unwrap_env name env))

let decode_env_cmd name f loc env =
  try
    unwrap_env name (Machine.evaluate_function f [ref (encode_location loc); ref (wrap_env env)])
  with
  | Vm_types.Types.Syntax_error (loc, msg) -> log_warn loc (UString.to_string (Array.to_list msg)); env
  | Vm_types.Types.Runtime_error msg    -> log_warn loc (UString.to_string (Array.to_list msg)); env

let encode_env_cmd name cmd = Types.Primitive2 (wrap_env_cmd name cmd)

(* skip args *)

let encode_skip_arg s =
  let f e =
    let env = unwrap_env "<unnamed>" e in
    Types.Number (s env)
  in
  Types.Primitive1 f

let decode_skip_arg name s = match !s with
  | Types.Number a -> (fun _ -> a)
  | _ ->
      let f = Machine.evaluate_function s in
      (fun env ->
        let x = f [ref (wrap_env env)] in
        Machine.decode_num name x)

(* dim args *)

let encode_dim_arg d =
  let f e =
    let env = unwrap_env "<unnamed>" e in
    wrap_dim (d env)
  in
  Types.Primitive1 f

let decode_dim_arg name d = match !d with
  | Types.Number a -> (fun _ -> Dim.fixed_dim a)
  | _ ->
      let f = Machine.evaluate_function d in
      (fun env ->
        let x = f [ref (wrap_env env)] in
        match !x with
        | Types.Number a -> Dim.fixed_dim a
        | _              -> unwrap_dim name x)

let lookup_skip name dict key = lookup (decode_skip_arg name) dict key
let lookup_dim  name dict key = lookup (decode_dim_arg  name) dict key

(* primitives *)

let env_quad x env =
  let e = unwrap_env "env_quad" env in
  let s = Machine.decode_num "env_quad" x in
  Types.Number (Evaluate.const_em s e)

let env_x_height x env =
  let e = unwrap_env "env_x_height" env in
  let s = Machine.decode_num "env_x_height" x in
  Types.Number (Evaluate.const_ex s e)

let env_math_unit x env =
  let e = unwrap_env "env_math_unit" env in
  let s = Machine.decode_num "env_math_unit" x in
  Types.Number (Evaluate.const_mu s e)

let prim_new_galley name width =
  let n = decode_uc_string   "new_galley" name in
  let w = Machine.decode_num "new_galley" width in
  encode_env_cmd "new_galley" (Environment.new_galley n w)

let prim_select_galley name =
  let n = decode_uc_string "select_galley" name in
  encode_env_cmd "select_galley" (Environment.select_galley n)

let prim_set_colour col =
  let c = decode_colour "set_colour" col in
  encode_env_cmd "set_colour" (Environment.set_colour c)

let prim_new_page_layout args = match args with
  | [name; width; height] ->
      let n = decode_uc_string   "new_page_layout" name in
      let w = Machine.decode_num "new_page_layout" width in
      let h = Machine.decode_num "new_page_layout" height in
      encode_env_cmd "new_page_layout" (Environment.new_page_layout n w h)
  | _ -> assert false

let prim_select_page_layout name =
  let n = decode_uc_string "select_page_layout" name in
  encode_env_cmd "select_page_layout" (Environment.select_page_layout n)

let prim_set_math_font def = match !def with
  | Types.Tuple [|math_family; family; series; shape; text_size; script_size; script2_size|] ->
      let mf  = decode_option "set_math_font" decode_int           math_family in
      let fam = decode_option "set_math_font" decode_uc_string     family in
      let ser = decode_option "set_math_font" decode_uc_string     series in
      let sha = decode_option "set_math_font" decode_uc_string     shape in
      let ts  = decode_option "set_math_font" Machine.decode_num text_size in
      let ss  = decode_option "set_math_font" Machine.decode_num script_size in
      let s2s = decode_option "set_math_font" Machine.decode_num script2_size in
      encode_env_cmd "set_math_font" (Environment.set_math_font (mf, fam, ser, sha, ts, ss, s2s))
  | _ -> Types.runtime_error "set_math_font: invalid argument"

let prim_adapt_fonts_to_math_style =
  encode_env_cmd "adapt_fonts_to_math_style" Environment.adapt_fonts_to_math_style

let decode_par_params name params = match !params with
  | Types.Dictionary d ->
      (lookup_num name d sym_Measure,
       lookup_dim name d sym_ParIndent,
       lookup_dim name d sym_ParFillSkip,
       lookup_dim name d sym_LeftSkip,
       lookup_dim name d sym_RightSkip,
       None, (* FIX: lookup_ name d sym_ParShape  *)
       lookup_dim name d sym_ParSkip,
       None, (* FIX: lookup_ name d sym_PreBreak  *)
       None, (* FIX: lookup_ name d sym_PostBreak *)
       None) (* FIX: lookup_ name d sym_PostProcessLine *)
  | _ -> Types.runtime_error (name ^ ": invalid argument")

let prim_set_par_params params =
  encode_env_cmd "set_par_params"
    (Environment.set_par_params (decode_par_params "set_par_params" params))

let prim_set_current_par_params params =
  encode_env_cmd "set_current_par_params"
    (Environment.set_current_par_params (decode_par_params "set_current_par_params" params))

let leading_map =
  let m = SymbolMap.empty in
  let m = SymbolMap.add sym_Fixed    Galley.leading_fixed m in
  let m = SymbolMap.add sym_Register Galley.leading_register m in
  let m = SymbolMap.add sym_TeX      Galley.leading_TeX m in
  let m = SymbolMap.add sym_Skyline  Galley.leading_skyline m in
  m

let decode_leading name leading = match leading with
  | None     -> None
  | Some sym ->
      try
        Some (SymbolMap.find sym leading_map)
      with
      | Not_found ->
          Types.runtime_error
            (name
             ^ ": unknown leading `"
             ^ UString.to_string (Array.to_list (Machine.symbol_to_string sym))
             ^ "'.")

let decode_line_params name params = match !params with
  | Types.Dictionary d ->
      (lookup_dim     name d sym_BaselineSkip,
       lookup_skip    name d sym_LineSkipLimit,
       lookup_dim     name d sym_LineSkip,
       decode_leading name (lookup_symbol name d sym_Leading),
       None)  (* FIX: lookup_ name d sym_ClubWidowPenalty *)
  | _ -> Types.runtime_error (name ^ ": invalid argument")

let prim_set_line_params params =
  encode_env_cmd "set_line_params"
    (Environment.set_line_params (decode_line_params "set_line_params" params))

let prim_set_current_line_params params =
  encode_env_cmd "set_current_line_params"
    (Environment.set_current_line_params (decode_line_params "set_current_line_params" params))

let decode_line_break_params name params = match !params with
  | Types.Dictionary d ->
      (lookup_num  name d sym_PreTolerance,
       lookup_num  name d sym_Tolerance,
       lookup_int  name d sym_Looseness,
       lookup_num  name d sym_LinePenalty,
       lookup_num  name d sym_AdjDemerits,
       lookup_num  name d sym_DoubleHyphenDemerits,
       lookup_num  name d sym_FinalHyphenDemerits,
       lookup_skip name d sym_EmergencyStretch,
       lookup_num  name d sym_RiverDemerits,
       lookup_skip name d sym_RiverThreshold,
       lookup_bool name d sym_SimpleBreaking)
  | _ -> Types.runtime_error (name ^ ": invalid argument")

let prim_set_line_break_params params =
  encode_env_cmd "set_line_break_params"
    (Environment.set_line_break_params (decode_line_break_params "set_line_break_params" params))

let prim_set_current_line_break_params params =
  encode_env_cmd "set_current_line_break_params"
    (Environment.set_current_line_break_params (decode_line_break_params "set_current_line_break_params" params))

let decode_hyphen_params name params = match !params with
  | Types.Dictionary d ->
      (lookup_string name d sym_HyphenTable,
       lookup_num    name d sym_HyphenPenalty,
       lookup_num    name d sym_ExHyphenPenalty,
       lookup_int    name d sym_LeftHyphenMin,
       lookup_int    name d sym_RightHyphenMin,
       lookup_string name d sym_ScriptLang)
  | _ -> Types.runtime_error (name ^ ": invalid argument")

let prim_set_hyphen_params params =
  encode_env_cmd "set_hyphen_params"
    (Environment.set_hyphen_params (decode_hyphen_params "set_hyphen_params" params))

let prim_set_current_hyphen_params params =
  encode_env_cmd "set_current_hyphen_params"
    (Environment.set_current_hyphen_params (decode_hyphen_params "set_current_hyphen_params" params))

let decode_space_params name params = match !params with
  | Types.Dictionary d ->
      (lookup_num  name d sym_SpaceFactor,
       lookup_dim  name d sym_SpaceSkip,
       lookup_dim  name d sym_XSpaceSkip,
       lookup_bool name d sym_VictorianSpacing)
  | _ -> Types.runtime_error (name ^ ": invalid argument")

let prim_set_space_params params =
  encode_env_cmd "set_space_params"
    (Environment.set_space_params (decode_space_params "set_space_params" params))

let prim_set_current_space_params params =
  encode_env_cmd "set_current_space_params"
    (Environment.set_current_space_params (decode_space_params "set_current_space_params" params))

let decode_math_params name params = match !params with
  | Types.Dictionary d ->
      (lookup_dim  name d sym_ThinMathSkip,
       lookup_dim  name d sym_MedMathSkip,
       lookup_dim  name d sym_ThickMathSkip,
       lookup_dim  name d sym_ScriptSpace,
       lookup_num  name d sym_RelPenalty,
       lookup_num  name d sym_BinOpPenalty,
       lookup_num  name d sym_DelimiterFactor,
       lookup_skip name d sym_DelimiterShortfall,
       lookup_dim  name d sym_NullDelimiterSpace)
  | _ -> Types.runtime_error (name ^ ": invalid argument")

let prim_set_math_params params =
  encode_env_cmd "set_math_params"
    (Environment.set_math_params (decode_math_params "set_math_params" params))

let prim_set_current_math_params params =
  encode_env_cmd "set_current_math_params"
    (Environment.set_current_math_params (decode_math_params "set_current_math_params" params))

let prim_get_space_factor env char =
  let e = unwrap_env  "get_space_factor" env in
  let c = decode_char "get_space_factor" char in
  Types.Number (Environment.get_space_factor e c)

let prim_adjust_space_factor char =
  let x = decode_char "adjust_space_factor" char in
  encode_env_cmd "adjust_space_factor" (Environment.adjust_space_factor x)
