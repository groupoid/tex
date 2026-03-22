
open XNum
open Runtime
open Logging

(* open Typesetting *)
open Engine
open ParseState
open ALCoding
open ALEnvironment
open ALDim

module UString = Unicode.UString
module SymbolMap = Unicode.SymbolTable.SymbolMap

(* gfx-commands *)
let encode_line_cap cap =
  match cap with
  | `Butt -> Types.Symbol sym_Butt
  | `Circle -> Types.Symbol sym_Circle
  | `Square -> Types.Symbol sym_Square

let encode_line_join join =
  match join with
  | `Miter -> Types.Symbol sym_Miter
  | `Round -> Types.Symbol sym_Round
  | `Bevel -> Types.Symbol sym_Bevel

let encode_gfx_cmd cmd =
  let rec encode_path path =
    match path with
    | [] -> Types.Nil
    | (ax, ay, bx, by, cx, cy, dx, dy) :: ps ->
        Types.List
          (ref
             (Types.Tuple
                [| ref (encode_dim_arg ax); ref (encode_dim_arg ay);
                   ref (encode_dim_arg bx); ref (encode_dim_arg by);
                   ref (encode_dim_arg cx); ref (encode_dim_arg cy);
                   ref (encode_dim_arg dx); ref (encode_dim_arg dy) |]),
           ref (encode_path ps))
  in
  match cmd with
  | Graphic.PutBox (x, y, _, _) ->
      Types.Tuple
        [| ref (Types.Symbol sym_PutBox); ref (encode_dim_arg x);
           ref (encode_dim_arg y); ref Types.Unbound; ref Types.Unbound |]
  | Graphic.Draw (pc, p) ->
      let sym =
        match pc with
        | `Stroke -> sym_Stroke
        | `Fill -> sym_Fill
        | `Clip -> sym_Clip
      in
      Types.Tuple [| ref (Types.Symbol sym); ref (encode_path p) |]
  | Graphic.Fill (pc, p) ->
      let sym =
        match pc with
        | `Stroke -> sym_Stroke
        | `Fill -> sym_Fill
        | `Clip -> sym_Clip
      in
      Types.Tuple [| ref (Types.Symbol sym); ref (encode_path p) |]
  | Graphic.Clip p ->
      Types.Tuple [| ref (Types.Symbol sym_Clip); ref (encode_path p) |]
  | Graphic.SetColour c ->
      Types.Tuple
        [| ref (Types.Symbol sym_SetColour); ref (encode_colour c) |]
  | Graphic.SetBgColour c ->
      Types.Tuple
        [| ref (Types.Symbol sym_SetBgColour); ref (encode_colour c) |]
  | Graphic.SetAlpha a ->
      Types.Tuple [| ref (Types.Symbol sym_SetAlpha); ref (Types.Number a) |]
  | Graphic.SetLineWidth w ->
      Types.Tuple
        [| ref (Types.Symbol sym_SetLineWidth); ref (ALEnvironment.encode_dim_arg w) |]
  | Graphic.SetLineCap c ->
      Types.Tuple
        [| ref (Types.Symbol sym_SetLineCap); ref (encode_line_cap c) |]
  | Graphic.SetLineJoin j ->
      Types.Tuple
        [| ref (Types.Symbol sym_SetLineJoin); ref (encode_line_join j) |]
  | Graphic.SetMiterLimit l ->
      Types.Tuple
        [| ref (Types.Symbol sym_SetMiterLimit); ref (Types.Number (XNum.float_of_num l)) |]

let decode_line_cap name_str cap_ref =
  let s = decode_symbol name_str cap_ref in
  if s = sym_Butt then `Butt
  else if s = sym_Circle then `Circle
  else if s = sym_Square then `Square
  else Types.runtime_error (name_str ^ ": invalid line cap")

let decode_line_join name_str join_ref =
  let s = decode_symbol name_str join_ref in
  if s = sym_Miter then `Miter
  else if s = sym_Round then `Round
  else if s = sym_Bevel then `Bevel
  else Types.runtime_error (name_str ^ ": invalid line join")

let decode_gfx_cmd name_str cmd_ref =
  let rec decode_path_internal name path_list_ref =
    List.map
      (fun x_ref ->
         match decode_tuple name x_ref with
         | [| ax; ay; bx; by; cx; cy; dx; dy |] ->
             (decode_dim_arg name ax, decode_dim_arg name ay,
              decode_dim_arg name bx, decode_dim_arg name by,
              decode_dim_arg name cx, decode_dim_arg name cy,
              decode_dim_arg name dx, decode_dim_arg name dy)
         | _ -> Types.runtime_error (name ^ ": invalid path segment"))
      (Machine.decode_list name path_list_ref)
  in
  let arr = decode_tuple name_str cmd_ref in
  if Array.length arr < 1 then
    Types.runtime_error (name_str ^ ": invalid argument")
  else
    let s = decode_symbol name_str arr.(0) in
    if s = sym_PutBox then
      if Array.length arr <> 4 then
        Types.runtime_error (name_str ^ ": PutBox expects 3 arguments")
      else
        Graphic.PutBox
          (decode_dim_arg name_str arr.(1), decode_dim_arg name_str arr.(2),
           Box.empty_box, None)
    else if s = sym_Stroke then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": Stroke expects 1 argument")
      else Graphic.Draw (`Stroke, decode_path_internal name_str arr.(1))
    else if s = sym_Fill then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": Fill expects 1 argument")
      else Graphic.Draw (`Fill, decode_path_internal name_str arr.(1))
    else if s = sym_Clip then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": Fill expects 1 argument")
      else Graphic.Draw (`Clip, decode_path_internal name_str arr.(1))
    else if s = sym_SetColour then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": SetColour expects 1 argument")
      else Graphic.SetColour (decode_colour name_str arr.(1))
    else if s = sym_SetBgColour then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": SetBgColour expects 1 argument")
      else Graphic.SetBgColour (decode_colour name_str arr.(1))
    else if s = sym_SetAlpha then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": SetAlpha expects 1 argument")
      else Graphic.SetAlpha (Machine.decode_num name_str arr.(1))
    else if s = sym_SetLineWidth then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": SetLineWidth expects 1 argument")
      else Graphic.SetLineWidth (fun _ -> Dim.fixed_dim (XNum.num_of_float (Machine.decode_num name_str arr.(1))))
    else if s = sym_SetLineCap then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": SetLineCap expects 1 argument")
      else Graphic.SetLineCap (decode_line_cap name_str arr.(1))
    else if s = sym_SetLineJoin then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": SetLineJoin expects 1 argument")
      else Graphic.SetLineJoin (decode_line_join name_str arr.(1))
    else if s = sym_SetMiterLimit then
      if Array.length arr <> 2 then
        Types.runtime_error (name_str ^ ": SetMiterLimit expects 1 argument")
      else Graphic.SetMiterLimit (Machine.decode_num name_str arr.(1))
    else Types.runtime_error (name_str ^ ": invalid graphics command")
