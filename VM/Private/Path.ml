
open Tools.XNum
open Vm_types.Types
open Unicode
open UTypes

module UString   = Unicode.UString
module SymbolMap = Unicode.SymbolTable.SymbolMap

(* Opaque type for path specifications *)
let apply_ps _ _ = runtime_error "application of non-function"

let cmp_ps p1 p2 = p1 == p2

let (ps_wrapper, ps_unwrapper) = Tools.Opaque.declare_type "path-specification" apply_ps cmp_ps cmp_ps

let wrap_ps ps = Opaque (ps_wrapper ps)

let unwrap_ps = Evaluate.evaluate_opaque "path-specification" ps_unwrapper

let evaluate_vec name v =
  match !v with
  | Tuple [|x; y|] ->
      (Evaluate.evaluate_num name x,
       Evaluate.evaluate_num name y)
  | _ -> runtime_error (name ^ ": pair expected but got " ^ type_name !v)

let make_path p =
  let (x, y) = evaluate_vec "make_path" p in
  wrap_ps (Tools.Bezier.make_spec x y)

let close_path cycle spec =
  let encode_pair x y =
    Tuple [|ref (Number x); ref (Number y)|]
  in
  let ps = unwrap_ps "close_path" spec in
  match !cycle with
  | Bool c ->
      Array.fold_right
        (fun (x0, y0, x1, y1, x2, y2, x3, y3) lst ->
          List
            (ref (Tuple
                   [|ref (encode_pair x0 y0);
                     ref (encode_pair x1 y1);
                     ref (encode_pair x2 y2);
                     ref (encode_pair x3 y3)|]),
            ref lst))
        (Tools.Bezier.close_spec ps c)
        Nil
  | _ -> runtime_error ("close_path: boolean expected but got " ^ type_name !cycle)

let add_point p spec =
  let ps    = unwrap_ps "path_add_point" spec in
  let (x, y) = evaluate_vec "path_add_point" p in
  wrap_ps (Tools.Bezier.add_point ps x y)

let add_in_dir dir spec =
  let ps       = unwrap_ps "path_add_in_dir" spec in
  let (dx, dy) = evaluate_vec "path_add_in_dir" dir in
  wrap_ps (Tools.Bezier.add_in_dir ps (Tools.Bezier.angle_of_vec dx dy))

let add_in_angle angle spec =
  let ps = unwrap_ps "path_add_in_angle" spec in
  let a  = Evaluate.evaluate_num "path_add_in_angle" angle in
  wrap_ps (Tools.Bezier.add_in_dir ps (float_of_num a))

let add_in_curl curl spec =
  let ps = unwrap_ps "path_add_in_curl" spec in
  let c  = Evaluate.evaluate_num "path_add_in_curl" curl in
  wrap_ps (Tools.Bezier.add_in_curl ps c)

let add_in_tension tension spec =
  let ps = unwrap_ps "path_add_in_tension" spec in
  let t  = Evaluate.evaluate_num "path_add_in_tension" tension in
  wrap_ps (Tools.Bezier.add_in_tension ps t)

let add_out_dir dir spec =
  let ps       = unwrap_ps "path_add_out_dir" spec in
  let (dx, dy) = evaluate_vec "path_add_out_dir" dir in
  wrap_ps (Tools.Bezier.add_out_dir ps (Tools.Bezier.angle_of_vec dx dy))

let add_out_angle angle spec =
  let ps = unwrap_ps "path_add_out_angle" spec in
  let a  = Evaluate.evaluate_num "path_add_out_angle" angle in
  wrap_ps (Tools.Bezier.add_out_dir ps (float_of_num a))

let add_out_curl curl spec =
  let ps = unwrap_ps "path_add_out_curl" spec in
  let c  = Evaluate.evaluate_num "path_add_out_curl" curl in
  wrap_ps (Tools.Bezier.add_out_curl ps c)

let add_out_tension tension spec =
  let ps = unwrap_ps "path_add_out_tension" spec in
  let t  = Evaluate.evaluate_num "path_add_out_tension" tension in
  wrap_ps (Tools.Bezier.add_out_tension ps t)

let add_control_points p1 p2 spec =
  let ps      = unwrap_ps "path_add_control_points" spec in
  let (x1, y1) = evaluate_vec "path_add_control_points" p1 in
  let (x2, y2) = evaluate_vec "path_add_control_points" p2 in
  wrap_ps (Tools.Bezier.add_control_points ps x1 y1 x2 y2)