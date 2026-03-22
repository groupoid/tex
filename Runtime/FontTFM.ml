
[@@@warning "-37"]
open Tools.XNum
open Substitute
open GlyphMetric
open FontMetric

module LigKern = struct
  type lig_kern_cmd = 
    | LigCmd of int * int * int * int
    | KernCmd of int * int * num

  let is_lig = function LigCmd _ -> true | _ -> false
  let is_kern = function KernCmd _ -> true | _ -> false
  let skip = function LigCmd (s, _, _, _) -> s | KernCmd (s, _, _) -> s
  let next = function LigCmd (_, n, _, _) -> n | KernCmd (_, n, _) -> n
  let operand = function LigCmd (_, _, o, _) -> o | KernCmd _ -> failwith "LigKern.operand"
  let remainder = function LigCmd (_, _, _, r) -> r | KernCmd _ -> failwith "LigKern.remainder"
  let kern = function LigCmd _ -> failwith "LigKern.kern" | KernCmd (_, _, k) -> k

  let next_lig_kern lk_array pos =
    let lk = lk_array.(pos) in
    let s = skip lk in
    let n = next lk in
    let next_pos = if s < 128 then pos + s + 1 else -1 in
    if s <= 128 then
      if is_lig lk then
        let op = operand lk in
        (next_pos, n, `Ligature (remainder lk, (op lsr 2), (op lsr 1 land 1 = 1), (op land 1 = 1)))
      else
        (next_pos, n, `Kern (kern lk))
    else
      (-1, -1, `NoLigKern)

  let rec get_lig_kern lk_array pos next_char =
    let lk = lk_array.(pos) in
    let s = skip lk in
    let n = next lk in
    if n = next_char && s <= 128 then
      if is_lig lk then
        let op = operand lk in
        `Ligature (remainder lk, (op lsr 2), (op lsr 1 land 1 = 1), (op land 1 = 1))
      else
        `Kern (kern lk)
    else if s < 128 then
      get_lig_kern lk_array (pos + s + 1) next_char
    else
      `NoLigKern

  let rec list_lig_kerns lk_array pos =
    let lk = lk_array.(pos) in
    let s = skip lk in
    let n = next lk in
    if s > 128 then []
    else
      let res = if is_lig lk then
        let op = operand lk in
        (n, `Ligature (remainder lk, (op lsr 2), (op lsr 1 land 1 = 1), (op land 1 = 1)))
      else
        (n, `Kern (kern lk))
      in
      if s < 128 then res :: list_lig_kerns lk_array (pos + s + 1)
      else [res]
end

let read_tfm _ _ _ = failwith "FontTFM.read_tfm: not implemented"
