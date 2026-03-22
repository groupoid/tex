open XNum

type 'a terms = (num * 'a) list
type 'a lin_form = { const : num; terms : 'a terms }
type compare_result = Lt | Eq | Gt

let empty _ = { const = num_zero; terms = [] }
let lin_zero = empty

let is_constant f = f.terms = []

let add_const f n = { f with const = f.const +/ n }

let scale a f =
  if a =/ num_zero then { const = num_zero; terms = [] }
  else {
    const = a */ f.const;
    terms = List.map (fun (b, v) -> (a */ b, v)) f.terms;
  }

let mult = scale
let neg f = scale num_minus_one f

let coefficient f x =
  let rec iter = function
    | [] -> num_zero
    | (a, y) :: ys -> if x == y then a else iter ys
  in iter f.terms

let of_num _ n = { const = n; terms = [] }
let of_unknown _ x = { const = num_zero; terms = [(num_one, x)] }
let of_scaled_unknown _ a x = { const = num_zero; terms = [(a, x)] }

let add_unknown f a x =
  { f with terms = (a, x) :: f.terms }

let remove_first_term f =
  match f.terms with
  | [] -> f
  | _ :: ts -> { f with terms = ts }

(* Simplified add/sub for now - just merge lists if we don't want to enforce sorting.
   However, TeX often benefits from sorted terms.
   If we want sorting, we'd need the cmp function here.
   But the interface says 'add : 'a lin_form -> 'a lin_form -> 'a lin_form' (no cmp).
   This implies either the cmp is stored in the record or we don't sort.
   Since the record didn't have it, we'll just append and handle it in evaluation.
*)

let add f1 f2 = {
  const = f1.const +/ f2.const;
  terms = f1.terms @ f2.terms;
}

let sub f1 f2 = add f1 (neg f2)

let lin_comb a f1 b f2 = add (scale a f1) (scale b f2)
