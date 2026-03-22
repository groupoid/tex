open XNum

type 'a terms = (num * 'a) list

type 'a lin_form = {
  const : num;
  terms : 'a terms;
}

type compare_result = Lt | Eq | Gt

val empty : ('a -> 'a -> compare_result) -> 'a lin_form
val lin_zero : ('a -> 'a -> compare_result) -> 'a lin_form

val add : 'a lin_form -> 'a lin_form -> 'a lin_form
val sub : 'a lin_form -> 'a lin_form -> 'a lin_form
val mult : num -> 'a lin_form -> 'a lin_form
val neg : 'a lin_form -> 'a lin_form
val scale : num -> 'a lin_form -> 'a lin_form
val add_const : 'a lin_form -> num -> 'a lin_form

val is_constant : 'a lin_form -> bool
val coefficient : 'a lin_form -> 'a -> num

val of_num : ('a -> 'a -> compare_result) -> num -> 'a lin_form
val of_unknown : ('a -> 'a -> compare_result) -> 'a -> 'a lin_form
val of_scaled_unknown : ('a -> 'a -> compare_result) -> num -> 'a -> 'a lin_form

val add_unknown : 'a lin_form -> num -> 'a -> 'a lin_form
val remove_first_term : 'a lin_form -> 'a lin_form

val lin_comb : num -> 'a lin_form -> num -> 'a lin_form -> 'a lin_form
