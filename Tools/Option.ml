
(* maybe : 'b -> ('a -> 'b) -> 'a optional -> 'b *)

let maybe n f x =
  match x with
    Some y -> f y
  | None -> n

(* is_some : 'a option -> bool *)

let is_some x =
  match x with
    Some _ -> true
  | None -> false

(* is_none : 'a option -> bool *)

let is_none x =
  match x with
    Some _ -> false
  | None -> true

(* from_some : 'a option -> 'a *)

let from_some x =
  match x with
    Some v -> v
  | None -> failwith "from_some"

(* from_option : 'a -> 'a option -> 'a *)

let from_option d x =
  match x with
    None -> d
  | Some v -> v

(* compare : 'a -> 'a option -> bool *)

let compare x y =
  match y with
    None -> false
  | Some v -> x = v

(* compareq : 'a -> 'a option -> bool *)

let compareq x y =
  match y with
    None -> false
  | Some v -> x == v

(* to_list : 'a option -> 'a list *)

let to_list x =
  match x with
    Some v -> [v]
  | None -> []

(* from_list : 'a list -> 'a option *)

let from_list l =
  match l with
    [] -> None
  | x :: _ -> Some x

(* concat : 'a option list -> 'a list *)

let rec concat l =
  match l with
    [] -> []
  | None :: xs -> concat xs
  | Some x :: xs -> x :: concat xs

(* map : ('a -> 'b option) -> 'a list -> 'b list *)

let map f l = concat (List.map f l)
