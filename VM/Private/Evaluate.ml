
open Tools.XNum
open Vm_types.Types
open Unicode
open UTypes
open Unicode.UTypes
open Unicode.SymbolTable

module UString = Unicode.UString (* we cannot open Unicode because of the name clash with Types *)
let tracing_bytecode = ref false

module Environment =
struct

  type environment = unknown array list

  let empty = []

  let push env (arr : unknown array) = arr :: env

  let push_unbound env n = Array.init n (fun _ -> create_unbound ()) :: env

  let pop env = match env with
    | _ :: es -> es
    | []        -> runtime_error "empty environment"

  let lookup env lvl idx =
    let rec iter lvl env = match env with
      | []      -> assert false
      | e :: es -> if lvl <= 0 then
                     e.(idx)
                   else
                     iter (lvl-1) es
    in iter lvl env

  let set env lvl idx x =
    let rec iter lvl env = match env with
      | []      -> assert false
      | e :: es -> if lvl <= 0 then
                     e.(idx) := x
                   else
                     iter (lvl-1) es
    in iter lvl env

end

module VMStack = struct
  let stack = ref []
  let call_stack = ref []

  let clear () =
    stack := [];
    call_stack := []

  let push x = stack := x :: !stack
  let push_list xs = stack := List.rev xs @ !stack

  let pop () = match !stack with
    | [] -> runtime_error "Stack underflow"
    | x :: xs -> stack := xs; x

  let top () = match !stack with
    | [] -> runtime_error "Stack underflow"
    | x :: _ -> x

  let get n =
    let rec iter n xs st =
      if n <= 0 then (stack := st; List.rev xs)
      else match st with
        | [] -> runtime_error "Stack underflow"
        | y :: ys -> iter (n-1) (y :: xs) ys
    in iter n [] !stack

  let remove n =
    let rec iter n st =
      if n <= 0 then stack := st
      else match st with
        | [] -> stack := []
        | _ :: ys -> iter (n-1) ys
    in iter n !stack

  let peek n =
    let rec iter n xs st =
      if n <= 0 then List.rev xs
      else match st with
        | [] -> runtime_error "Stack underflow"
        | y :: ys -> iter (n-1) (y :: xs) ys
    in Array.of_list (iter n [] !stack)

  let call cont = call_stack := cont :: !call_stack
  let return () = match !call_stack with
    | [] -> runtime_error "Call stack underflow"
    | c :: cs -> call_stack := cs; c ()
end

(* debugging *)

let print_pattern pat = match pat with
  | PCAnything   -> Printf.printf " _"
  | PCVariable i -> Printf.printf " v%d" i
  | PCNumber n   -> Printf.printf " %f" (float_of_num n)
  | PCChar c     -> Printf.printf " c%d" c
  | PCSymbol s   -> Printf.printf " s\"%s\"" (UString.to_string (Array.to_list (symbol_to_string s)))
  | PCTuple n    -> Printf.printf " (%d)" n
  | PCNil        -> Printf.printf " []"
  | PCConsList   -> Printf.printf " :"
  | PCAssign i   -> Printf.printf " =%d" i

let print_list p sep l = match l with
  | []      -> ()
  | [x]     -> p x
  | x :: xs ->
      p x;
      List.iter (fun y -> Printf.printf "%s" sep; p y) xs

let rec print_partial lvl x = match x with
  | Unbound      -> Printf.printf " <unbound>"
  | Constraint _ -> Printf.printf " <contraint>"
  | Bool b       -> Printf.printf "%s" (if b then " true" else " false")
  | Number n     -> Printf.printf " %f" (float_of_num n)
  | Char c       -> Printf.printf " '\\x%x'" c
  | Symbol s     -> Printf.printf " %s" (UString.to_string (Array.to_list (symbol_to_string s)))
  | LinForm lin  ->
      (Printf.printf " (%f" (float_of_num lin.Tools.LinForm.const);
       List.iter
         (fun (b, x) ->
            Printf.printf " + %f *" (float_of_num b);
            if lvl > 0 then
              print_partial (lvl-1) !x
            else
              Printf.printf " ...")
         lin.Tools.LinForm.terms;
       Printf.printf ")")
  | Application _ -> Printf.printf " <unevaluated>"
  | Primitive1 _      -> Printf.printf " <prim 1>"
  | Primitive2 _      -> Printf.printf " <prim 2>"
  | PrimitiveN (n, _)    -> Printf.printf " <prim %d>" n
  | Function (_, n, c)  ->
      if lvl > 0 then
        (Printf.printf " fun %d {\n" n;
         Array.iteri (print_b_cmd (lvl-1)) c;
         Printf.printf "}\n")
      else
        Printf.printf " fun %d {...}" n
  | Chain _      -> Printf.printf " <cfun>"
  | Relation _ -> Printf.printf " <rel>"
  | Nil          -> Printf.printf " []"
  | List (a, b)     ->
      if lvl > 0 then
        (Printf.printf " [";
         print_partial (lvl-1) !a;
         let rec iter x = match !x with
           | Nil -> Printf.printf "]"
           | List (a, b) ->
               Printf.printf ",";
               print_partial (lvl-1) !a;
               iter b
           | _ ->
               Printf.printf " :";
               print_partial (lvl-1) !x
         in iter b)
      else
        Printf.printf " [...]"
  | Tuple y ->
      if lvl > 0 then
        (Printf.printf " (";
         print_list
           (fun a -> print_partial (lvl-1) !a)
           ","
           (Array.to_list y);
         Printf.printf ")")
      else
        Printf.printf " (...)"
  | Dictionary d ->
      if lvl > 0 then
        (Printf.printf " <dict";
         SymbolMap.iter
           (fun k v ->
              Printf.printf " %s" (UString.to_string (Array.to_list (symbol_to_string k)));
              print_partial (lvl-1) !v)
           d;
         Printf.printf ">")
      else
        (Printf.printf " <dict";
         SymbolMap.iter
           (fun k _ -> Printf.printf " %s ..." (UString.to_string (Array.to_list (symbol_to_string k))))
           d;
         Printf.printf ">")
  | Opaque y -> Printf.printf " <opaque %s>" (Tools.Opaque.type_name y)

and print_bytecode lvl code =
  Array.iteri (print_b_cmd lvl) code

and print_b_cmd lvl i c = match c with
  | BDup              -> Printf.printf "%3d: dup\n" i
  | BPop              -> Printf.printf "%3d: pop\n" i
  | BPopN n           -> Printf.printf "%3d: pop %d\n" i n
  | BConst c          -> (Printf.printf "%3d: const" i;  print_partial lvl c;  Printf.printf "\n")
  | BGlobal x         -> (Printf.printf "%3d: global" i; print_partial lvl !x; Printf.printf "\n")
  | BVariable (k, l)     -> Printf.printf "%3d: var %d %d\n" i k l
  | BFunction (n, c)     ->
      (Printf.printf "%3d: fun %d {\n" i n;
       Array.iteri (print_b_cmd lvl) c;
       Printf.printf "}\n")
  | BDictionary syms  ->
      (Printf.printf "%3d: dict { " i;
       Array.iter
         (fun s ->
            Printf.printf " %s" (UString.to_string (Array.to_list (symbol_to_string s))))
         syms;
       Printf.printf " }\n")
  | BPair             -> Printf.printf "%3d: pair\n" i
  | BTuple n          -> Printf.printf "%3d: tuple %d\n" i n
  | BSet (k, l)          -> Printf.printf "%3d: set %d %d\n" i k l
  | BApply n          -> Printf.printf "%3d: apply %d\n" i n
  | BReturn           -> Printf.printf "%3d: return\n" i
  | BCondJump off     -> Printf.printf "%3d: cond-jump %d\n" i off
  | BJump off         -> Printf.printf "%3d: jump %d\n" i off
  | BLocal n          -> Printf.printf "%3d: local %d\n" i n
  | BEndLocal         -> Printf.printf "%3d: end-local\n" i
  | BMatch1 (p, s, v, off) ->
      (Printf.printf "%3d: match 1 %d %d %d {" i s v off;
       List.iter print_pattern p;
       Printf.printf "}\n")
  | BMatchN (ps, s, v, off) ->
      (Printf.printf "%3d: match %d %d %d %d" i (Array.length ps) s v off;
       Array.iter (fun p ->
         Printf.printf " {";
         List.iter print_pattern p;
         Printf.printf "}"
       ) ps;
       Printf.printf "\n")
  | BUnify            -> Printf.printf "%3d: unify\n" i
  | BRaise msg        -> Printf.printf "%3d: raise \"%s\"\n" i msg

let check_patterns checks stack (vars : unknown array) expr =
  let rec iter checks expr used_stack =
    let continue cs = match cs with
      | [] -> true
      | _  -> iter cs stack.(used_stack-1) (used_stack-1)
    in
    match checks with
    | []      -> true
    | c :: cs -> match c with
        | PCAnything   -> continue cs
        | PCVariable i ->
            vars.(i) := expr;
            continue cs
        | PCAssign i ->
            vars.(i) := expr;
            iter cs expr used_stack
        | PCNumber n -> (match expr with
            | Number i    -> if n <>/ i then false else continue cs
            | _ -> false)
        | PCChar c -> (match expr with
            | Char d      -> if c <> d then false else continue cs
            | _ -> false)
        | PCSymbol sym -> (match expr with
            | Symbol s    -> if sym <> s then false else continue cs
            | _ -> false)
        | PCTuple arity -> (match expr with
            | Tuple xs ->
                if arity = Array.length xs then
                  (for i = 0 to Array.length xs - 2 do
                     stack.(used_stack + i) <- !(xs.(Array.length xs - i - 1))
                   done;
                   iter cs !(xs.(0)) (used_stack + Array.length xs - 1))
                else
                  false
            | _ -> false)
        | PCNil -> (match expr with
            | Nil         -> continue cs
            | _ -> false)
        | PCConsList -> (match expr with
            | List (x, y) ->
                stack.(used_stack) <- !y;
                iter cs !x (used_stack + 1)
            | _ -> false)
  in iter checks expr 0

let binom n k =
  let rec iter k n x =
    if k <= 0 then
      x
    else
      iter (k-1) (n-1) ((num_of_int n // num_of_int k) */ x)
  in iter (if 2 * k > n then n-k else k) n num_one

let rec add_unknowns res x y = match (!x, !y) with
  | (Number m,  Number n)  -> res := Number (m +/ n)
  | (LinForm m, Number n)  -> res := LinForm (Tools.LinForm.add_const m n)
  | (Number m,  LinForm n) -> res := LinForm (Tools.LinForm.add_const n m)
  | (LinForm m, LinForm n) -> res := LinForm (Tools.LinForm.add m n)
  | (List (_, _), Nil)        -> res := !x
  | (Nil, List (_, _))        -> res := !y
  | (List (a, b), List (_, _))   ->
      let c = ref Unbound in
      res := List (a, c);
      add_unknowns c b y
  | (Tuple xs, Tuple ys)   ->
      if Array.length xs <> Array.length ys then
        runtime_error "+: tuples differ in length"
      else
        let len = Array.length xs in
        let zs  = Array.init len (fun _ -> create_unbound ()) in
        res := Tuple zs;
        for i = 0 to len - 1 do
          add_unknowns zs.(i) xs.(i) ys.(i)
        done
  | (LinForm lin, Tuple xs)
  | (Tuple xs, LinForm lin) ->
      let dim = Array.length xs in
      if lin.Tools.LinForm.const <>/ num_zero then
        runtime_error "+: invalid arguments"
      else
        let rec iter result terms = match terms with
          | []            -> res := Tuple (Array.map (fun l -> ref (LinForm l)) result)
          | (a, y) :: ys ->
              let z = Array.init dim (fun _ -> create_unbound ()) in
              forced_unify y (ref (Tuple z));
              let u = Array.init dim (fun i -> Tools.LinForm.add_unknown result.(i) a z.(i)) in
              iter u ys
        in iter (Array.init dim (fun i -> Tools.LinForm.of_unknown compare_unknowns xs.(i))) lin.Tools.LinForm.terms
  | (Unbound, _)      -> res := LinForm (Tools.LinForm.add (Tools.LinForm.of_unknown compare_unknowns x)
                                                     (Tools.LinForm.of_unknown compare_unknowns y))
  | (_, Unbound)      -> res := LinForm (Tools.LinForm.add (Tools.LinForm.of_unknown compare_unknowns x)
                                                     (Tools.LinForm.of_unknown compare_unknowns y))
  | (Constraint _, _) -> res := LinForm (Tools.LinForm.add (Tools.LinForm.of_unknown compare_unknowns x)
                                                     (Tools.LinForm.of_unknown compare_unknowns y))
  | (_, Constraint _) -> res := LinForm (Tools.LinForm.add (Tools.LinForm.of_unknown compare_unknowns x)
                                                     (Tools.LinForm.of_unknown compare_unknowns y))
  | (Number n, _) when n =/ num_zero -> res := !y
  | (_, Number n) when n =/ num_zero -> res := !x
  | _                                -> runtime_error "+: invalid argument"

and sub_unknowns res x y = match (!x, !y) with
  | (Number m,  Number n)  -> res := Number (m -/ n)
  | (LinForm m, Number n)  -> res := LinForm (Tools.LinForm.add_const m (minus_num n))
  | (Number m,  LinForm n) -> res := LinForm (Tools.LinForm.sub (Tools.LinForm.of_num compare_unknowns m) n)
  | (LinForm m, LinForm n) -> res := LinForm (Tools.LinForm.sub m n)
  | (Tuple xs,  Tuple ys)  ->
      if Array.length xs <> Array.length ys then
        runtime_error "-: I cannot subtract tuples of different length"
      else
        let len = Array.length xs in
        let zs  = Array.init len (fun _ -> create_unbound ()) in
        res := Tuple zs;
        for i = 0 to len - 1 do
          sub_unknowns zs.(i) xs.(i) ys.(i)
        done
  | (Unbound, _) -> res := LinForm (Tools.LinForm.add (Tools.LinForm.of_unknown compare_unknowns x)
                                                 (Tools.LinForm.of_scaled_unknown compare_unknowns (minus_num num_one) y))
  | (_, Unbound) -> res := LinForm (Tools.LinForm.add (Tools.LinForm.of_unknown compare_unknowns x)
                                                 (Tools.LinForm.of_scaled_unknown compare_unknowns (minus_num num_one) y))
  | _            -> runtime_error "-: invalid argument"

and mul_unknowns res x y = match (!x, !y) with
  | (Number m, Number n)   -> res := Number (m */ n)
  | (Number m, LinForm l)  -> res := LinForm (Tools.LinForm.scale m l)
  | (LinForm l, Number n)  -> res := LinForm (Tools.LinForm.scale n l)
  | (LinForm m, LinForm n) ->
      (evaluate_lin_form x m;
       evaluate_lin_form y n;
       match (!x, !y) with
       | (LinForm _, LinForm _) -> runtime_error "*: non-linear equation"
       | _                      -> mul_unknowns res x y)
  | (Number m,     Unbound)
  | (Number m,     Constraint _) -> res := LinForm (Tools.LinForm.of_scaled_unknown compare_unknowns m y)
  | (Unbound,      Number n)
  | (Constraint _, Number n)     -> res := LinForm (Tools.LinForm.of_scaled_unknown compare_unknowns n x)
  | (Number _,     Tuple ys)
  | (Unbound,      Tuple ys)
  | (Constraint _, Tuple ys) ->
      let len = Array.length ys in
      let zs  = Array.init len (fun _ -> create_unbound ()) in
      res := Tuple zs;
      for i = 0 to len - 1 do
        mul_unknowns zs.(i) x ys.(i)
      done
  | (Tuple xs, Number _)
  | (Tuple xs, Unbound)
  | (Tuple xs, Constraint _) ->
      let len = Array.length xs in
      let zs  = Array.init len (fun _ -> create_unbound ()) in
      res := Tuple zs;
      for i = 0 to len - 1 do
        mul_unknowns zs.(i) xs.(i) y
      done
  | (Tuple xs, Tuple ys) ->
      let len = Array.length xs in
      if Array.length ys <> len then
        runtime_error "*: tuples differ in length"
      else
        (let zs = Array.init len (fun i ->
            let z = create_unbound () in
            mul_unknowns z xs.(i) ys.(i);
            z) in
          let rec iter i lin =
            if i >= len then
              res := LinForm lin
            else
              iter (i+1) (Tools.LinForm.add lin (Tools.LinForm.of_unknown compare_unknowns zs.(i)))
          in iter 0 (Tools.LinForm.lin_zero compare_unknowns))
  | _ -> runtime_error ("*: invalid argument of type " ^ type_name !x ^ " and " ^ type_name !y)

and div_unknowns res x y = match (!x, !y) with
  | (Number m,  Number n) -> res := Number (m // n)
  | (LinForm l, Number n) -> res := LinForm (Tools.LinForm.scale (num_one // n) l)
  | (Unbound,   Number n) -> res := LinForm (Tools.LinForm.of_scaled_unknown compare_unknowns (num_one // n) x)
  | (Tuple xs,  Number _) ->
      let len = Array.length xs in
      let zs  = Array.init len (fun _ -> create_unbound ()) in
      res := Tuple zs;
      for i = 0 to len - 1 do
        div_unknowns zs.(i) xs.(i) y
      done
  | (_, LinForm l)   ->
      (evaluate_lin_form y l;
       match !y with
       | Number _ -> div_unknowns res x y
       | _        -> runtime_error "/: invalid argument")
  | _ -> runtime_error "/: invalid argument"

and prim_add x y =
  let z = ref Unbound in
  add_unknowns z x y;
  !z

and prim_sub x y =
  let z = ref Unbound in
  sub_unknowns z x y;
  !z

and prim_mul x y =
  let z = ref Unbound in
  mul_unknowns z x y;
  !z

and prim_div x y =
  let z = ref Unbound in
  div_unknowns z x y;
  !z

and evaluate_lin_form x lin =
  (* We check for unbound, constraint, and other unknowns. *)
  x := Unbound;
  let rec collect terms = match terms with
    | [] -> (Tools.LinForm.lin_zero compare_unknowns,
             num_zero,
             ref (Number lin.Tools.LinForm.const))
    | (a, y) :: ts ->
        (let (lin', coeff, const) = collect ts in
         match !y with
         | Unbound ->
             if identical x y then
               (lin', coeff +/ a, const)
             else
               (Tools.LinForm.add_unknown lin' a y, coeff, const)
         | Constraint _ ->
             (Tools.LinForm.add_unknown lin' a y, coeff, const)
         | _ ->
             let z1 = ref Unbound in
             let z2 = ref Unbound in
             mul_unknowns z1 (ref (Number a)) y;
             add_unknowns z2 const z1;
             (lin', coeff, z2))
  in
  let compute_x x coeff sum =
    if coeff =/ num_zero then
      x := !sum
    else if coeff =/ num_one then
      (let z = ref Unbound in
       sub_unknowns z sum x;
       forced_unify z (ref (Number num_zero)))
    else
      mul_unknowns x
        (ref (Number (num_one // (num_one -/ coeff))))
        sum
  in
  let sum = ref Unbound in
  let (lin', coeff, const) = collect lin.Tools.LinForm.terms in
  (if Tools.LinForm.is_constant lin' then
     add_unknowns sum const (ref (Number lin'.Tools.LinForm.const))
   else
     add_unknowns sum const (ref (LinForm lin'));
   compute_x x coeff sum)

and evaluate_list name x = match !x with
  | Nil      -> []
  | List (a, b) -> a :: evaluate_list name b
  | Unbound
  | Constraint _ -> runtime_error (name ^ ": argument undefined")
  | _            -> runtime_error (name ^ ": invalid argument")

and bind_unknown x y = match !y with
  | Unbound ->
      let c = [x; y] in
      let cv = Constraint c in
      x := cv;
      y := cv
  | Constraint c ->
      let us    = add_constraint x c in
      let cv = Constraint us in
      List.iter (fun z -> z := cv) us
  | LinForm lin ->
      let a = Tools.LinForm.coefficient lin x in
      if a =/ num_zero then
        x := !y
      else
        (let l = Tools.LinForm.sub lin (Tools.LinForm.of_scaled_unknown compare_unknowns a x) in
         if a =/ num_one then
           (x := Unbound;
            forced_unify (ref (LinForm l)) (ref (Number num_zero)))
         else
           x := LinForm (Tools.LinForm.scale (num_one // (num_one -/ a)) l))
  | _ -> x := !y

and forced_unify x y =
  if not (unify x y) then
    runtime_error ("unification error: " ^ type_name !x ^ " and " ^ type_name !y)
  else ()

and unify x y =
  let set_unknowns c v =
    List.iter (fun x -> x := v) c
  in
  match (!x, !y) with
  | (Unbound, _) -> (bind_unknown x y; true)
  | (_, Unbound) -> (bind_unknown y x; true)
  | (Constraint a, Constraint b) ->
      let c = merge_constraints a b in
      set_unknowns c (Constraint c);
      true
  | (Constraint a, _)        -> (set_unknowns a !y; true)
  | (_, Constraint b)        -> (set_unknowns b !x; true)
  | (Bool a,     Bool b)     -> (a = b)
  | (Char a,     Char b)     -> (a = b)
  | (Symbol a,   Symbol b)   -> (a = b)
  | (Nil,        Nil)        -> true
  | (List (a1, a2), List (b1, b2)) -> unify a1 b1 && unify a2 b2
  | (Number a,   Number b)   -> a =/ b
  | (Number a,  LinForm lin) ->
      let x' = ref Unbound in
      (evaluate_lin_form x' lin;
       match !x' with
       | Number b    -> b =/ a
       | LinForm lin -> (match lin.Tools.LinForm.terms with
           | [(c, z)]      -> unify z (ref (Number ((a -/ lin.Tools.LinForm.const) // c)))
           | (c, z) :: _ ->
               unify z (ref (LinForm (Tools.LinForm.add_const
                                       (Tools.LinForm.scale
                                          (minus_num num_one // c)
                                          (Tools.LinForm.remove_first_term lin))
                                       a)))
           | _ -> assert false)
       | _ -> assert false)
  | (LinForm lin, Number a) ->
      let x' = ref Unbound in
      (evaluate_lin_form x' lin;
       match !x' with
       | Number b    -> b =/ a
       | LinForm lin -> (match lin.Tools.LinForm.terms with
           | [(c, z)]      -> unify z (ref (Number ((a -/ lin.Tools.LinForm.const) // c)))
           | (c, z) :: _ ->
               unify z (ref (LinForm (Tools.LinForm.add_const
                                       (Tools.LinForm.scale
                                          (minus_num num_one // c)
                                          (Tools.LinForm.remove_first_term lin))
                                       a)))
           | _ -> assert false)
       | _ -> assert false)
  | (LinForm a, LinForm b) ->
      let x' = ref Unbound in
      let y' = ref Unbound in
      let z' = ref Unbound in
      (evaluate_lin_form x' a;
       evaluate_lin_form y' b;
       let c = match !x' with
         | Number n  -> Tools.LinForm.of_num compare_unknowns n
         | LinForm l -> l
         | _         -> assert false
       in
       let d = match !y' with
         | Number n  -> Tools.LinForm.of_num compare_unknowns n
         | LinForm l -> l
         | _         -> assert false
       in
       evaluate_lin_form z' (Tools.LinForm.lin_comb num_one c (minus_num num_one) d);
       match !z' with
       | Number c    -> c =/ num_zero
       | LinForm lin -> (match lin.Tools.LinForm.terms with
           | [(c, u)]      -> unify u (ref (Number (minus_num lin.Tools.LinForm.const // c)))
           | (c, u) :: _ ->
               unify u (ref (LinForm (Tools.LinForm.scale
                                       (minus_num num_one // c)
                                       (Tools.LinForm.remove_first_term lin))))
           | _ -> assert false)
       | _ -> assert false)
  | (Tuple a, Tuple b) ->
      if Array.length a <> Array.length b then
        false
      else
        let rec iter i =
          if i >= Array.length a then
            true
          else if unify a.(i) b.(i) then
            iter (i+1)
          else
            false
        in iter 0
  | (Dictionary a, Dictionary b) ->
      let l0 = SymbolMap.bindings a in
      let l1 = SymbolMap.bindings b in
      let rec iter l0 l1 = match (l0, l1) with
        | ([], []) -> true
        | ([], _)  -> false
        | (_, [])  -> false
        | ((k0, v0) :: kv0, (k1, v1) :: kv1) ->
            if k0 <> k1 then
              false
            else if unify v0 v1 then
              iter kv0 kv1
            else
              false
      in iter l0 l1
  | (Opaque a, Opaque b) -> Tools.Opaque.same_type a b && Tools.Opaque.unify a b
  | _ -> false

and execute_code (env : environment) code =
  let pc = ref 0 in
  while !pc < Array.length code do
    if !tracing_bytecode then (Printf.printf "PC=%d instruction=" !pc; print_b_cmd 0 !pc code.(!pc));
    match code.(!pc) with
    | BDup -> pc := !pc + 1; VMStack.push (VMStack.top ())
    | BPop -> pc := !pc + 1; ignore (VMStack.pop ())
    | BPopN n -> pc := !pc + 1; VMStack.remove n
    | BConst c -> pc := !pc + 1; VMStack.push c
    | BGlobal x -> pc := !pc + 1; VMStack.push !x
    | BVariable (k, l) -> pc := !pc + 1; VMStack.push !(Environment.lookup env k l)
    | BFunction (n, c) -> pc := !pc + 1; VMStack.push (Function (env, n, c))
    | BDictionary syms ->
        pc := !pc + 1;
        let d = ref SymbolMap.empty in
        for i = 0 to Array.length syms - 1 do
          d := SymbolMap.add syms.(i) (create_unknown (VMStack.pop ())) !d
        done;
        VMStack.push (Dictionary !d)
    | BPair ->
        pc := !pc + 1;
        let y = VMStack.pop () in
        let x = VMStack.pop () in
        VMStack.push (List (create_unknown x, create_unknown y))
    | BTuple n ->
        pc := !pc + 1;
        let args = Array.init n (fun _ -> create_unknown (VMStack.pop ())) in
        VMStack.push (Tuple args)
    | BSet (k, l) -> pc := !pc + 1; Environment.set env k l (VMStack.pop ())
    | BApply n ->
        pc := !pc + 1;
        let f = VMStack.pop () in
        let args = VMStack.get n in
        
        (match f with
          | Primitive1 g -> VMStack.push (g (create_unknown (List.hd (VMStack.get n))))
          | Primitive2 g ->
              
              VMStack.push (g (create_unknown (List.hd args)) (create_unknown (List.nth args 1)))
          | PrimitiveN (_, g) -> VMStack.push (g (List.map create_unknown (VMStack.get n)))
          | Function (e, ar, body) ->
              let args = List.map create_unknown (VMStack.get n) in
             if ar = List.length args then
               execute_code (Environment.push e (Array.of_list args)) body
             else
               runtime_error ("apply: arity mismatch, expected " ^ string_of_int ar ^ " but got " ^ string_of_int (List.length args))
         | _ -> runtime_error ("apply: not a function but " ^ type_name f))
    | BReturn -> pc := Array.length code
    | BCondJump off ->
        pc := !pc + 1;
        (match VMStack.pop () with
         | Bool b -> if not b then pc := !pc + off - 1
         | _      -> runtime_error "cond jump: not a boolean")
    | BJump off -> pc := !pc + off
    | BLocal n ->
        pc := !pc + 1;
        let env' = Environment.push_unbound env n in
        execute_code env' code
    | BEndLocal -> pc := Array.length code
    | BMatch1 (p, s, v, off) ->
        pc := !pc + 1;
        let expr = VMStack.pop () in
        let stack = Array.make s Unbound in
        let vars = Array.make v (ref Unbound) in
        if check_patterns p stack vars expr then
          execute_code (Environment.push env vars) code
        else
          pc := !pc + off - 1
    | BMatchN (ps, s, v, off) ->
        begin
          pc := !pc + 1;
          let n = Array.length ps in
          let exprs = VMStack.get n in
          let stack = Array.make s Unbound in
          let vars = Array.make v (ref Unbound) in
          let rec iter i exprs = match exprs with
            | [] -> true
            | e :: es ->
                if check_patterns ps.(i) stack vars e then
                  iter (i + 1) es
                else
                  false
          in
          if iter 0 exprs then
            execute_code (Environment.push env vars) code
          else
            pc := !pc + off - 1
        end
    | BUnify ->
        pc := !pc + 1;
        let y = VMStack.pop () in
        let x = VMStack.pop () in
        forced_unify (create_unknown x) (create_unknown y)
    | BRaise msg -> runtime_error msg
  done

let execute code (args : unknown list) : unknown =
  VMStack.clear ();
  VMStack.push_list (List.map (!) args);
  let _ = execute_code Environment.empty code in
  ref (VMStack.pop ())

let evaluate_num name x = match !x with
  | Number n -> n
  | LinForm l ->
      (evaluate_lin_form x l;
       match !x with
       | Number n -> n
       | _ -> runtime_error (name ^ ": number expected but got " ^ type_name !x))
  | _ -> runtime_error (name ^ ": number expected but got " ^ type_name !x)

let evaluate_opaque opaque_name unwrapper name x = match !x with
  | Opaque y ->
      (try unwrapper y with
         Tools.Opaque.Type_error -> runtime_error (name ^ ": " ^ opaque_name ^ " expected but got " ^ type_name !x))
  | Unbound
  | Constraint _ -> runtime_error (name ^ ": argument not defined")
  | _ -> runtime_error (name ^ ": " ^ opaque_name ^ " expected but got " ^ type_name !x)
