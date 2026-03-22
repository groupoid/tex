
open Tools.XNum
open Vm_types.Types
open Unicode
open UTypes
open Unicode.UTypes
open Unicode.SymbolTable

module UString = Unicode.UString

let true_sym  = string_to_symbol (UString.uc_string_of_ascii "True")
let false_sym = string_to_symbol (UString.uc_string_of_ascii "False")

let make_tuple_pat elements = match elements with
  | []  -> assert false
  | [x] -> x
  | _   -> Parser.PTuple elements

let rec compile_pattern off pat = 
  let (stack_depth, num_vars, var_names, checks) = compile_pat off pat (0, 0, [], []) in
  (stack_depth, num_vars, List.rev var_names, checks)

and compile_pat off pat ((stack_depth, num_vars, var_names, checks) as state) = match pat with
  | Parser.PAnything   -> (stack_depth, num_vars,     var_names,          PCAnything                  :: checks)
  | Parser.PId sym     -> (stack_depth, num_vars + 1, sym :: var_names, PCVariable (off + num_vars) :: checks)
  | Parser.PNumber x   -> (stack_depth, num_vars,     var_names,          PCNumber x                  :: checks)
  | Parser.PChar x     -> (stack_depth, num_vars,     var_names,          PCChar x                    :: checks)
  | Parser.PSymbol sym -> (stack_depth, num_vars,     var_names,          PCSymbol sym                :: checks)
  | Parser.PAssign (x, p) -> 
      let (s, n, v, c) = compile_pat off p state in
      (s, n + 1, x :: v, PCAssign (off + n) :: c)
  | Parser.PTuple xs   -> 
      let len = List.length xs in
      let (s, n, v, c) = List.fold_right (compile_pat off) xs state in
      (max (s + len - 1) stack_depth, n, v, PCTuple len :: c)
  | Parser.PList xs -> 
      List.fold_right
        (fun x s1 -> 
          let (s, n, v, c) = compile_pat off x s1 in
          (max (s + 1) stack_depth, n, v, PCConsList :: c))
        xs
        (stack_depth, num_vars, var_names, PCNil :: checks)
  | Parser.PListTail (xs, tail) -> 
      List.fold_right
        (fun x s1 -> 
          let (s, n, v, c) = compile_pat off x s1 in
          (max (s + 1) stack_depth, n, v, PCConsList :: c))
        xs
        (compile_pat off tail state)

let get_declared_variables decls = 
  let rec iter names decls = match decls with
    | [] -> names
    | Parser.DFun (name, pat, _, _) as d :: ds -> 
        let defs = 
          try SymbolMap.find name names 
          with Not_found -> [] 
        in
        (match defs with
         | Parser.DFun (_, p, _, _) :: _ -> 
             if List.length p <> List.length pat then
               raise (Syntax_error (("", 0, 0),
                        UString.append (UString.uc_string_of_ascii "arity mismatch in declaration of ")
                                       (symbol_to_string name)))
         | _ -> ());
        iter (SymbolMap.add name (d :: defs) names) ds
    | Parser.DPattern (p, _) :: ds -> 
        let rec iter_pattern names p = match p with
          | Parser.PId name       -> SymbolMap.add name [] names
          | Parser.PAnything
          | Parser.PNumber _
          | Parser.PChar _
          | Parser.PSymbol _      -> names
          | Parser.PTuple ps      -> List.fold_left iter_pattern names ps
          | Parser.PList ps       -> List.fold_left iter_pattern names ps
          | Parser.PListTail (ps, p) -> List.fold_left iter_pattern (iter_pattern names p) ps
          | Parser.PAssign (name, p) -> SymbolMap.add name [] (iter_pattern names p)
        in iter (iter_pattern names p) ds
  in iter SymbolMap.empty decls

let rec compile_expr scope expr = match expr with
  | Parser.TUnbound -> [BConst Unbound]
  | Parser.TId x -> 
      (try [Scope.lookup scope x]
       with Not_found -> 
         raise (Syntax_error (("", 0, 0),
                  UString.append (UString.uc_string_of_ascii "undefined symbol ")
                                 (symbol_to_string x))))
  | Parser.TNumber x  -> [BConst (Number x)]
  | Parser.TChar x    -> [BConst (Char x)]
  | Parser.TFun cases -> compile_function scope cases
  | Parser.TSymbol x  -> 
      if x = true_sym then [BConst (Bool true)]
      else if x = false_sym then [BConst (Bool false)]
      else [BConst (Symbol x)]
  | Parser.TApp (f, args) -> 
      BApply (List.length args) :: 
      compile_expr scope f @ 
      List.fold_right (fun arg code -> compile_expr scope arg @ code) args []
  | Parser.TTuple xs -> 
      let rec compile_elements is_const n els xs = match xs with
        | [] -> (is_const, n, els)
        | y :: ys -> 
            let e = compile_expr scope y in
            let c = is_const && match e with [BConst _] -> true | _ -> false in
            compile_elements c (n + 1) (e :: els) ys
      in
      let (is_const, n, elements) = compile_elements true 0 [] xs in
      if is_const then begin
        let values = Array.init n (fun _ -> ref Unbound) in
        let _ = List.fold_left
                  (fun i t -> match t with
                    | [BConst v] -> values.(i) <- ref v; i - 1
                    | _ -> assert false)
                  (n - 1)
                  elements in
        [BConst (Tuple values)]
      end else
        BTuple n :: List.fold_left (fun es e -> e @ es) [] elements
  | Parser.TList xs -> 
      let rec iter xs = match xs with
        | [] -> [BConst Nil]
        | y :: ys -> 
            let t    = compile_expr scope y in
            let tail = iter ys in
            match (t, tail) with
            | ([BConst a], [BConst b]) -> [BConst (List (ref a, ref b))]
            | _ -> BPair :: t @ tail
      in iter xs
  | Parser.TListTail (xs, tail) -> 
      let rec iter xs = match xs with
        | [] -> compile_expr scope tail
        | y :: ys -> 
            let t    = compile_expr scope y in
            let rest = iter ys in
            match (t, rest) with
            | ([BConst a], [BConst b]) -> [BConst (List (ref a, ref b))]
            | _ -> BPair :: t @ rest
      in iter xs
  | Parser.TLocal (decls, expr) -> 
      let (new_scope, init_code) = compile_local_declarations scope decls in
      BEndLocal :: compile_expr new_scope expr @ init_code
  | Parser.TSequence (stmts, expr) -> 
      compile_expr scope expr @ 
      List.fold_left (fun code s -> compile_statement scope s @ code) [] stmts
  | Parser.TDo exprs -> 
      let state_sym = Unicode.SymbolTable.string_to_symbol (Unicode.UString.uc_string_of_ascii "state") in
      let (new_scope, _) = Scope.push scope [state_sym] in
      [BFunction (1, Array.of_list (List.rev (BReturn :: List.fold_left (fun code s -> compile_monad new_scope s @ code) [BVariable (0, 0)] exprs)))]
  | Parser.TIfThenElse (p, e0, e1) -> 
      let then_code = compile_expr scope e0 in
      let else_code = compile_expr scope e1 in
      else_code @ 
      (BJump (List.length else_code + 1) :: then_code) @ 
      (BCondJump (List.length then_code + 2) :: compile_expr scope p)
  | Parser.TMatch (expr, pats) -> 
      compile_matching scope (List.map (fun (p, g, e) -> ([p], g, e)) pats) @ 
      compile_expr scope expr

and compile_matching scope cases = 
  let num_pats = List.length cases in
  let (_, _size, cases_code) =
    List.fold_left
      (fun (i, size, cases) (pats, g, e) -> 
        let (stack_depth, num_vars, vars, compiled_pats) =
          List.fold_left
            (fun (depth, num_vars, vars, pats) pat -> 
              let (sd, n, v, p) = compile_pattern num_vars pat in
              (max depth sd, num_vars + n, v :: vars, p :: pats))
            (0, 0, [], [])
            pats in
        let (new_scope, _) = Scope.push scope (List.flatten vars) in
        let expr_code = compile_expr new_scope e in
        let expr_size = List.length expr_code in
        let guard_code = match g with
          | None -> []
          | Some e -> BCondJump (expr_size + 3) :: compile_expr new_scope e in
        let guard_size = List.length guard_code in
        let off = if i < num_pats then guard_size + expr_size + 4 else 0 in
        let cpat = Array.of_list (List.rev compiled_pats) in
        let pop = if Array.length cpat = 1 then BPop else BPopN (Array.length cpat) in
        let match_cmd = if Array.length cpat = 1 then BMatch1 (cpat.(0), stack_depth, num_vars, off)
                        else BMatchN (cpat, stack_depth, num_vars, off) in
        let code = expr_code @ [pop; match_cmd] @ guard_code in
        (i + 1, size + expr_size + guard_size + 4, code :: cases))
      (1, 0, [])
      cases 
  in
  let rec iter cases size = match cases with
    | []      -> []
    | x :: xs -> 
        let new_size = size - List.length x - 2 in
        if new_size > 0 then BEndLocal :: BJump new_size :: x @ iter xs new_size
        else BEndLocal :: x @ iter xs new_size
  in iter (List.rev cases_code) _size

and compile_function scope cases = 
  let arity = match cases with (ps, _, _) :: _ -> List.length ps | _ -> 0 in
  let rec make_dictionary dict cases = match cases with
    | [] -> Some dict
    | ([Parser.PSymbol s], None, e) :: cs ->
        make_dictionary (SymbolMap.add s (compile_expr scope e) dict) cs
    | _ -> None
  in
  match make_dictionary SymbolMap.empty cases with
  | Some d -> 
      let (code, syms) = SymbolMap.fold (fun s c (code, syms) -> (c @ code, s :: syms)) d ([], []) in
      BDictionary (Array.of_list syms) :: code
  | None -> [BFunction (arity, Array.of_list (List.rev (BReturn :: compile_matching scope cases)))]

and compile_local_declarations scope decls = 
  let names = get_declared_variables decls in
  let vars = SymbolMap.fold (fun n _ vars -> n :: vars) names [] in
  let (new_scope, num_vars) = Scope.push scope vars in
  let fun_defs =
    SymbolMap.fold
      (fun name defs code -> match defs with
       | [] -> code
       | _ -> 
           let cases = List.map (function Parser.DFun (_, ps, g, t) -> (ps, g, t) | _ -> assert false) defs in
           let (i1, i2) = Scope.lookup_local new_scope name in
           match cases with
           | [([], None, term)] -> BSet (i1, i2) :: compile_expr (Scope.shift scope 1) term @ code
           | _ -> BSet (i1, i2) :: compile_function new_scope cases @ code)
      names
      [BLocal num_vars] in
  (new_scope, iter scope new_scope fun_defs decls)

and iter scope new_scope code decls = match decls with
  | [] -> code
  | Parser.DFun _ :: ds -> iter scope new_scope code ds
  | Parser.DPattern (pat, term) :: ds -> 
      (match pat with
       | Parser.PId id ->
           let (i1, i2) = Scope.lookup_local new_scope id in
           iter scope new_scope (BSet (i1, i2) :: compile_expr new_scope term @ code) ds
       | _ -> 
           let (sd, nv, v, c) = compile_pattern 0 pat in
           let (local_scope, _) = Scope.push new_scope v in
           let pat_code =
             List.fold_left
               (fun code var -> 
                 let (a1, a2) = Scope.lookup_local new_scope var in
                 let (b1, b2) = Scope.lookup_local local_scope var in
                 BSet (a1 + 1, a2) :: BVariable (b1, b2) :: code)
               [BPop; BMatch1 (c, sd, nv, 0)]
               v in
           iter scope new_scope (BEndLocal :: pat_code @ compile_expr (Scope.shift scope 1) term @ code) ds)

and compile_statement scope stmt = match stmt with
  | Parser.SEquation (x, y) -> BUnify :: compile_expr scope x @ compile_expr scope y
  | Parser.SFunction t -> BPop :: compile_expr scope t
  | Parser.SIfThen (p, s) -> 
      let then_code = compile_statement scope s in
      then_code @ (BCondJump (List.length then_code) :: compile_expr scope p)
  | Parser.SIfThenElse (p, s0, s1) -> 
      let then_code = compile_statement scope s0 in
      let else_code = compile_statement scope s1 in
      else_code @ 
      (BJump (List.length else_code) :: then_code) @ 
      (BCondJump (List.length then_code + 1) :: compile_expr scope p)

and compile_monad scope stmt = match stmt with
  | Parser.SEquation (x, y) -> BUnify :: compile_expr scope x @ compile_expr scope y
  | Parser.SFunction t -> BApply 1 :: compile_expr scope t
  | Parser.SIfThen (p, s) -> 
      let then_code = compile_monad scope s in
      then_code @ (BCondJump (List.length then_code + 1) :: compile_expr scope p)
  | Parser.SIfThenElse (p, s0, s1) -> 
      let then_code = compile_monad scope s0 in
      let else_code = compile_monad scope s1 in
      else_code @ 
      (BJump (List.length else_code + 1) :: then_code) @ 
      (BCondJump (List.length then_code + 2) :: compile_expr scope p)

let compile_global_declarations scope decls = 
  let names = get_declared_variables decls in
  SymbolMap.iter (fun n _ -> Scope.add_global scope n Unbound) names;
  let fun_defs = 
    SymbolMap.fold
      (fun name defs code -> match defs with
       | [] -> code
       | _ -> 
           let cases = List.map (function Parser.DFun (_, ps, g, t) -> (ps, g, t) | _ -> assert false) defs in
           let var = Scope.lookup_global scope name in
           BUnify :: BGlobal var :: compile_function scope cases @ code)
      names
      [] in
  let rec iter code decls = match decls with
    | [] -> Array.of_list (List.rev code)
    | Parser.DFun _ :: ds -> iter code ds
    | Parser.DPattern (pat, term) :: ds -> 
        let (sd, nv, v, c) = compile_pattern 0 pat in
        let (local_scope, _) = Scope.push scope v in
        let pat_code =
          List.fold_left
            (fun code var -> 
              let v = Scope.lookup_global scope var in
              let (b1, b2) = Scope.lookup_local local_scope var in
              BUnify :: BGlobal v :: BVariable (b1, b2) :: code)
            [BMatch1 (c, sd, nv, 0)]
            v in
        iter (pat_code @ compile_expr scope term @ code) ds
  in iter fun_defs decls

let compile_declarations scope stream = 
  let lexer = Lexer.make_lexer (Scope.symbol_table scope) stream in
  let decls = Parser.parse_program lexer in
  compile_global_declarations scope decls

let compile_expression scope stream = 
  let lexer = Lexer.make_lexer (Scope.symbol_table scope) stream in
  let expr = Parser.parse_expression lexer in
  Array.of_list (List.rev (compile_expr scope expr))
