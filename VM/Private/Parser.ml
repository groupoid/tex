
open Tools.XNum
open Unicode
open UTypes
open Unicode
open Vm_types.Types
open Unicode.SymbolTable
open Lexer

type pattern = 
  | PAnything
  | PId of symbol
  | PNumber of num
  | PChar of uc_char
  | PSymbol of symbol
  | PTuple of pattern list
  | PList of pattern list
  | PListTail of pattern list * pattern
  | PAssign of symbol * pattern

type term = 
  | TUnbound
  | TId of symbol
  | TNumber of num
  | TChar of uc_char
  | TSymbol of symbol
  | TApp of term * term list
  | TTuple of term list
  | TList of term list
  | TListTail of term list * term
  | TFun of (pattern list * term option * term) list
  | TLocal of decl list * term
  | TSequence of stmt list * term
  | TDo of stmt list
  | TIfThenElse of term * term * term
  | TMatch of term * (pattern * term option * term) list

and decl = 
  | DFun of symbol * pattern list * term option * term
  | DPattern of pattern * term

and stmt = 
  | SEquation of term * term
  | SIfThen of term * stmt
  | SIfThenElse of term * stmt * stmt
  | SFunction of term

type stmt_or_expr = 
  | Expression of term
  | Statement of stmt

let rec parse_program lexer = 
  match parse_decl_list lexer with
  | (ds, EOF) -> ds
  | (_, tok)  -> 
      syntax_error_uc lexer
        (Unicode.UString.append (Unicode.UString.uc_string_of_ascii "unexpected ") (token_to_string tok))

and parse_decl_list lexer = 
  match read_token lexer with
  | EOF -> ([], EOF)
  | END -> ([], END)
  | tok -> 
      match parse_decl tok lexer with
      | (d, SEMICOLON) ->
          let (ds, tok_next) = parse_decl_list lexer in
          (d @ ds, tok_next)
      | x -> x

and parse_decl first_token lexer = match first_token with
  | LID id -> parse_arg_list lexer id first_token
  | INFIX assoc -> 
      (match read_token lexer with
       | NUMBER n -> 
           let (syms, tok) = parse_id_list lexer in
           (match tok with
            | SEMICOLON ->
                List.iter (Lexer.add_bin_op lexer (int_of_num (round_num n)) assoc) syms;
                parse_decl (read_token lexer) lexer
            | _ -> syntax_error lexer "; expected")
       | _ -> syntax_error lexer "number expected")
  | PREFIX -> 
      (match parse_id_list lexer with
       | (syms, SEMICOLON) ->
           List.iter (Lexer.add_pre_op lexer) syms;
           parse_decl (read_token lexer) lexer
       | _ -> syntax_error lexer "; expected")
  | POSTFIX -> 
      (match parse_id_list lexer with
       | (syms, SEMICOLON) ->
           List.iter (Lexer.add_post_op lexer) syms;
           parse_decl (read_token lexer) lexer
       | _ -> syntax_error lexer "; expected")
  | _ ->
      let bin_op_in_front = 
        match first_token with
        | PARENOPEN -> 
            (let t1 = read_token lexer in
             match t1 with
             | BINOP (x, _, _) -> 
                 (match read_token lexer with
                  | PARENCLOSE -> Some x
                  | t2 -> 
                      restore_token lexer t2;
                      restore_token lexer t1;
                      None)
             | _ -> restore_token lexer t1; None)
        | _ -> None
      in
      (match bin_op_in_front with
       | Some x -> parse_arg_list lexer x first_token
       | None -> 
           let (p0, t0) = parse_pattern first_token lexer in
           match t0 with
           | BINOP (x, _, _) -> 
               (match parse_pattern (read_token lexer) lexer with
                | (p1, COLON_EQUAL) ->
                    let (e, tok) = parse_stmt_list_expr lexer in
                    ([DFun (x, [p0; p1], None, e)], tok)
                | (p1, AMPERSAND) -> 
                    (match parse_expr (read_token lexer) lexer with
                     | (g, COLON_EQUAL) ->
                         let (e, tok) = parse_stmt_list_expr lexer in
                         ([DFun (x, [p0; p1], Some g, e)], tok)
                     | _ -> syntax_error lexer ":= expected")
                | _ -> syntax_error lexer ":= expected")
           | COLON_EQUAL->
               let (e, tok) = parse_stmt_list_expr lexer in
               ([DPattern (p0, e)], tok)
           | _ -> syntax_error lexer "binary operator or := expected after pattern")

and parse_arg_list lexer id first_token = 
  let tok = read_token lexer in
  match tok with
  | BINOP (x, _, _) -> 
      (let p1 = parse_simple_pattern (read_token lexer) lexer in
       match read_token lexer with
       | COLON_EQUAL->
           let (e, tok) = parse_stmt_list_expr lexer in
           ([DFun (x, [PId id; p1], None, e)], tok)
       | AMPERSAND -> 
           (match parse_expr (read_token lexer) lexer with
            | (g, COLON_EQUAL) ->
                let (e, tok) = parse_stmt_list_expr lexer in
                ([DFun (x, [PId id; p1], Some g, e)], tok)
            | _ -> syntax_error lexer ":= expected")
       | _ -> syntax_error lexer ":= or & expected")
  | COLON_EQUAL->
      let (e, tok) = parse_stmt_list_expr lexer in
      ([DPattern (PId id, e)], tok)
  | AMPERSAND -> 
      (match parse_expr (read_token lexer) lexer with
       | (g, COLON_EQUAL) ->
           let (e, tok) = parse_stmt_list_expr lexer in
           ([DFun (id, [], Some g, e)], tok)
       | _ -> syntax_error lexer ":= expected")
  | EQUAL-> 
      (restore_token lexer EQUAL;
       match parse_pattern first_token lexer with
       | (p, COLON_EQUAL) ->
           let (e, tok) = parse_stmt_list_expr lexer in
           ([DPattern (p, e)], tok)
       | _ -> syntax_error lexer ":= expected")
  | END       -> ([DFun (id, [], None, TUnbound)], END)
  | SEMICOLON -> ([DFun (id, [], None, TUnbound)], SEMICOLON)
  | _ -> 
      (match parse_pattern_list tok lexer with
       | (ps, COLON_EQUAL) ->
           let (e, tok) = parse_stmt_list_expr lexer in
           ([DFun (id, ps, None, e)], tok)
       | (ps, AMPERSAND) -> 
           (match parse_expr (read_token lexer) lexer with
            | (g, COLON_EQUAL) ->
                let (e, tok) = parse_stmt_list_expr lexer in
                ([DFun (id, ps, Some g, e)], tok)
            | _ -> syntax_error lexer ":= expected")
       | (ids, SEMICOLON) ->
           let rec iter ids = match ids with
             | [] -> [DFun (id, [], None, TUnbound)]
             | PId s :: is -> DFun (s, [], None, TUnbound) :: iter is
             | _ -> syntax_error lexer ":= expected"
           in (iter ids, SEMICOLON)
       | (ids, END) ->
           let rec iter ids = match ids with
             | [] -> [DFun (id, [], None, TUnbound)]
             | PId s :: is -> DFun (s, [], None, TUnbound) :: iter is
             | _ -> syntax_error lexer ":= expected"
           in (iter ids, END)
       | _ -> syntax_error lexer ":= or & expected")

and parse_stmt_list_expr lexer = 
  let rec iter first_token = 
    match parse_stmt_or_expr first_token lexer with
    | (Expression e, tok) -> ([], e, tok)
    | (Statement s,  tok) -> 
        if tok = COMMA then
          let (ss, e, tok_next) = iter (read_token lexer) in
          (s :: ss, e, tok_next)
        else
          syntax_error lexer ", expected"
  in
  match iter (read_token lexer) with
  | ([], e, tok) -> (e, tok)
  | (s, e, tok)  -> (TSequence (s, e), tok)

and parse_stmt first_token lexer = match first_token with
  | IF -> parse_if_stmt lexer
  | _ -> 
      match parse_expr first_token lexer with
      | (e0, EQUAL) ->
          let (e1, tok) = parse_expr (read_token lexer) lexer in
          (SEquation (e0, e1), tok)
      | _ -> syntax_error lexer "= expected"

and parse_if_stmt lexer = 
  match parse_expr (read_token lexer) lexer with
  | (p, THEN) -> 
      (match parse_stmt (read_token lexer) lexer with
       | (s0, ELSE) -> 
           (match parse_stmt (read_token lexer) lexer with
            | (s1, END) -> (SIfThenElse (p, s0, s1), read_token lexer)
            | _ -> syntax_error lexer "end expected")
       | (s0, ELSEIF) -> 
           let (s1, tok) = parse_if_stmt lexer in
           (SIfThenElse (p, s0, s1), tok)
       | (s0, END) -> (SIfThen (p, s0), read_token lexer)
       | _ -> syntax_error lexer "else or end expected")
  | _ -> syntax_error lexer "then expected"

and multiply_symbol = TId (string_to_symbol [|42|]) (* * *)

and parse_expr first_token lexer = 
  match parse_expr_pri 0 first_token lexer with
  | (e, WHERE) -> 
      (match parse_decl_list lexer with
       | (decls, END) -> (TLocal (decls, e), read_token lexer)
       | _ -> syntax_error lexer "end expected")
  | x -> x

and parse_expr_pri pri first_token lexer = match first_token with
  | LOCAL-> 
      (match read_token lexer with
       | BEGIN -> 
           (match parse_decl_list lexer with
            | (decls, END) -> 
                let (e, tok) = parse_expr_pri pri (read_token lexer) lexer in
                (TLocal (decls, e), tok)
            | _ -> syntax_error lexer "end expected")
       | tok -> 
           match parse_decl tok lexer with
           | (decl, SEMICOLON) -> 
               let (e, t) = parse_expr_pri pri (read_token lexer) lexer in
               (TLocal (decl, e), t)
           | _ -> syntax_error lexer "; expected")
  | _ -> 
      let (term, tok) = 
        match parse_simple_expr_list first_token lexer with
        | ([e], tok) -> (e, tok)
        | (TNumber n :: es, tok) -> 
            let x = match es with
              | [c] -> c
              | c :: cs -> TApp (c, cs)
              | [] -> assert false
            in (TApp (multiply_symbol, [TNumber n; x]), tok)
        | (e :: es, tok) -> (TApp (e, es), tok)
        | (_, _) -> assert false
      in parse_sub_expr term pri tok lexer

and parse_do_expr lexer = 
  let convert x = match x with
    | Statement s -> s
    | Expression e -> SFunction e
  in
  let rec iter tok = match parse_stmt_or_expr tok lexer with
    | (e, SEMICOLON) -> 
        (match read_token lexer with
         | END -> [convert e]
         | tok_next -> convert e :: iter tok_next)
    | (e, END) -> [convert e]
    | _ -> syntax_error lexer "end or ; expected"
  in TDo (iter (read_token lexer))

and parse_if_expr lexer = 
  match parse_expr (read_token lexer) lexer with
  | (p, THEN) -> 
      (match parse_stmt_list_expr lexer with
       | (e0, ELSE) -> 
           (match parse_stmt_list_expr lexer with
            | (e1, END) -> TIfThenElse (p, e0, e1)
            | _ -> syntax_error lexer "end expected")
       | (e0, ELSEIF) -> TIfThenElse (p, e0, parse_if_expr lexer)
       | _ -> syntax_error lexer "else expected")
  | _ -> syntax_error lexer "then expected"

and parse_stmt_or_expr first_token lexer = match first_token with
  | IF -> 
      (match parse_if_stmt_or_expr lexer with
       | (Expression expr, tok) -> 
           let (e, t) = parse_sub_expr expr 0 tok lexer in
           (Expression e, t)
       | stmt -> stmt)
  | _ -> 
      match parse_expr first_token lexer with
      | (e0, EQUAL) ->
          let (e1, tok) = parse_expr (read_token lexer) lexer in
          (Statement (SEquation (e0, e1)), tok)
      | (e, tok) -> (Expression e, tok)

and parse_if_stmt_or_expr lexer = 
  match parse_expr (read_token lexer) lexer with
  | (p, THEN) -> 
      (match parse_stmt_or_expr (read_token lexer) lexer with
       | (Statement s0, ELSE) -> 
           (match parse_stmt (read_token lexer) lexer with
            | (s1, END) -> (Statement (SIfThenElse (p, s0, s1)), read_token lexer)
            | _ -> syntax_error lexer "end expected")
       | (Expression e0, ELSE) -> 
           (match parse_expr (read_token lexer) lexer with
            | (e1, END) -> (Expression (TIfThenElse (p, e0, e1)), read_token lexer)
            | _ -> syntax_error lexer "end expected")
       | (Statement s0, ELSEIF) -> 
           let (s1, tok) = parse_if_stmt lexer in
           (Statement (SIfThenElse (p, s0, s1)), tok)
       | (Expression e0, ELSEIF) -> 
           let e1 = parse_if_expr lexer in
           (Expression (TIfThenElse (p, e0, e1)), read_token lexer)
       | (Expression e0, END) -> (Statement (SIfThen (p, SFunction e0)), read_token lexer)
       | (Statement s0,  END) -> (Statement (SIfThen (p, s0)), read_token lexer)
       | _ -> syntax_error lexer "else or end expected")
  | _ -> syntax_error lexer "then expected"

and parse_sub_expr term pri first_token lexer = match first_token with
  | BINOP (x, p, a) -> 
      if p >= pri then 
        match read_token lexer with
        | PARENCLOSE -> 
            restore_token lexer PARENCLOSE;
            (term, first_token)
        | tok -> 
            (match a with
             | Left -> 
                 let (e, t) = parse_expr_pri (p + 1) tok lexer in
                 parse_sub_expr (TApp (TId x, [term; e])) pri t lexer
             | Right -> 
                 let (e, t) = parse_expr_pri p tok lexer in
                 parse_sub_expr (TApp (TId x, [term; e])) pri t lexer
             | NonA -> 
                 let (e, t) = parse_expr_pri (p + 1) tok lexer in
                 if p > pri then
                   parse_sub_expr (TApp (TId x, [term; e])) pri t lexer
                 else
                   (TApp (TId x, [term; e]), t))
      else (term, first_token)
  | _ -> (term, first_token)

and parse_simple_expr_with_post_op first_tok lexer = 
  let (e, tok) = parse_simple_expr first_tok lexer in
  let rec iter e tok = match tok with
    | POSTOP x -> iter (TApp (TId x, [e])) (read_token lexer)
    | _ -> (e, tok)
  in iter e tok

and parse_simple_expr first_tok lexer = match first_tok with
  | UID x       -> (TSymbol x, read_token lexer)
  | LID x       -> (TId x,     read_token lexer)
  | UNDERSCORE  -> (TUnbound,  read_token lexer)
  | NUMBER x    -> (TNumber x, read_token lexer)
  | CHARACTER x -> (TChar x,   read_token lexer)
  | STRING str  -> (TList (List.map (fun c -> TChar c) str), read_token lexer)
  | PREOP x -> 
      (match read_token lexer with
       | PARENCLOSE -> (TId x, PARENCLOSE)
       | tok -> 
           let (e, t) = parse_simple_expr_with_post_op tok lexer in
           (TApp (TId x, [e]), t))
  | PARENOPEN -> 
      (match read_token lexer with
       | BINOP (x, p, _) -> 
           (match read_token lexer with
            | PARENCLOSE -> (TId x, read_token lexer)
            | tok -> 
                (match parse_expr_pri p tok lexer with
                 | (e, PARENCLOSE) -> 
                     let v = alloc_symbol () in
                     (TFun [([PId v], None, TApp (TId x, [TId v; e]))], read_token lexer)
                 | _ -> syntax_error lexer ") expected"))
       | tok -> 
           (match parse_expr_comma_list tok lexer with
            | ([e], PARENCLOSE) -> (e, read_token lexer)
            | (e, PARENCLOSE)   -> (TTuple e, read_token lexer)
            | ([e], BINOP (x, _, _)) -> 
                (match read_token lexer with
                 | PARENCLOSE -> (TApp (TId x, [e]), read_token lexer)
                 | _ -> syntax_error lexer ") expected")
            | (_, _) -> syntax_error lexer ", or ) expected"))
  | BRACKETOPEN -> 
      (match parse_expr_comma_list (read_token lexer) lexer with
       | (e, BRACKETCLOSE) -> (TList e, read_token lexer)
       | (e :: es, COLON) -> 
           (match parse_expr (read_token lexer) lexer with
            | (tl, BRACKETCLOSE) -> (TListTail (e :: es, tl), read_token lexer)
            | _ -> syntax_error lexer "] expected")
       | _ -> syntax_error lexer ", or ] expected")
  | BRACEOPEN -> 
      (match parse_function_body lexer with
       | (pats, BRACECLOSE) -> (TFun pats, read_token lexer)
       | _ -> syntax_error lexer "} expected")
  | BEGIN -> 
      (match parse_stmt_list_expr lexer with
       | (e, END) -> (e, read_token lexer)
       | _ -> syntax_error lexer "end expected")
  | DO -> let e = parse_do_expr lexer in (e, read_token lexer)
  | IF -> let e = parse_if_expr lexer in (e, read_token lexer)
  | MATCH -> 
      (match parse_expr (read_token lexer) lexer with
       | (e, WITH) -> 
           if read_token lexer = BRACEOPEN then
             match parse_match_body lexer with
             | (ps, BRACECLOSE) -> (TMatch (e, ps), read_token lexer)
             | _ -> syntax_error lexer "} expected"
           else syntax_error lexer "{ expected"
       | _ -> syntax_error lexer "with expected")
  | tok -> 
      syntax_error_uc lexer
        (Unicode.UString.append (Unicode.UString.uc_string_of_ascii "unexpected ") (token_to_string tok))

and parse_expr_comma_list first_token lexer = match first_token with
  | PARENCLOSE | BRACKETCLOSE | BRACECLOSE -> ([], first_token)
  | _ -> 
      let (e, tok) = parse_expr first_token lexer in
      match tok with
      | COMMA -> 
          let (es, t) = parse_expr_comma_list (read_token lexer) lexer in
          (e :: es, t)
      | _ -> ([e], tok)

and divide_symbol = string_to_symbol [|47|] (* / *)

and parse_simple_expr_list first_token lexer = 
  let (e, tok) = parse_simple_expr_with_post_op first_token lexer in
  match tok with
  | UID _ | LID _ | NUMBER _ | CHARACTER _ | STRING _ | PREOP _ | DO
  | PARENOPEN | BRACKETOPEN | BRACEOPEN -> 
      let (es, t) = parse_simple_expr_list tok lexer in
      (e :: es, t)
  | BINOP (x, p, _) when x = divide_symbol -> 
      (match e with
       | TNumber m -> 
           (match read_token lexer with
            | NUMBER n -> 
                (match read_token lexer with
                 | BINOP (_, q, _) as t -> 
                     if q > p then (restore_token lexer t; restore_token lexer (NUMBER n); ([e], tok))
                     else 
                       let (es, t_next) = parse_simple_expr_list tok lexer in
                       (TNumber (m // n) :: es, t_next)
                 | POSTOP y -> (restore_token lexer (POSTOP y); restore_token lexer (NUMBER n); ([e], tok))
                 | _ -> 
                     let (es, t_next) = parse_simple_expr_list tok lexer in
                     (TNumber (m // n) :: es, t_next))
            | t -> (restore_token lexer t; ([e], tok)))
       | _ -> ([e], tok))
  | _ -> ([e], tok)

and parse_pattern first_token lexer = 
  let p = parse_simple_pattern first_token lexer in
  match read_token lexer with
  | EQUAL-> 
      let p2 = parse_simple_pattern (read_token lexer) lexer in
      (match (p, p2) with
       | (PId x, _) -> (PAssign (x, p2), read_token lexer)
       | (_, PId x) -> (PAssign (x, p),  read_token lexer)
       | _ -> syntax_error lexer "= expects identifier")
  | tok -> (p, tok)

and parse_simple_pattern first_token lexer = match first_token with
  | UNDERSCORE  -> PAnything
  | LID x       -> PId x
  | NUMBER x    -> PNumber x
  | UID x       -> PSymbol x
  | CHARACTER x -> PChar x
  | STRING str  -> PList (List.map (fun c -> PChar c) str)
  | PARENOPEN -> 
      (match parse_pattern_comma_list lexer with
       | ([e], PARENCLOSE) -> e
       | (e, PARENCLOSE) -> PTuple e
       | _ -> syntax_error lexer ", or ) expected")
  | BRACKETOPEN -> 
      (match parse_pattern_comma_list lexer with
       | (e, BRACKETCLOSE) -> PList e
       | (e :: es, COLON) -> 
           (match parse_pattern (read_token lexer) lexer with
            | (tl, BRACKETCLOSE) -> PListTail (e :: es, tl)
            | _ -> syntax_error lexer "] expected")
       | _ -> syntax_error lexer ", or ] expected")
  | BRACEOPEN -> syntax_error lexer "6FIX"
  | tok -> 
      syntax_error_uc lexer
        (Unicode.UString.append (Unicode.UString.uc_string_of_ascii "unexpected ") (token_to_string tok))

(* ... and many other parsing functions ... *)
(* I will provide the full implementation here, ensuring OCaml 5 syntax *)

and parse_pattern_list first_token lexer = match first_token with
  | COLON_EQUAL| SEMICOLON | END | AMPERSAND -> ([], first_token)
  | _ -> 
      let (p, tok) = parse_pattern first_token lexer in
      match tok with
      | COMMA -> 
          let (ps, t) = parse_pattern_list (read_token lexer) lexer in
          (p :: ps, t)
      | _ -> ([p], tok)

and parse_pattern_comma_list lexer = 
  parse_pattern_list (read_token lexer) lexer

and parse_id_list lexer = 
  let rec iter first_token = match first_token with
    | LID x -> 
        (match read_token lexer with
         | COMMA -> 
             let (xs, tok) = iter (read_token lexer) in
             (x :: xs, tok)
         | tok -> ([x], tok))
    | tok -> ([], tok)
  in iter (read_token lexer)

and parse_match_body lexer = 
  let rec iter () = 
    let (p, tok) = parse_pattern (read_token lexer) lexer in
    let (g, tok2) = match tok with
      | AMPERSAND -> 
          let (g, t) = parse_expr (read_token lexer) lexer in
          (Some g, t)
      | _ -> (None, tok)
    in
    match tok2 with
    | COLON_EQUAL-> 
        let (e, tok3) = parse_stmt_list_expr lexer in
        (match tok3 with
         | BAR -> let (res, t) = iter () in ((p, g, e) :: res, t)
         | _   -> ([(p, g, e)], tok3))
    | _ -> syntax_error lexer ":= expected"
  in
  let (res, tok) = iter () in
  (res, tok)

and parse_function_body lexer = 
  let rec iter () = 
    let (ps, tok) = parse_pattern_list (read_token lexer) lexer in
    let (g, tok2) = match tok with
      | AMPERSAND -> 
          let (g, t) = parse_expr (read_token lexer) lexer in
          (Some g, t)
      | _ -> (None, tok)
    in
    match tok2 with
    | COLON_EQUAL-> 
        let (e, tok3) = parse_stmt_list_expr lexer in
        (match tok3 with
         | BAR -> let (res, t) = iter () in ((ps, g, e) :: res, t)
         | _   -> ([(ps, g, e)], tok3))
    | _ -> syntax_error lexer ":= expected"
  in
  let (res, tok) = iter () in
  (res, tok)

let parse_expression lexer =
  let (e, _) = parse_expr (read_token lexer) lexer in
  e
