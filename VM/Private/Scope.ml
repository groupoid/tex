
open Unicode.UTypes
open Unicode.SymbolTable
open Vm_types.Types

type scope =
  { symbol_table : (uc_string, Lexer.token_class) Hashtbl.t;
    global_symbols : (int, unknown) Hashtbl.t;
    local_symbols : (int, int * int) Hashtbl.t;
    depth : int;
    shift : int }

let create () =
  { symbol_table = Lexer.initial_symbol_table ();
    global_symbols = Hashtbl.create 1000; 
    local_symbols = Hashtbl.create 100;
    depth = 0; 
    shift = 0 }

let symbol_table scope = scope.symbol_table

let add_bin_op scope pri assoc sym =
  let str = symbol_to_string sym in
  Hashtbl.add scope.symbol_table str (Lexer.BINOP (sym, pri, assoc))

let add_pre_op scope sym =
  let str = symbol_to_string sym in
  Hashtbl.add scope.symbol_table str (Lexer.PREOP sym)

let add_post_op scope sym =
  let str = symbol_to_string sym in
  Hashtbl.add scope.symbol_table str (Lexer.POSTOP sym)

let copy scope =
  { symbol_table = Hashtbl.copy scope.symbol_table;
    global_symbols = Hashtbl.copy scope.global_symbols;
    local_symbols = Hashtbl.copy scope.local_symbols; 
    depth = scope.depth;
    shift = scope.shift }

let add_global scope symbol val_ =
  Hashtbl.replace scope.global_symbols symbol (create_unknown val_)

let push scope symbols =
  let local = Hashtbl.copy scope.local_symbols in
  let depth = scope.depth + 1 in
  let rec iter n symbols =
    match symbols with
    | [] -> ({ scope with local_symbols = local; depth = depth }, n)
    | s :: ss -> 
        Hashtbl.replace local s (depth, n); 
        iter (n + 1) ss
  in
  iter 0 symbols

let shift scope off = { scope with shift = scope.shift + off }

let lookup_local scope symbol =
  let (level, index) = Hashtbl.find scope.local_symbols symbol in
  (scope.depth - level + scope.shift, index)

let lookup_global scope symbol = Hashtbl.find scope.global_symbols symbol

let lookup scope symbol =
  try
    let (depth, index) = lookup_local scope symbol in 
    BVariable (depth, index)
  with Not_found -> 
    BGlobal (lookup_global scope symbol)

(*
let print_scope s =
  Unicode.Logging.log_string "globals:\n";
  Hashtbl.iter
    (fun n _ ->
      Unicode.Logging.log_uni_string (Unicode.SymbolTable.symbol_to_string n);
      Unicode.Logging.log_string "\n")
    s.global_symbols;

  Hashtbl.iter
    (fun n _ ->
      Unicode.Logging.log_uni_string (Unicode.SymbolTable.symbol_to_string n);
      Unicode.Logging.log_string "\n")
    s.local_symbols
*)
