
open Runtime
open Unicode
open Logging

(* counters *)

type counter = { mutable number : int; mutable reset : counter list }
let make value = {number = value; reset = []}

let rec set ctr value = 
  ctr.number <- value; 
  List.iter reset ctr.reset

and reset ctr = set ctr 0

let add_reset ctr sub_ctr =
  if List.memq sub_ctr ctr.reset then ()
  else ctr.reset <- sub_ctr :: ctr.reset

(* counter table *)

type counter_table = counter Unicode.DynUCTrie.t

let empty_table = Unicode.DynUCTrie.empty

let new_counter loc table name value super =
  if Unicode.DynUCTrie.mem_string name table then
    begin
      log_warn loc "counter \"";
      log_uc_string name;
      log_string "\" redefined!\n"
    end;
  let new_ctr = make value in
  let new_table = Unicode.DynUCTrie.add_string name new_ctr table in
  begin match super with
    | None -> ()
    | Some ctr ->
        try
          let c = Unicode.DynUCTrie.find_string ctr new_table in add_reset c new_ctr
        with Not_found ->
          log_warn loc "counter \"";
          log_uc_string name;
          log_string "\" undefined!\n"
  end;
  new_table

let get_counter loc table name =
  try 
    let c = Unicode.DynUCTrie.find_string name table in c.number 
  with Not_found ->
    log_warn loc "counter \"";
    log_uc_string name;
    log_string "\" undefined!\n";
    0

let set_counter loc table name value =
  try 
    let c = Unicode.DynUCTrie.find_string name table in 
    set c value;
    table
  with Not_found -> 
    new_counter loc table name value None

