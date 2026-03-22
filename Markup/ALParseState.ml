
open XNum
open Runtime
open Logging

open Encodings

open ParseState
open ALCoding
open ALDim
open ALEnvironment
open ALNodes
open ALGraphics

module UString     = Unicode.UString
module SymbolTable = Unicode.SymbolTable
module SymbolMap   = SymbolTable.SymbolMap

let tracing_al_commands = ref false

(* Opaque type for parse-states. *)

let apply_ps _ _ = Types.runtime_error "application of non-function"

let cmp_ps p1 p2 = p1 == p2

let (ps_wrapper, ps_unwrapper) = Opaque.declare_type "parse-state" apply_ps cmp_ps cmp_ps

let wrap_ps ps = Types.Opaque (ps_wrapper ps)

let unwrap_ps = Machine.evaluate_opaque "parse-state" ps_unwrapper;;

let decode_ps = decode_opaque "parse-state" ps_unwrapper;;

let execute_ps_command_unknown name f ps = begin
  try
    ignore
      (decode_ps name
        (Machine.evaluate_function f [ref (wrap_ps ps)]))
  with
  | Types.Syntax_error (loc, msg) -> log_warn loc (UString.to_string (Array.to_list msg))
  | Types.Runtime_error msg    -> log_warn (location ps) (UString.to_string (Array.to_list msg))
end

let execute_ps_command name stream ps = begin
  try
    ignore
      (decode_ps name
        (ref (Machine.evaluate_monad_expr ps.al_scope stream (wrap_ps ps))))
  with
  | Types.Syntax_error (loc, msg) -> log_warn loc (UString.to_string (Array.to_list msg))
  | Types.Runtime_error msg    -> log_warn (location ps) (UString.to_string (Array.to_list msg))
end

let dummy_parse_state = ParseState.create Job.empty;;

let ps_cmd name parse_state f = begin
  let ps = unwrap_ps name parse_state in

  if !tracing_al_commands then begin
    log_string "\n#AL: ";
    log_string name
  end
  else ();

  f ps;

  !parse_state
end

(* primitives *)

(* globals *)

let ps_get_global args = match args with
| [var; sym; parse_command] -> begin
    ps_cmd "ps_get_global" parse_command
      (fun ps ->
          let s = decode_symbol "ps_get_global" sym in

          try
            Machine.set_unknown var (SymbolTable.SymbolMap.find s ps.global_variables)
          with
          | Not_found -> ()
        )
  end
| _ -> assert false


let ps_set_global args = match args with
| [sym; val_; parse_command] -> begin
    ps_cmd "ps_set_global" parse_command
      (fun ps ->
          let s = decode_symbol "ps_set_global" sym in
          ps.global_variables <- SymbolTable.SymbolMap.add s !val_ ps.global_variables
      )
  end
| _ -> assert false


(* two helper functions *)

let set_num_global ps sym n = begin
  ps.global_variables <-
    SymbolTable.SymbolMap.add
      (SymbolTable.string_to_symbol sym)
      (Types.Number n)
      ps.global_variables
end

let set_string_global ps sym str = begin
  ps.global_variables <-
    SymbolTable.SymbolMap.add
      (SymbolTable.string_to_symbol sym)
      (Machine.uc_string_to_char_list str)
      ps.global_variables
end

(* stream commands *)

let ps_next_char c parse_command = begin
  ps_cmd "ps_next_char" parse_command
    (fun ps -> 
        Machine.set_unknown c (Types.Char (UCStream.next_char ps.input_stream))
      )
end

let ps_get_char args = match args with
| [c; pos; parse_command] -> begin
    ps_cmd "ps_get_char" parse_command
      (fun ps -> 
          let n = decode_int "ps_get_char" pos in

          Machine.set_unknown c (Types.Char (UCStream.get_char ps.input_stream n))
        )
  end
| _ -> assert false


let ps_remove_chars n parse_command = begin
  ps_cmd "ps_remove_chars" parse_command
    (fun ps -> 
        let k = decode_int "ps_remove_chars" n in

        UCStream.remove ps.input_stream k
      )
end

let ps_insert_string str parse_command = begin
  ps_cmd "ps_insert_string" parse_command
    (fun ps -> 
        let s  = Machine.decode_string "ps_insert_string" str in

        UCStream.insert_list ps.input_stream s
      )
end

let ps_location loc parse_command = begin
  ps_cmd "ps_location" parse_command
    (fun ps -> 
        Machine.set_unknown loc (encode_location (location ps))
      )
end

let ps_read_arg arg parse_command = begin
  ps_cmd "ps_read_arg" parse_command
    (fun ps -> 
        let str = Parser.read_argument ps.input_stream in

        Machine.set_unknown arg (Machine.uc_string_to_char_list (Array.of_list str))
      )
end

let ps_arg_expanded arg parse_command = begin
  ps_cmd "ps_arg_expanded" parse_command
    (fun ps -> 
        let str = ParseArgs.arg_expanded ps in

        Machine.set_unknown arg (Machine.uc_list_to_char_list str)
      )
end

let ps_arg_execute args = match args with
| [result; mode; parse_command] -> begin
    ps_cmd "ps_arg_execute" parse_command
      (fun ps -> 
          let m = decode_mode "ps_arg_execute" mode in
          let n = ParseArgs.arg_execute ps m in

          Machine.set_unknown result (encode_node_list n)
        )
  end
| _ -> assert false


let ps_arg_num arg parse_command = begin
  ps_cmd "ps_arg_num" parse_command
    (fun ps -> 
        let n  = ParseArgs.arg_num ps in

        Machine.set_unknown arg (Vm_types.Types.Number n)
      )
end

let ps_arg_int arg parse_command = begin
  ps_cmd "ps_arg_int" parse_command
    (fun ps -> 
        let n  = ParseArgs.arg_int ps in

        Machine.set_unknown arg (Types.Number (num_of_int n))
      )
end

let ps_arg_skip arg parse_command = begin
  ps_cmd "ps_arg_skip" parse_command
    (fun ps -> 
        let s = ParseArgs.arg_skip ps in

        Machine.set_unknown arg (encode_skip_arg s)
      )
end

let ps_arg_dim arg parse_command = begin
  ps_cmd "ps_arg_dim" parse_command
    (fun ps -> 
        let d = ParseArgs.arg_dim ps in

        Machine.set_unknown arg (encode_dim_arg d)
      )
end

let ps_arg_key_val arg parse_command = begin
  ps_cmd "ps_arg_key_val" parse_command
    (fun ps -> 
        let kv = ParseArgs.arg_key_val ps in

        let code v = match v with
                     | None   -> ref (Types.Symbol sym_None)
                     | Some x -> ref (Machine.uc_list_to_char_list x)
        in

        Machine.set_unknown arg
          (Types.Dictionary
            (DynUCTrie.fold
              (fun k v m -> SymbolMap.add (SymbolTable.string_to_symbol k) (code v) m)
              kv
              SymbolMap.empty))
      )
end

let ps_arg_dict args = match args with
| [arg; dict; parse_command] -> begin
    ps_cmd "ps_arg_dict" parse_command
      (fun ps -> 
          let d  = decode_dict "ps_arg_dict" dict in
          let kv = ParseArgs.arg_key_val ps in

          let key_map =
            SymbolMap.fold
              (fun sym key map ->
                DynUCTrie.add_list (Machine.decode_string "ps_arg_dict" key) sym map)
              d
              DynUCTrie.empty in

          let add_entry key v map = match DynUCTrie.lookup_string key key_map with
          | Some sym -> begin
              let v' = match v with
              | None   -> ref (Types.Symbol sym_None)
              | Some x -> ref (Machine.uc_list_to_char_list x)
              in
              SymbolMap.add sym v' map
            end
          | None -> begin
              log_warn (location ps)
                ("Unknown key `" ^ (UString.bytes_to_string (Array.to_list key)) ^ "'!");
              map
            end
          in

          Machine.set_unknown arg
            (Types.Dictionary
              (DynUCTrie.fold add_entry kv SymbolMap.empty))
        )
  end
| _ -> assert false


let ps_opt_expanded args = match args with
| [arg; default; parse_command] -> begin
    ps_cmd "ps_opt_expanded" parse_command
      (fun ps -> 
          let d  = Machine.decode_string "ps_opt_expanded" default in

          Machine.set_unknown arg (Machine.uc_list_to_char_list (ParseArgs.opt_expanded ps d))
        )
  end
| _ -> assert false


let ps_opt_key_val arg parse_command = begin
  ps_cmd "ps_opt_key_val" parse_command
    (fun ps -> 
        let kv = ParseArgs.opt_key_val ps in

        let code v = (match v with
                     | None   -> ref (Types.Symbol sym_None)
                     | Some x -> ref (Machine.uc_list_to_char_list x))
        in

        Machine.set_unknown arg
          (Types.Dictionary
            (DynUCTrie.fold
              (fun k v m -> SymbolMap.add (SymbolTable.string_to_symbol k) (code v) m)
              kv
              SymbolMap.empty))
      )
end

let ps_opt_int args = match args with
| [arg; default; parse_command] -> begin
    ps_cmd "ps_opt_int" parse_command
      (fun ps -> 
          let d = decode_int "ps_opt_int" default in

          Machine.set_unknown arg (Types.Number (num_of_int (ParseArgs.opt_int ps d)))
        )
  end
| _ -> assert false


let ps_arg_TeX_dim arg parse_command = begin
  ps_cmd "ps_arg_TeX_dim" parse_command
    (fun ps -> 
        let d = ParseArgs.arg_TeX_dim ps in

        Machine.set_unknown arg (encode_dim_arg d)
      )
end

(* modes and node-list *)

let ps_current_mode m parse_command = begin
  ps_cmd "ps_current_mode" parse_command
    (fun ps -> 
        Machine.set_unknown m (encode_mode (current_mode ps))
      )
end

let ps_open_node_list mode parse_command = begin
  ps_cmd "ps_open_node_list" parse_command
    (fun ps -> 
        open_node_list ps (decode_mode "ps_open_node_list" mode)
      )
end

let ps_close_node_list args = match args with
| [result; mode; parse_command] -> begin
    ps_cmd "ps_close_node_list" parse_command
      (fun ps -> 
          let nodes = close_node_list ps (decode_mode "ps_close_node_list" mode) in

          Machine.set_unknown result (encode_node_list nodes)
        )
  end
| _ -> assert false


let ps_add_node node parse_command = begin
  ps_cmd "ps_add_node" parse_command
    (fun ps -> 
        add_node ps (decode_node "ps_add_node" node)
      )
end

(* commands *)

let decode_command name execute expand = begin
  let exe ps = execute_ps_command_unknown name execute ps in
  let exp ps tok = try
      let result  = ref Types.Unbound in
      let command = Machine.evaluate_function
                      expand
                      [result;
                       ref ((Machine.uc_list_to_char_list tok));
                       ref (wrap_ps ps)] in

      let _ = decode_ps name command in

      Machine.decode_string name result
    with
    | Vm_types.Types.Syntax_error (loc, msg) -> begin
        log_warn loc (UString.to_string (Array.to_list msg));
        []
      end
    | Vm_types.Types.Runtime_error msg -> begin
        log_warn (location ps) (UString.to_string (Array.to_list msg));
        []
      end
    in

  { execute = exe; expand = exp }
end

let decode_unexpandable_command name execute = begin
  { execute = execute_ps_command_unknown name execute;
    expand  = Macro.noexpand }
end

let ps_set_default_char_cmd args = match args with
| [execute; expand; parse_command] -> begin
    ps_cmd "ps_set_default_char_cmd" parse_command
      (fun ps -> match !expand with
        | Types.Symbol s -> begin
            if s = sym_None then
              set_default_char_cmd ps
                (decode_unexpandable_command "default_char_cmd" execute)
            else
              log_warn (location ps) "ps_set_default_char_cmd: None or a function expected"
          end
        | _ -> begin
            set_default_char_cmd ps
              (decode_command "default_char_cmd" execute expand)
          end
        )
  end
| _ -> assert false

let ps_define_command args = match args with
| [name; execute; expand; parse_command] -> begin
    ps_cmd "ps_define_command" parse_command
      (fun ps -> 
          let name_uc  = Machine.decode_string "ps_define_command" name in
          let name_str = UString.to_string name_uc in

          match !expand with
          | Types.Symbol s -> begin
              if s = sym_None then
                define_command ps name_uc
                  (decode_unexpandable_command name_str execute)
              else
                log_warn (location ps) "ps_define_command: None or a function expected"
            end
          | _ -> begin
              define_command ps name_uc
                (decode_command name_str execute expand)
            end
        )
  end
| _ -> assert false


let ps_define_pattern args = match args with
| [name; execute; expand; parse_command] -> begin
    ps_cmd "ps_define_pattern" parse_command
      (fun ps -> 
          let name_uc  = Machine.decode_string "ps_define_pattern" name in
          let name_str = UString.to_string name_uc in

          match !expand with
          | Types.Symbol s -> begin
              if s = sym_None then
                define_pattern ps name_uc
                  (decode_unexpandable_command name_str execute)
              else
                log_warn (location ps) "ps_define_pattern: None or a function expected"
            end
          | _ -> begin
              define_pattern ps name_uc
                (decode_command name_str execute expand)
            end
        )
  end
| _ -> assert false


let decode_arg_templ name args = begin
  let rec iter args = match args with
  | []      -> []
  | a::aa -> match !a with
    | Types.Symbol s -> begin
        if s = sym_Mandantory then
          Macro.Arg :: iter aa
        else if s = sym_Optional then
          Macro.Opt (UString.of_ascii "\\NoValue") :: iter aa
        else if s = sym_Bool then
          Macro.Bool :: iter aa
        else
          Types.runtime_error ("unknown argument specifier " ^ UString.to_string (Array.to_list (SymbolTable.symbol_to_string s)))
      end
    | Types.Tuple x -> begin match x with
      | [| y; z |] -> begin match !y with
          | Types.Symbol s -> begin
              if s = sym_Optional then
                Macro.Opt (Machine.decode_string name z) :: iter aa
              else
                Types.runtime_error (name ^ ": unknown argument specifier "
                                          ^ UString.to_string (Array.to_list (SymbolTable.symbol_to_string s)))
            end
          | _ -> Types.runtime_error (name ^ ": symbol expected but got " ^ Types.type_name !y)
        end
      | _ -> Types.runtime_error (name ^ ": array of 2 elements expected")
      end
    | _ -> Types.runtime_error (name ^ ": tuple expected")
  in iter (Machine.decode_list name args)
end

let ps_define_macro args = match args with
| [name; arg_template; body; parse_command] -> begin
    ps_cmd "ps_define_macro" parse_command
      (fun ps -> 
          let name = Machine.decode_string "ps_define_macro" name in
          let body = Machine.decode_string "ps_define_macro" body in
          let args = decode_arg_templ "ps_define_macro" arg_template in

          define_command ps name
            { execute = Macro.execute_macro args body;
              expand  = Macro.expand_macro  args body }
            )
  end
| _ -> assert false


let ps_save_command name parse_command = begin
  ps_cmd "ps_save_command" parse_command
    (fun ps -> 
        let n = Machine.decode_string "ps_save_command" name in

        save_command ps n
      )
end

let ps_restore_command name parse_command = begin
  ps_cmd "ps_restore_command" parse_command
    (fun ps -> 
        let n = Machine.decode_string "ps_restore_command" name in

        restore_command ps n
      )
end

let ps_save_pattern name parse_command = begin
  ps_cmd "ps_save_pattern" parse_command
    (fun ps -> 
        let n = Machine.decode_string "ps_save_pattern" name in

        save_pattern ps n
      )
end

let ps_restore_pattern name parse_command = begin
  ps_cmd "ps_restore_pattern" parse_command
    (fun ps -> 
        let n = Machine.decode_string "ps_restore_pattern" name in

        restore_pattern ps n
      )
end

let encode_command name command = begin
  let execute parse_command = begin
      command.execute (decode_ps name parse_command);

      !parse_command
    end in
  let expand args = match args with
  | [result; tok; parse_command] -> begin
      let ps = decode_ps name parse_command in

      let t = Machine.decode_string name tok in

      Machine.set_unknown result (Machine.uc_list_to_char_list (command.expand ps t));

      !parse_command
    end
  | _ -> assert false
  in

  Types.Tuple
    [|ref (Types.Primitive1 execute);
      ref (Types.PrimitiveN (3, expand))|]
end

let ps_lookup_command args = match args with
| [command; name; parse_command] -> begin
    ps_cmd "ps_lookup_command" parse_command
      (fun ps -> 
          let name_uc  = Machine.decode_string "ps_lookup_command" name in
          let name_str = UString.to_string name_uc in

          match lookup_command ps name_uc with
          | None   -> Machine.set_unknown command (Types.Symbol sym_None)
          | Some c -> Machine.set_unknown command (encode_command name_str c)
        )
  end
| _ -> assert false


let ps_push_env args = match args with
| [name; arg; parse_command] -> begin
    ps_cmd "ps_push_env" parse_command
      (fun ps -> 
          let n    = Machine.decode_string "ps_push_env" name in
          let args = List.map
                       (Machine.decode_string "ps_push_env")
                       (Machine.decode_list "ps_push_env" arg) in

          push_env ps n args
        )
  end
| _ -> assert false


let ps_pop_env args = match args with
| [name; arg; parse_command] -> begin
    ps_cmd "ps_pop_env" parse_command
      (fun ps -> 
          let (n,args) = pop_env ps in

          Machine.set_unknown name (Machine.uc_list_to_char_list n);
          Machine.set_unknown arg
            (List.fold_right
              (fun a l -> Types.List
                            (ref (Machine.uc_list_to_char_list a),
                             ref l))
              args
              Types.Nil)
        )
  end
| _ -> assert false


let ps_set_env_args args parse_command = begin
  ps_cmd "ps_set_env_args" parse_command
    (fun ps -> 
        let args = List.map
                     (Machine.decode_string "ps_set_env_args")
                     (Machine.decode_list "ps_set_env_args" args) in

        set_env_args ps args
      )
end

let ps_top_env args = match args with
| [name; arg; parse_command] -> begin
    ps_cmd "ps_top_env" parse_command
    (fun ps -> 
        let (n,args) = top_env ps in

        Machine.set_unknown name (Machine.uc_list_to_char_list n);
        Machine.set_unknown arg
          (List.fold_right
            (fun a l -> Types.List
                          (ref (Machine.uc_list_to_char_list a),
                           ref l))
            args
            Types.Nil)
      )
  end
| _ -> assert false


let ps_lookup_env args = match args with
| [commands; name; parse_command] -> begin
    ps_cmd "ps_lookup_env" parse_command
      (fun ps ->
          let name_uc  = Machine.decode_string "ps_lookup_env" name in
          let name_str = UString.to_string name_uc in

          match lookup_env ps name_uc with
          | None       -> Machine.set_unknown commands (Types.Symbol sym_None)
          | Some (b,e) -> Machine.set_unknown commands
                            (Types.Tuple [|ref (encode_command name_str b);
                                           ref (encode_command name_str e)|])
        )
  end
| _ -> assert false


let ps_define_env args = match args with
| [name; execute_begin; expand_begin; execute_end; expand_end; parse_command] -> begin
    ps_cmd "ps_define_env" parse_command
      (fun ps -> begin
          let name_uc  = Machine.decode_string "ps_define_env" name in
          let name_str = UString.to_string name_uc in

          match (!expand_begin, !expand_end) with
          | (Types.Symbol s1, Types.Symbol s2) -> begin
              if s1 = sym_None && s2 = sym_None then
                define_env ps name_uc
                  (decode_unexpandable_command name_str execute_begin)
                  (decode_unexpandable_command name_str execute_end)
              else
                log_warn (location ps) "ps_define_env: None or a function expected"
            end
          | (Types.Symbol s, _) -> begin
              if s = sym_None then
                define_env ps name_uc
                  (decode_unexpandable_command name_str execute_begin)
                  (decode_command              name_str execute_end expand_end)
              else
                log_warn (location ps) "ps_define_env: None or a function expected"
            end
          | (_, Types.Symbol s) -> begin
              if s = sym_None then
                define_env ps name_uc
                  (decode_command              name_str execute_begin expand_begin)
                  (decode_unexpandable_command name_str execute_end)
              else
                log_warn (location ps) "ps_define_env: None or a function expected"
            end
          | _ -> begin
              define_env ps name_uc
                (decode_command name_str execute_begin expand_begin)
                (decode_command name_str execute_end   expand_end)
            end
        end)
  end
| _ -> assert false


(* page layout *)

let ps_shipout_pages args = match args with
| [number; even; odd; parse_command] -> begin
    ps_cmd "ps_shipout_pages" parse_command
      (fun ps -> begin
          let n        = decode_int "ps_shipout_pages" number in
          let even_str = Array.of_list (Machine.decode_string "ps_shipout_pages" even) in
          let odd_str  = Array.of_list (Machine.decode_string "ps_shipout_pages" odd) in

          add_node ps (Node.ShipOut (location ps, even_str, odd_str, max 0 n))
        end)
  end
| _ -> assert false


let ps_new_page_layout args = match args with
| [name; width; height; parse_command] -> begin
    ps_cmd "ps_new_page_layout" parse_command
      (fun ps -> begin
          let name_str = Array.of_list (Machine.decode_string "ps_new_page_layout" name) in
          let w        = Machine.decode_num "ps_new_page_layout" width in
          let h        = Machine.decode_num "ps_new_page_layout" height in

          add_node ps
            (Node.NewLayout (location ps,
               name_str,
               (fun _ -> w),
               (fun _ -> h)))
        end)
  end
| _ -> assert false


let encode_page_info pi = begin
  Types.Dictionary
    (SymbolMap.add
      sym_Width    (ref (Types.Number pi.Box.pi_width))
    (SymbolMap.add
      sym_Height   (ref (Types.Number pi.Box.pi_height))
    (SymbolMap.add
      sym_PageNo   (ref (Types.Number (num_of_int pi.Box.pi_page_no)))
    (SymbolMap.add
      sym_OldMarks (ref (List.fold_right
                          (fun (a,b) c ->
                              Types.List
                                (ref (Types.Tuple
                                       [|ref (Machine.uc_string_to_char_list a);
                                         ref (Machine.uc_string_to_char_list b)|]),
                                ref c))
                          []
                          Types.Nil))
    (SymbolMap.add
      sym_NewMarks (ref (List.fold_right
                          (fun (a,b) c ->
                              Types.List
                                (ref (Types.Tuple
                                       [|ref (Machine.uc_string_to_char_list a);
                                         ref (Machine.uc_string_to_char_list b)|]),
                                ref c))
                          []
                          Types.Nil))
    SymbolMap.empty)))))
end

let ps_new_area args = match args with
| [name; pos_x; pos_y; width; height; max_top; max_bot; area_type; param; parse_command] -> begin
    ps_cmd "ps_new_area" parse_command
      (fun ps -> begin
        let name_str = Array.of_list (Machine.decode_string "ps_new_area" name) in
        let x        = Machine.decode_num "ps_new_area" pos_x in
        let y        = Machine.decode_num "ps_new_area" pos_y in
        let w        = Machine.decode_num "ps_new_area" width in
        let h        = Machine.decode_num "ps_new_area" height in
        let t        = Machine.decode_num "ps_new_area" max_top in
        let b        = Machine.decode_num "ps_new_area" max_bot in
        let at       = decode_symbol      "ps_new_area" area_type in

        if at = sym_Galley then begin
          let ap = decode_dict "ps_new_area" param in

          add_node ps
            (Node.NewArea (location ps,
               name_str, (fun _ -> x), (fun _ -> y), (fun _ -> w), (fun _ -> h), (fun _ -> t), (fun _ -> b),
               (`Galley
                 (Option.from_option [||]
                    (lookup_string  "ps_new_area" ap sym_Name),
                  Option.from_option (Evaluate.const_em num_one)
                     (lookup_skip   "ps_new_area" ap sym_TopSkip),
                  Option.from_option (Evaluate.const_em num_one)
                     (lookup_skip   "ps_new_area" ap sym_BottomSkip),
                  Option.from_option (Evaluate.const_em (num_of_int 5))
                     (lookup_skip   "ps_new_area" ap sym_MinSize),
                  Option.from_option (Evaluate.const_em num_one)
                     (lookup_skip   "ps_new_area" ap sym_GridSize)))))
        end
        else if at = sym_Float then begin
          let ap    = decode_dict "ps_new_area" param in
          let align = match lookup_symbol "ps_new_area" ap sym_Alignment with
            | None   -> FloatVertical.Top
            | Some s -> if s = sym_Bottom then
                          FloatVertical.Bottom
                        else
                          FloatVertical.Top
            in

          add_node ps
            (Node.NewArea (location ps,
               name_str, (fun _ -> x), (fun _ -> y), (fun _ -> w), (fun _ -> h), (fun _ -> t), (fun _ -> b),
               (`Float
                 (align,
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip    "ps_new_area" ap sym_TopSkip),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip    "ps_new_area" ap sym_BottomSkip),
                  Option.from_option (Evaluate.const_fixed_dim (Evaluate.const_em num_one))
                    (lookup_dim     "ps_new_area" ap sym_FloatSep)))))
        end
        else if at = sym_Footnote then begin
          let lookup_dict dict key = try
            SymbolMap.find key dict
          with
          | Not_found -> ref (Types.Dictionary SymbolMap.empty)
          in

          let ap                = decode_dict "ps_new_area" param in
          let line_params       = lookup_dict ap sym_LineParams in
          let par_params        = lookup_dict ap sym_ParParams in
          let line_break_params = lookup_dict ap sym_LineBreakParams in
          let hyphen_params     = lookup_dict ap sym_HyphenParams in
          let space_params      = lookup_dict ap sym_SpaceParams in
          let math_params       = lookup_dict ap sym_MathParams in

          add_node ps
            (Node.NewArea (location ps,
               name_str, (fun _ -> x), (fun _ -> y), (fun _ -> w), (fun _ -> h), (fun _ -> t), (fun _ -> b),
               (`Footnote
                 (Option.from_option []
                    (lookup (decode_node_list "ps_new_area") ap sym_Separator),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip "ps_new_area" ap sym_TopSkip),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip "ps_new_area" ap sym_BottomSkip),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip "ps_new_area" ap sym_GridSize),
                  decode_line_params "ps_new_area" line_params,
                  decode_par_params "ps_new_area" par_params,
                  decode_line_break_params "ps_new_area" line_break_params,
                  decode_hyphen_params "ps_new_area" hyphen_params,
                  decode_space_params "ps_new_area" space_params,
                  decode_math_params "ps_new_area" math_params))))
        end
        else if at = sym_Direct then begin match !param with
        | Types.Nil
        | Types.List (_, _) -> begin
            (* <param> is a string with ant code. *)

            let code       = Machine.decode_string "ps_new_area" param in
            let current_ps = duplicate ps in
            let stream     = UCStream.of_list code in

            let f pi _ = begin
              UCStream.assign current_ps.input_stream stream;

              set_counter current_ps (UString.uc_string_of_ascii "page") pi.Box.pi_page_no;

              List.iter
                (fun (m,v) -> begin
                    set_string_global current_ps
                      (Array.of_list (UString.of_ascii "OldMark" @ Array.to_list m))
                      v;
                    set_string_global current_ps
                      (Array.of_list (UString.of_ascii "NewMark" @ Array.to_list m))
                      v
                  end)
                [];
              List.iter
                (fun (m,v) ->
                    set_string_global current_ps
                      (Array.of_list (UString.of_ascii "NewMark" @ Array.to_list m))
                      v
                )
                [];

              run_parser current_ps `VBox;
            end in

            add_node ps
              (Node.NewArea ((location ps),
                 name_str, (fun _ -> x), (fun _ -> y), (fun _ -> w), (fun _ -> h), (fun _ -> t), (fun _ -> b),
                 (`Direct f)))
          end
        | _ -> begin
            (* <param> is a function that returns a node list. *)

            let f pi (x,y) = begin
              decode_node_list
                "<anonymous>"
                (Machine.evaluate_function
                  param
                  [ref (encode_page_info pi);
                   ref (Types.Tuple [|ref (Types.Number x); ref (Types.Number y)|])])
            end in

            add_node ps
              (Node.NewArea ((location ps),
                 name_str, (fun _ -> x), (fun _ -> y), (fun _ -> w), (fun _ -> h), (fun _ -> t), (fun _ -> b),
                 (`Direct f)))
          end
        end
        else begin
          log_warn (location ps) "unknown area type ";
          log_uc_string (SymbolTable.symbol_to_string at);
          log_string "!\n"
        end
        end)
  end
| _ -> assert false


let ps_new_galley args = match args with
| [name; measure; parse_command] -> begin
    ps_cmd "ps_new_galley" parse_command
      (fun ps -> 
          let name_str = Array.of_list (Machine.decode_string "ps_new_galley" name) in
          let m        = Machine.decode_num "ps_new_galley" measure in

          add_node ps
            (Node.NewGalley ((location ps), name_str, (fun _ -> m)))
        )
  end
| _ -> assert false


(* fonts *)

let decode_glyph_spec name g = match !g with
| Types.Number _ -> `GlyphIndex (decode_int name g)
| Types.Char c   -> `Char c
| Types.List (_, _) -> `GlyphName (UString.to_string (Array.to_list (decode_uc_string name g)))
| _              -> Types.runtime_error (name ^ ": invalid glyph specification")

let decode_extra_kern name k = begin
  let x = decode_tuple name k in
  let n = Array.length x in

  if n < 2 || n > 7 then
    Types.runtime_error (name ^ ": invalid border kern data")
  else
    (decode_glyph_spec name x.(0),
     {
       GlyphMetric.ki_after_margin   = if n < 2 then num_zero else Machine.decode_num name x.(1);
       GlyphMetric.ki_before_margin  = if n < 3 then num_zero else Machine.decode_num name x.(2);
       GlyphMetric.ki_after_foreign  = if n < 4 then num_zero else Machine.decode_num name x.(3);
       GlyphMetric.ki_before_foreign = if n < 5 then num_zero else Machine.decode_num name x.(4);
       GlyphMetric.ki_after_space    = if n < 6 then num_zero else Machine.decode_num name x.(5);
       GlyphMetric.ki_before_space   = if n < 7 then num_zero else Machine.decode_num name x.(6)
     })
end

let decode_font_load_params name params = begin
  let get_glyph g = if g < 0 then `Undef else `GlyphIndex g in

  let p = decode_dict name params in

  let encoding      = Array.map
                        (decode_uc_string name)
                        (Option.from_option [||]
                          (lookup_tuple name p sym_Encoding)) in
  let hyphen        = Option.from_option (-1)
                       (lookup_int name p sym_HyphenGlyph) in
  let skew          = Option.from_option (-1)
                       (lookup_int name p sym_SkewGlyph) in
  let scale         = Option.from_option num_one
                       (lookup_num name p sym_Scale) in
  let letterspacing = Option.from_option num_zero
                       (lookup_num name p sym_LetterSpacing) in
  let adjustments   = Option.from_option []
                       (lookup_list name p sym_Adjustments) in
  let auto_lig      = Option.from_option false
                       (lookup_bool name p sym_AutoLigatures) in
  let extra_kern    = List.map
                        (decode_extra_kern name)
                        (Option.from_option []
                          (lookup_list name p sym_BorderKern)) in

  let ligs = if auto_lig then begin
      let rec lookup_glyph i char = begin
        if i >= Array.length encoding then
          -1
        else if encoding.(i) = [|char|] then
          i
        else
          lookup_glyph (i+1) char
      end in

      let rec iter i l = begin
        if i >= Array.length encoding then
          l
        else begin
          let n = Array.length encoding.(i) in

          if n > 1 then
            iter (i+1) (GlyphSpecTrie.add_list
                         (Array.to_list (Array.map (lookup_glyph 0) encoding.(i)))
                         i
                         l)
          else
            iter (i+1) l
        end
      end in
      iter 0 GlyphSpecTrie.empty
    end
    else
      GlyphSpecTrie.empty in

  let rec iter extra_pos extra_subst adjustments = begin match adjustments with
  | [] -> {
            FontMetric.flp_encoding       = encoding;
            FontMetric.flp_letter_spacing = letterspacing;
            FontMetric.flp_size           = scale;
            FontMetric.flp_hyphen_glyph   = get_glyph hyphen;
            FontMetric.flp_skew_glyph     = get_glyph skew;
            FontMetric.flp_extra_kern     = extra_kern;
            FontMetric.flp_extra_pos      = extra_pos;
            FontMetric.flp_extra_subst    = extra_subst;
            FontMetric.flp_magnification  = XNum.num_one
          }
  | (a::adjs) -> begin match decode_tuple name a with
    | [| glyphs; sym; vl |] -> begin
        let gs = List.map
                   (decode_glyph_spec name)
                   (Machine.decode_list name glyphs) in
        let s  = decode_symbol name sym in

        if s = sym_Kern then begin
          let _v = Machine.decode_num name vl in
          let gs_ints = List.map (function | `GlyphIndex i -> i | `Char c -> c | _ -> -1) gs in
          iter
            (Encodings.GlyphSpecTrie.add_list gs_ints 0 extra_pos)
            extra_subst
            adjs
(*          let c = Substitute.simple_pair_kerning_cmd v;

          iter (DynUCTrie.add_list gs (c, 1) extra_pos) extra_subst adjs *)
        end
        else if s = sym_Ligature then begin
          let v_int = match decode_glyph_spec name vl with
            | `GlyphIndex i -> i | `Char c -> c | _ -> -1 in
          let gs_ints = List.map (function | `GlyphIndex i -> i | `Char c -> c | _ -> -1) gs in
          iter
            extra_pos
            (Encodings.GlyphSpecTrie.add_list gs_ints v_int extra_subst)
            adjs
(*          let c = Substitute.replace_with_single_glyph_cmd
                    2 (Substitute.Simple v);

          iter extra_pos (DynUCTrie.add_list gs (c, 0) extra_subst) adjs*)
        end
        else
          Types.runtime_error (name ^ ": unknown adjustment command, Kern or Ligature expected")
      end
    | _ -> Types.runtime_error (name ^ ": triple expected")
    end
  end in
  iter Encodings.GlyphSpecTrie.empty ligs adjustments
end

let ps_declare_font args = match args with
| [name; family; series; shape; sizes; params; parse_command] -> begin
    ps_cmd "ps_declare_font" parse_command
      (fun ps -> begin
          let n       = decode_uc_string "ps_declare_font" name in
          let fam     = decode_uc_string "ps_declare_font" family in
          let ser     = decode_uc_string "ps_declare_font" series in
          let sha     = decode_uc_string "ps_declare_font" shape in
          let (s1,s2) = begin match decode_tuple "ps_declare_font" sizes with
            | [| s1; s2 |] -> (s1,s2)
            | _ -> Types.runtime_error "ps_declare_font: pair expected"
            end in
          let flp = decode_font_load_params "ps_declare_font" params in

          add_node ps
            (Node.Command (location ps,
              Environment.declare_font
                n fam ser sha
                (Machine.decode_num "ps_declare_font" s1,
                 Machine.decode_num "ps_declare_font" s2)
                flp))
        end)
  end
| _ -> assert false


let ps_define_math_symbol args = match args with
| [name; math_code; font; glyph; parse_command] -> begin
    ps_cmd "ps_define_math_symbol" parse_command
      (fun ps -> begin
          let name = Machine.decode_string "ps_define_math_symbol" name in
          let mc   = decode_math_code      "ps_define_math_symbol" math_code in
          let f    = decode_int            "ps_define_math_symbol" font in
          let g    = decode_int            "ps_define_math_symbol" glyph in

          define_command ps name
            { execute = (fun ps -> add_node ps
                                                (Node.MathChar (location ps, (mc, (f, f), (g, g)))));
              expand  = Macro.noexpand }
        end)
  end
| _ -> assert false


let ps_define_root_symbol args = match args with
| [name; small_font; small_glyph; large_font; large_glyph; parse_command] -> begin
    ps_cmd "ps_define_root_symbol" parse_command
      (fun ps -> begin
          let name = Machine.decode_string "ps_define_root_symbol" name in
          let sf   = decode_int            "ps_define_root_symbol" small_font in
          let sg   = decode_int            "ps_define_root_symbol" small_glyph in
          let lf   = decode_int            "ps_define_root_symbol" large_font in
          let lg   = decode_int            "ps_define_root_symbol" large_glyph in

          define_command ps name
            { execute = (fun ps -> add_node ps
                                                (Node.Root (location ps, sf, sg, lf, lg, (ParseArgs.arg_execute ps `Math))));
              expand  = Macro.noexpand }
        end)
  end
| _ -> assert false


let ps_define_math_accent args = match args with
| [name; font; glyph; parse_command] -> begin
    ps_cmd "ps_define_math_accent" parse_command
      (fun ps -> begin
          let name = Machine.decode_string "ps_define_math_accent" name in
          let f    = decode_int            "ps_define_math_accent" font in
          let g    = decode_int            "ps_define_math_accent" glyph in

          define_command ps name
            { execute = (fun ps -> add_node ps
                                     (Node.MathAccent (location ps, f, g, (ParseArgs.arg_execute ps `Math))));
              expand  = Macro.noexpand }
        end)
  end
| _ -> assert false


let ps_set_math_code args = match args with
| [char; math_code; font1; glyph1; font2; glyph2; parse_command] -> begin
    ps_cmd "ps_set_math_code" parse_command
      (fun ps -> begin
          let c  = decode_char      "ps_set_math_code" char in
          let mc = decode_math_code "ps_set_math_code" math_code in
          let f1 = decode_int       "ps_set_math_code" font1 in
          let g1 = decode_int       "ps_set_math_code" glyph1 in
          let f2 = decode_int       "ps_set_math_code" font2 in
          let g2 = decode_int       "ps_set_math_code" glyph2 in

          set_math_code ps c mc f1 g1 f2 g2
        end)
  end
| _ -> assert false


(* boxes *)

(* graphics *)

let decode_coord name z = begin match !z with
| Types.Tuple [|x;y|] -> (Machine.decode_num name x,
                          Machine.decode_num name y)
| _                   -> Types.runtime_error (name ^ ": pair expected but got " ^ Types.type_name !z)
end

let decode_bezier name z = begin match !z with
| Types.Tuple [|a;b;c;d|] -> begin
    let (ax,ay) = decode_coord name a in
    let (bx,by) = decode_coord name b in
    let (cx,cy) = decode_coord name c in
    let (dx,dy) = decode_coord name d in

    ((fun _ -> Dim.fixed_dim ax), (fun _ -> Dim.fixed_dim ay),
     (fun _ -> Dim.fixed_dim bx), (fun _ -> Dim.fixed_dim by),
     (fun _ -> Dim.fixed_dim cx), (fun _ -> Dim.fixed_dim cy),
     (fun _ -> Dim.fixed_dim dx), (fun _ -> Dim.fixed_dim dy))
  end
| _ -> Types.runtime_error (name ^ ": 4-tuple expected but got " ^ Types.type_name !z)
end

let decode_path name p = begin
  List.map (decode_bezier name)
    (Machine.decode_list name p)
end

let ps_set_colour colour parse_command = begin
  ps_cmd "ps_set_colour" parse_command
    (fun ps -> begin
        let c = decode_colour "ps_set_colour" colour in

        add_node ps
          (Node.GfxCommand (location ps, Graphic.SetColour c))
      end)
end

let ps_set_bg_colour colour parse_command = begin
  ps_cmd "ps_set_bg_colour" parse_command
    (fun ps -> 
        let c = decode_colour "ps_set_bg_colour" colour in

        add_node ps
          (Node.GfxCommand (location ps, Graphic.SetBgColour c))
      )
end

let ps_set_alpha alpha parse_command = begin
  ps_cmd "ps_set_alpha" parse_command
    (fun ps -> 
        let a = Machine.decode_num "ps_set_alpha" alpha in

        add_node ps
          (Node.GfxCommand (location ps, Graphic.SetAlpha a))
      )
end

let ps_draw name mode path parse_command = begin
  ps_cmd "ps_draw" parse_command
    (fun ps -> 
        let p = decode_path name path in

        add_node ps
          (Node.GfxCommand (location ps, Graphic.Draw (mode, p)))
      )
end

let ps_set_line_width width parse_command = begin
  ps_cmd "ps_set_line_width" parse_command
    (fun ps -> 
        let w = Machine.decode_num "ps_set_line_width" width in

        add_node ps
          (Node.GfxCommand (location ps, Graphic.SetLineWidth (fun _ -> Dim.fixed_dim w)))
      )
end

let ps_set_line_cap cap parse_command = begin
  ps_cmd "ps_set_line_cap" parse_command
    (fun ps -> 
        let c = decode_line_cap "ps_set_line_cap" cap in

        add_node ps
          (Node.GfxCommand (location ps, Graphic.SetLineCap c))
      )
end

let ps_set_line_join join parse_command = begin
  ps_cmd "ps_set_line_join" parse_command
    (fun ps -> 
        let j = decode_line_join "ps_set_line_join" join in

        add_node ps
          (Node.GfxCommand (location ps, Graphic.SetLineJoin j))
      )
end

let ps_set_miter_limit limit parse_command = begin
  ps_cmd "ps_set_miter_limit" parse_command
    (fun ps -> begin
        let l = Machine.decode_num "ps_set_miter_limit" limit in

        add_node ps
          (Node.GfxCommand (location ps, Graphic.SetMiterLimit l))
      end)
end

(* page commands *)

let ps_page_command cmd parse_command = begin
  ps_cmd "ps_page_command" parse_command
    (fun ps -> begin
        let f pi (x,y) = begin
          execute_ps_command_unknown "ps_page_command"
            (ref (Types.Application (!cmd, 1,
                  [ref (encode_page_info pi);
                   ref (Types.Tuple [|ref (Types.Number x);
                                      ref (Types.Number y)|])])))
            ps
        end in

        add_node ps
          (Node.CommandBox (location ps, `PageCmd (Box.CallPageFunction f)))
      end)
end

let ps_par_command cmd parse_command = begin
  ps_cmd "ps_par_command" parse_command
    (fun ps -> begin
        let f line = begin
          execute_ps_command_unknown "ps_par_command"
            (ref (Types.Application (!cmd, 1, [ref (Types.Number (num_of_int line))])))
            ps
        end in

        add_node ps
          (Node.CommandBox (location ps, `ParCmd (Box.CallParFunction f)))
      end)
end

let ps_dvi_special special parse_command = begin
  ps_cmd "ps_dvi_special" parse_command
    (fun ps -> begin
        let s = Machine.decode_string "ps_dvi_special" special in

        add_node ps
          (Node.CommandBox (location ps,
            `Special (`DVI_Special (UString.to_string s))))
      end)
end

(* counters *)

let ps_new_counter args = match args with
| [name; vl; super; parse_command] -> begin
    ps_cmd "ps_new_counter" parse_command
      (fun ps -> begin
          let n = decode_uc_string "ps_new_counter" name in
          let v = decode_int       "ps_new_counter" vl in
          let s = decode_option    "ps_new_counter" decode_uc_string super in

          new_counter ps n v s
        end)
  end
| _ -> assert false


let ps_get_counter args = match args with
| [vl; name; parse_command] -> begin
    ps_cmd "ps_get_counter" parse_command
      (fun ps -> begin
          let n = decode_uc_string "ps_get_counter" name in

          Machine.set_unknown vl (Types.Number (num_of_int (get_counter ps n)))
        end)
  end
| _ -> assert false


let ps_set_counter args = match args with
| [name; vl; parse_command] -> begin
    ps_cmd "ps_set_counter" parse_command
      (fun ps -> begin
          let n = decode_uc_string "ps_set_counter" name in
          let v = decode_int       "ps_set_counter" vl in

          set_counter ps n v
        end)
  end
| _ -> assert false


(* misc *)

let ps_warning msg parse_command = begin
  ps_cmd "ps_warning" parse_command
    (fun ps -> begin
        let s = Machine.decode_string "ps_warning" msg in

        log_warn (location ps) (UString.to_string s)
      end)
end

let ps_error msg parse_command = begin
  ps_cmd "ps_error" parse_command
    (fun ps -> begin
        let s = Machine.decode_string "ps_error" msg in

        log_error (location ps) (UString.to_string s)
      end)
end


(* running the parser *)

let ps_execute_next_char finished parse_command = begin
  ps_cmd "ps_execute_next_char" parse_command
    (fun ps -> 
        Machine.set_unknown finished (Types.Bool (execute_next_char ps))
      )
end

let ps_execute_stream string parse_command = begin
  ps_cmd "ps_execute_stream" parse_command
    (fun ps -> begin
        let str = Machine.decode_string "ps_execute_stream" string in

        execute_stream ps (UCStream.of_list str)
      end)
end

let ps_execute_argument parse_command = begin
  ps_cmd "ps_execute_argument" parse_command
    (fun ps -> 
        execute_argument ps
      )
end

let ps_run_parser args = match args with
| [result; mode; parse_command] -> begin
    ps_cmd "ps_run_parser" parse_command
      (fun ps -> begin
          let m = decode_mode "ps_run_parser" mode in

          Machine.set_unknown result (encode_node_list (run_parser ps m))
        end)
  end
| _ -> assert false


let call_at_exit ps = begin
  try
    let f = Vm.Machine.lookup_symbol ps.al_scope (Array.of_list (UString.string_to_bytes "ps_at_exit")); in
    execute_ps_command_unknown "ps_at_exit" (ref f) ps
  with
  | Not_found -> ()
end
