
open Runtime
open Unicode
open Types
open Logging
open ParseState

type arg_specifier = Arg
| Opt of uc_list
| Bool


(* |substitute <args> <body>| replaces each argument specifier in <body> by its value. *)
let substitute args body =
  let arg_arr = Array.of_list args in
  let result  = Tools.ListBuilder.make () in
  let rec iter body = match body with
    | []      -> Tools.ListBuilder.get result
    | c::cs ->
        if c <> CharCode.macro_char then (
          Tools.ListBuilder.add result c;
          iter cs
        ) else match cs with
          | []        -> Tools.ListBuilder.get result
          | c2::cs2 ->
              if c2 < 49 || c2 > 57 then (
                Tools.ListBuilder.add result c2;
                iter cs2
              ) else if c2 - 49 < Array.length arg_arr then (
                Tools.ListBuilder.add_list result arg_arr.(c2 - 49);
                iter cs2
              ) else
                iter cs2
  in
  iter body

(* expanding strings *)

(* |expand <parse-state> <stream>| expands all macros in <stream>. *)
let rec expand ps =
  let c = UCStream.next_char ps.input_stream in
  match CharCode.cat_code c with
  | CharCode.EOF    -> []
  | CharCode.Escape ->
      let token = Parser.read_token ps.input_stream in
      (match lookup_command ps token with
      | Some cmd -> cmd.expand ps token
      | None     -> token @ expand ps)
  | _ ->
      ignore (UCStream.pop ps.input_stream);
      c :: expand ps

(* |expand_string <parse-state> <str>| expands all macros in <str>. *)
let expand_string ps str =
  let str_stream = UCStream.of_list str in
  UCStream.exchange ps.input_stream str_stream;
  let result = expand ps in
  UCStream.exchange ps.input_stream str_stream;
  result

let noexpand ps tok =
  tok @ expand ps

(* macros *)

(*
  |parse_arg_template| reads an argument template consisting of a list of specifiers: 

  |m|             mandantory argument
  |o|             optional argument without default
  |O{|<default|}| optional argument with default
  |s|             boolean argument
  |x|             a mandantory argument which will be expanded
*)

let parse_arg_template loc template =
  let rec iter templ = match templ with
    | []          -> []
    | 109 :: ts -> Arg :: iter ts
    | 111 :: ts -> Opt (UString.of_ascii "\\NoValue") :: iter ts
    | 79 :: t :: ts ->
        if t <> CharCode.begin_group_char then
          Opt [t] :: iter ts
        else
          let rec read_default default nest str = match str with
            | []      ->
                log_error loc "invalid macro template!";
                []
            | c::cs -> match CharCode.cat_code c with
              | CharCode.BeginGroup -> read_default (c :: default) (nest + 1) cs
              | CharCode.EndGroup   -> if nest = 0 then
                                         Opt (List.rev default) :: iter cs
                                       else
                                         read_default (c :: default) (nest - 1) cs
              | _                   -> read_default (c :: default) nest cs
          in
          read_default [] 0 ts
    | _  :: ts -> iter ts
  in
  iter template

(*
  |parse_args <template>| reads the arguments of a macro according to the argument template which
  is a list of argument specifiers.
*)
let rec parse_args ps stream template = match template with
| []              -> []
| Arg    :: ts -> let arg = Parser.read_argument stream in
                     arg :: parse_args ps stream ts
| Opt d  :: ts -> let arg = Parser.read_optional stream d in
                     arg :: parse_args ps stream ts
| Bool   :: ts -> if Parser.read_bool stream then
                       UString.of_ascii "\\True"  :: parse_args ps stream ts
                     else
                       UString.of_ascii "\\False" :: parse_args ps stream ts

(* |execute_macro <args> <body> <parse-state>| executes the given macro. *)

let execute_macro args body ps =
  if !tracing_macros then (
    log_string " -> ";
    log_string (UString.to_string body)
  ) else ();
  UCStream.insert_list
    ps.input_stream
    (substitute (parse_args ps ps.input_stream args) body)

let expand_macro args body ps _tok =
  let str = substitute (parse_args ps ps.input_stream args) body in
  UCStream.insert_list ps.input_stream str;
  expand ps

(* |execute <name>| executes the command with the given name. *)
let execute_command ps name =
  match lookup_command ps name with
  | None     -> log_error
                  (location ps)
                  ("undefined command " ^ UString.to_string name ^ ".")
  | Some cmd ->
      if !tracing_macros then (
        log_string "\n#M: ";
        log_string (UString.to_string name)
      ) else ();
      cmd.execute ps

(* environments *)

(* |begin_env <name>| starts the environment <name> and |end_env <name>| ends it. *)
let begin_env ps name =
  match lookup_env ps name with
  | Some (cmd, _) ->
      cmd.execute ps
  | None ->
      log_error (location ps) "undefined environment ";
      log_uc_list name;
      log_string "."

let end_env ps name =
  let (cur_name, _) = top_env ps in
  if name <> cur_name then (
    log_error (location ps) "environment ";
    log_uc_list cur_name;
    log_string " ended by \\end{";
    log_uc_list name;
    log_string "}."
  ) else (
    match lookup_env ps name with
    | Some (_, cmd) -> cmd.execute ps
    | None          -> assert false
  )

(* abbreviations for begin- and end-environment to access the arguments of a macro *)
let execute_begin_environment name args body ps =
  if !tracing_macros then (
    log_string " -> ";
    log_string (UString.to_string body)
  ) else ();
  (* Since the <body> is executed in its own stream it cannot remove any spaces after
     the |\begin{...}| command. But this is desirable in almost every case. Therefore,
     we do it by default.  *)

  Parser.skip_blanks ps.input_stream;
  let params = parse_args ps ps.input_stream args in
  let stream = UCStream.of_list (substitute params body) in
  ParseState.execute_stream ps stream;
  push_env ps name params       (* store the arguments for the \end{...} command *)

let expand_begin_environment name args body ps _tok =
  (* As above we remove spaces after the |\begin{...}| command. *)

  Parser.skip_blanks ps.input_stream;

  let params = parse_args ps ps.input_stream args in
  let s = UCStream.of_list (substitute params body) in
  ParseState.execute_stream ps s; (* FIX !!! *)

  push_env ps name params;

  UCStream.to_list s @ expand ps

let execute_end_environment body ps =
  let (_, args) = pop_env ps in
  (* As above we remove spaces after the |\end{...}| command. *)

  Parser.skip_blanks ps.input_stream;

  let stream = UCStream.of_list (substitute args body) in
  ParseState.execute_stream ps stream

let expand_end_environment body ps _tok =
  let (_, args) = pop_env ps in
  (* As above we remove spaces after the |\end{...}| command. *)

  Parser.skip_blanks ps.input_stream;

  UCStream.insert_list ps.input_stream (substitute args body);
  expand ps
