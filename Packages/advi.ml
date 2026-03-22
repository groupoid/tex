
open XNum
open Runtime
open Unicode
open Unicode.UTypes
open Logging
open Markup
open ParseState
  let advi_special ps cmd = begin
  add_node ps (`CommandBox (location ps, `Special (`DVI_Special ("advi: " ^ cmd))))
}

(* |\adviwait [time]| *)

let advi_wait ps = begin
  match Parser.read_optional ps.input_stream [] with
  [ [] -> advi_special ps "pause"
  | x  -> advi_special ps ("wait sec=" ^ UString.to_string x)
  ]
}

(* |\advibeginrecording * {tag}| *)

let advi_begin_recording ps = begin
  let play = if Parser.read_bool ps.input_stream then
               " play"
             else
               "" in
  let tag  = Macro.expand_string ps (Parser.read_argument ps.input_stream) in
  advi_special ps ("proc=" ^ (UString.to_string tag) ^ " record=start" ^ play)
}

(* |\adviendrecording| *)

let advi_end_recording ps = begin
  advi_special ps "proc record=end"
}

(* |\adviplay {tag}| *)

let advi_play ps = begin
  let tag = Macro.expand_string ps (Parser.read_argument ps.input_stream) in
  advi_special ps ("proc=" ^ (UString.to_string tag) ^ " play")
}

(* |\adviembed {name} {mode} {width} {height} {cmd}| *)

let advi_embed ps = begin
  let name   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let mode   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let width  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let height = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let cmd    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  advi_special ps ("embed name=\"" ^ name ^ "\" mode=" ^ mode ^
                   " width=" ^ width ^ " height=" ^ height ^ " command=\"" ^ cmd ^ "\"")
}

(* |\advikillembed {name} {signal}| *)

let advi_kill_embed ps = begin
  let name   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let signal = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  advi_special ps ("kill name=\"" ^ name ^ "\" signal=\"" ^ signal ^ "\"")
}

(* |\advisetbg {name} {signal}| *)

let advi_set_bg ps = begin
  let colour = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let image  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let alpha  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let blend  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let fit    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  advi_special ps ("setbg " ^ colour ^ " " ^ image ^ " " ^ alpha ^ " " ^ blend ^ " " ^ fit)
}

let advi_eps_transparent ps = begin
  advi_special ps "epstransparent push true"
}

let advi_eps_white ps = begin
  advi_special ps "epstransparent push false"
}

let advi_reset_eps_transparent ps = begin
  advi_special ps "epstransparent pop"
}

let advi_set_alpha ps = begin
  let alpha = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  advi_special ps ("alpha push " ^ alpha)
}

let advi_reset_alpha ps = begin
  advi_special ps "alpha pop"
}

let advi_set_blend ps = begin
  let blend = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  advi_special ps ("blend push " ^ blend)
}

let advi_reset_blend ps = begin
  advi_special ps "blend pop"
}

let advi_transition ps = begin
  let mode    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let from    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let steps   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let start_x = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let start_y = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let stop_x  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let stop_y  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let genpath = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  advi_special ps ("trans " ^ mode ^ " " ^ from ^ " " ^ steps ^ " " ^
                   start_x ^ " " ^ start_y ^ " " ^ stop_x ^ " " ^ stop_y ^ " " ^ genpath)
}

let advi_trans_box_save ps = begin
  let width  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let height = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let depth  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  advi_special ps ("transbox save width=" ^ width ^ " height=" ^ height ^ " depth=" ^ depth)
}

let advi_trans_box_go ps = begin
  let mode    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let from    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let steps   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let start_x = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let start_y = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let stop_x  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let stop_y  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let genpath = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  advi_special ps ("transbox go " ^ mode ^ " " ^ from ^ " " ^ steps ^ " " ^
                   start_x ^ " " ^ start_y ^ " " ^ stop_x ^ " " ^ stop_y ^ " " ^ genpath)
}

let advi_move_to ps = begin
  advi_special ps "moveto"
}

let advi_begin_anchor ps = begin
  let mode = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let tag  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  if mode = "over" then
    advi_special ps ("html:<a advi=\"" ^ tag ^ "\">")
  else if mode = "click" then
    advi_special ps ("html:<a hdvi=\"" ^ tag ^ "\">")
  else
    log_warn (location ps) ("Incorrect anchor mode " ^ mode ^ ".")
}

let advi_end_anchor ps = begin
  advi_special ps "html:</a>"
}

let advi_edit ps = begin
  let comm   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let name   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let line   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let file   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let unit   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let x      = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let y      = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let w      = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let h      = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let move   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  let resize = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream)) in
  advi_special ps ("edit comm=\"" ^ comm ^ "\" name=\"" ^ name ^ "\" line=" ^ line ^
                   " file=" ^ file ^ " unit=" ^ unit ^
                   " x=" ^ x ^ " y=" ^ y ^ " w=" ^ w ^ " h=" ^ h ^
                   " move=" ^ move ^ " resize=" ^ resize)
}

let init_commands ps = begin
  let def_cmd name cmd = do
  {
    define_command ps (UString.of_ascii name) { execute = cmd expand = Macro.noexpand }
  } in
  def_cmd "\\adviwait"                advi_wait;
  def_cmd "\\advibeginrecording"      advi_begin_recording;
  def_cmd "\\adviendrecording"        advi_end_recording;
  def_cmd "\\adviplay"                advi_play;
  def_cmd "\\adviembed"               advi_embed;
  def_cmd "\\advikillembed"           advi_kill_embed;
  def_cmd "\\advisetbg"               advi_set_bg;
  def_cmd "\\adviepstransparent"      advi_eps_transparent;
  def_cmd "\\adviepswhite"            advi_eps_white;
  def_cmd "\\adviresetepstransparent" advi_reset_eps_transparent;
  def_cmd "\\advisetalpha"            advi_set_alpha;
  def_cmd "\\adviresetalpha"          advi_reset_alpha;
  def_cmd "\\advisetblend"            advi_set_blend;
  def_cmd "\\adviresetblend"          advi_reset_blend;
  def_cmd "\\advitransition"          advi_transition;
  def_cmd "\\advitransboxsave"        advi_trans_box_save;
  def_cmd "\\advitransboxgo"          advi_trans_box_go;
  def_cmd "\\advimoveto"              advi_move_to;
  def_cmd "\\advibeginanchor"         advi_begin_anchor;
  def_cmd "\\adviendanchor"           advi_end_anchor;
  def_cmd "\\adviedit"                advi_edit;
};

begin
  Run.register_parse_state_hook init_commands
};

