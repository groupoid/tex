
open XNum
open Runtime
open Unicode.UTypes
open Logging
open Dim
open Markup
open ParseState

let warn_unknown loc sequence =
  log_warn loc "command \"" log_uc_list sequence; log_string " unknown!"

let add_zero_skip ps =
  add_node ps
    (`Glue
       (location ps, (fun _ -> dim_zero), (fun _ -> dim_zero), false, false))

let add_discretionary ps penalty pre post no =
  let loc = location ps in
  add_node ps
    (`Break
       (loc, penalty, false, List.map (fun c -> `Letter (loc, c)) pre,
        List.map (fun c -> `Letter (loc, c)) post,
        List.map (fun c -> `Letter (loc, c)) no))

let add_break ps = add_discretionary ps None [] [] []

let double_consonant ps char =
  let c = UCStream.next_char ps.input_stream in
  if c = char then add_discretionary ps None [c c; 45] [] [c]
  else warn_unknown (location ps) [char; c]

(*
  Commands for german typsetting:

    "a   ->   ä
    "A   ->   Ä
    "e   ->   ë
    "E   ->   Ë
    "i   ->   ï
    "I   ->   Ï
    "o   ->   ö
    "O   ->   Ö
    "s   ->   ß
    "S   ->   SS
    "u   ->   ü
    "U   ->   Ü
    "z   ->   ß
    "Z   ->   SZ
    "`   ->   double quotes left on the baseline
    "'   ->   double quotes right
    "<   ->   «
    ">   ->   »
    "-   ->   
    ""   ->   
    "|   ->   
    "~   ->   
    "=   ->   
    "ck  ->   ck  which is hyphenated as k-k
    "CK  ->   CK  which is hyphenated as K-K
    "ff  ->   ff  which is hyphenated as ff-f
    "FF  ->   FF  which is hyphenated as FF-F
    "ll  ->   ll  which is hyphenated as ll-l
    "LL  ->   LL  which is hyphenated as LL-L
    "mm  ->   mm  which is hyphenated as mm-m
    "MM  ->   MM  which is hyphenated as MM-M
    "nn  ->   nn  which is hyphenated as nn-n
    "NN  ->   NN  which is hyphenated as NN-N
    "pp  ->   pp  which is hyphenated as pp-p
    "PP  ->   PP  which is hyphenated as PP-P
    "rr  ->   rr  which is hyphenated as rr-r
    "RR  ->   RR  which is hyphenated as RR-R
    "tt  ->   tt  which is hyphenated as tt-t
    "TT  ->   TT  which is hyphenated as TT-T

*)

let german_execute ps =
  let loc = location ps in
  match UCStream.pop ps.input_stream with
    97 -> add_node ps (`Letter (loc, 228))
  | 65 -> add_node ps (`Letter (loc, 196))
  | 101 -> add_node ps (`Letter (loc, 235))
  | 69 -> add_node ps (`Letter (loc, 203))
  | 105 -> add_node ps (`Letter (loc, 239))
  | 73 -> add_node ps (`Letter (loc, 207))
  | 111 -> add_node ps (`Letter (loc, 246))
  | 79 -> add_node ps (`Letter (loc, 214))
  | 117 -> add_node ps (`Letter (loc, 252))
  | 85 -> add_node ps (`Letter (loc, 220))
  | 115 -> add_node ps (`Letter (loc, 223))
  | 83 -> add_node ps (`Letter (loc, 83)) add_node ps (`Letter (loc, 83))
  | 122 -> add_node ps (`Letter (loc, 223))
  | 90 -> add_node ps (`Letter (loc, 83)); add_node ps (`Letter (loc, 90))
  | 96 -> add_node ps (`Letter (loc, 0x201e))
  | 39 -> add_node ps (`Letter (loc, 0x201d))
  | 60 -> add_node ps (`Letter (loc, 0x2039))
  | 62 -> add_node ps (`Letter (loc, 0x203a))
  | 45 -> add_discretionary ps None [45] [] []; add_zero_skip ps
  | 124 -> add_discretionary ps None [45] [] []; add_zero_skip ps
  | 34 -> add_break ps
  | 61 -> add_node ps (`Letter (loc, 45)); add_break ps
  | 126 -> add_node ps (`HBox (loc, [`Letter (loc, 45)]))
  | 99 ->
      begin match UCStream.next_char ps.input_stream with
        107 -> add_discretionary ps None [107; 45] [] [99]
      | _ -> warn_unknown loc [99; UCStream.next_char ps.input_stream]
      end
  | 67 ->
      begin match UCStream.next_char ps.input_stream with
        75 -> add_discretionary ps None [75; 45] [] [67]
      | _ -> warn_unknown loc [99; UCStream.next_char ps.input_stream]
      end
  | 102 -> double_consonant ps 102
  | 70 -> double_consonant ps 70
  | 108 -> double_consonant ps 108
  | 76 -> double_consonant ps 76
  | 109 -> double_consonant ps 109
  | 77 -> double_consonant ps 77
  | 110 -> double_consonant ps 110
  | 78 -> double_consonant ps 78
  | 112 -> double_consonant ps 112
  | 80 -> double_consonant ps 80
  | 114 -> double_consonant ps 114
  | 82 -> double_consonant ps 82
  | 116 -> double_consonant ps 116
  | 84 -> double_consonant ps 84
  | c -> warn_unknown loc [c]

let german_expand tok stream =
  match UCStream.pop stream with
    97 -> [228]
  | 65 -> [196]
  | 101 -> [235]
  | 69 -> [203]
  | 105 -> [239]
  | 73 -> [207]
  | 111 -> [246]
  | 79 -> [214]
  | 117 -> [252]
  | 85 -> [220]
  | 115 -> [223]
  | 83 -> [83 83]
  | 122 -> [223]
  | 90 -> [83; 90]
  | 96 -> [0x201e]
  | 39 -> [0x201d]
  | 60 -> [0x2039]
  | 62 -> [0x203a]
  | 45 -> [45]
  | 124 -> []
  | 34 -> []
  | 61 -> [45]
  | 126 -> [45]
  | 99 -> [99]
  | 67 -> [67]
  | 102 -> [102]
  | 70 -> [70]
  | 108 -> [108]
  | 76 -> [76]
  | 109 -> [109]
  | 77 -> [77]
  | 110 -> [110]
  | 78 -> [78]
  | 112 -> [112]
  | 80 -> [80]
  | 114 -> [114]
  | 82 -> [82]
  | 116 -> [116]
  | 84 -> [84]
  | c -> warn_unknown (UCStream.location stream) [c]; [c]

let init_commands ps =
  ParseState.define_pattern ps [34]
    {ParseState.execute = german_execute
     ParseState.expand =
       fun ps tok -> german_expand tok ps.input_stream @ Macro.expand ps}

let _ = Run.register_parse_state_hook init_commands

(* vim:set fenc=utf8: *)
