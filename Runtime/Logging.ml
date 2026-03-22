
open Tools.XNum

let log_file = ref None

let log_open file = 
  log_file := Some (open_out file)

let log_string str =
  print_string str;
  flush stdout;
  match !log_file with
  | None -> ()
  | Some oc -> output_string oc str

let log_uc_list   str = log_string (Unicode.UString.to_string str)
let log_uc_string str = log_string (Unicode.UString.to_string (Array.to_list str))

let log_int x = log_string (string_of_int x)
let log_num x = log_string (string_of_float (float_of_num x))

let log_info (file, line, col) msg =
  if file <> "" then begin
    log_string "\nIn file ";
    log_string file;
    log_string ", line ";
    log_int line;
    log_string ", column ";
    log_int col;
    log_string ":\n";
    log_string msg
  end else begin
    log_string "\n";
    log_string msg
  end

let log_warn (file, line, col) msg =
  if file <> "" then begin
    log_string "\nWarning, in file ";
    log_string file;
    log_string ", line ";
    log_int line;
    log_string ", column ";
    log_int col;
    log_string ":\n";
    log_string msg
  end else begin
    log_string "\nWarning: ";
    log_string msg
  end

let log_error (file, line, col) msg =
  if file <> "" then begin
    log_string "\nError, in file ";
    log_string file;
    log_string ", line ";
    log_int line;
    log_string ", column ";
    log_int col;
    log_string ":\n";
    log_string msg
  end else begin
    log_string "\nError: ";
    log_string msg
  end
