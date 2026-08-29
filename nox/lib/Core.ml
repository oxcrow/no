(* Common useful commands *)
let write = print_endline
let unit = ()
let fmt = Printf.sprintf

(* ANSI terminal characters *)
let ansiItalic = "\x1b[3m"
let ansiReset = "\x1b[0m"
let ansiRed = "\x1b[31m"
let ansiGreen = "\x1b[32m"
let ansiCyan = "\x1b[36m"

(* Unicoded characters *)
let whyChar = "●" (* U+25CF *)
let tipChar = "○" (* U+25C7 *)
let bendChar = "╰" (* U+2570 *)
let dashChar = "─" (* U+2500 *)
let sideChar = "│" (* U+2502 *)
let tabChar = "   "
let space = " "
let caret = "\n"

(* Message line formatters *)
let errorLine message = ansiRed ^ whyChar ^ ansiReset ^ " " ^ message ^ ansiReset
let italicLine message = ansiItalic ^ message ^ ansiReset
let quote message = "'" ^ message ^ "'"

(** Read the contents of file into a string. *)
let readFileContent filename : string =
  let fileChannel = open_in_bin filename in
  let fileLength = in_channel_length fileChannel in
  let content = really_input_string fileChannel fileLength in
  content
;;
