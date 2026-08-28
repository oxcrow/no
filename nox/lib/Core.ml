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
let italicLine message = ansiItalic ^ message ^ ansiReset
