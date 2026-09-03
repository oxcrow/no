(* Current position within our code, useful to track bugs. *)
external source : string * int * int * int = "%loc_POS"

(* Type for the source code position. *)
type tSOURCE = string * int * int * int

(* Error report data. *)
type 'a uERROR = { message : string; source : uLOC; error : (int * any) option }
and uLOC = { file : string; line : int; start : int; end' : int }
and any = Any : 'a -> any

exception Report of any uERROR

(* Create source code location record. *)
let xSOURCE (source : tSOURCE) : uLOC =
  let file, line, start, end' = source in
  { file; line; start; end' }
;;

(* Extract filename from location. *)
let xFILE (location : uLOC) : string = location.file

(* Extract line number from location. *)
let xLINE (location : uLOC) : int = location.line

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
let bendLine message = bendChar ^ dashChar ^ space ^ message
let sideLine message = sideChar ^ space ^ message
let tipLine message = tipChar ^ dashChar ^ space ^ message
let italicLine message = ansiItalic ^ message ^ ansiReset
let quote message = "'" ^ message ^ "'"

(** Read the contents of file into a string. *)
let readFileContent filename : string =
  let fileChannel = open_in_bin filename in
  let fileLength = in_channel_length fileChannel in
  let content = really_input_string fileChannel fileLength in
  content
;;

(** Print error message and kill process *)
let die message =
  write message;
  exit 0
;;

let debug source message =
  let source = xSOURCE source in
  write (message ^ fmt "(File: %s, Line: %d)" source.file source.line)
;;

(* Is value a Some or None? *)
let isSome value = match value with Some _ -> true | None -> false
let isNone value = not (isSome value)

(* Raise an error report stating something isn't implemented.
 * Use to mark an incomplete section of code. *)
let todo (source : tSOURCE) (message : string) =
  raise
    (Report
       {
         message = errorLine "Code is not yet implemented!\n" ^ bendLine message;
         source = xSOURCE source;
         error = None;
       })
;;

(* Raise an error report stating something isn't correct.
 * Use to mark an unreachable section of code. *)
let never (source : tSOURCE) (message : string) =
  raise
    (Report
       {
         message = errorLine "Unreachable code is executed!\n" ^ bendLine message;
         source = xSOURCE source;
         error = None;
       })
;;

(* Assure that a condition is met, else execute a lambda, to do what the user wants. *)
let assure (source : tSOURCE) (condition : bool) reporter = if condition then () else reporter ()

(* Extract data from Option if it is Some, or raise error.
 * Use as, `data |> some source` *)
let some (source : tSOURCE) (value : 'a option) : 'a =
  match value with
  | Some x -> x
  | None ->
      raise
        (Report
           {
             message = errorLine "Unable to unwrap Option value. (Since it is None)";
             source = xSOURCE source;
             error = None;
           })
;;

(* Extract value from option; or else return a default value *)
let someOrElse (value : 'a option) (default : 'a) : 'a =
  match value with Some x -> x | None -> default
;;

(* Extract data from Option if it is Some, or raise error, using custom reporter.
 * Use as, `data |> maybe source (fun _ -> (* code *))` *)
let maybe (source : tSOURCE) (value : 'a option) reporter : 'a =
  match value with Some x -> x | None -> reporter ()
;;

let later f = fun () -> f
let laterList f = fun () -> [ f ]

type 'a group = One of 'a | Many of 'a list

let one (source : tSOURCE) (value : 'a group) : 'a =
  match value with
  | One x -> x
  | _ ->
      raise
        (Report
           {
             message = errorLine "Unable to unwrap Group value. (Sine it is Many)";
             source = xSOURCE source;
             error = None;
           })
;;

let rec lastOfList list =
  match list with [] -> None | [ x ] -> Some x | _ :: tail -> lastOfList tail
;;

let splitListAt list index =
  let rec aux list accList accIdx =
    match list with
    | [] -> List.rev accList
    | head :: tail -> (
        match accIdx > index with
        | true -> List.rev accList
        | false -> aux tail (head :: accList) (accIdx + 1))
  in
  aux list [] 0
;;

let firstOfTwo tuple = match tuple with a, _ -> a
let secondOfTwo tuple = match tuple with _, b -> b
let firstOfThree tuple = match tuple with a, _, _ -> a
let secondOfThree tuple = match tuple with _, b, _ -> b
let thirdOfThree tuple = match tuple with _, _, c -> c
