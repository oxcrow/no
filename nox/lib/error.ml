open Core

type errorKind =
  (* parser errors *)
  | UnknownParserError of string * (int * int)
  | UnknownToken of string * Token.uToken
  | InvalidIdentifier of string * Token.uToken
  | InvalidGrammar of string * Token.uToken
  (* basic errors *)
  | UnwrapNone of string * Token.uToken
  | Fatal of string * string

let rec toString report : string option =
  let fmt = Printf.sprintf in
  let head = fmt "╭───%s─ %s\n" (red Fmt.dotChar) report.message in
  let text =
    match report.error with
    | Some (Any hiddenError) -> (
        let error : errorKind = Obj.magic hiddenError in
        match error with
        | UnknownParserError (file, (lineIdx, colIdx)) ->
            let l1 = around file lineIdx colIdx in
            let l2 = enx "Perhaps see here?" in
            Some (l1 ^ l2)
        | UnknownToken (file, token) ->
            let l1 = around file token.lineIdx token.colIdx in
            let l2 = enx "Perhaps use a valid token?" in
            Some (l1 ^ l2)
        | InvalidIdentifier (file, token) ->
            let l1 = around file token.lineIdx token.colIdx in
            let l2 = enx "Perhaps use a valid identifier?" in
            Some (l1 ^ l2)
        | InvalidGrammar (file, token) ->
            let l1 = around file token.lineIdx token.colIdx in
            let l2 = enx "Perhaps try to fix this?" in
            Some (l1 ^ l2)
        | UnwrapNone (file, token) ->
            let l1 = around file token.lineIdx token.colIdx in
            let l2 = ice "I unwrapped an Option's None value." in
            Some (l1 ^ l2)
        | _ -> None)
    | None -> None
  in
  match text with Some text -> Some (head ^ text) | None -> None

and around file (lineIdx : int) (colIdx : int) =
  let fmt = Printf.sprintf in
  let l1 = tip (fmt "Around (Line: %d, Col: %d, File: \"%s\")" lineIdx colIdx file) in
  let l2 = txt file lineIdx colIdx in
  l1 ^ l2

and txt file lineIdx colIdx =
  let fmt = Printf.sprintf in
  let linesOfCode = File.readFileContent file |> String.split_on_char '\n' in
  let line lineIdx =
    (* Since lists are 0 indexed; but our lines are 1 indexed *)
    let listIdx = lineIdx - 1 in
    let text =
      match lineIdx > 0 && lineIdx < List.length linesOfCode with
      | true -> Format.sprintf "%*d" 6 lineIdx ^ " ┊  " ^ List.nth linesOfCode listIdx
      | false -> "     ~ ┊"
    in
    fmt "%s     %s" Fmt.sideChar text
  in
  let l1 = fmt "%s\n" (line (lineIdx - 2)) in
  let l2 = fmt "%s\n" (line (lineIdx - 1)) in
  let l3 = fmt "%s    %s\n" (line (lineIdx + 0)) (red "<<< ERROR!") in
  let l4 = fmt "%s\n" (line (lineIdx + 1)) in
  let l5 = fmt "%s\n" (line (lineIdx + 2)) in
  l1 ^ l2 ^ l3 ^ l4 ^ l5

and tip message = Printf.sprintf "%s   %s  %s\n" Fmt.sideChar Fmt.tipChar message
and spc message = Printf.sprintf "%s      %s\n" Fmt.sideChar message
and enx message = Printf.sprintf "%s─  %s  %s\n" Fmt.bendChar Fmt.tipChar message
and red message = Printf.sprintf "%s%s%s" Fmt.ansiRed message Fmt.ansiReset

and ice message =
  let l1 = tip (red "THIS IS AN INTERNAL COMPILER ERROR! (OUR FAULT! SORRY!)") in
  let l2 = spc message in
  let l3 = enx "Kindly REPORT this bug." in
  l1 ^ l2 ^ l3
;;
