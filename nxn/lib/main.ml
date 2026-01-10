open Error.Failure

(* Location *)
external loc : string = "%loc_LOC"

let write = print_endline
let unit = ()

(** Debug AST *)
let dbg ast = write (Ast.show_file ast)

(** User guide *)
let usage () =
  write "NxN: An elegant systems programming language.";
  write "";
  write "Usage:";
  write "    nxn <file.nxn>";
  write ""
;;

(** What file to compile? *)
let what () =
  let file =
    match Array.length Sys.argv < 2 with
    | true ->
        usage ();
        failwith "Expected file to compile."
    | false -> Sys.argv.(1)
  in
  file
;;

(** Evalulate error message *)
let emsg file pos msg =
  let lines = String.split_on_char '\n' (File.read_file_content file) in
  let lnum, cnum = pos in
  let black = "\027[0m" in
  let red = "\027[31m" in
  let line lnum =
    let code =
      match lnum > 0 && lnum < List.length lines with
      | true -> List.nth lines (lnum - 1)
      | false -> ""
    in
    " : " ^ Format.sprintf "%*d" 5 lnum ^ " ┊ " ^ code
  in
  let l0 =
    msg ^ "\n (Around approximate position: " ^ "(Line: " ^ string_of_int lnum
    ^ ", Column: " ^ string_of_int cnum ^ ") of File: \"" ^ file ^ "\")\n"
  in
  let l1 = line (lnum - 2) ^ "\n" in
  let l2 = line (lnum - 1) ^ "\n" in
  let l3 = red ^ line (lnum - 0) ^ " <<< " ^ msg ^ black ^ "\n" in
  let l4 = line (lnum + 1) ^ "\n" in
  let l5 = line (lnum + 2) ^ "\n" in
  l0 ^ l1 ^ l2 ^ l3 ^ l4 ^ l5
;;

(** Parse nxn code and create AST *)
let parse code file =
  let buf = Lexing.from_string code in
  let ast =
    let epos (buf : Lexing.lexbuf) =
      let lnum = buf.lex_curr_p.pos_lnum in
      let cnum = buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol in
      (lnum, cnum)
    in
    try Parser.file Lexer.token buf with
    | Parser.Error ->
        let why =
          emsg file (epos buf)
            "Parser error.(Error could also be from lines above.)"
        in
        error loc why
    | Failure msg ->
        let why =
          emsg file (epos buf)
            "Parser error.(Error could also be from lines above.)"
        in
        error loc (why ^ ", With message: " ^ msg)
  in
  let ast =
    match ast with Ast.File f -> Ast.File { entities = f.entities; file }
  in
  ast
;;

(** Semantic analyze the AST.

    It's annoying to write AST visitors all the time. So, we will analyze,
    infer, and lower the AST in one horrifying lovecraftian monolithic step,
    without caring about anything else.

    May Gods save the poor soul who has to extend/maintain this in future. *)
let analyze ast = (unit, unit)

(** Compile code and return the emitted result *)
let compile file =
  let code = File.read_file_content file in
  let ast, mir = parse code file |> analyze in
  Some ""
;;

(** Verify a file can be compiled *)
let pass file =
  let _emitted =
    try compile file with
    | Failure msg ->
        write msg;
        None
    | exn ->
        write (Printexc.to_string exn);
        None
  in
  unit
;;

(** Driver *)
let main =
  pass (what ());
  unit
;;
