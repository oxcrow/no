open Core

let usage () =
  write "No: A language for workers of the world.";
  write "";
  write "Usage: no <COMMAND> [OPTIONS] <FILE.no>";
  write "";
  write "File:";
  write "    An ASCII file with .no extension.";
  write "Command:";
  write (fmt "    %scompile, c%s             Compile and build artifacts into an executable" ansiGreen ansiReset);
  write (fmt "    %shelp, h%s <COMMAND>      Display help message for command." ansiGreen ansiReset);
  write "Common Options:";
  write (fmt "    %s--help, -h%s             Display this help message." ansiCyan ansiReset);
  write "";
  write (italicLine "(Believe in yourself)")
  [@@ocamlformat "disable"]

(* Parse command line arguments *)
let what () =
  let command, filePath =
    match Array.length Sys.argv < 2 with
    (* In case of not enough arguments, crash out! *)
    | true ->
        usage ();
        exit 0
    (* In case we found enough arguments, parse them with care! *)
    | false -> (
        (* Extract the arguments by first discarding the executable binary path *)
        let args = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) in
        (* NOTE: The command that is expected to be run by the compiler toolchain
         * Such as, `no compile`, `no help`, `no clean`, `no test`, `no run`, etc.
         * While this may seem complex, it's necessary. lol. *)
        let cmd = List.hd args in
        match cmd with
        | "-h" | "--help" ->
            usage ();
            exit 0
        (* If nothing else is found search for the root file to compile *)
        | _ -> (
            let filePath =
              List.nth_opt
                (List.filter
                   (* An argument that does not start with - is our root file path.
                    * Multiple such arguments can exist (by mistake),
                    * thus only the first one is parsed. *)
                   (fun arg -> not (String.starts_with ~prefix:"-" arg))
                   (List.tl args))
                0
            in
            match filePath with
            | Some file -> (cmd, file)
            | None -> failwith "Unable to parse filename from arguments."))
  in
  (command, filePath)
;;

(* Parse file *)
let parse filePath =
  let code = readFileContent filePath in
  (* Parse code using Menhir parser. Most of this is boilerplate code. No need to worry. *)
  let posOfError (pos : Lexing.position) =
    let lineIndex = pos.pos_lnum in
    let colIndex = pos.pos_cnum - pos.pos_bol + 1 in
    (lineIndex, colIndex)
  in
  let scream file lineIndex colIndex =
    let lines = String.split_on_char '\n' (readFileContent file) in
    let message lineIndex =
      match lineIndex >= 1 && lineIndex < List.length lines with
      | true -> (fmt "%4d │ " lineIndex, List.nth lines (lineIndex - 1))
      | false -> (fmt "   ~ │", "")
    in
    let fmtA, lineA = message (lineIndex - 2) in
    let fmtB, lineB = message (lineIndex - 1) in
    let fmtC, lineC = message lineIndex in
    let lineX =
      let splitAt string i =
        let n = String.length string in
        match i < 0 || i > n with
        | true -> ("", string)
        | false ->
            let left = String.sub string 0 i in
            let right = String.sub string i (n - i) in
            (left, right)
      in
      let left, right = splitAt lineC colIndex in
      left ^ ansiRed ^ right ^ ansiReset
    in
    let fmtD, lineD = message (lineIndex + 1) in
    let fmtE, lineE = message (lineIndex + 2) in
    String.concat "\n" [ fmtA ^ lineA; fmtB ^ lineB; fmtC ^ lineX; fmtD ^ lineD; fmtE ^ lineE ]
  in
  let buffer = Lexing.from_string code in
  let parsed =
    try Parser.file Lexer.token buffer with
    | Parser.Error state ->
        let lineIndex, colIndex = posOfError buffer.lex_curr_p in
        let message = scream filePath lineIndex colIndex in
        write (errorLine "Unable to parse code!");
        write message;
        exit 0
    | Failure message ->
        write message;
        exit 0
    | exn ->
        write (Printexc.to_string exn);
        exit 0
  in
  (* Menhir parser doesn't know the root file's path,
   * but it's needed for debugging and analysis,
   * so we store it. *)
  let ast = match parsed with Ast.File f -> Ast.File { f with file = filePath } in
  ast
;;

(* Compiler execution starts from here *)
let main () =
  let command, filePath = what () in
  (match command with
  | "compile" | "c" ->
      (* Parse code from the root file *)
      let rootAst = parse filePath in
      unit
  | _ -> failwith ("Unknown command: " ^ quote command));
  unit
;;
