open Core

let usage () =
  write "Usage: no <command> [arguments]";
  write "";
  write "No: A language for the workers of the world.";
  write "";
  write "Command:";
  write (fmt "    %scompile, c%s             Compile and build artifacts into an executable" ansiGreen ansiReset);
  write (fmt "    %shelp, h%s <command>      Display help message for command." ansiGreen ansiReset);
  write "";
  write (italicLine "(Believe in yourself)")
  [@@ocamlformat "disable"]

let usageCompile () =
  write "Usage: no compile [options] -- <file>";
  write "";
  write "Positional Arguments:";
  write (fmt "    %sfile%s                  The file we want to compile" ansiGreen ansiReset);
  write "";
  write "Options:";
  write (fmt "    %s--dump-ast%s            Display the AST of each file" ansiCyan ansiReset);
  write (fmt "    %s--dump-ir%s             Display the IR of each file" ansiCyan ansiReset);
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
        (* If help command is found, show usage, then exit normally *)
        | "help" | "h" -> (
            let forWhat = List.nth_opt args 1 in
            match forWhat with
            | None ->
                usage ();
                exit 0
            | Some cmd -> (
                match cmd with
                | "compile" | "c" ->
                    usageCompile ();
                    exit 0
                | _ -> failwith "Unknown command."))
        (* If compile command is found search for the root file to compile *)
        | "compile" | "c" -> (
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
            | None -> failwith "Unable to parse filename from arguments.")
        (* If nothing is found, show usage, then crash out! *)
        | _ ->
            usage ();
            exit 0)
  in
  (command, filePath)
;;

(* Parse file *)
let parse filePath =
  let code = readFileContent filePath in
  (* Parse code using Menhir parser. Most of this is boilerplate code. No need to worry. *)
  let buffer = Lexing.from_string code in
  let parsed =
    try Parser.file Lexer.token buffer with
    | Parser.Error state ->
        write "Unable to parse code";
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

(* Dump AST to debug *)
let dump ast = write (Ast.show_file ast)

(* Compile code *)
let dev () =
  (* Parse the command line arguments to figure out what commands to run *)
  let command, filePath = what () in
  (* Run the command that was parsed from command line arguments *)
  (match command with
  | "compile" | "c" ->
      (* Parse code from the root file of the module *)
      let rootAst = parse filePath in
      (* Create environment from all files *)
      let env, files =
        let rec addFilesToEnv env files =
          let addToEnv env ast = env in
          match files with
          | [] -> env
          | headFile :: tailFile ->
              let env = addToEnv env headFile in
              addFilesToEnv env tailFile
        in
        let files = [ rootAst ] in
        (addFilesToEnv () files, files)
      in
      (* Infer and check types for all files *)
      let files = List.map (fun file -> Infer.inferFile env file) files in
      unit
  | _ -> failwith ("Unknown command: " ^ quote command));
  unit
;;

(* Compiler execution starts from here *)
let main () =
  try dev () with
  | Report report ->
      let message = report.message in
      let file = report.source.file in
      let line = report.source.line in
      write message;
      write (italicLine (fmt "(File: %s, Line: %d)" file line))
  | _ ->
      write "We messed up. Sorry.";
      exit 0
;;
