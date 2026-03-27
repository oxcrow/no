open Core

(* Aliases *)
let write = print_endline
let printf = Printf.printf

(** User guide *)
let usage () =
  write "No: A language for workers of the world.";
  write "";
  write "Usage:";
  write "    no <COMMAND> <FILE.no>";
  write "";
  write "FILE:";
  write "    An ASCII file with .no extension.";
  write "";
  write "COMMAND:";
  write "    build          Compile and build artifacts into an executable";
  write "    -h --help      Display this help message.";
  write ""
;;

(** What file to compile? *)
let what () =
  let file =
    match Array.length Sys.argv < 2 with
    | true ->
        usage ();
        failwith "Expected file to compile."
    | false -> (
        let cmd = Sys.argv.(1) in
        match cmd with
        | "build" -> Sys.argv.(2)
        | _ ->
            usage ();
            failwith "Expected command to compile.")
  in
  file
;;

(** Compile a file and generate QBE/LLVM IR *)
let compile file =
  let ast = Parser.parseFile file in
  Some ""
;;

(** Compile a file and verify it *)
let pass file =
  let _emitted_code_ =
    try compile file with
    | Failure message ->
        write message;
        None
    | Core.Report report ->
        printf "%s%s%s %s\n\n" Fmt.ansiRed Fmt.dotChar Fmt.ansiReset report.message;
        printf "%sThis error was raised from compiler source,\n" Fmt.ansiItalic;
        printf "(File: \"%s\", Line: %d)\n" report.source.file report.source.line;
        printf "(You may ignore this section)%s\n" Fmt.ansiReset;
        None
    | exn ->
        write (Printexc.to_string exn);
        None
  in
  ()
;;

(** Compiler driver *)
let main () = pass (what ())
