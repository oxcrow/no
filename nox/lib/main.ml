open Core

(* Aliases *)
let write = print_endline

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
    | false -> Sys.argv.(1)
  in
  file
;;

(** Compile a file and generate QBE/LLVM IR *)
let compile file =
  let code = File.readFileContent file in
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
        write report.message;
        None
    | exn ->
        write (Printexc.to_string exn);
        None
  in
  ()
;;

(** Compiler driver *)
let main () = pass (what ())
