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
        | "-h" | "--help" ->
            usage ();
            exit 0
        | _ -> (
            let file =
              List.nth_opt
                (List.filter
                   (fun arg -> not (String.starts_with ~prefix:"-" arg))
                   (Array.to_list Sys.argv |> List.tl))
                0
            in
            match file with
            | Some file -> file
            | None -> failwith "Unable to parse filename from arguments."))
  in
  file
;;

(* What kind of result to emit? *)
let whatEmitMode () =
  let emitModes =
    Array.map
      (fun x ->
        match x with
        | "--emit-qbe" -> Some "--emit-qbe"
        | "--emit-asm" -> Some "--emit-asm"
        | _ -> None)
      Sys.argv
    |> Array.to_list |> List.filter isSome
  in
  let mode = match emitModes with Some head :: _ -> Some head | _ -> None in
  mode
;;

(** Debug print the AST *)
let dbgAst ast = write (Ast.show_file ast)

(** Compile a module and generate QBE/LLVM IR *)
let compileModule rootFile =
  let rootAst = Parser.parseFileExt rootFile in
  let mir = Lower.lowerFile rootAst in
  let qbe = Emit.emitQbe mir in
  (* dbgAst rootAst; *)
  Some (qbe, rootFile)
;;

(** Compile a file and verify it *)
let pass file =
  let result =
    try compileModule file with
    | Failure message ->
        write message;
        None
    | Core.Report report ->
        printf "%s"
          (match Error.toString report with
          | Some message -> message
          | _ -> report.message ^ "\n");
        printf "%sThis error was raised from compiler source,\n" Fmt.ansiItalic;
        printf "(File: \"%s\", Line: %d)\n" report.source.file report.source.line;
        printf "(You may ignore this section)%s\n" Fmt.ansiReset;
        None
    | exn ->
        write (Printexc.to_string exn);
        None
  in
  let qbe = String.trim (String.concat "" (result |> xSOME uPOS |> fst)) in
  let rootFile =
    result |> xSOME uPOS |> snd
    |> String.map (fun c -> if c == '\\' then '/' else c)
    |> Filename.basename |> Filename.chop_extension
  in
  match whatEmitMode () with
  | Some "--emit-qbe" -> write qbe
  | Some "--emit-asm" ->
      let asm, _ = runShellCmd [ "qbe" ] (Some qbe) in
      write asm
  | None ->
      let outflag = "-o " ^ rootFile ^ if Sys.win32 then ".exe" else "" in
      let asm, _ = runShellCmd [ "qbe" ] (Some qbe) in
      let _, _ = runShellCmd [ "cc"; "-x assembler -"; outflag ] (Some asm) in
      ()
  | _ -> xNEVER uPOS "emit-mode"
;;

(** Compiler driver *)
let main () = pass (what ())
