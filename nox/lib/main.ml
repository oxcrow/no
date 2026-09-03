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
  let buffer = Lexing.from_string code in

  let parsed =
    try Parser.file Lexer.token buffer with
    | Parser.Error state ->
        let lineIndex, colIndex = Hecc.posOfError buffer.lex_curr_p in
        let message = Hecc.scream filePath lineIndex colIndex in
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
let dev () =
  let command, filePath = what () in
  (match command with
  | "compile" | "c" ->
      (* Parse code from the root file *)
      let rootAst = parse filePath in

      (* Parse all other files in the module, include the root file too. *)
      let modAsts =
        let modNames = List.map Ast.getStringOfModule (Ast.getModulesOfFile rootAst) in

        (* Verify that no duplicate exist in the module list *)
        let rec hasDuplicateFiles fileNames =
          match fileNames with
          | [] | [ _ ] -> None
          | fileNameHead :: fileNameTail ->
              let found =
                match List.find_opt (fun tailName -> fileNameHead = tailName) fileNameTail with
                | Some n -> Some n
                | None -> hasDuplicateFiles fileNameTail
              in
              found
        in

        (* Parse all files in module *)
        let modAsts =
          match hasDuplicateFiles modNames with
          | Some n -> die ("Unable to parse duplicate filename " ^ quote n ^ " in mod list!")
          | None ->
              List.map
                (fun modName ->
                  let modPath = Filename.dirname filePath ^ "/" ^ modName ^ ".no" in
                  let modAst = parse modPath in
                  modAst)
                modNames
        in

        rootAst :: modAsts
      in

      (* Create environment from all high level entities in each module *)
      let env =
        let rec envOfMod env mods =
          match mods with
          | [] -> env
          | headMod :: tailMod ->
              let envOfFile env file =
                let fileName = Ast.getStringNameOfFile file in
                let entys = Ast.getEntitiesOfFile file in

                (* Recurse through all entities in the module, and insert them in env *)
                let rec envOfEnty env entys =
                  match entys with
                  | [] -> env
                  | headEnty :: tailEnty ->
                      let nameId, record =
                        match headEnty with
                        | Ast.Function o ->
                            let nameId = Store.Module.getIdOfName o.name in
                            let name = Ast.getStringOfName o.name in
                            ( nameId,
                              Store.Module.FunctionRecord
                                {
                                  name;
                                  nameId;
                                  args = o.args;
                                  types = o.types;
                                  file = fileName;
                                  loc = o.loc;
                                } )
                        | Ast.Struct o ->
                            let nameId = Store.Module.getIdOfName o.name in
                            let name = Ast.getStringOfName o.name in
                            ( nameId,
                              Store.Module.StructRecord
                                {
                                  name;
                                  nameId;
                                  elems = o.elems;
                                  file = fileName;
                                  entys = [];
                                  loc = o.loc;
                                } )
                        | _ -> never source "wut?"
                      in

                      let env = Store.Module.SymbolMap.add nameId record env in
                      envOfEnty env tailEnty
                in

                let env = envOfEnty env entys in
                env
              in

              let env = envOfFile env headMod in
              envOfMod env tailMod
        in
        let env = envOfMod Store.Module.SymbolMap.empty modAsts in
        env
      in

      (* Infer all types inside each file in the module *)
      let modAsts = List.map (fun modFile -> Infer.inferFile env modFile) modAsts in

      unit
  | _ -> failwith ("Unknown command: " ^ quote command));
  unit
;;

let main () =
  try dev () with
  | Report report ->
      let message = report.message in
      let file = report.source.file in
      let line = report.source.line in
      write message;
      write (italicLine (fmt "(File: %s, Line: %d)" file line))
  | _ -> exit 0
;;
