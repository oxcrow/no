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

(* Compiler execution starts from here *)
let main () =
  let command, filePath = what () in
  unit
;;
