open Core
open Get.X

let fmt = Printf.sprintf
let pmt = Printf.printf
let strOfInt = string_of_int

(* Utils *)
let isFunction x = match x with Ast.Function _ -> true | _ -> false

(** Lower a function's AST to QBE IR instructions *)
let rec lowerFunction file env entity =
  let types = Analyser.analyseFunction file env entity in
  match entity with
  | Ast.Function f -> todo "lower-function"
  | _ -> xNEVER uPOS "expected-function"

and todo message =
  exit 0;
  xTODO uPOS message
;;

(** Lower file's AST to QBE IR instructions *)
let lowerFile ast =
  let env = Env.initFileEnv ast in
  let defns =
    match ast with
    | Ast.File f ->
        List.map
          (fun entity -> lowerFunction f.file env entity)
          (List.filter isFunction f.entities)
  in
  (* List.iter (fun d -> print_endline (Qbe.show_defns d)) defns; *)
  exit 0;
  defns
;;
