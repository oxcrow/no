open Core

let isFunction x = match x with Ast.Function _ -> true | _ -> false

(** Analyse function and lower it to IR *)
let analyseFunction (file : string) (env : Env.env) (entity : Ast.entities) =
  match entity with
  | Ast.Function f -> print_endline (Get.Ast.name f.name)
  | _ ->
      raise
        (Report
           {
             message = "Unable to analyze entity.";
             source = xSOURCE uPOS;
             error =
               (let lineIdx, colIdx =
                  match Get.Ast.locofEntity entity with
                  | Ast.Location l -> (l.lineIdx, l.colIdx)
                  | Ast.Nowhere -> xNEVER uPOS "wut?"
                in
                Some (Any (Error.UnknownParserError (file, (lineIdx, colIdx)))));
           })
;;

(** Analyse file and lower it to IR *)
let analyseFile (ast : Ast.file) =
  let env = Env.initFileEnv ast in
  match ast with
  | Ast.File f ->
      List.map
        (fun entity -> analyseFunction f.file env entity)
        (List.filter isFunction f.entities)
;;
