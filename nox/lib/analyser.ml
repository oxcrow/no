open Core

let isFunction x = match x with Ast.Function _ -> true | _ -> false

(** Analyse function and lower it to IR *)
let analyseFunction (env : Env.env) (entity : Ast.entities) =
  match entity with
  | Ast.Function f -> print_endline (Get.Ast.name f.name)
  | _ -> xTODO uPOS "analyze-entity"
;;

(** Analyse file and lower it to IR *)
let analyseFile (ast : Ast.file) =
  let env = Env.initFileEnv ast in
  match ast with
  | Ast.File f ->
      List.map
        (fun entity -> analyseFunction env entity)
        (List.filter isFunction f.entities)
;;
