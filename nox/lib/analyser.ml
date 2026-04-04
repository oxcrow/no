open Core

let isFunction x = match x with Ast.Function _ -> true | _ -> false

(** Analyse function and lower it to IR *)
let analyseFunction entity env = ()

(** Analyse file and lower it to IR *)
let analyseFile (ast : Ast.file) =
  let env = Env.initFileEnv ast in
  match ast with
  | Ast.File f ->
      List.map
        (fun entity -> analyseFunction entity env)
        (List.filter isFunction f.entities)
;;
