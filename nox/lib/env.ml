open Core

type env = { table : (string, record) Hashtbl.t; scope : string list list }

and record =
  | Function of { name : string; type' : Ast.types; loc : Ast.loc }
  | Variable of { name : string; type' : Ast.types; loc : Ast.loc }
  | Struct of { name : string; env : env; loc : Ast.loc }
  | Enum of unit

let isFunction x = match x with Ast.Function _ -> true | _ -> false

let addEntities (entities : Ast.entities list) =
  let rec aux entities env =
    match entities with
    | [] -> env
    | head :: tail ->
        let name, loc =
          match head with
          | Ast.Function f -> (Get.Ast.name f.name, Get.Ast.locOfName f.name)
          | _ -> xTODO uPOS "add-entity-to-environment"
        in
        let type' =
          match head with
          | Ast.Function f -> f.type'
          | _ -> xTODO uPOS "add-entity-type-to-environment"
        in
        Hashtbl.replace env.table name (Function { name; type'; loc });
        aux tail env
  in
  aux entities { table = Hashtbl.create 100; scope = [] }
;;

let addScope (env : env) = { env with scope = [] :: env.scope }
let removeScope (env : env) = { env with scope = List.tl env.scope }

(* Initialize an environment of a file, by storing its entities in it *)
let initFileEnv ast = match ast with Ast.File f -> addEntities f.entities
