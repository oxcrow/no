open Core

type env = { table : (string, record) Hashtbl.t }

and record =
  | Function of { name : string; type' : Ast.types; loc : Ast.loc }
  | Variable of { name : string; type' : Ast.types; loc : Ast.loc }
  | Struct of { name : string; env : env; loc : Ast.loc }
  | Enum of unit

let isFunction x = match x with Ast.Function _ -> true | _ -> false

(* *)
let addEntities entities =
  let rec aux entities env =
    match entities with
    | [] -> env
    | head :: tail ->
        let name, loc =
          match head with
          | Ast.Function f -> (Get.Ast.name f.name, Get.Ast.locOfName f.name)
          | _ -> xTODO uPOS "add-entity-to-environment"
        in
        Hashtbl.replace env.table name (Function { name; type' = Ast.UnitType; loc });
        aux tail env
  in
  aux entities { table = Hashtbl.create 100 }
;;

(* *)
let initFileEnv ast = match ast with Ast.File f -> addEntities f.entities
