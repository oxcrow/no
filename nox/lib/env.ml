open Core

type env = { table : (string, record) Hashtbl.t; scope : string list list }

and record =
  | Function of { name : string; args : Ast.vars list; type' : Ast.types; loc : Ast.loc }
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
          | Ast.Function f ->
              (Get.Ast.name f.name, match f.name with Ast.Name n -> n.loc)
          | _ -> xTODO uPOS "add-entity-to-environment"
        in
        let type' =
          match head with
          | Ast.Function f -> f.type'
          | _ -> xTODO uPOS "add-entity-type-to-environment"
        in
        let args =
          match head with
          | Ast.Function f -> f.args
          | _ -> xTODO uPOS "add-entity-arguments-to-environment"
        in
        Hashtbl.replace env.table name (Function { name; args; type'; loc });
        aux tail env
  in
  aux entities { table = Hashtbl.create 100; scope = [] }
;;

let addScope (env : env) = { env with scope = [] :: env.scope }

let removeScope (env : env) =
  let lastScope = match List.nth_opt env.scope 0 with Some x -> x | None -> [] in
  let rec removeRecord env records =
    match records with
    | [] -> ()
    | head :: tail ->
        Hashtbl.remove env.table head;
        removeRecord env tail
  in
  removeRecord env lastScope;
  {
    env with
    scope =
      (match List.length env.scope > 0 with true -> List.tl env.scope | false -> []);
  }
;;

let addRecord (env : env) (record : record) (name : string) =
  let addToFirstList list elem =
    (elem :: (match List.length list > 0 with true -> first list | false -> []))
    :: (match List.length list > 0 with true -> List.tl list | false -> [])
  in
  Hashtbl.add env.table name record;
  { env with scope = addToFirstList env.scope name }
;;

(* Initialize an environment of a file, by storing its entities in it *)
let initFileEnv ast = match ast with Ast.File f -> addEntities f.entities

let getFunctionType (env : env) (name : string) =
  match Hashtbl.find_opt env.table name with
  | Some (Function f) -> Some f.type'
  | _ -> xNEVER uPOS "wut?"
;;
