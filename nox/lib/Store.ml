open Core

let stringTable : (string, int) Hashtbl.t = Hashtbl.create 4096
let nextStringId = ref 0
let nextEntyId = ref 0

(** Insert / intern string into table with a unique ID *)
let internString string =
  match Hashtbl.find_opt stringTable string with
  | Some id -> id
  | None ->
      let id = !nextStringId in
      nextStringId := !nextStringId + 1;
      Hashtbl.add stringTable string id;
      id
;;

let getIdOfString string =
  match Hashtbl.find_opt stringTable string with Some id -> id | None -> failwith "wut?"
;;

module SymbolMap = Map.Make (Int)

type env = { records : unit }

and records =
  | FunctionRecord of {
      name : string;
      args : unit list;
      types : Ast.types;
      uuid : Ast.uuids;
      loc : Ast.loc;
    }
  | VarRecord of { name : string; types : Ast.types; uuid : Ast.uuids; loc : Ast.loc }
