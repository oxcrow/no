open Core

module Module = struct
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

  let getIdOfName name = getIdOfString (Ast.getStringOfName name)

  let reset () =
    Hashtbl.clear stringTable;
    nextStringId := 0;
    nextEntyId := 0
  ;;

  module SymbolMap = Map.Make (Int)

  type record =
    | FunctionRecord of {
        name : string;
        nameId : int;
        args : Ast.vars list;
        types : Ast.types;
        file : string;
        loc : Ast.loc;
      }
    | StructRecord of {
        name : string;
        nameId : int;
        elems : Ast.vars list;
        entys : record list;
        file : string;
        loc : Ast.loc;
      }
    | VarRecord of {
        name : string;
        nameId : int;
        types : Ast.types;
        entyId : int;
        varId : int;
        loc : Ast.loc;
      }

  let printEnv env =
    SymbolMap.iter
      (fun key value ->
        Printf.printf "%d -> %s\n" key
          (match value with
          | FunctionRecord f -> f.name
          | StructRecord s -> s.name
          | VarRecord v -> v.name))
      env
  ;;
end
