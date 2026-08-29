open Core

module Module = struct
  let stringTable : (string, int) Hashtbl.t = Hashtbl.create 4096
  let nextStringId = ref 0

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

  let reset () =
    Hashtbl.clear stringTable;
    !nextStringId = 0
  ;;
end
