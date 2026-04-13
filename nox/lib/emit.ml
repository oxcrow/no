open Core

let rec emitDefinition def =
  let fmt = Printf.sprintf in
  match def with
  | Qbe.Function f ->
      let l1 =
        fmt "%sfunction %s $%s() {\n" (emitExport f.export) (emitType f.type') f.name
      in
      let l2 = "@start\n" in
      let l3 = emitStmts f.stmts in
      let l4 = "}\n" in
      let l5 = "\n" in
      l1 ^ l2 ^ l3 ^ l4 ^ l5
  | _ -> xTODO uPOS "emit-def"

and emitStmts stmts =
  let rec aux stmts acc =
    match stmts with
    | [] -> List.rev acc
    | head :: tail ->
        let stmt = "\t" ^ emitStmt head in
        aux tail (stmt :: acc)
  in
  aux stmts [] |> String.concat ""

and emitStmt stmt =
  let fmt = Printf.sprintf in
  match stmt with
  | Qbe.LetStmt s ->
      fmt "%%%d.%d = %s %s\n" s.id s.ix
        (emitType (Get.Qbe.typeOfExpr s.expr))
        (emitExpr s.expr)
  | Qbe.ReturnStmt s -> fmt "ret %%%d.%d\n" s.id s.ix

and emitExpr expr = "# (to-do)"
and emitExport export = match export with true -> "export " | _ -> ""

and emitType type' =
  match type' with
  | Qbe.VoidType -> ""
  | Qbe.LongType -> "l"
  | _ -> xTODO uPOS "emit-type"
;;

let emitQbe (defs : Qbe.definitions list) =
  List.map emitDefinition defs |> String.concat ""
;;
