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
      fmt "%%x%d.%d = %s %s\n" s.id s.ix
        (emitType (Get.Qbe.typeOfExpr s.expr))
        (emitExpr s.expr)
  | Qbe.ReturnStmt s -> (
      (* Using a bool flag feels wrong. I don't like this. *)
      match s.void with
      | true -> "ret\n"
      | false -> fmt "ret %%x%d.%d\n" s.id s.ix)

and emitExpr expr =
  let fmt = Printf.sprintf in
  match expr with
  | Qbe.CallExpr x -> fmt "call $%s()" x.name
  | Qbe.TermExpr x -> fmt "copy %s" x.value
  | Qbe.BinOpExpr x -> fmt "%s %s, %s" (emitBinOp x.op) (emitReg x.lreg) (emitReg x.rreg)

and emitExport export = match export with true -> "export " | _ -> ""

and emitBinOp op =
  match op with
  | Qbe.AddOp -> "add"
  | Qbe.SubOp -> "sub"
  | Qbe.MulOp -> "mul"
  | Qbe.DivOp -> "div"
  | Qbe.RemOp -> "rem"
  | Qbe.UDivOp -> "udiv"
  | Qbe.URemOp -> "urem"

and emitType type' =
  match type' with
  | Qbe.VoidType -> ""
  | Qbe.LongType -> "l"
  | Qbe.WordType -> "w"
  | Qbe.SingleType -> "s"
  | Qbe.DoubleType -> "d"
  | _ -> xTODO uPOS "emit-type"

and emitReg register =
  let fmt = Printf.sprintf in
  fmt "%%x%d.%d" register.id register.ix
;;

let emitQbe (defs : Qbe.definitions list) = List.map emitDefinition defs
