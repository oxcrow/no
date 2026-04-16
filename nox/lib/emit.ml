open Core

let fmt = Printf.sprintf

let rec emitDefinition def =
  match def with
  | Qbe.Function f ->
      let l1 =
        fmt "%sfunction %s $%s(%s) {\n" (emitExport f.export) (emitType f.type') f.name
          (emitArgs f.args)
      in
      let l2 = "@start\n" in
      let l3 = emitStmts f.stmts in
      let l4 = "}\n" in
      let l5 = "\n" in
      l1 ^ l2 ^ l3 ^ l4 ^ l5
  | _ -> xTODO uPOS "emit-def"

and emitArgs args =
  let rec aux args acc =
    match args with
    | [] -> String.concat ", " acc
    | (head : Qbe.args) :: tail ->
        let arg = fmt "%s %%x.%s" (emitType head.type') head.name in
        aux tail (arg :: acc)
  in
  aux args []

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
      fmt "%%x.%s = %s %s\n" s.name
        (emitType (Get.Qbe.typeOfExpr s.expr))
        (emitExpr s.expr)
  | Qbe.ReturnStmt s -> (
      (* Using a bool flag feels wrong. I don't like this. *)
      match s.void with
      | true -> "ret\n"
      | false -> fmt "ret %%x.%s\n" s.name)

and emitExpr expr =
  let fmt = Printf.sprintf in
  match expr with
  | Qbe.CallExpr x -> fmt "call $%s(%s)" x.name (emitArgExprs x.args)
  | Qbe.TermExpr x -> fmt "copy %s" x.value
  | Qbe.BinOpExpr x -> fmt "%s %s, %s" (emitBinOp x.op) (emitReg x.lreg) (emitReg x.rreg)
  | Qbe.IdValExpr x -> fmt "copy %%x.%s" x.name
  | Qbe.RegExpr x -> fmt "%s" (emitReg x.reg)

and emitArgExprs args =
  let rec aux args acc =
    match args with
    | [] -> String.concat ", " (List.rev acc)
    | head :: tail ->
        let arg =
          fmt "%s %%x.%s" (emitType (Get.Qbe.typeOfExpr head)) (Get.Qbe.regOfExpr head)
        in
        aux tail (arg :: acc)
  in
  aux args []

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
  fmt "%%x.%s" register.name
;;

let emitQbe (defs : Qbe.definitions list) = List.map emitDefinition defs
