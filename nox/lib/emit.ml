open Core
open Get.X

let fmt = Printf.sprintf

(** Emit definitions in QBE IR *)
let rec emitDef def =
  match def with
  | Qbe.Function f ->
      let l1 =
        fmt "%sfunction %s $%s(%s) {\n" (emitExport f.scope) (emitType f.type') f.name
          (emitArgs f.args)
      in
      let l2 = emitStmts f.stmts in
      let l3 = "}\n" in
      let l4 = "\n" in
      l1 ^ l2 ^ l3 ^ l4
  | _ -> "# (todo-def)\n"

and emitStmts stmts =
  let rec aux stmts acc =
    match stmts with
    | [] -> List.rev acc
    | h :: t ->
        let stmt = emitStmt h in
        aux t (stmt :: acc)
  in
  aux stmts [] |> String.concat ""

and emitStmt stmt =
  let fmt = Printf.sprintf in
  match stmt with
  | Qbe.BlockStmt s -> fmt "@%s\n" s.name
  | Qbe.LetStmt s ->
      fmt "\t%%x.%s = %s %s\n" (getQbeNameOfReg s.var)
        (emitType (Get.Qbe.typeOfExpr s.expr))
        (emitExpr s.expr)
  | Qbe.StoreStmt s ->
      let type', from, dest =
        match s.expr with
        | Qbe.StoreExpr e -> (e.type', e.from, e.dest)
        | _ -> xNEVER uPOS "wut?"
      in
      fmt "\tstore%s %s, %s\n" (emitType type') (emitReg from) (emitReg dest)
  | Qbe.ReturnStmt s -> (
      match s.var with
      | Some reg -> fmt "\tret %%x.%s\n" (getQbeNameOfReg reg)
      | None -> "\tret\n")
  | _ -> xTODO uPOS "emit-stmt"

and emitExpr expr =
  let fmt = Printf.sprintf in
  match expr with
  | Qbe.AllocExpr e -> fmt "alloc%d %d" e.align e.size
  | Qbe.CallExpr e -> fmt "call $%s(%s)" e.name (emitArgExprs e.args)
  | Qbe.TermExpr e -> fmt "copy %s" e.value
  | Qbe.BinOpExpr e -> fmt "%s %s, %s" (emitBinOp e.op) (emitReg e.lreg) (emitReg e.rreg)
  | Qbe.FieldExpr e -> fmt "add %s, %d" (emitReg e.var) e.offset
  | Qbe.LoadExpr e -> fmt "load %s" (emitReg e.from)
  | Qbe.IdValExpr e -> fmt "copy %%x.%s" e.name
  | Qbe.RegExpr e -> fmt "copy %s" (emitReg e.var)
  | _ -> xTODO uPOS "emit-expr"

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

and emitArgs args =
  let rec aux args acc =
    match args with
    | [] -> String.concat ", " acc
    | (h : Qbe.args) :: t ->
        let arg = fmt "%s %%x.%s" (emitType h.type') h.name in
        aux t (arg :: acc)
  in
  aux args []

and emitExport scope = match scope with Qbe.ExportScope -> "export " | _ -> ""

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

and emitReg register = fmt "%%x.%s" (getQbeNameOfReg register)

(** Emit QBE IR *)
let emitQbe defs = List.map emitDef defs
