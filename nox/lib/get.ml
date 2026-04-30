open Core

(* *)
module Ast = struct
  let name x = match x with Ast.Name n -> n.name

  let locOfLoc x =
    match x with
    | Ast.Location l -> (l.lineIdx, l.colIdx)
    | Ast.Nowhere -> xNEVER uPOS "invalid-location"
  ;;

  let locOfName x =
    let simplify y =
      match y with
      | Ast.Location l -> (l.lineIdx, l.colIdx)
      | Ast.Nowhere -> xNEVER uPOS "invalid-location"
    in
    match x with Ast.Name n -> simplify n.loc
  ;;

  let nameOfVar x = match x with Ast.Variable v -> name v.name
  let typeOfVar x = match x with Ast.Variable v -> v.type'
  let idOfVar x = match x with Ast.Variable v -> v.id

  let locofEntity x =
    let simplify y =
      match y with
      | Ast.Location l -> (l.lineIdx, l.colIdx)
      | Ast.Nowhere -> xNEVER uPOS "invalid-location"
    in
    match x with
    | Ast.Function y -> simplify y.loc
    | Ast.Struct y -> simplify y.loc
    | Ast.Use y -> simplify y.loc
    | _ -> xTODO uPOS "loc-of-entity"
  ;;

  let locOfExpr x =
    match x with
    | Ast.InvokeExpr y -> y.loc
    | Ast.BinOpExpr y -> y.loc
    | Ast.UnOpExpr y -> y.loc
    | Ast.IfExpr y -> y.loc
    | Ast.ElseIfExpr y -> y.loc
    | Ast.ElseExpr y -> y.loc
    | Ast.BlockExpr y -> y.loc
    | Ast.EntityExpr y -> y.loc
    | Ast.TupleVal y -> y.loc
    | Ast.FloatVal y -> y.loc
    | Ast.BoolVal y -> y.loc
    | Ast.IntVal y -> y.loc
    | Ast.UndefinedVal y -> y.loc
    | Ast.UnitVal y -> y.loc
    | Ast.IdVal y -> y.loc
  ;;

  let idOfExpr x =
    match x with
    | Ast.InvokeExpr y -> y.id
    | Ast.BinOpExpr y -> y.id
    | Ast.UnOpExpr y -> y.id
    | Ast.IfExpr y -> y.id
    | Ast.ElseIfExpr y -> y.id
    | Ast.ElseExpr y -> y.id
    | Ast.BlockExpr y -> y.id
    | Ast.EntityExpr y -> y.id
    | Ast.TupleVal y -> y.id
    | Ast.FloatVal y -> y.id
    | Ast.BoolVal y -> y.id
    | Ast.IntVal y -> y.id
    | Ast.UndefinedVal y -> y.id
    | Ast.UnitVal y -> y.id
    | Ast.IdVal y -> y.id
  ;;

  let typeOfExpr types x = types.(idOfExpr x)

  let blockOfExpr x =
    match x with
    | Ast.BlockExpr b -> b.block
    | _ -> xNEVER uPOS "Expected block expression."
  ;;

  let stmtsOfBlock x = match x with Ast.Block b -> b.stmts
end

(* *)
module Cfg = struct
  let regOfStmt x =
    match x with
    | Cfg.LetStmt y -> y.reg
    | Cfg.SetStmt y -> y.reg
    | Cfg.RetStmt y -> y.reg
    | Cfg.CmdStmt y -> None
    | Cfg.NoneStmt _ -> None
  ;;

  let regOfExpr x =
    match x with
    | Cfg.TupleExpr y -> y.reg
    | Cfg.CallExpr y -> y.reg
    | Cfg.BlockExpr y -> y.reg
    | Cfg.LoadExpr y -> y.reg
    | Cfg.RegExpr y -> y.reg
    | Cfg.IdExpr y -> y.reg
    | Cfg.IntExpr y -> y.reg
    | Cfg.UnitExpr y -> y.reg
    | _ -> xTODO uPOS "reg-of-expr"
  ;;

  let splitRegOfExpr x =
    match regOfExpr x with
    | Some r ->
        let regKind, regBase, regIdx = r in
        (regKind, regBase, regIdx)
    | None -> xNEVER uPOS "wut?"
  ;;

  let stmtsOfBlock x = match x with Cfg.Block b -> b.stmts
end

(* *)
module Qbe = struct
  let nameOfReg (x : Qbe.regs) =
    Printf.sprintf "%s%d.%d"
      (match x.kind with
      | Qbe.AllocReg -> "a"
      | Qbe.FieldReg -> "f"
      | Qbe.LoadReg -> "l"
      | Qbe.DataReg -> "d")
      x.baseId x.nextId
  ;;

  let regOfExpr x =
    match x with
    | Qbe.AllocExpr y -> nameOfReg y.var
    | Qbe.CallExpr y -> nameOfReg y.var
    | Qbe.BinOpExpr y -> nameOfReg y.var
    | Qbe.FieldExpr y -> nameOfReg y.var
    | Qbe.IdValExpr y -> nameOfReg y.var
    | Qbe.RegExpr y -> nameOfReg y.var
    | Qbe.TermExpr y -> nameOfReg y.var
    | Qbe.BlockExpr y -> nameOfReg y.var
    | Qbe.LoadExpr y -> nameOfReg y.var
    | Qbe.StoreExpr y -> nameOfReg y.var
    | _ -> xTODO uPOS (Qbe.show_exprs x)
  ;;

  let typeOfExpr x =
    match x with
    | Qbe.AllocExpr y -> y.type'
    | Qbe.CallExpr y -> y.type'
    | Qbe.TermExpr y -> y.type'
    | Qbe.BinOpExpr y -> y.type'
    | Qbe.FieldExpr y -> y.type'
    | Qbe.LoadExpr y -> y.type'
    | Qbe.IdValExpr y -> y.type'
    | Qbe.RegExpr y -> y.type'
    | Qbe.BlockExpr y -> y.type'
    | Qbe.StoreExpr y -> y.type'
    | _ -> xTODO uPOS "wut?"
  ;;
end

module X = struct
  let getAstName = Ast.name
  let getAstNameOfVar = Ast.nameOfVar
  let getAstTypeOfVar = Ast.typeOfVar
  let getAstIdOfExpr = Ast.idOfExpr
  let getAstTypeOfExpr = Ast.typeOfExpr
  let getAstStmtsOfBlock = Ast.stmtsOfBlock
  let getQbeRegOfExpr = Qbe.regOfExpr
  let getQbeTypeOfExpr = Qbe.typeOfExpr
  let getQbeNameOfReg = Qbe.nameOfReg
end
