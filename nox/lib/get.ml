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
end

(* *)
module Qbe = struct
  let nameOfReg (x : Qbe.regs) = x.name

  let regOfExpr x =
    match x with
    | Qbe.AllocExpr y -> y.var.name
    | Qbe.CallExpr y -> y.var.name
    | Qbe.BinOpExpr y -> y.var.name
    | Qbe.FieldExpr y -> y.var.name
    | Qbe.IdValExpr y -> y.var.name
    | Qbe.RegExpr y -> y.var.name
    | Qbe.TermExpr y -> y.var.name
    | Qbe.BlockExpr y -> y.var.name
    | Qbe.StoreExpr y -> y.var.name
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
  let getQbeRegOfExpr = Qbe.regOfExpr
  let getQbeTypeOfExpr = Qbe.typeOfExpr
  let getQbeNameOfReg = Qbe.nameOfReg
end
