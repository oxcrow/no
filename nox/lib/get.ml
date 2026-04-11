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
  let idOfExpr x =
    match x with
    | Qbe.CallExpr y -> y.reg.id
    | Qbe.TermExpr y -> y.reg.id
    | Qbe.BinOpExpr y -> y.reg.id
  ;;

  let ixOfExpr x =
    match x with
    | Qbe.CallExpr y -> y.reg.ix
    | Qbe.TermExpr y -> y.reg.ix
    | Qbe.BinOpExpr y -> y.reg.ix
  ;;
end
