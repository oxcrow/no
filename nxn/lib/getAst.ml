open Error.Failure

(* Location *)
external loc : string = "%loc_LOC"

module Loc = struct
  let lnum x = match x with Ast.Loc y -> y.lnum
  let cnum x = match x with Ast.Loc y -> y.cnum
end

module Pos = struct
  let xpos x = match x with Ast.Pos y -> (Loc.lnum y.start, Loc.cnum y.start)
end

module Id = struct
  let value x = match x with Ast.Id y -> y.value
  let xpos x = match x with Ast.Id y -> Pos.xpos y.pos
end

module Type = struct end

module Var = struct
  let name x =
    match x with
    | Ast.Var y -> Id.value y.name
    | NoneVar -> never loc "NoneVar is not a valid variable."
  ;;

  let type' x =
    match x with
    | Ast.Var y -> y.type'
    | NoneVar -> never loc "NoneVar is not a valid variable."
  ;;

  let xpos x =
    match x with
    | Ast.Var y -> Id.xpos y.name
    | NoneVar -> never loc "NoneVar is not a valid variable"
  ;;
end

module Expr = struct
  let id expr =
    match expr with
    | Ast.TerminalExpr t -> (
        match t.value with
        | Ast.IdVal x ->
            let id = Id.value x.value in
            Some id
        | _ -> None)
    | _ -> None
  ;;

  let type' x =
    match x with
    | Ast.TerminalExpr y -> y.type'
    | Ast.InvokeExpr y -> y.type'
    | Ast.BinOpExpr y -> y.type'
    | Ast.UnOpExpr y -> y.type'
    | Ast.IfExpr y -> y.type'
    | Ast.ElseIfExpr y -> y.type'
    | Ast.ElseExpr y -> y.type'
    | Ast.BlockExpr y -> y.type'
    | _ -> failwith @@ "Implement method to get expresison type."
  ;;

  let pos x =
    match x with
    | Ast.TerminalExpr y -> y.pos
    | Ast.InvokeExpr y -> y.pos
    | Ast.BinOpExpr y -> y.pos
    | Ast.UnOpExpr y -> y.pos
    | Ast.IfExpr y -> y.pos
    | Ast.ElseIfExpr y -> y.pos
    | Ast.ElseExpr y -> y.pos
    | Ast.BlockExpr y -> y.pos
    | _ -> failwith @@ "Implement method to get expresison type."
  ;;

  let xpos x =
    match x with
    | Ast.TerminalExpr y -> Pos.xpos y.pos
    | Ast.InvokeExpr y -> Pos.xpos y.pos
    | Ast.BinOpExpr y -> Pos.xpos y.pos
    | Ast.UnOpExpr y -> Pos.xpos y.pos
    | Ast.IfExpr y -> Pos.xpos y.pos
    | Ast.ElseIfExpr y -> Pos.xpos y.pos
    | Ast.ElseExpr y -> Pos.xpos y.pos
    | Ast.BlockExpr y -> Pos.xpos y.pos
    | _ -> failwith @@ "Implement method to get expresison position."
  ;;
end

module Stmt = struct
  let vars x =
    match x with
    | Ast.LetStmt y -> y.vars
    | _ -> never loc "Only let statements can have variables"
  ;;

  let type' x =
    match x with
    | Ast.SetStmt y -> Expr.type' y.expr
    | _ -> todo loc "Type for statement"
  ;;

  let xpos x =
    match x with
    | Ast.SetStmt y -> Pos.xpos y.pos
    | _ -> todo loc "Position for statement"
  ;;
end

module Block = struct
  let xpos x = match x with Ast.Block y -> Pos.xpos y.pos
end

module Entity = struct
  let xpos x =
    match x with
    | Ast.Function y -> Id.xpos y.name
    | Ast.Struct y -> Id.xpos y.name
  ;;
end

module File = struct
  let name x = match x with Ast.File y -> y.file
end
