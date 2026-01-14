open Error.Failure

(* Location *)
external loc : string = "%loc_LOC"

module Var = struct
  let with_type x t =
    match x with
    | Ast.Var v ->
        Ast.Var { name = v.name; state = v.state; shadow = v.shadow; type' = t }
    | _ -> never loc "Never use with anything else except vars."
  ;;
end

module Stmt = struct
  let with_expr x e =
    match x with
    | Ast.LetStmt y -> Ast.LetStmt { vars = y.vars; expr = e; pos = y.pos }
    | Ast.SetStmt y -> Ast.SetStmt { expr = e; pos = y.pos }
    | Ast.AssignStmt y ->
        Ast.AssignStmt { vars = y.vars; expr = e; pos = y.pos }
    | Ast.ReturnStmt y -> Ast.ReturnStmt { expr = e; pos = y.pos }
    | Ast.InvokeStmt y -> Ast.InvokeStmt { expr = e; pos = y.pos }
    | Ast.IfStmt y -> Ast.IfStmt { expr = e; pos = y.pos }
    | Ast.BlockStmt _ -> todo loc "Block statement"
    | Ast.NoneStmt -> x
  ;;

  let with_vars x v =
    match x with
    | Ast.LetStmt y -> Ast.LetStmt { vars = v; expr = y.expr; pos = y.pos }
    | _ -> never loc "Never use with anything else except let statements"
  ;;

  let with_assign_vars x v =
    match x with
    | Ast.AssignStmt y ->
        Ast.AssignStmt { vars = v; expr = y.expr; pos = y.pos }
    | _ -> never loc "Never use with anything else except assign statements"
  ;;
end

module Expr = struct
  let with_type x t =
    match x with
    | Ast.TerminalExpr y ->
        Ast.TerminalExpr { value = y.value; type' = t; pos = y.pos }
    | Ast.InvokeExpr y ->
        Ast.InvokeExpr
          { value = y.value; args = y.args; type' = t; pos = y.pos }
    | Ast.BinOpExpr y ->
        Ast.BinOpExpr
          {
            lvalue = y.lvalue;
            op = y.op;
            rvalue = y.rvalue;
            type' = t;
            pos = y.pos;
          }
    | Ast.UnOpExpr y ->
        Ast.UnOpExpr { value = y.value; op = y.op; type' = t; pos = y.pos }
    | Ast.IfExpr _ -> todo loc "If expression"
    | Ast.ElseIfExpr _ -> todo loc "Else if expression"
    | Ast.ElseExpr _ -> todo loc "Else expression"
    | Ast.EntityExpr _ -> todo loc "Entity expression"
    | Ast.BlockExpr _ -> todo loc "Block expression"
  ;;

  let with_args x a =
    match x with
    | Ast.InvokeExpr y ->
        Ast.InvokeExpr
          { value = y.value; args = a; type' = y.type'; pos = y.pos }
    | _ -> never loc "Never use with anything else except invoke exprs"
  ;;
end
