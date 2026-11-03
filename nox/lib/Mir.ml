open Core

type modules = Module of { entities : entities list } [@@deriving show { with_path = false }]

(* Entity definitions that exist in the file *)
and entities =
  | Function of { name : string; block : blocks; types : types; ast : Ast.entities }
  | NoneEnty

(* Block that contains instructions to be compiled *)
and blocks = Block of { blockId : int; stmts : stmts list; ast : Ast.blocks }

(* Statements that emit things *)
and stmts =
  | BindStmt of { selfId : int; exprId : int; ast : Ast.stmts }
  | RetStmt of { exprId : int; ast : Ast.stmts }
  | IfStmt of string
  | NoneStmt

and exprs =
  | MoveExpr of { exprId : int; types : types; ast : Ast.exprs }
  | LockExpr of { exprId : int; types : types; ast : Ast.exprs }
  | LoanExpr of { exprId : int; types : types; ast : Ast.exprs }
  | CopyExpr of { exprId : int; types : types; ast : Ast.exprs }
  | BoolExpr of { exprId : int; value : bools; ast : Ast.exprs }
  | IntExpr of { exprId : int; value : string; ast : Ast.exprs }
  | UnitExpr of { exprId : int; ast : Ast.exprs }

and bools = True | False

(* High level types *)
and types = Ast.types

(* Scope of the exported element *)
and scopes = ExportScope | ModuleScope

(* Location *)
and loc = Ast.loc
