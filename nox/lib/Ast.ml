open Core

type file = File of { modules : modules list; entities : entities list; file : string }
[@@deriving show { with_path = false }]

(* Top level module imports *)
and modules = Mod of { name : names; loc : loc }

(* Top level entities in each file *)
and entities =
  | Function of { scope : scopes; name : names; block : stmts list; loc : loc }
  | Struct of { scope : scopes; name : names; elems : vars list; entys : entities list; loc : loc }
  | NoneEnty

(* Statements *)
and stmts =
  | LetStmt of { vars : pats list; expr : exprs; stmtId : int; loc : loc }
  | ReturnStmt of { expr : exprs; stmtId : int; loc : loc }
  | YieldStmt of { expr : exprs; stmtId : int; loc : loc }
  | AssignStmt of { vars : exprs list; expr : exprs; stmtId : int; loc : loc }
  | CallStmt of { expr : exprs; stmtId : int; loc : loc }
  | IfStmt of { expr : exprs; stmtId : int; loc : loc }

(* Exprssions *)
and exprs =
  | GhostExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | ResultExpr of { expr : exprs option; types : types; exprId : int; loc : loc }
  | OptionExpr of { expr : exprs option; types : types; exprId : int; loc : loc }
  | StructExpr of {
      name : names option;
      elems : elems list;
      types : types;
      exprId : int;
      loc : loc;
    }
  | TupleExpr of { exprs : exprs list; types : types; exprId : int; loc : loc }
  | ArrayExpr of { exprs : exprs list; types : types; exprId : int; loc : loc }
  | SliceExpr of { expr : exprs; args : exprs list; types : types; exprId : int; loc : loc }
  | CallExpr of { name : names; args : exprs list; types : types; exprId : int; loc : loc }
  | TryExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | UnwrapExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | DerefExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | ConRefExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | MutRefExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | TypeExpr of { types : types; exprId : int; loc : loc }
  | AddExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | SubExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | MulExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | DivExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | DotExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | NotEqExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | EqEqExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | LtEqExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | GtEqExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | LtExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | GtExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | IfExpr of {
      cond : exprs;
      block : stmts list;
      rest : exprs option;
      types : types;
      exprId : int;
      loc : loc;
    }
  | ElseIfExpr of {
      cond : exprs;
      block : stmts list;
      rest : exprs option;
      types : types;
      exprId : int;
      loc : loc;
    }
  | ElseExpr of { block : stmts list; types : types; exprId : int; loc : loc }
  | NameExpr of { value : names; types : types; exprId : int; loc : loc }
  | IntExpr of { value : string; types : types; exprId : int; loc : loc }
  | LaterExpr of { exprId : int; loc : loc }
  | NoneExpr

(* Elements *)
and elems = Elem of { name : names; expr : exprs; loc : loc }

(* Patterns *)
and pats =
  | TuplePattern of { pats : pats list }
  | ArrayPattern of { pats : pats list }
  | LonePattern of { var : vars }

(* Variables *)
and vars = Var of { state : states; name : names; type' : types; varId : int }

(* Types of nodes *)
and types =
  | TupleType of { types : types list; offsets : int list; align : int; size : int }
  | StructType of { types : entities; offsets : int list; align : int; size : int }
  | ResultType of { types : types; align : int; size : int }
  | OptionType of { types : types; align : int; size : int }
  | ConRefType of { types : types }
  | NameType of { name : names }
  | BoolType
  | FloatType
  | IntType
  | UnitType
  | TodoType
  | NoneType

(* States to control how variables mutate *)
and states = ConState | MutState | SetState

(* Scopes of entities *)
and scopes = ExportScope | ModuleScope | LocalScope

(* Names of identifiers *)
and names =
  | Name of { name : string; nameId : int; loc : loc }
  | Nick of { name : string; nameId : int; loc : loc }

(* Location *)
and loc = (lox[@opaque])
and lox = Location of { lineIndex : int; colIndex : int } | Nowhere
