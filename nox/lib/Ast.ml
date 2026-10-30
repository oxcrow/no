open Core

type file = File of { modules : modules list; entities : entities list; file : string }
[@@deriving show { with_path = false }]

(* Top level module imports *)
and modules = Mod of { name : names; loc : loc }

(* Top level entities in each file *)
and entities =
  | Function of {
      scope : scopes;
      name : names;
      args : int list;
      block : blocks;
      types : types;
      entyId : int;
      loc : loc;
    }

(* Blocks of statements *)
and blocks = Block of { stmts : stmts list; types : types }

(* Statements *)
and stmts =
  | ReturnStmt of { expr : exprs; stmtId : int; loc : loc }
  | YieldStmt of { expr : exprs; stmtId : int; loc : loc }
  | CallStmt of { expr : exprs; stmtId : int; loc : loc }

(* Exprssions *)
and exprs =
  | CallExpr of { name : names; args : exprs list; types : types; exprId : int; loc : loc }
  | AddExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | SubExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | MulExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | DivExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | NameExpr of { value : names; types : types; exprId : int; loc : loc }
  | IntExpr of { value : string; types : types; exprId : int; loc : loc }
  | UnitExpr of { types : types; exprId : int; loc : loc }

(* Types of nodes *)
and types = IntType | UnitType | TodoType

(* States to control how variables mutate *)
and states = ConState | MutState | SetState

(* Scopes of entities *)
and scopes = ExportScope | ModuleScope | LocalScope

(* Names of identifiers *)
and names = Name of { name : string; nameKind : nameKinds; nameId : int; uuid : uuids; loc : loc }
and nameKinds = IdentifierName | ReservedName

(* Unique identifiers for everything *)
and uuids = { modId : int; fileId : int; entyId : int; varId : int; elemId : int }

(* Location *)
and loc = (lox[@opaque])
and lox = Location of { lineIndex : int; colIndex : int } | Nowhere

let getTypeOfExpr e =
  match e with
  | CallExpr e -> e.types
  | AddExpr e -> e.types
  | SubExpr e -> e.types
  | MulExpr e -> e.types
  | DivExpr e -> e.types
  | NameExpr e -> e.types
  | IntExpr e -> e.types
  | UnitExpr e -> e.types
;;

let getLocOfExpr e =
  match e with
  | CallExpr e -> e.loc
  | AddExpr e -> e.loc
  | SubExpr e -> e.loc
  | MulExpr e -> e.loc
  | DivExpr e -> e.loc
  | NameExpr e -> e.loc
  | IntExpr e -> e.loc
  | UnitExpr e -> e.loc
;;
