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
      numExprs : int;
      entyId : int;
      loc : loc;
    }

(* Blocks of statements *)
and blocks = Block of { stmts : stmts list; types : types }

(* Statements *)
and stmts =
  | LetStmt of { vars : vars list; pats : pats list; expr : exprs; stmtId : int; loc : loc }
  | ReturnStmt of { expr : exprs; stmtId : int; loc : loc }
  | YieldStmt of { expr : exprs; stmtId : int; loc : loc }
  | CallStmt of { expr : exprs; stmtId : int; loc : loc }
  | IfStmt of { expr : exprs; stmtId : int; loc : loc }

(* Exprssions *)
and exprs =
  | BlockExpr of { block : blocks; types : types; exprId : int; loc : loc }
  | TupleExpr of { exprs : exprs list; types : types; exprId : int; loc : loc }
  | ArrayExpr of { exprs : exprs list; types : types; exprId : int; loc : loc }
  | MoveExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | CopyExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | UnwrapExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | TryExpr of { expr : exprs; types : types; exprId : int; loc : loc }
  | DotExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | CallExpr of { name : names; args : exprs list; types : types; exprId : int; loc : loc }
  | IfExpr of {
      cond : exprs;
      block : blocks;
      rest : exprs option;
      types : types;
      exprId : int;
      loc : loc;
    }
  | ElseIfExpr of {
      cond : exprs;
      block : blocks;
      rest : exprs option;
      types : types;
      exprId : int;
      loc : loc;
    }
  | ElseExpr of { block : blocks; types : types; exprId : int; loc : loc }
  | AddExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | SubExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | MulExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | DivExpr of { lexpr : exprs; rexpr : exprs; types : types; exprId : int; loc : loc }
  | NameExpr of { value : names; types : types; exprId : int; loc : loc }
  | BoolExpr of { value : bools; exprId : int; loc : loc }
  | IntExpr of { value : string; types : types; exprId : int; loc : loc }
  | UnitExpr of { exprId : int; loc : loc }

(* Boolean expressions *)
and bools = True | False

(* Patterns *)
and pats =
  | TuplePattern of { pats : pats list; types : types option }
  | ArrayPattern of { pats : pats list; types : types option }
  | LonePattern of { var : vars }

(* Variables *)
and vars =
  | Var of { state : states; name : names; exprId : int; type' : types; uuid : uuids; loc : loc }

(* Types of nodes *)
and types =
  | TupleType of { types : types list; offsets : int list; align : int; size : int }
  | ArrayType of { types : types; len : int }
  | BoolType
  | IntType
  | UnitType
  | LaterType
  | TodoType

(* States to control how variables mutate *)
and states = ConState | MutState | SetState

(* Scopes of entities *)
and scopes = ExportScope | ModuleScope | LocalScope

(* Names of identifiers *)
and names = Name of { name : string; nameKind : nameKinds; nameId : int; uuid : uuids; loc : loc }
and nameKinds = IdentifierName | ReservedName

(* Unique identifiers for everything *)
and uuids = { modId : int; entyId : int; varId : int; elemId : int }

(* Location *)
and loc = (lox[@opaque])
and lox = Location of { lineIndex : int; colIndex : int } | Nowhere

let getStringOfName n = match n with Name n -> n.name
let getVarIdOfName n = match n with Name n -> n.uuid.varId
let getIdOfName n = match n with Name n -> n.nameId

(* *)
let getNameOfVar v = match v with Var v -> v.name
let getTypeOfVar v = match v with Var v -> v.type'
let getUuidOfVar v = match v with Var v -> v.uuid
let getLocOfVar v = match v with Var v -> v.loc

let getTypeOfExpr e =
  match e with
  | BlockExpr e -> e.types
  | TupleExpr e -> e.types
  | ArrayExpr e -> e.types
  | MoveExpr e -> e.types
  | CopyExpr e -> e.types
  | UnwrapExpr e -> e.types
  | TryExpr e -> e.types
  | DotExpr e -> e.types
  | CallExpr e -> e.types
  | IfExpr e -> e.types
  | ElseIfExpr e -> e.types
  | ElseExpr e -> e.types
  | AddExpr e -> e.types
  | SubExpr e -> e.types
  | MulExpr e -> e.types
  | DivExpr e -> e.types
  | NameExpr e -> e.types
  | BoolExpr _ -> BoolType
  | IntExpr e -> e.types
  | UnitExpr e -> UnitType
;;

let getIdOfExpr e =
  match e with
  | BlockExpr e -> e.exprId
  | TupleExpr e -> e.exprId
  | ArrayExpr e -> e.exprId
  | MoveExpr e -> e.exprId
  | CopyExpr e -> e.exprId
  | UnwrapExpr e -> e.exprId
  | TryExpr e -> e.exprId
  | DotExpr e -> e.exprId
  | CallExpr e -> e.exprId
  | IfExpr e -> e.exprId
  | ElseIfExpr e -> e.exprId
  | ElseExpr e -> e.exprId
  | AddExpr e -> e.exprId
  | SubExpr e -> e.exprId
  | MulExpr e -> e.exprId
  | DivExpr e -> e.exprId
  | NameExpr e -> e.exprId
  | BoolExpr e -> e.exprId
  | IntExpr e -> e.exprId
  | UnitExpr e -> e.exprId
;;

let getLocOfExpr e =
  match e with
  | BlockExpr e -> e.loc
  | TupleExpr e -> e.loc
  | ArrayExpr e -> e.loc
  | MoveExpr e -> e.loc
  | CopyExpr e -> e.loc
  | UnwrapExpr e -> e.loc
  | TryExpr e -> e.loc
  | DotExpr e -> e.loc
  | CallExpr e -> e.loc
  | IfExpr e -> e.loc
  | ElseIfExpr e -> e.loc
  | ElseExpr e -> e.loc
  | AddExpr e -> e.loc
  | SubExpr e -> e.loc
  | MulExpr e -> e.loc
  | DivExpr e -> e.loc
  | NameExpr e -> e.loc
  | BoolExpr e -> e.loc
  | IntExpr e -> e.loc
  | UnitExpr e -> e.loc
;;

let getEntyOfFile f = match f with File f -> f.entities
