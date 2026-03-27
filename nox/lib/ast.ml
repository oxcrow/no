type file = File of { entities : entities list; file : string }
[@@deriving show { with_path = false }]

(* Top Level Entities *)
and entities =
  | Function of {
      name : names;
      type' : types;
      body : blocks;
      args : vars list;
      scope : scopes;
      loc : loc;
    }
  | Struct of { name : names; value : unit; scope : scopes; loc : loc }
  | Use of { name : names; scope : scopes; import : bool; loc : loc }
  | NoneEnty

(* Statement blocks *)
and blocks = Block of { stmts : stmts list }

(* Statements *)
and stmts =
  | LetStmt of { vars : vars list; expr : exprs; loc : loc }
  | SetStmt of { expr : exprs; loc : loc }
  | AssignStmt of { vars : exprs list; expr : exprs; loc : loc }
  | ReturnStmt of { expr : exprs; loc : loc }
  | InvokeStmt of { expr : exprs; loc : loc }
  | BlockStmt of { block : blocks; loc : loc }
  | IfStmt of { expr : exprs; loc : loc }
  | NoneStmt

(* Expressions inside each statement  *)
and exprs =
  | InvokeExpr of { name : names; args : exprs list; id : int; loc : loc }
  | BinOpExpr of { lexpr : exprs; op : biops; rexpr : exprs; id : int; loc : loc }
  | UnOpExpr of { expr : exprs; op : unops; id : int; loc : loc }
  | IfExpr of { cond : exprs; block : blocks; rest : exprs option; id : int; loc : loc }
  | ElseIfExpr of {
      cond : exprs;
      block : blocks;
      rest : exprs option;
      id : int;
      loc : loc;
    }
  | ElseExpr of { block : blocks; id : int; loc : loc }
  | BlockExpr of { block : blocks; id : int; loc : loc }
  | EntityExpr of { value : entities; id : int; loc : loc }
  | TupleVal of { value : exprs list; id : int }
  | FloatVal of { value : string; id : int }
  | BoolVal of { value : bool; id : int }
  | IntVal of { value : string; id : int }
  | UndefinedVal of { id : int }
  | UnitVal of { id : int }
  | IdVal of { name : names; id : int }

(* Argument variables *)
and vars =
  | Var of { name : names; state : states; shadow : bool; type' : types option }
  | NoneVar

(* Binary operations *)
and biops =
  | AddOp
  | SubOp
  | MulOp
  | DivOp
  | DotOp
  | EqOp
  | NeOp
  | LeOp
  | GeOp
  | LtOp
  | GtOp

(* Unary operations *)
and unops = PosOp | NegOp | NotOp | TryOp | ConRefOp | MutRefOp | DerefOp

(* Types *)
and types =
  | UnitType
  | UndefinedType
  | IntType
  | FloatType
  | BoolType
  | TupleType of { types : types list }
  | ConRefType of { life : names option; types : types }
  | MutRefType of { life : names option; types : types }
  | FunctionType of { args : types list; type' : types }
  | NoneType

(* States *)
and states = ImmutableState | MutableState

(* Scopes *)
and scopes = PublicScope | PrivateScope | LocalScope

(* Names *)
and names = Name of { name : string; loc : loc }

(* Location *)
and loc = (lox[@opaque])
and lox = Location of { lineIdx : int; colIdx : int } | Nowhere

(** Extract location from token *)
let tokenLocation (token : Token.uToken) : loc =
  Location { lineIdx = token.lineIdx; colIdx = token.colIdx }
;;
