type file = File of { entities : entities list; file : string }
[@@deriving show { with_path = false }]

(* Entities inside each file *)
and entities =
  | Function of {
      name : id;
      args : vars list;
      type' : types;
      block : blocks;
      pos : pos;
    }
  | Struct of { name : id; pos : pos }

(* Blocks of codde inside each function *)
and blocks = Block of { stmts : stmts list; pos : pos }

(* Statements inside each block *)
and stmts =
  | LetStmt of { vars : vars list; expr : exprs; pos : pos }
  | SetStmt of { expr : exprs; pos : pos }
  | AssignStmt of { vars : exprs list; expr : exprs; pos : pos }
  | ReturnStmt of { expr : exprs; pos : pos }
  | InvokeStmt of { expr : exprs; pos : pos }
  | BlockStmt of { block : blocks; pos : pos }
  | IfStmt of { expr : exprs; pos : pos }
  | NoneStmt

(* Expressions inside each statement  *)
and exprs =
  | TerminalExpr of { value : terms; type' : types; pos : pos }
  | InvokeExpr of { value : id; args : exprs list; type' : types; pos : pos }
  | BinOpExpr of {
      lvalue : exprs;
      op : biops;
      rvalue : exprs;
      type' : types;
      pos : pos;
    }
  | UnOpExpr of { value : exprs; op : unops; type' : types; pos : pos }
  | IfExpr of {
      cond : exprs;
      block : blocks;
      other : exprs option;
      is_stmt : bool;
      type' : types;
      pos : pos;
    }
  | ElseIfExpr of {
      cond : exprs;
      block : blocks;
      other : exprs option;
      type' : types;
      pos : pos;
    }
  | ElseExpr of { block : blocks; type' : types; pos : pos }
  | BlockExpr of { block : blocks; type' : types; pos : pos }
  | EntityExpr of { value : entities; type' : types; pos : pos }

(* Argument variables *)
and vars =
  | Var of { name : id; state : states; shadow : bool; type' : types }
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

(* Terminals *)
and terms =
  | UnitVal
  | UndefinedVal
  | IntVal of { value : int }
  | FloatVal of { value : float }
  | BoolVal of { value : bool }
  | IdVal of { value : id }
  | TupleVal of { value : exprs list }

(* Types *)
and types =
  | UnitType
  | UndefinedType
  | IntType
  | FloatType
  | BoolType
  | TupleType of { types : types list }
  | ConRefType of { life : id option; types : types }
  | MutRefType of { life : id option; types : types }
  | FunctionType of { args : types list; type' : types }
  | NoneType

(* Mutability states of variables *)
and states = ConState | MutState | SetState

(* Identifiers *)
and id = Id of { value : string; pos : pos }

(* Location of each token *)
and pos = (poc[@opaque])
and loc = Loc of { lnum : int; cnum : int }
and poc = Pos of { start : loc; end' : loc }
