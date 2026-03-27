type file = File of { entities : entities list; file : string }

(* Top Level Entities *)
and entities =
  | Function of { name : names; type' : types; body : block; scope : scopes; loc : loc }
  | Struct of { name : string; value : unit; scope : scopes; loc : loc }
  | Use of { name : string; scope : scopes; loc : loc }
  | NoneEnty

(* Statement blocks *)
and block = stmts list

(* Statements *)
and stmts = LetStmt | AssignStmt | InvokeStmt | YieldStmt | ReturnStmt

(* Types *)
and types = UnitType | BoolType | IntType | FloatType

(* Scopes *)
and scopes = PublicScope | PrivateScope | LocalScope

(* Names *)
and names = string

(* Location *)
and loc = (lox[@opaque])
and lox = Location of { lineIdx : int; colIdx : int }

(** Extract location from token *)
let tokenLocation (token : Token.uToken) : loc =
  Location { lineIdx = token.lineIdx; colIdx = token.colIdx }
;;
