open Core

type binaries = { modules : modules list } [@@deriving show { with_path = false }]
and modules = { files : files list }
and files = { defs : defs list; file : string }

(* Definitions *)
and defs = FunctionDef of { scope : scopes; name : string; blocks : blocks }

(* Blocks *)
and blocks = Block of { label : string; stmts : stmts list; rest : forks option }
and forks = Fork of { left : blocks; right : blocks option }

(* Statements *)
and stmts = LetStmt of { reg : int; name : string; expr : exprs } | ReturnStmt

(* Expressions *)
and exprs =
  (* Compound *)
  | TupleExpr of { type' : types list; expr : exprs list; stmts : stmts list }
  | CallExpr of { name : string; args : exprs list; type' : types; stmts : stmts list }
  | BinaryExpr of { lexpr : exprs; rexpr : exprs; type' : types; stmts : stmts list }
  (* Simple *)
  | IdExpr of { name : string; type' : types; stmts : stmts list }
  | FloatExpr of { value : string; stmts : stmts list }
  | IntExpr of { value : string; stmts : stmts list }
  | UnitExpr of { stmts : stmts list }

(* Types *)
and types =
  | TupleType of { offsets : int option list; sizes : int list }
  | FloatType
  | IntType
  | UnitType

(* Scopes *)
and scopes = ExportScope | LocalScope
