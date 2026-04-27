open Core

type binaries = { modules : modules list } [@@deriving show { with_path = false }]
and modules = { files : files list }
and files = { defs : defs list; file : string }

(* Definitions *)
and defs = Function of { scope : scopes; name : string; blocks : blocks }

(* Blocks *)
and blocks = Block of { label : string option; stmts : stmts list; rest : forks option }
and forks = Fork of { left : blocks; right : blocks option }

(* Statements *)
and stmts =
  | LetStmt of {
      reg : regs option;
      name : string option;
      expr : exprs;
      stmts : stmts list;
    }
  | SetStmt of { reg : regs option; expr : exprs; stmts : stmts list }
  | RetStmt of { reg : regs option; expr : exprs option; stmts : stmts list }
  | CmdStmt of { expr : exprs; stmts : stmts list }
  | NoneStmt of string

(* Expressions *)
and exprs =
  (* Compound *)
  | TupleExpr of { type' : types list; expr : exprs list; stmts : stmts list }
  | CallExpr of { name : string; args : exprs list; type' : types; stmts : stmts list }
  | BinaryExpr of { lexpr : exprs; rexpr : exprs; type' : types; stmts : stmts list }
  | BlockExpr of { type' : types; stmts : stmts list }
  (* Simple *)
  | IdExpr of { name : string; type' : types; stmts : stmts list }
  | FloatExpr of { value : string; stmts : stmts list }
  | IntExpr of { value : string; stmts : stmts list }
  | UnitExpr of { stmts : stmts list }
  (* Extra *)
  | AllocExpr of { type' : types; align : int; size : int; stmts : stmts list }
  | StoreExpr of { type' : types; from : regs; dest : regs; stmts : stmts list }
  | LoadExpr of { type' : types; from : regs; stmts : stmts list }
  | FieldExpr of { type' : types; offset : int; size : int; from : regs }
  | NoneExpr

(* Types *)
and types =
  | TupleType of { type' : types list; offsets : int option list; sizes : int list }
  | FloatType
  | IntType
  | UnitType

(* Scopes *)
and scopes = ExportScope | LocalScope

(* Registers *)
and regs = regKinds option * int * int
and regKinds = AllocReg | FieldReg | VarReg
