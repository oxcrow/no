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
  | LetStmt of { reg : regs option; name : string option; expr : exprs; stmts : smx list }
  | SetStmt of { reg : regs option; expr : exprs; stmts : smx list }
  | RetStmt of { reg : regs option; expr : exprs option; stmts : smx list }
  | CmdStmt of { expr : exprs; stmts : smx list }
  | NoneStmt of string

and smx = Before of stmts | After of stmts

(* Expressions *)
and exprs =
  (* Compound *)
  | TupleExpr of { reg : regs option; type' : types list; stmts : smx list }
  | CallExpr of {
      reg : regs option;
      name : string;
      args : exprs list;
      type' : types;
      stmts : smx list;
    }
  | BinaryExpr of {
      reg : regs option;
      lexpr : exprs;
      rexpr : exprs;
      type' : types;
      stmts : smx list;
    }
  | BlockExpr of { reg : regs option; type' : types; stmts : stmts list }
  (* Simple *)
  | IdExpr of { reg : regs option; name : string; type' : types; stmts : smx list }
  | FloatExpr of { reg : regs option; value : string; stmts : smx list }
  | IntExpr of { reg : regs option; value : string; stmts : smx list }
  | UnitExpr of { reg : regs option; stmts : smx list }
  (* Extra *)
  | AllocExpr of { type' : types; align : int; size : int; stmts : smx list }
  | StoreExpr of { type' : types; from : regs; dest : regs; stmts : smx list }
  | LoadExpr of { type' : types; from : regs; stmts : smx list }
  | FieldExpr of { type' : types; offset : int; size : int; from : regs }
  | BlitExpr of { size : int; from : regs; dest : regs; stmts : smx list }
  | NoneExpr of string

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
and regKinds = AllocReg | FieldReg | LoadReg | DataReg
