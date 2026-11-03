open Core

type file = File of { entities : entities list } [@@deriving show { with_path = false }]

(* Entity definitions that exist in the file *)
and entities =
  | Function of {
      scope : scopes;
      name : string;
      block : blocks Store.BlockMap.t; [@opaque]
      types : types;
    }
  | NoneEnty

(* Block that contains instructions to be compiled *)
and blocks = { blockId : int; stmts : stmts list }

(* Statements that emit things *)
and stmts =
  | AddStmt of { dest : regs; fromL : regs; fromR : regs }
  | SubStmt of { dest : regs; fromL : regs; fromR : regs }
  | MulStmt of { dest : regs; fromL : regs; fromR : regs }
  | DivStmt of { dest : regs; fromL : regs; fromR : regs }
  | UDivStmt of { dest : regs; fromL : regs; fromR : regs }
  | CopyStmt of { dest : regs; value : string }
  | RetStmt of { from : regs option }
  | NoneStmt

(* Types that we can convert to IR *)
and types = IntType | UnitType

(* Scope of the exported element *)
and scopes = ExportScope | ModuleScope

(* Registers *)
and regs = { baseId : int; otherId : int; types : types }
