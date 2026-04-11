open Core

type modules = { files : files list } [@@deriving show { with_path = false }]
and files = { definitions : definitions list; file : string }

and definitions =
  | Function of {
      export : bool;
      name : string;
      args : args list;
      stmts : stmts list;
      type' : types;
    }
  | Data of unit

and stmts =
  | LetStmt of { expr : exprs; id : int; ix : int }
  | ReturnStmt of { void : bool; id : int; ix : int }

and exprs =
  | CallExpr of { name : string; args : args list; type' : types; reg : regs }
  | TermExpr of { value : string; type' : types; reg : regs }
  | BinOpExpr of { lreg : regs; rreg : regs; type' : types; op : binops; reg : regs }

and linkages =
  | ExportLink (* export : for globally exported symbols *)
  | ThreadLink (* thread : for thread local variables *)
  | SectionLink (* section : for linker data storage *)

and args = { type' : types; id : int; ix : int }
and binops = AddOp | SubOp | MulOp | DivOp | RemOp | UDivOp | URemOp
and regs = { id : int; ix : int }

and types =
  | IdType of string (* :ID : for agregates *)
  | WordType (* w : for 32 bit integer *)
  | LongType (* l : for 64 bit integer *)
  | SingleType (* s : for 32 bit floats *)
  | DoubleType (* d : for 64 bit floats *)
  | HalfType of types (* h : for 16 bit *)
  | ByteType of types (* b : for 8 bit *)
  | VoidType (* for no type *)

and sigils =
  | AggregateSigil (* : for structs, enums, unions, etc. *)
  | GlobalSigil (* $ : for functions, strings, etc. *)
  | LocalSigil (* % : for local registers *)
  | BlockSigil (* @ : for block labels *)
