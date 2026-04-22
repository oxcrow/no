open Core

type modules = { files : files list } [@@deriving show { with_path = false }]
and files = { defns : defns list; file : string }

(* Definitions *)
and defns =
  | Function of {
      scope : scopes;
      name : string;
      args : args list;
      stmts : stmts list;
      type' : types;
    }
  | Data of unit

(* Statements *)
and stmts =
  (* Compound *)
  | LetStmt of { var : regs; expr : exprs }
  | ReturnStmt of { var : regs option }
  | BlitStmt
  | JmpStmt
  | JnzStmt
  | HaltStmt
  (* Simple *)
  | StoreStmt of { expr : exprs }
  | CallStmt of { expr : exprs }
  (* Extra *)
  | LabelStmt of { name : string }
  | LotsOfStmt of { stmts : stmts list }

(* Expressions *)
and exprs =
  (* Compound *)
  | AllocExpr of { var : regs; align : int; size : int; type' : types }
  | CallExpr of { var : regs; name : string; args : exprs list; type' : types }
  | BinOpExpr of { var : regs; lreg : regs; rreg : regs; type' : types; op : binops }
  | FieldExpr of {
      var : regs;
      align : int;
      size : int;
      offset : int;
      idx : int;
      type' : types;
    }
  | StoreExpr of { var : regs; type' : types; from : regs; dest : regs }
  | LoadExpr of { var : regs; type' : types; from : regs }
  (* Simple *)
  | IdValExpr of { var : regs; name : string; type' : types }
  | RegExpr of { var : regs; type' : types }
  | TermExpr of { var : regs; value : string; type' : types }
  | VoidExpr
  (* Other *)
  | BlockExpr of { var : regs; stmts : stmts list; type' : types }

(* Binary expression operators *)
and binops = AddOp | SubOp | MulOp | DivOp | RemOp | UDivOp | URemOp

and linkages =
  | ExportLink (* export : for globally exported symbols *)
  | ThreadLink (* thread : for thread local variables *)
  | SectionLink (* section : for linker data storage *)

and args = { name : string; type' : types }
and regs = { name : string }

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

and scopes = ExportScope | PrivateScope
