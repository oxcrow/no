open Core
open Get.X

let fmt = Printf.sprintf
let pmt = Printf.printf
let strOfInt = string_of_int

(* A mutable counter to create unique expression IDs *)
(* This violates immutability of our code; but should be safe if used correctly. *)
let exprId = ref 0

(* Get next ID *)
let nxid () =
  let id = !exprId in
  incr exprId;
  id
;;

(* Reset ID *)
let rsid () = exprId := 0

(* Utils *)
let isFunction x = match x with Ast.Function _ -> true | _ -> false

(** Lower a function's AST to QBE IR instructions *)
let rec lowerFunction file env entity =
  let types = Analyser.analyseFunction file env entity in
  match entity with
  | Ast.Function f ->
      let node =
        Cfg.Function
          {
            scope = lowerScope f.scope;
            name = getAstName f.name;
            blocks = lowerBlock types (Some "start") f.body;
          }
      in
      node
  | _ -> xNEVER uPOS "expected-function"

and lowerBlock types label block =
  let node =
    Cfg.Block { label; stmts = lowerStmts types (getAstStmtsOfBlock block); rest = None }
  in
  node

and lowerStmts types stmts =
  let rec aux types stmts acc =
    match stmts with
    | [] -> List.rev acc
    | h :: t ->
        let node = lowerStmt types h in
        aux types t (node :: acc)
  in
  aux types stmts []

and lowerStmt types stmt =
  let stmtCFG =
    match stmt with
    | Ast.LetStmt s ->
        let varNames = List.map getAstNameOfVar s.vars in
        let exprType = getAstTypeOfExpr types s.expr in
        let typeCFG = lowerType exprType in
        let exprCFG = lowerExpr types s.expr in

        let exprRegCFG = nxid () in
        let allocRegCFG = nxid () in
        let fieldRegCFG = nxid () in

        let stmtCFG =
          let align = alignOfType exprType in
          let size = sizeOfType exprType in
          let allocCFG =
            Cfg.LetStmt
              {
                reg = Some (Some Cfg.AllocReg, allocRegCFG, 0);
                name = Some (List.hd varNames);
                expr = Cfg.AllocExpr { type' = typeCFG; align; size; stmts = [] };
                stmts = [];
              }
          in
          let fieldCFG, storeCFG =
            let offsets, sizes = calcStructLayout exprType in
            let rec createField types offsets sizes idx fieldAcc storeAcc =
              match (types, offsets, sizes) with
              | [], [], [] -> (List.rev fieldAcc, List.rev storeAcc)
              | th :: tt, oh :: ot, sh :: st ->
                  let field =
                    Cfg.LetStmt
                      {
                        reg = Some (Some Cfg.FieldReg, fieldRegCFG, idx);
                        name = None;
                        expr =
                          Cfg.FieldExpr
                            {
                              type' = th;
                              offset = oh;
                              size = sh;
                              from = (Some Cfg.AllocReg, allocRegCFG, idx);
                            };
                        stmts = [];
                      }
                  in
                  let store =
                    Cfg.CmdStmt
                      {
                        expr =
                          Cfg.StoreExpr
                            {
                              type' =
                                (match typeCFG with
                                | Cfg.TupleType t ->
                                    List.nth_opt t.type' idx |> xSOME uPOS
                                | _ -> typeCFG);
                              dest = (Some Cfg.AllocReg, allocRegCFG, idx);
                              from = (None, exprRegCFG, 0);
                              stmts = [];
                            };
                        stmts = [];
                      }
                  in

                  createField tt ot st (idx + 1) (field :: fieldAcc) (store :: storeAcc)
              | _ -> xNEVER uPOS "wut?"
            in
            createField (listOfType typeCFG) offsets sizes 0 [] []
          in
          [ allocCFG ] @ fieldCFG @ storeCFG
        in

        let node =
          Cfg.LetStmt
            {
              reg = Some (Some Cfg.VarReg, exprRegCFG, 0);
              name = None;
              expr = exprCFG;
              stmts = stmtCFG;
            }
        in
        node
    | Ast.SetStmt s -> Cfg.NoneStmt "SetStmt"
    | Ast.ReturnStmt s -> Cfg.NoneStmt "RetStmt"
    | _ -> xTODO uPOS "lower-stmt"
  in
  stmtCFG

and lowerExpr types expr =
  let exprId = getAstIdOfExpr expr in
  let exprType = getAstTypeOfExpr types expr in
  let exprCFG =
    match expr with
    | Ast.BlockExpr e ->
        let block = lowerBlock types None (Get.Ast.blockOfExpr expr) in
        Cfg.BlockExpr { type' = lowerType exprType; stmts = Get.Cfg.stmtsOfBlock block }
    | Ast.IntVal e -> Cfg.IntExpr { value = e.value; stmts = [] }
    | Ast.UnitVal _ -> Cfg.IntExpr { value = "0"; stmts = [] }
    | _ -> xTODO uPOS ("lower-expr" ^ Ast.show_exprs expr)
  in
  exprCFG

and lowerScope scope =
  match scope with Ast.PublicScope -> Cfg.ExportScope | _ -> Cfg.LocalScope

and sizeOfType type' =
  match type' with
  | Ast.TupleType t ->
      let rec calcStructSize types offset acc =
        match types with
        | [] -> acc
        | h :: t ->
            let align = alignOfType h in
            let size = sizeOfType h in
            let newOffset =
              match h with
              | Ast.UnitType | _ -> (
                  match offset mod align = 0 with
                  | true -> offset
                  | false -> offset + align - (offset mod align))
            in
            calcStructSize t (newOffset + size) (acc + (newOffset - offset) + size)
      in
      calcStructSize t.types 0 0
  | Ast.FunctionType t -> sizeOfType t.type'
  | Ast.FloatType -> 8
  | Ast.IntType -> 8
  | Ast.UnitType -> 8 (* Until we figure out how to handle zero sized structs. *)
  | _ -> xTODO uPOS "size-of-type"

and alignOfType type' =
  match type' with
  | Ast.TupleType t ->
      let aligns = List.map alignOfType t.types in
      let sum = List.fold_left max (List.hd aligns) (List.tl aligns) in
      sum
  | Ast.FunctionType t -> sizeOfType t.type'
  | Ast.FloatType -> 8
  | Ast.IntType -> 8
  | Ast.UnitType -> 8 (* Until we figure out how to handle zero sized structs. *)
  | _ -> xTODO uPOS "align-of-type"

and calcStructLayout type' =
  let rec aux types offset accOffset accSize =
    match types with
    | [] -> (List.rev accOffset, List.rev accSize)
    | h :: t ->
        let align = alignOfType h in
        let size = sizeOfType h in
        let newOffset =
          match h with
          | Ast.UnitType | _ -> (
              match offset mod align = 0 with
              | true -> offset
              | false -> offset + align - (offset mod align))
        in
        aux t (newOffset + size) (newOffset :: accOffset) (size :: accSize)
  in
  match type' with
  | Ast.TupleType t -> aux t.types 0 [] []
  | Ast.FloatType -> aux [ type' ] 0 [] []
  | Ast.IntType -> aux [ type' ] 0 [] []
  | Ast.UnitType -> aux [ type' ] 0 [] []
  | _ -> xNEVER uPOS "calc-struct-offset"

and canLowerType type' =
  match type' with
  | Ast.FunctionType _ | Ast.FloatType | Ast.IntType | Ast.UnitType -> true
  | _ -> false

and lowerType type' =
  match type' with
  | Ast.FunctionType t -> lowerType t.type'
  | Ast.TupleType t ->
      let offsets, sizes = calcStructLayout type' in
      let types = List.map lowerType t.types in
      Cfg.TupleType
        {
          type' = types;
          offsets =
            List.map2 (fun o s -> match s with 0 -> None | _ -> Some o) offsets sizes;
          sizes;
        }
  | Ast.FloatType -> Cfg.FloatType
  | Ast.IntType -> Cfg.IntType
  | Ast.UnitType -> Cfg.UnitType
  | _ -> xTODO uPOS "lower-type"

and listOfType type' =
  match type' with
  | Cfg.TupleType t -> t.type'
  | Cfg.FloatType -> [ type' ]
  | Cfg.IntType -> [ type' ]
  | Cfg.UnitType -> [ type' ]

and todo message =
  exit 0;
  xTODO uPOS message
;;

(** Lower file's AST to QBE IR instructions *)
let lowerFile ast =
  let env = Env.initFileEnv ast in
  let defs =
    match ast with
    | Ast.File f ->
        List.map
          (fun entity -> lowerFunction f.file env entity)
          (List.filter isFunction f.entities)
  in
  List.iter (fun d -> print_endline (Cfg.show_defs d)) defs;
  exit 0;
  defs
;;
