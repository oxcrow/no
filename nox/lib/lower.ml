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

(* Get current ID *)
let cxid () =
  let id = !exprId in
  id
;;

(* Reset ID *)
let rsid () = exprId := 0

(* Utils *)
let isFunction x = match x with Ast.Function _ -> true | _ -> false
let isBefore x = match x with Cfg.Before _ -> true | _ -> false
let isAfter x = match x with Cfg.After _ -> true | _ -> false

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
            blocks = lowerBlock env types (Some "start") f.body;
          }
      in
      node
  | _ -> xNEVER uPOS "expected-function"

and lowerBlock env types label block =
  let env = Env.addScope env in
  let env, stmts = lowerStmts env types (getAstStmtsOfBlock block) in
  let node = Cfg.Block { label; stmts; rest = None } in
  let env = Env.removeScope env in
  node

and lowerStmts env types stmts =
  let rec aux env types stmts acc =
    match stmts with
    | [] -> (env, List.rev acc)
    | stmtHead :: stmtTail ->
        let env, node = lowerStmt env types stmtHead in
        aux env types stmtTail (node :: acc)
  in
  aux env types stmts []

and lowerStmt env types stmt =
  let env, stmtCFG =
    match stmt with
    | Ast.LetStmt s ->
        let varNames = List.map getAstNameOfVar s.vars in
        let exprTypeAST = getAstTypeOfExpr types s.expr in
        let exprTypeCFG = lowerType exprTypeAST in

        let exprRegCFG, exprCFG = lowerExpr env types s.expr in

        let env, stmtCFG =
          let offsets, sizes = calcStructLayout exprTypeAST in
          let align = alignOfType exprTypeAST in
          let size = sizeOfType exprTypeAST in

          let rec trackVariable (env : Env.env) names idx =
            match names with
            | [] -> env
            | nameHead :: nameTail ->
                let env =
                  Env.addRecord env
                    (Env.Reg { name = nameHead; reg = (exprRegCFG, idx) })
                    nameHead
                in
                trackVariable env nameTail (idx + 1)
          in
          let env = trackVariable env varNames 0 in

          let allocCFG =
            [
              Cfg.After
                (Cfg.LetStmt
                   {
                     reg = Some (Some Cfg.AllocReg, exprRegCFG, 0);
                     name = None;
                     expr = Cfg.AllocExpr { type' = exprTypeCFG; align; size; stmts = [] };
                     stmts = [];
                   });
            ]
          in

          let fieldCFG =
            let rec createField types offsets sizes idx fieldAcc =
              match (types, offsets, sizes) with
              | [], [], [] -> List.rev fieldAcc
              | typeHead :: typeTail, offsetHead :: offsetTail, sizeHead :: sizeTail ->
                  let field =
                    Cfg.LetStmt
                      {
                        reg = Some (Some Cfg.FieldReg, exprRegCFG, idx);
                        name = None;
                        expr =
                          Cfg.FieldExpr
                            {
                              type' = typeHead;
                              offset = offsetHead;
                              size = sizeHead;
                              from = (Some Cfg.AllocReg, exprRegCFG, 0);
                            };
                        stmts = [];
                      }
                  in
                  createField typeTail offsetTail sizeTail (idx + 1)
                    (Cfg.After field :: fieldAcc)
              | _ -> xNEVER uPOS "wut?"
            in
            createField (listOfType exprTypeCFG) offsets sizes 0 []
          in

          let storeCFG =
            match exprTypeAST with
            | Ast.TupleType _ ->
                [
                  Cfg.After
                    (Cfg.CmdStmt
                       {
                         expr =
                           Cfg.BlitExpr
                             {
                               size;
                               from = (Some Cfg.AllocReg, exprRegCFG - 1, 0);
                               dest = (Some Cfg.AllocReg, exprRegCFG, 0);
                               stmts = [];
                             };
                         stmts = [];
                       });
                ]
            | _ ->
                [
                  Cfg.After
                    (Cfg.CmdStmt
                       {
                         expr =
                           Cfg.StoreExpr
                             {
                               type' = exprTypeCFG;
                               from = (Some Cfg.DataReg, exprRegCFG - 1, 0);
                               dest = (Some Cfg.AllocReg, exprRegCFG, 0);
                               stmts = [];
                             };
                         stmts = [];
                       });
                ]
          in

          (env, allocCFG @ fieldCFG @ storeCFG)
        in

        let node =
          Cfg.LetStmt
            {
              reg = Some (Some Cfg.DataReg, exprRegCFG, 0);
              name = None;
              expr = exprCFG;
              stmts = stmtCFG;
            }
        in
        (env, node)
    | Ast.SetStmt s ->
        let exprTypeAST = getAstTypeOfExpr types s.expr in
        let exprTypeCFG = lowerType exprTypeAST in

        let exprRegCFG, exprCFG = lowerExpr env types s.expr in

        let node =
          Cfg.SetStmt
            { reg = Some (Some Cfg.DataReg, exprRegCFG, 0); expr = exprCFG; stmts = [] }
        in
        (env, node)
    | Ast.ReturnStmt s ->
        let exprTypeAST = getAstTypeOfExpr types s.expr in
        let exprTypeCFG = lowerType exprTypeAST in

        let exprRegCFG, exprCFG = lowerExpr env types s.expr in
        let _, _, exprIdCFG = Get.Cfg.splitRegOfExpr exprCFG in

        let node =
          match exprTypeAST with
          | Ast.UnitType -> Cfg.RetStmt { reg = None; expr = None; stmts = [] }
          | _ ->
              Cfg.RetStmt
                {
                  reg =
                    Some
                      ( Some
                          (match s.expr with
                          | Ast.FloatVal _ | Ast.BoolVal _ -> xTODO uPOS "ret-expr"
                          | Ast.IntVal _ -> Cfg.DataReg
                          | _ -> Cfg.LoadReg),
                        exprRegCFG,
                        exprIdCFG );
                  expr = Some exprCFG;
                  stmts = [];
                }
        in
        (env, node)
    | _ -> xTODO uPOS "lower-stmt"
  in
  (env, stmtCFG)

and lowerExpr env types expr =
  let exprId = getAstIdOfExpr expr in
  let exprType = getAstTypeOfExpr types expr in
  let align = alignOfType exprType in
  let size = sizeOfType exprType in

  let exprTypeCFG = lowerType exprType in

  let exprCFG =
    match expr with
    | Ast.BlockExpr e ->
        let block = lowerBlock env types None (Get.Ast.blockOfExpr expr) in
        let exprRegCFG = nxid () in
        Cfg.BlockExpr
          {
            reg = Some (Some Cfg.DataReg, exprRegCFG, 0);
            type' = lowerType exprType;
            stmts = Get.Cfg.stmtsOfBlock block;
          }
    | Ast.TupleVal e ->
        let exprRegCFGs, exprCFGs =
          List.split (List.map (fun expr -> lowerExpr env types expr) e.value)
        in
        let exprRegCFG = nxid () in
        let stmtCFG =
          List.map
            (fun expr ->
              Cfg.Before
                (Cfg.LetStmt
                   { reg = Get.Cfg.regOfExpr expr; name = None; expr; stmts = [] }))
            exprCFGs
        in
        let allocCFG =
          Cfg.Before
            (Cfg.LetStmt
               {
                 reg = Some (Some Cfg.AllocReg, exprRegCFG, 0);
                 name = None;
                 expr = Cfg.AllocExpr { type' = exprTypeCFG; align; size; stmts = [] };
                 stmts = [];
               })
        in
        let fieldCFG, storeCFG =
          let offsets, sizes = calcStructLayout exprType in
          let rec createField types offsets sizes exprRegs idx fieldAcc storeAcc =
            match (types, offsets, sizes, exprRegs) with
            | [], [], [], [] -> (List.rev fieldAcc, List.rev storeAcc)
            | ( typeHead :: typeTail,
                offsetHead :: offsetTail,
                sizeHead :: sizeTail,
                exprHead :: exprTail ) ->
                let field =
                  Cfg.LetStmt
                    {
                      reg = Some (Some Cfg.FieldReg, exprRegCFG, idx);
                      name = None;
                      expr =
                        Cfg.FieldExpr
                          {
                            type' = typeHead;
                            offset = offsetHead;
                            size = sizeHead;
                            from = (Some Cfg.AllocReg, exprRegCFG, idx);
                          };
                      stmts = [];
                    }
                in
                let store =
                  match typeHead with
                  | Cfg.TupleType _ ->
                      Cfg.CmdStmt
                        {
                          expr =
                            Cfg.BlitExpr
                              {
                                size = sizeHead;
                                from = (Some Cfg.DataReg, exprHead, 0);
                                dest = (Some Cfg.AllocReg, exprRegCFG, idx);
                                stmts = [];
                              };
                          stmts = [];
                        }
                  | _ ->
                      Cfg.CmdStmt
                        {
                          expr =
                            Cfg.StoreExpr
                              {
                                type' = typeHead;
                                from = (Some Cfg.DataReg, exprHead, 0);
                                dest = (Some Cfg.AllocReg, exprRegCFG, idx);
                                stmts = [];
                              };
                          stmts = [];
                        }
                in
                createField typeTail offsetTail sizeTail exprTail (idx + 1)
                  (Cfg.Before field :: fieldAcc)
                  (Cfg.Before store :: storeAcc)
            | _ -> xNEVER uPOS "wut?"
          in
          createField (listOfType exprTypeCFG) offsets sizes exprRegCFGs 0 [] []
        in
        Cfg.TupleExpr
          {
            reg = Some (Some Cfg.DataReg, exprRegCFG, 0);
            type' = List.map (fun expr -> lowerType (getAstTypeOfExpr types expr)) e.value;
            stmts = stmtCFG @ [ allocCFG ] @ fieldCFG @ storeCFG;
          }
    | Ast.IdVal e ->
        let exprRegCFG, exprIdxCFG =
          match Hashtbl.find_opt env.table (getAstName e.name) |> xSOME uPOS with
          | Env.Reg x -> x.reg
          | _ -> xNEVER uPOS "wut?"
        in
        Cfg.LoadExpr
          {
            reg = Some (Some Cfg.LoadReg, exprRegCFG, exprIdxCFG);
            name = Some (getAstName e.name);
            type' = exprTypeCFG;
            from = (Some Cfg.FieldReg, exprRegCFG, exprIdxCFG);
            stmts = [];
          }
    | Ast.IntVal e ->
        let exprRegCFG = nxid () in
        Cfg.RegExpr
          {
            reg = Some (Some Cfg.DataReg, exprRegCFG, 0);
            type' = exprTypeCFG;
            stmts =
              [
                Before
                  (Cfg.LetStmt
                     {
                       reg = Some (Some Cfg.DataReg, exprRegCFG, 0);
                       name = None;
                       expr =
                         Cfg.IntExpr
                           {
                             reg = Some (Some Cfg.DataReg, exprRegCFG, 0);
                             value = e.value;
                             stmts = [];
                           };
                       stmts = [];
                     });
              ];
          }
    | Ast.UnitVal _ ->
        let exprRegCFG = nxid () in
        Cfg.LoadExpr
          {
            reg = Some (Some Cfg.LoadReg, exprRegCFG, 0);
            name = None;
            type' = Cfg.IntType;
            from = (Some Cfg.DataReg, exprRegCFG, 0);
            stmts =
              [
                Before
                  (Cfg.LetStmt
                     {
                       reg = Some (Some Cfg.DataReg, exprRegCFG, 0);
                       name = None;
                       expr =
                         Cfg.UnitExpr
                           {
                             reg = Some (Some Cfg.DataReg, exprRegCFG, 0);
                             value = "0";
                             stmts = [];
                           };
                       stmts = [];
                     });
              ];
          }
    | _ -> xTODO uPOS ("lower-expr" ^ Ast.show_exprs expr)
  in
  (cxid (), exprCFG)

and lowerScope scope =
  match scope with Ast.PublicScope -> Cfg.ExportScope | _ -> Cfg.LocalScope

and sizeOfType type' =
  match type' with
  | Ast.TupleType t ->
      let rec calcStructSize types offset acc =
        match types with
        | [] -> acc
        | typeHead :: typeTail ->
            let align = alignOfType typeHead in
            let size = sizeOfType typeHead in
            let newOffset =
              match typeHead with
              | Ast.UnitType | _ -> (
                  match offset mod align = 0 with
                  | true -> offset
                  | false -> offset + align - (offset mod align))
            in
            calcStructSize typeTail (newOffset + size) (acc + (newOffset - offset) + size)
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
  | Ast.FunctionType t -> alignOfType t.type'
  | Ast.FloatType -> 8
  | Ast.IntType -> 8
  | Ast.UnitType -> 8 (* Until we figure out how to handle zero sized structs. *)
  | _ -> xTODO uPOS "align-of-type"

and alignOfTypeCFG type' = ()

and calcStructLayout type' =
  let rec aux types offset accOffset accSize =
    match types with
    | [] -> (List.rev accOffset, List.rev accSize)
    | typeHead :: typeTail ->
        let align = alignOfType typeHead in
        let size = sizeOfType typeHead in
        let newOffset =
          match typeHead with
          | Ast.UnitType | _ -> (
              match offset mod align = 0 with
              | true -> offset
              | false -> offset + align - (offset mod align))
        in
        aux typeTail (newOffset + size) (newOffset :: accOffset) (size :: accSize)
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
      Cfg.TupleType { type' = types; offsets; sizes }
  | Ast.FloatType -> Cfg.FloatType
  | Ast.IntType -> Cfg.IntType
  | Ast.UnitType -> Cfg.UnitType
  | _ -> xTODO uPOS "lower-type"

and listOfTypeAst type' =
  match type' with
  | Ast.TupleType t -> t.types
  | Ast.FloatType -> [ type' ]
  | Ast.IntType -> [ type' ]
  | Ast.UnitType -> [ type' ]
  | _ -> xTODO uPOS "list-of-type-ast"

and listOfType type' =
  match type' with
  | Cfg.TupleType t -> t.type'
  | Cfg.FloatType -> [ type' ]
  | Cfg.IntType -> [ type' ]
  | Cfg.UnitType -> [ type' ]

and regOfExpr expr = match expr with "" | _ -> ()

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
  defs
;;
