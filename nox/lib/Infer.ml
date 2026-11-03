open Core
open Hecc

let file = ref "WUT.TF?"
let locOfExpr e = Ast.getLocOfExpr e

let typeOfBlock stmts =
  match lastOfList stmts with
  | Some lastStmt -> (
      match lastStmt with Ast.YieldStmt s -> Ast.getTypeOfExpr s.expr | _ -> Ast.UnitType)
  | None -> Ast.UnitType
;;

let rec inferEntity env entity =
  let env, lowEnty =
    match entity with
    | Ast.Function f ->
        let env, lowBlock, lowBlockType = inferBlock env f.block in
        let lowEnty = Ast.Function { f with block = lowBlock } in
        (env, lowEnty)
  in
  (env, lowEnty)

and inferBlock env block =
  let env, lowBlock, lowBlockType =
    match block with
    | Ast.Block b ->
        let env, lowStmts, lowStmtsType = inferStmts env b.stmts [] in
        (env, Ast.Block { b with stmts = lowStmts; types = lowStmtsType }, lowStmtsType)
  in
  (env, lowBlock, lowBlockType)

and inferStmts env stmts acc =
  let env, lowStmts, lowStmtsType =
    match stmts with
    | [] ->
        let lowStmts = List.rev acc in
        (env, lowStmts, typeOfBlock lowStmts)
    | headStmt :: tailStmt ->
        let env, lowStmt = inferStmt env headStmt in
        inferStmts env tailStmt (lowStmt :: acc)
  in
  (env, lowStmts, lowStmtsType)

and inferStmt env stmt =
  let env, lowStmt =
    match stmt with
    | Ast.LetStmt s ->
        (* Infer the expression type, then destructure into variables. *)
        (* Insert the variables in environment *)
        let env, lowExpr, lowType = inferExpr env s.expr in
        let env, lowVars = inferVars env lowExpr s.pats in
        let lowStmt =
          Ast.LetStmt
            {
              s with
              expr = lowExpr;
              (* Flatten the variables in a simple list *)
              vars = lowVars;
              (* Delete old patterns to clean things up *)
              pats = [];
            }
        in
        (env, lowStmt)
    | Ast.ReturnStmt s ->
        let env, lowExpr, lowType = inferExpr env s.expr in
        let lowStmt = Ast.ReturnStmt { s with expr = lowExpr } in
        (env, lowStmt)
    | Ast.YieldStmt s ->
        let env, lowExpr, lowType = inferExpr env s.expr in
        let lowStmt = Ast.YieldStmt { s with expr = lowExpr } in
        (env, lowStmt)
    | _ -> todo source "infer-stmt"
  in
  (env, lowStmt)

and inferVars env expr pat =
  let rec destructPattern env expr pat =
    let rec unwrapPattern (env : Store.env) exprs pats acc =
      match (exprs, pats) with
      | [], [] -> (env, acc)
      | headExpr :: tailExpr, headPat :: tailPat ->
          (* Unwrap expression and recurse destruct pattern when needed. *)
          let env, lowVars =
            match headPat with
            | Ast.TuplePattern p ->
                let env, lowVars = destructPattern env headExpr [ headPat ] in
                (env, lowVars)
            | Ast.ArrayPattern p ->
                let env, lowVars = destructPattern env headExpr [ headPat ] in
                (env, lowVars)
            | Ast.LonePattern p ->
                let headVar = p.var in
                let expectedType = Ast.getTypeOfVar headVar in
                let exprType = Ast.getTypeOfExpr headExpr in
                let exprId = Ast.getIdOfExpr headExpr in
                let varType = match exprType with Ast.LaterType -> expectedType | _ -> exprType in
                (* Ensure that the expected type of variable (if present), is respected *)
                assure source
                  (match (expectedType, exprType) with
                  | Ast.TodoType, _ -> true
                  | _, Ast.LaterType -> true
                  | _ -> expectedType = exprType)
                  (fun _ ->
                    raise
                      (Report
                         {
                           message = "Unable to match expected type in pattern.";
                           source = xSOURCE source;
                           error = None;
                         }));
                (* Insert variable in environment *)
                let name = Ast.getStringOfName (Ast.getNameOfVar headVar) in
                let nameId = Ast.getIdOfName (Ast.getNameOfVar headVar) in
                let uuid : Ast.uuids = Ast.getUuidOfVar headVar in
                let loc = Ast.getLocOfVar headVar in
                let var =
                  match headVar with Ast.Var v -> Ast.Var { v with type' = varType; exprId }
                in
                let record = Store.VarRecord { name; nameId; types = varType; uuid; loc } in
                let env : Store.env =
                  { env with records = Store.SymbolMap.add nameId record env.records }
                in
                (env, [ var ])
          in
          unwrapPattern env tailExpr tailPat (lowVars @ acc)
      | _ -> never source "unwrap-pattern"
    in
    (* Find which pattern and expression we need to destructure *)
    let env, lowVars =
      match pat with
      | headPat :: tailPat -> (
          match (expr, headPat) with
          (* Complex expressions with complex patterns *)
          | Ast.TupleExpr e, Ast.TuplePattern p ->
              let env, lowVars = unwrapPattern env e.exprs p.pats [] in
              (env, lowVars)
          | Ast.ArrayExpr e, Ast.ArrayPattern p ->
              let env, lowVars = unwrapPattern env e.exprs p.pats [] in
              (env, lowVars)
          (* Complex expressions with lone patterns *)
          | Ast.TupleExpr e, Ast.LonePattern p ->
              let env, lowVars =
                match List.length pat with
                | 1 -> unwrapPattern env [ expr ] pat []
                | _ -> unwrapPattern env e.exprs pat []
              in
              (env, lowVars)
          | Ast.ArrayExpr e, Ast.LonePattern p ->
              let env, lowVars =
                match List.length pat with
                | 1 -> unwrapPattern env [ expr ] pat []
                | _ -> unwrapPattern env e.exprs pat []
              in
              (env, lowVars)
          (* Simple expressions with lone patterns *)
          | _, Ast.LonePattern p ->
              let env, lowVars = unwrapPattern env [ expr ] [ headPat ] [] in
              (env, lowVars)
          | _ -> duck source "Unable to match expected type in pattern." !file (locOfExpr expr))
      | _ -> never source "infer-vars"
    in
    (env, lowVars)
  in
  let env, lowVars = destructPattern env expr pat in
  (env, List.rev lowVars)

and inferExprs env exprs accExpr accTypes =
  match exprs with
  | [] -> (env, List.rev accExpr, List.rev accTypes)
  | headExpr :: tailExpr ->
      let env, lowExpr, lowType = inferExpr env headExpr in
      inferExprs env tailExpr (lowExpr :: accExpr) (lowType :: accTypes)

and inferExpr env expr =
  let oldEnv = env in
  let inferBinaryExpr lexpr rexpr =
    let env, lowExprL, lowTypeL = inferExpr env lexpr in
    let env, lowExprR, lowTypeR = inferExpr env rexpr in
    let newType =
      match lowTypeL = lowTypeR with
      | true -> lowTypeL
      | false -> duck source "Binary expression types don't match." !file (locOfExpr expr)
    in
    (env, lowExprL, lowExprR, lowTypeL)
  in
  let env, lowExpr, lowType =
    match expr with
    | Ast.TupleExpr e ->
        let env, lowExpr, lowType = inferExprs env e.exprs [] [] in
        let newType = Ast.TupleType { types = lowType; offsets = []; align = 0; size = 0 } in
        (env, Ast.TupleExpr { e with exprs = lowExpr; types = newType }, newType)
    | Ast.ArrayExpr e ->
        let env, lowExpr, lowType = inferExprs env e.exprs [] [] in
        let newType = Ast.ArrayType { types = List.hd lowType; len = 0 } in
        (env, Ast.ArrayExpr { e with exprs = lowExpr; types = newType }, newType)
    | Ast.MoveExpr e -> (env, expr, Ast.TodoType)
    | Ast.CopyExpr e -> (env, expr, Ast.TodoType)
    | Ast.IfExpr e ->
        let env, lowCondExpr, lowCondType = inferExpr env e.cond in
        let env, lowBlock, lowBlockType = inferBlock env e.block in
        let _, lowRest, lowRestType =
          match e.rest with
          | Some (Ast.ElseIfExpr _) ->
              let env, lowRest, lowRestType = inferExpr oldEnv (some source e.rest) in
              if lowBlockType <> lowRestType then
                never source "Else if branch's type does not match.";
              (env, Some lowRest, lowRestType)
          | Some (Ast.ElseExpr _) ->
              let env, lowRest, lowRestType = inferExpr oldEnv (some source e.rest) in
              if lowBlockType <> lowRestType then never source "Else branch's type does not match.";
              (env, Some lowRest, lowRestType)
          | _ -> (env, None, Ast.TodoType)
        in
        let newExpr =
          Ast.IfExpr
            { e with cond = lowCondExpr; block = lowBlock; rest = lowRest; types = lowBlockType }
        in
        (env, newExpr, lowBlockType)
    | Ast.ElseIfExpr e ->
        let env, lowCondExpr, lowCondType = inferExpr env e.cond in
        let env, lowBlock, lowBlockType = inferBlock env e.block in
        let _, lowRest, lowRestType =
          match e.rest with
          | Some (Ast.ElseIfExpr _) ->
              let env, lowRest, lowRestType = inferExpr oldEnv (some source e.rest) in
              if lowBlockType <> lowRestType then
                never source "Else if branch's type does not match.";
              (env, Some lowRest, lowRestType)
          | Some (Ast.ElseExpr _) ->
              let env, lowRest, lowRestType = inferExpr oldEnv (some source e.rest) in
              if lowBlockType <> lowRestType then never source "Else branch's type does not match.";
              (env, Some lowRest, lowRestType)
          | _ -> (env, None, Ast.TodoType)
        in
        let newExpr =
          Ast.ElseIfExpr
            { e with cond = lowCondExpr; block = lowBlock; rest = lowRest; types = lowBlockType }
        in
        (env, newExpr, lowBlockType)
    | Ast.ElseExpr e ->
        let env, lowBlock, lowBlockType = inferBlock env e.block in
        let newExpr = Ast.ElseExpr { e with block = lowBlock; types = lowBlockType } in
        (env, newExpr, lowBlockType)
    | Ast.BlockExpr e ->
        let env, lowBlock, lowBlockType = inferBlock env e.block in
        (env, Ast.BlockExpr { e with block = lowBlock; types = lowBlockType }, lowBlockType)
    | Ast.AddExpr e ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr e.lexpr e.rexpr in
        (env, Ast.AddExpr { e with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.SubExpr e ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr e.lexpr e.rexpr in
        (env, Ast.SubExpr { e with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.MulExpr e ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr e.lexpr e.rexpr in
        (env, Ast.MulExpr { e with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.DivExpr e ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr e.lexpr e.rexpr in
        (env, Ast.DivExpr { e with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.NameExpr e ->
        let nameId = Ast.getIdOfName e.value in
        let name = Ast.getStringOfName e.value in
        let varId, lowType =
          match Store.SymbolMap.find_opt nameId env.records with
          | Some t -> (
              match t with
              | Store.VarRecord v -> (v.uuid.varId, v.types)
              | _ -> todo source "infer-environment-symbol")
          | None -> duck source (fmt "Unable to find identifier %s." (quote name)) !file e.loc
        in
        ( env,
          Ast.NameExpr
            {
              e with
              types = lowType;
              value =
                (match e.value with
                | Ast.Name n -> Ast.Name { n with uuid = { n.uuid with varId } });
            },
          lowType )
    | Ast.BoolExpr e -> (env, expr, Ast.BoolType)
    | Ast.IntExpr e -> (env, Ast.IntExpr { e with types = Ast.IntType }, Ast.IntType)
    | Ast.UnitExpr _ -> (env, expr, Ast.UnitType)
    | _ -> todo source "infer-expr"
  in
  (env, lowExpr, lowType)
;;

let inferFile env fileAst =
  match fileAst with
  | Ast.File f ->
      file := f.file;
      let newEntities =
        List.map
          (fun entity ->
            let _, newEntity = inferEntity env entity in
            newEntity)
          f.entities
      in
      Ast.File { f with entities = newEntities }
;;
