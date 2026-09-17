open Core
open Hecc

let locOfExpr e = Ast.getLocOfExpr e

let rec inferEntities env file entys acc =
  match entys with
  | [] -> (env, List.rev acc)
  | headEnty :: tailEnty ->
      (* We discard the environment returned! This cleans our local scope! *)
      let _, lowEnty = inferEntity env file headEnty in
      inferEntities env file tailEnty (lowEnty :: acc)

and inferEntity env file enty =
  let env, lowEnty =
    match enty with
    | Ast.Function o ->
        (* Infer all blocks of code in function *)
        let env, lowStmts = inferStmts env file o.block [] in
        let lowEnty = Ast.Function { o with block = lowStmts } in
        (env, lowEnty)
    | _ -> todo source "infer-entity"
  in
  (env, lowEnty)

and inferStmts env file stmts acc =
  match stmts with
  | [] -> (env, List.rev acc)
  | headStmt :: tailStmt ->
      let env, lowStmt = inferStmt env file headStmt in
      inferStmts env file tailStmt (lowStmt :: acc)

and inferStmt env file stmt =
  let env, lowStmt =
    match stmt with
    | Ast.LetStmt o ->
        (* Infer the expression type, then destructure into variables. *)
        (* Insert the variables in environment *)
        let env, lowExpr, lowType = inferExpr env file o.expr in
        let env, lowVars = inferVars env file lowExpr o.pats in
        let lowStmt = Ast.LetStmt { o with expr = lowExpr; vars = lowVars; pats = [] } in
        (env, lowStmt)
    | Ast.ReturnStmt o ->
        let env, lowExpr, lowType = inferExpr env file o.expr in
        let lowStmt = Ast.ReturnStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | Ast.YieldStmt o ->
        let env, lowExpr, lowType = inferExpr env file o.expr in
        let lowStmt = Ast.YieldStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | Ast.AssignStmt o ->
        let env, lowValExpr, lowValTypes = inferExprs env file o.vars [] [] in
        let env, lowExpr, lowType = inferExpr env file o.expr in
        (match lowType with
        | Ast.TupleType _ | Ast.ArrayType _ -> todo source "assign-tuple/array-to-lvalue"
        | _ ->
            let expectedType = List.hd lowValTypes in
            assure source (expectedType = lowType) (fun _ ->
                quack source "Unable to assign expression, as types don't match." file o.loc));
        let lowStmt = Ast.AssignStmt { o with vars = lowValExpr; expr = lowExpr } in
        (env, lowStmt)
    | Ast.IfStmt o ->
        let env, lowExpr, lowType = inferExpr env file o.expr in
        let lowStmt = Ast.IfStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | _ -> todo source "infer-stmt"
  in
  (env, lowStmt)

and inferVars env file expr pat =
  let rec destructPattern env expr pat =
    let rec unwrapPattern env exprs pats acc =
      match (exprs, pats) with
      | [], [] -> (env, acc)
      | headExpr :: tailExpr, headPat :: tailPat ->
          (* Unwrap expression and recurse destruct pattern when needed. *)
          let env, lowVars =
            match headPat with
            | Ast.TuplePattern p ->
                let env, lowVars = destructPattern env headExpr p.pats in
                (env, lowVars)
            | Ast.ArrayPattern p ->
                let env, lowVars = destructPattern env headExpr p.pats in
                (env, lowVars)
            | Ast.LonePattern p ->
                let headVar = p.var in
                let expectedType = Ast.getTypeOfVar headVar in
                let exprType = Ast.getTypeOfExpr headExpr in

                let varType = match exprType with Ast.LaterType -> expectedType | _ -> exprType in

                (* Ensure that the expected type of variable (if present), is respected *)
                assure source
                  (match (expectedType, exprType) with
                  | Ast.NoneType, _ -> true
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

                let name = Ast.getStringOfName (Ast.getNameOfVar headVar) in
                let nameId = Ast.getIdOfName (Ast.getNameOfVar headVar) in
                let uuid : Ast.uuids = Ast.getUuidOfVar headVar in
                let loc = Ast.getLocOfVar headVar in
                let record =
                  Store.Module.VarRecord
                    { name; nameId; types = varType; varId = uuid.varId; entyId = uuid.entyId; loc }
                in

                let env = Store.Module.SymbolMap.add nameId record env in
                let var = match headVar with Ast.Var v -> Ast.Var { v with type' = varType } in
                (env, [ var ])
            | _ -> never source "wut?"
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
          | _ -> quack source "Unable to match expected type in pattern." file (locOfExpr expr))
      | _ -> never source "infer-vars"
    in

    (env, lowVars)
  in

  let env, lowVars = destructPattern env expr pat in
  (env, List.rev lowVars)

and inferExprs env file exprs accExpr accTypes =
  match exprs with
  | [] -> (env, List.rev accExpr, List.rev accTypes)
  | headExpr :: tailExpr ->
      let env, lowExpr, lowType = inferExpr env file headExpr in
      inferExprs env file tailExpr (lowExpr :: accExpr) (lowType :: accTypes)

(* BUG: The complex types are not calculated correctly.
 * We don't know where the memory offsets will be, or their sizes, and alignments.
 * We NEED this for creating C like structs, that are aligned correctly in memory. *)
and inferExpr env file expr =
  let inferBinaryExpr lexpr rexpr =
    let env, lowExprL, lowTypeL = inferExpr env file lexpr in
    let env, lowExprR, lowTypeR = inferExpr env file rexpr in
    let newType =
      match lowTypeL = lowTypeR with
      | true -> lowTypeL
      | false -> quack source "Binary expression types don't match." file (locOfExpr rexpr)
    in
    (env, lowExprL, lowExprR, lowTypeL)
  in
  let env, lowExpr, lowType =
    match expr with
    | Ast.TupleExpr o ->
        let env, lowExpr, lowType = inferExprs env file o.exprs [] [] in
        let newType = Ast.TupleType { types = lowType; offsets = []; align = 0; size = 0 } in
        (env, Ast.TupleExpr { o with exprs = lowExpr; types = newType }, newType)
    | Ast.ArrayExpr o ->
        let env, lowExpr, lowType = inferExprs env file o.exprs [] [] in
        let newType = Ast.ArrayType { types = List.hd lowType; elems = Ast.NoneExpr } in
        (env, Ast.ArrayExpr { o with exprs = lowExpr; types = newType }, newType)
    | Ast.DerefExpr o ->
        let env, lowExpr, lowType = inferExpr env file o.expr in
        let newType =
          match lowType with
          | Ast.ConRefType t -> t.types
          | _ -> quack source "Unable to dereference expression." file o.loc
        in
        (env, Ast.DerefExpr { o with expr = lowExpr; types = newType }, newType)
    | Ast.ConRefExpr o ->
        let env, lowExpr, lowType = inferExpr env file o.expr in
        let newType = Ast.ConRefType { types = lowType } in
        (env, Ast.ConRefExpr { o with expr = lowExpr; types = newType }, newType)
    | Ast.AddExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.AddExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.SubExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.SubExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.MulExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.MulExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.DivExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.DivExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.NotEqExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.NotEqExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.EqEqExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.EqEqExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.LtEqExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.LtEqExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.GtEqExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.GtEqExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.LtExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.LtExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.GtExpr o ->
        let env, lowExprL, lowExprR, lowType = inferBinaryExpr o.lexpr o.rexpr in
        (env, Ast.GtExpr { o with lexpr = lowExprL; rexpr = lowExprR; types = lowType }, lowType)
    | Ast.IfExpr o ->
        let env, lowCondExpr, lowCondType = inferExpr env file o.cond in
        let env, lowBlock = inferStmts env file o.block [] in
        let env, lowRest, lowRestType =
          match o.rest with
          | Some (Ast.ElseIfExpr e) ->
              let env, lowRest, lowRestType = inferExpr env file (o.rest |> some source) in
              (env, Some lowRest, lowRestType)
          | Some (Ast.ElseExpr e) ->
              let env, lowRest, lowRestType = inferExpr env file (o.rest |> some source) in
              (env, Some lowRest, lowRestType)
          | _ -> (env, None, Ast.NoneType)
        in
        let newExpr = Ast.IfExpr { o with cond = lowCondExpr; block = lowBlock; rest = lowRest } in
        let newType = Ast.TodoType in
        (env, newExpr, newType)
    | Ast.ElseIfExpr o ->
        let env, lowCondExpr, lowCondType = inferExpr env file o.cond in
        let env, lowBlock = inferStmts env file o.block [] in
        let env, lowRest, lowRestType =
          match o.rest with
          | Some (Ast.ElseIfExpr e) ->
              let env, lowRest, lowRestType = inferExpr env file (o.rest |> some source) in
              (env, Some lowRest, lowRestType)
          | Some (Ast.ElseExpr e) ->
              let env, lowRest, lowRestType = inferExpr env file (o.rest |> some source) in
              (env, Some lowRest, lowRestType)
          | _ -> (env, None, Ast.NoneType)
        in
        let newExpr =
          Ast.ElseIfExpr { o with cond = lowCondExpr; block = lowBlock; rest = lowRest }
        in
        let newType = Ast.TodoType in
        (env, newExpr, newType)
    | Ast.ElseExpr o ->
        let env, lowBlock = inferStmts env file o.block [] in
        let newExpr = Ast.ElseExpr { o with block = lowBlock } in
        let newType = Ast.TodoType in
        (env, newExpr, newType)
    | Ast.NameExpr o ->
        let nameId = Ast.getIdOfName o.value in
        let name = Ast.getStringOfName o.value in
        let types =
          match Store.Module.SymbolMap.find_opt nameId env with
          | Some t -> (
              match t with
              | Store.Module.VarRecord v -> v.types
              | _ -> todo source "infer-environment-symbol")
          | None -> quack source (fmt "Unable to find identifier %s." (quote name)) file o.loc
        in
        (env, Ast.NameExpr { o with types }, types)
    | Ast.BoolExpr o -> (env, expr, Ast.BoolType)
    | Ast.IntExpr o -> (env, Ast.IntExpr { o with types = Ast.IntType }, Ast.IntType)
    | Ast.UnitExpr _ -> (env, expr, Ast.UnitType)
    | Ast.LaterExpr _ -> (env, expr, Ast.LaterType)
    | _ -> todo source "infer-expr"
  in
  (env, lowExpr, lowType)
;;

let inferFile env file =
  let env, entys =
    inferEntities env (Ast.getStringNameOfFile file) (Ast.getEntitiesOfFile file) []
  in
  let file = match file with Ast.File f -> Ast.File { f with entities = entys } in
  file
;;
