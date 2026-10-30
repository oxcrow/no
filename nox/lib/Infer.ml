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
  (env, stmts, lowStmtsType)

and inferStmt env stmt =
  let env, lowStmt =
    match stmt with
    | Ast.ReturnStmt o ->
        let env, lowExpr, lowType = inferExpr env o.expr in
        let lowStmt = Ast.ReturnStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | Ast.YieldStmt o ->
        let env, lowExpr, lowType = inferExpr env o.expr in
        let lowStmt = Ast.YieldStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | _ -> todo source "infer-stmt"
  in
  (env, lowStmt)

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
    | Ast.IntExpr o -> (env, Ast.IntExpr { o with types = Ast.IntType }, Ast.IntType)
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
