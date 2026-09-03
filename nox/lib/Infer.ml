open Core

let rec inferEntities env file entys acc =
  match entys with
  | [] -> (env, acc)
  | headEnty :: tailEnty ->
      let _, lowEnty = inferEntity env file headEnty in
      inferEntities env file tailEnty (lowEnty :: acc)

and inferEntity env file enty =
  let lowEnty =
    match enty with
    | Ast.Function o ->
        (* Infer all blocks of code in function *)
        let env, lowStmts = inferStmts env file o.block [] in
        let lowEnty = Ast.Function { o with block = lowStmts } in
        (env, lowEnty)
    | _ -> todo source "infer-entity"
  in
  lowEnty

and inferStmts env file stmts acc =
  match stmts with
  | [] -> (env, acc)
  | headStmt :: tailStmt ->
      let env, lowStmt = inferStmt env file headStmt in
      inferStmts env file tailStmt (lowStmt :: acc)

and inferStmt env file stmt =
  let env, lowStmt =
    match stmt with
    | Ast.LetStmt o ->
        (* Infer the expression type, then destructure into variables. *)
        (* Insert the variables in environment *)
        let env, lowExpr = inferExpr env file o.expr in
        let env, lowVars = inferVars env file lowExpr o.vars in
        let lowStmt = Ast.LetStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | Ast.ReturnStmt o ->
        let env, lowExpr = inferExpr env file o.expr in
        let lowStmt = Ast.ReturnStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | Ast.YieldStmt o ->
        let env, lowExpr = inferExpr env file o.expr in
        let lowStmt = Ast.YieldStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | _ -> todo source "infer-stmt"
  in
  (env, lowStmt)

and inferVars env file expr vars =
  let rec destruct exprs vars acc =
    match (exprs, vars) with
    | [], [] -> acc
    | headExpr :: tailExpr, headVar :: tailVar -> destruct tailExpr tailVar acc
    | _ -> never source "infer-vars"
  in

  (* Ensure that we can destructure the expression as per the expected pattern. *)
  (match (expr, List.hd vars) with
  | Ast.TupleExpr _, Ast.TuplePattern _ -> ()
  | Ast.ArrayExpr _, Ast.ArrayPattern _ -> ()
  | _, Ast.LonePattern _ -> ()
  | _ -> never source "Unable to match pattern.");

  let lowVars =
    destruct
      (match expr with
      (* If variable can be destructured, extract their expression list *)
      | Ast.TupleExpr o -> o.exprs
      | Ast.ArrayExpr o -> o.exprs
      | _ -> [ expr ])
      vars []
  in
  (env, lowVars)

and inferExpr env file expr =
  let env, lowExpr =
    match expr with
    | Ast.NameExpr o ->
        let nameId = Ast.getIdOfName o.value in
        let name = Ast.getStringOfName o.value in
        let types =
          match Store.Module.SymbolMap.find_opt nameId env with
          | Some t -> (
              match t with
              | Store.Module.VarRecord v -> v.types
              | _ -> todo source "infer-environment-symbol")
          | None ->
              raise
                (Report
                   {
                     message = errorLine (fmt "Unable to find identifier %s." (quote name));
                     source = xSOURCE source;
                     error = None;
                   })
        in
        (env, Ast.NameExpr { o with types })
    | Ast.IntExpr o -> (env, Ast.IntExpr { o with types = Ast.IntType })
    | Ast.UnitExpr _ -> (env, expr)
    | _ -> todo source "infer-expr"
  in
  (env, lowExpr)
;;

let inferFile env file =
  let env, entys =
    inferEntities env (Ast.getStringNameOfFile file) (Ast.getEntitiesOfFile file) []
  in
  let file = match file with Ast.File f -> Ast.File { f with entities = entys } in
  file
;;
