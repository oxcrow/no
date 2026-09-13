open Core

let rec inferEntities env file entys acc =
  match entys with
  | [] -> (env, List.rev acc)
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
        Store.Module.printEnv env;
        (env, lowEnty)
    | _ -> todo source "infer-entity"
  in
  lowEnty

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
        let env, lowVars = inferVars env file lowExpr o.vars in
        let lowStmt = Ast.LetStmt { o with expr = lowExpr; vars = lowVars } in
        (env, lowStmt)
    | Ast.ReturnStmt o ->
        let env, lowExpr, lowType = inferExpr env file o.expr in
        let lowStmt = Ast.ReturnStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | Ast.YieldStmt o ->
        let env, lowExpr, lowType = inferExpr env file o.expr in
        let lowStmt = Ast.YieldStmt { o with expr = lowExpr } in
        (env, lowStmt)
    | _ -> todo source "infer-stmt"
  in
  (env, lowStmt)

and inferVars env file expr vars =
  let rec destructPattern env expr pat acc =
    let rec unwrapPattern env exprs pats acc =
      match (exprs, pats) with
      | [], [] -> (env, List.rev acc)
      | headExpr :: tailExpr, headPat :: tailPat ->
          (* Unwrap expression and recurse destruct pattern when needed. *)
          let env, lowVars =
            match headPat with
            | Ast.TuplePattern p ->
                let env, lowVars = destructPattern env headExpr p.pats [] in
                (env, lowVars)
            | Ast.ArrayPattern p ->
                let env, lowVars = destructPattern env headExpr p.pats [] in
                (env, lowVars)
            | Ast.LonePattern p ->
                let headVar = p.var in
                let expectedType = Ast.getTypeOfVar headVar in
                let exprType = Ast.getTypeOfExpr headExpr in

                (* Ensure that the expected type of variable (if present), is respected *)
                assure source
                  (match expectedType with Ast.NoneType -> true | _ -> expectedType = exprType)
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
                    {
                      name;
                      nameId;
                      types = exprType;
                      varId = uuid.varId;
                      entyId = uuid.entyId;
                      loc;
                    }
                in

                let env = Store.Module.SymbolMap.add nameId record env in
                let var = match headVar with Ast.Var v -> Ast.Var { v with type' = exprType } in
                debug source (Ast.show_types exprType);

                (env, var :: acc)
            | _ -> never source "wut?"
          in
          unwrapPattern env tailExpr tailPat acc
      | _ -> never source "unwrap-pattern"
    in

    (* Find which pattern and expression we need to destructure *)
    let env, lowVars =
      match pat with
      | headPat :: tailPat -> (
          match (expr, headPat) with
          (* Complex expressions with complex patterns *)
          | Ast.TupleExpr e, Ast.TuplePattern p ->
              write "UNWRAPPING (TUPLE EXPRESSION, TUPLE PATTERN)!";
              let env, lowVars = unwrapPattern env e.exprs p.pats [] in
              (env, lowVars)
          | Ast.ArrayExpr e, Ast.ArrayPattern p ->
              write "UNWRAPPING (ARRAY EXPRESSION, ARRAY PATTERN)!";
              let env, lowVars = unwrapPattern env e.exprs p.pats [] in
              (env, lowVars)
          (* Complex expressions with lone patterns *)
          | Ast.TupleExpr e, Ast.LonePattern p ->
              write "UNWRAPPING (TUPLE EXPRESSION, LONE PATTERN)!";
              let env, lowVars =
                match List.length pat with
                | 1 -> unwrapPattern env [ expr ] pat []
                | _ -> unwrapPattern env e.exprs pat []
              in
              (env, lowVars)
          | Ast.ArrayExpr e, Ast.LonePattern p ->
              write "UNWRAPPING (ARRAY EXPRESSION, LONE PATTERN)!";
              let env, lowVars =
                match List.length pat with
                | 1 -> unwrapPattern env [ expr ] pat []
                | _ -> unwrapPattern env e.exprs pat []
              in
              (env, lowVars)
          (* Simple expressions with lone patterns *)
          | _, Ast.LonePattern p ->
              write "UNWRAPPING (SIMPLE EXPRESSION, LONE PATTERN)!";
              let env, lowVars = unwrapPattern env [ expr ] [ headPat ] [] in
              (env, lowVars)
          | _ ->
              raise
                (Report
                   {
                     message = errorLine "Unable to match expected type in pattern.";
                     source = xSOURCE source;
                     error = None;
                   }))
      | _ -> never source "infer-vars"
    in
    (env, lowVars)
  in

  let env, lowVars = destructPattern env expr vars [] in
  Store.Module.printEnv env;
  (env, [ Ast.Vars { vars = lowVars } ])

and inferExprs env file exprs accExpr accTypes =
  match exprs with
  | [] -> (env, List.rev accExpr, List.rev accTypes)
  | headExpr :: tailExpr ->
      let env, lowExpr, lowType = inferExpr env file headExpr in
      inferExprs env file tailExpr (lowExpr :: accExpr) (lowType :: accTypes)

(* BUG: The complex types are not calculated correctly. *)
and inferExpr env file expr =
  let env, lowExpr, lowType =
    match expr with
    | Ast.TupleExpr o ->
        let env, lowExpr, lowType = inferExprs env file o.exprs [] [] in
        let types = Ast.TupleType { types = lowType; offsets = []; align = 0; size = 0 } in
        (env, Ast.TupleExpr { o with exprs = lowExpr; types }, types)
    | Ast.ArrayExpr o ->
        let env, lowExpr, lowType = inferExprs env file o.exprs [] [] in
        let types = Ast.ArrayType { types = List.hd lowType; elems = Ast.NoneExpr } in
        (env, Ast.ArrayExpr { o with exprs = lowExpr }, types)
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
        (env, Ast.NameExpr { o with types }, types)
    | Ast.IntExpr o -> (env, Ast.IntExpr { o with types = Ast.IntType }, Ast.IntType)
    | Ast.UnitExpr _ -> (env, expr, Ast.UnitType)
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
