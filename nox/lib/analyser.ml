open Core

let isFunction x = match x with Ast.Function _ -> true | _ -> false

let rec inferFunction (file : string) (name : string) (env : Env.env)
    (entity : Ast.entities) =
  match entity with
  | Ast.Function f ->
      let numExprs = f.numExprs in
      let types = Array.make numExprs Ast.NoneType in
      let env, types = inferArgs file env types f.args in
      let env, types = inferBlock file env types f.body in
      (env, types)
  | _ -> xNEVER uPOS "wut?"

and inferBlock file env types body =
  let env = Env.addScope env in
  let rec inferStmts file env types stmts =
    match stmts with
    | [] -> (env, types)
    | head :: tail ->
        let env, types = inferStmt file env types head in
        inferStmts file env types tail
  in
  let env, types = inferStmts file env types (match body with Ast.Block b -> b.stmts) in
  let env = Env.removeScope env in
  (env, types)

and inferStmt file env types stmt =
  let inferExpr file env types expr =
    let id = Get.Ast.idOfExpr expr in
    types.(id) <- inferExprType file env types expr;
    (env, types)
  in
  let env, types =
    match stmt with
    | Ast.LetStmt s ->
        let env, types = inferVars file env types s.vars s.expr in
        (env, types)
    | Ast.SetStmt s ->
        let env, types = inferExpr file env types s.expr in
        (env, types)
    | Ast.AssignStmt s ->
        let env, types = inferAssign file env types s.vars s.expr in
        (env, types)
    | Ast.ReturnStmt s ->
        let env, types = inferExpr file env types s.expr in
        (env, types)
    | Ast.InvokeStmt s ->
        let env, types = inferExpr file env types s.expr in
        (env, types)
    | Ast.BlockStmt s ->
        let env, types = inferBlock file env types s.block in
        (env, types)
    | Ast.IfStmt s ->
        let id = Get.Ast.idOfExpr s.expr in
        (env, types)
    | _ -> (env, types)
  in
  (env, types)

(* Infer arguments *)
and inferArgs (file : string) (env : Env.env) (types : Ast.types array)
    (args : Ast.vars list) =
  let rec aux file env types args argIdx =
    match args with
    | [] -> (env, types)
    | head :: tail ->
        let type' = match head with Ast.Variable arg -> arg.type' |> xSOME uPOS in
        let name = Get.Ast.nameOfVar head in
        let loc =
          match head with
          | Ast.Variable arg -> ( match arg.name with Ast.Name name -> name.loc)
        in
        let env = Env.addRecord env (Env.Variable { name; type'; loc }) name in
        let env, types = aux file env types tail (argIdx + 1) in
        (env, types)
  in
  aux file env types args 0

(* Infer variables *)
and inferVars (file : string) (env : Env.env) (types : Ast.types array)
    (vars : Ast.vars list) (expr : Ast.exprs) =
  let type' = inferExprType file env types expr in
  let id = Get.Ast.idOfExpr expr in
  types.(id) <- type';
  let rec aux file env types vars varIdx =
    match vars with
    | [] -> (env, types)
    | head :: tail ->
        let expectedType = match head with Ast.Variable v -> v.type' in
        let type' =
          match type' with Ast.TupleType v -> List.nth v.types varIdx | _ -> type'
        in
        assure uPOS
          (match expectedType with
          | Some expectedType -> expectedType = type'
          | None -> true)
          (fun _ ->
            raise
              (Report
                 {
                   message = "Unable to match expected and infered type.";
                   source = xSOURCE uPOS;
                   error =
                     Some
                       (Any
                          (Error.UnknownParserError
                             (file, Get.Ast.locOfExpr expr |> Get.Ast.locOfLoc)));
                 }));
        let name = Get.Ast.nameOfVar head in
        let loc = Get.Ast.locOfExpr expr in
        let env = Env.addRecord env (Env.Variable { name; type'; loc }) name in
        let env, types = aux file env types tail (varIdx + 1) in
        (env, types)
  in
  assure uPOS
    (match type' with
    | Ast.TupleType v -> (
        match List.length v.types = List.length vars with true -> true | false -> false)
    | _ -> true)
    (fun _ ->
      raise
        (Report
           {
             message = "Unable to destructure expression.";
             source = xSOURCE uPOS;
             error =
               Some
                 (Any
                    (Error.UnknownParserError
                       (file, Get.Ast.locOfExpr expr |> Get.Ast.locOfLoc)));
           }));
  aux file env types vars 0

(* Infer assignments *)
and inferAssign (file : string) (env : Env.env) (types : Ast.types array)
    (vars : Ast.exprs list) (expr : Ast.exprs) =
  let type' = inferExprType file env types expr in
  let id = Get.Ast.idOfExpr expr in
  types.(id) <- type';
  let rec aux file env types vars varIdx =
    match vars with
    | [] -> (env, types)
    | head :: tail ->
        let expectedType = inferExprType file env types head in
        let assignedType =
          match type' with Ast.TupleType t -> List.nth t.types varIdx | _ -> type'
        in
        let varExprId = Get.Ast.idOfExpr head in
        types.(varExprId) <- expectedType;
        assure uPOS (expectedType = assignedType) (fun _ ->
            raise
              (Report
                 {
                   message = "Unable to assign expression since types don't match.";
                   source = xSOURCE uPOS;
                   error =
                     Some
                       (Any
                          (Error.UnknownParserError
                             (file, Get.Ast.locOfExpr expr |> Get.Ast.locOfLoc)));
                 }));
        let env, types = aux file env types tail (varIdx + 1) in
        (env, types)
  in
  assure uPOS
    (match type' with
    | Ast.TupleType v -> (
        match List.length v.types = List.length vars with true -> true | false -> false)
    | _ -> true)
    (fun _ ->
      raise
        (Report
           {
             message = "Unable to destructure expression.";
             source = xSOURCE uPOS;
             error =
               Some
                 (Any
                    (Error.UnknownParserError
                       (file, Get.Ast.locOfExpr expr |> Get.Ast.locOfLoc)));
           }));
  aux file env types vars 0

(* Infer expression type *)
and inferExprType (file : string) (env : Env.env) (types : Ast.types array) expr =
  let exprId = Get.Ast.idOfExpr expr in
  let fmt = Printf.sprintf in
  let type' =
    match expr with
    | Ast.UnitVal _ -> Ast.UnitType
    | Ast.UndefinedVal _ -> Ast.UndefinedType
    | Ast.IntVal _ -> Ast.IntType
    | Ast.FloatVal _ -> Ast.FloatType
    | Ast.BoolVal _ -> Ast.BoolType
    | Ast.IdVal o ->
        let name = Get.Ast.name o.name in
        let type' =
          match Hashtbl.find_opt env.table name with
          | Some record -> (
              match record with
              | Env.Function f -> f.type'
              | Env.Variable v -> v.type'
              | _ -> xTODO uPOS "infer-environment-record-type")
          | None ->
              raise
                (Report
                   {
                     message = fmt "Identifier \"%s\" does not exist in scope." name;
                     source = xSOURCE uPOS;
                     error =
                       Some
                         (Any
                            (Error.UnknownIdentifier (file, name, Get.Ast.locOfName o.name)));
                   })
        in
        type'
    | Ast.TupleVal o ->
        let types = List.map (fun value -> inferExprType file env types value) o.value in
        let type' = Ast.TupleType { types } in
        type'
    | Ast.InvokeExpr o ->
        let name = Get.Ast.name o.name in
        let type', argtypes =
          match Env.getFunctionType env name with
          | Some type' -> (
              match type' with
              | Ast.FunctionType t -> (t.type', t.args)
              | _ -> xNEVER uPOS "wut?")
          | None ->
              raise
                (Report
                   {
                     message = fmt "Identifier \"%s\" does not exist in scope." name;
                     source = xSOURCE uPOS;
                     error =
                       Some
                         (Any
                            (Error.UnknownIdentifier (file, name, Get.Ast.locOfName o.name)));
                   })
        in
        (* Validate that the function argument types match *)
        assure uPOS
          (List.map (fun arg -> inferExprType file env types arg) o.args
          = List.map simplifyType argtypes)
          (fun _ ->
            raise
              (Report
                 {
                   message = "Function argument types do not match.";
                   source = xSOURCE uPOS;
                   error =
                     Some
                       (Any
                          (Error.UnknownParserError
                             (file, Get.Ast.locOfExpr expr |> Get.Ast.locOfLoc)));
                 }));
        type'
    | Ast.BinOpExpr o ->
        let matchTypes file env lexpr rexpr =
          let ltype = inferExprType file env types lexpr |> simplifyType in
          let rtype = inferExprType file env types rexpr |> simplifyType in
          (* Ensure that the left and right operand types match *)
          assure uPOS (ltype = rtype) (fun _ ->
              raise
                (Report
                   {
                     message = "Binary operand types do not match.";
                     source = xSOURCE uPOS;
                     error =
                       Some
                         (Any
                            (Error.UnknownParserError
                               (file, Get.Ast.locOfExpr expr |> Get.Ast.locOfLoc)));
                   }));
          ltype
        in
        let exprType = matchTypes file env o.lexpr o.rexpr in
        let type' =
          match o.op with
          | Ast.EqOp -> Ast.BoolType
          | Ast.NeOp -> Ast.BoolType
          | Ast.LeOp -> Ast.BoolType
          | Ast.GeOp -> Ast.BoolType
          | Ast.LtOp -> Ast.BoolType
          | Ast.GtOp -> Ast.BoolType
          | _ -> exprType
        in
        type'
    | Ast.UnOpExpr o ->
        let exprType = inferExprType file env types o.expr in
        let type' =
          match o.op with
          | Ast.PosOp -> exprType
          | Ast.NegOp -> exprType
          | Ast.NotOp -> exprType
          | Ast.TryOp -> exprType
          | Ast.ConRefOp -> Ast.ConRefType { life = None; types = exprType }
          | Ast.MutRefOp -> Ast.MutRefType { life = None; types = exprType }
          | Ast.DerefOp -> (
              match exprType with
              | Ast.ConRefType t -> t.types
              | Ast.MutRefType t -> t.types
              | _ ->
                  raise
                    (Report
                       {
                         message = "Unable to dereference expression.";
                         source = xSOURCE uPOS;
                         error = None;
                       }))
        in
        type'
    | Ast.IfExpr _ -> xTODO uPOS "Infer if expression"
    | Ast.ElseIfExpr _ -> xTODO uPOS "Infer else if expression"
    | Ast.ElseExpr _ -> xTODO uPOS "Infer else expression"
    | Ast.EntityExpr _ -> xTODO uPOS "Infer entity expression"
    | Ast.BlockExpr o -> (
        let env, types = inferBlock file env types o.block in
        let lastStmt = last (match o.block with Ast.Block b -> b.stmts) in
        let lastExprId =
          match lastStmt with
          | Ast.SetStmt e -> Some (Get.Ast.idOfExpr e.expr)
          | _ -> None
        in
        match lastExprId with Some id -> types.(id) | None -> Ast.UnitType)
  in
  types.(exprId) <- type';
  type'

(* Simplify types and remove differences, so they can be compared *)
and simplifyType type' =
  let type' =
    match type' with
    | Ast.ConRefType t -> Ast.ConRefType { life = None; types = t.types }
    | Ast.MutRefType t -> Ast.MutRefType { life = None; types = t.types }
    | _ -> type'
  in
  type'
;;

(** Analyse function and lower it to IR *)
let analyseFunction (file : string) (env : Env.env) (entity : Ast.entities) =
  match entity with
  | Ast.Function f ->
      let env, types = inferFunction file (Get.Ast.name f.name) env entity in
      Array.iter (fun t -> if t = Ast.NoneType then xNEVER uPOS "wut?") types;
      (* Array.iter (fun t -> print_endline (Ast.show_types t)) types; *)
      types
  | _ ->
      raise
        (Report
           {
             message = "Unable to analyze entity.";
             source = xSOURCE uPOS;
             error =
               Some (Any (Error.UnknownParserError (file, Get.Ast.locofEntity entity)));
           })
;;

(** Analyse file and lower it to IR *)
let analyseFile (ast : Ast.file) =
  let env = Env.initFileEnv ast in
  match ast with
  | Ast.File f ->
      List.map
        (fun entity -> analyseFunction f.file env entity)
        (List.filter isFunction f.entities)
;;
