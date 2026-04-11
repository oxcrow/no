open Core

let isFunction x = match x with Ast.Function _ -> true | _ -> false

(** Analyse function and lower it to IR *)
let rec lowerFunction (file : string) (env : Env.env) (entity : Ast.entities) =
  let types = Analyser.analyseFunction file env entity in
  match entity with
  | Ast.Function f ->
      Qbe.Function
        {
          export = (match f.scope with Ast.PublicScope -> true | _ -> false);
          name = Get.Ast.name f.name;
          args = [];
          stmts = lowerBlock file types f.body;
          type' = lowerType f.type';
        }
  | _ -> xNEVER uPOS "wut?"

and lowerBlock (file : string) (types : Ast.types array) (body : Ast.blocks) =
  let rec lowerStmts file types stmts acc =
    match stmts with
    | [] -> acc
    | head :: tail ->
        let stmtIRs = lowerStmt file types head in
        lowerStmts file types tail (stmtIRs @ acc)
  in
  match body with Ast.Block b -> lowerStmts file types b.stmts []

and lowerStmt file types stmt =
  let stmtIRs =
    match stmt with
    | Ast.ReturnStmt s ->
        let exprIRs = lowerExpr file types s.expr in
        let lastExprIR = last exprIRs in
        let letStmtIRs =
          List.map
            (fun expr ->
              Qbe.LetStmt { expr; id = Get.Qbe.idOfExpr expr; ix = Get.Qbe.ixOfExpr expr })
            exprIRs
        in
        letStmtIRs
        @ (Qbe.ReturnStmt { void = false; id = Get.Qbe.idOfExpr lastExprIR; ix = 0 } :: [])
    | _ -> xTODO uPOS "lower-stmt"
  in
  stmtIRs

and lowerExpr file types expr =
  let exprId = Get.Ast.idOfExpr expr in
  let exprType = types.(exprId) in
  let exprIRs =
    match expr with
    | Ast.IntVal x ->
        Qbe.TermExpr
          { value = x.value; type' = lowerType exprType; reg = { id = exprId; ix = 0 } }
        :: []
    | Ast.InvokeExpr x ->
        Qbe.CallExpr
          {
            name = Get.Ast.name x.name;
            args = [];
            type' = lowerType exprType;
            reg = { id = exprId; ix = 0 };
          }
        :: []
    | Ast.BinOpExpr x ->
        let lexprId = Get.Ast.idOfExpr x.lexpr in
        let rexprId = Get.Ast.idOfExpr x.rexpr in
        let lexprType = types.(lexprId) in
        let op =
          match x.op with
          | Ast.AddOp -> Qbe.AddOp
          | Ast.SubOp -> Qbe.SubOp
          | Ast.MulOp -> Qbe.MulOp
          | Ast.DivOp -> (
              (* Since int is supposed to be same as u64, we need udiv *)
              match lexprType with
              | Ast.IntType -> Qbe.UDivOp
              | _ -> Qbe.DivOp)
          | _ -> xTODO uPOS "infer-binop-code"
        in
        let lexprIRs = lowerExpr file types x.lexpr in
        let rexprIRs = lowerExpr file types x.rexpr in
        (lexprIRs @ rexprIRs)
        @ Qbe.BinOpExpr
            {
              lreg = { id = lexprId; ix = 0 };
              rreg = { id = rexprId; ix = 0 };
              type' = lowerType lexprType;
              op;
              reg = { id = exprId; ix = 0 };
            }
          :: []
    | _ -> xTODO uPOS "infer-expr"
  in
  exprIRs

and lowerType type' =
  match type' with
  | Ast.UnitType -> Qbe.VoidType
  | Ast.IntType -> Qbe.LongType
  | Ast.FloatType -> Qbe.DoubleType
  | Ast.FunctionType t -> lowerType t.type'
  | _ -> xTODO uPOS "lower-type"
;;

(** Analyse file and lower it to IR *)
let lowerFile (ast : Ast.file) =
  let env = Env.initFileEnv ast in
  let definitions =
    match ast with
    | Ast.File f ->
        List.map
          (fun entity -> lowerFunction f.file env entity)
          (List.filter isFunction f.entities)
  in
  (* List.iter (fun x -> print_endline (Qbe.show_definitions x)) definitions; *)
  definitions
;;
