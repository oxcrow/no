open Core

let fmt = Printf.sprintf
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
          args = lowerArgs file types f.args;
          stmts = lowerBlock file types f.body;
          type' = lowerType f.type';
        }
  | _ -> xNEVER uPOS "wut?"

and lowerArgs (file : string) (types : Ast.types array) (args : Ast.vars list) =
  let rec aux file types args acc =
    match args with
    | [] -> acc
    | head :: tail ->
        let name = Get.Ast.name (match head with Ast.Variable v -> v.name) in
        let type' =
          lowerType (match head with Ast.Variable v -> v.type' |> xSOME uPOS)
        in
        let argIR : Qbe.args = { name; type' } in
        aux file types tail (argIR :: acc)
  in
  aux file types args []

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
            (fun expr -> Qbe.LetStmt { expr; name = Get.Qbe.regOfExpr expr })
            exprIRs
        in
        letStmtIRs
        @ (Qbe.ReturnStmt { void = false; name = Get.Qbe.regOfExpr lastExprIR } :: [])
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
          {
            value = x.value;
            type' = lowerType exprType;
            reg = { name = exprId |> string_of_int };
          }
        :: []
    | Ast.InvokeExpr x ->
        let argsIRs = List.map (fun arg -> lowerExpr file types arg) x.args in
        let lastArgIRs =
          List.map
            (fun args ->
              let lastExprIR = last args in
              let type' = Get.Qbe.typeOfExpr lastExprIR in
              let name = Get.Qbe.regOfExpr lastExprIR in
              Qbe.RegExpr { reg = { name }; type' })
            argsIRs
        in
        (argsIRs |> List.flatten)
        @ Qbe.CallExpr
            {
              name = Get.Ast.name x.name;
              args = lastArgIRs;
              type' = lowerType exprType;
              reg = { name = exprId |> string_of_int };
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
              (* Since int is supposed to be unsigned, we need udiv *)
              (* For all unsigned types, such as u8, u16, u32, u64, etc. we need this. *)
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
              lreg = { name = lexprId |> string_of_int };
              rreg = { name = rexprId |> string_of_int };
              type' = lowerType lexprType;
              op;
              reg = { name = exprId |> string_of_int };
            }
          :: []
    | Ast.IdVal x ->
        let exprIR =
          match canLowerType exprType with
          | true ->
              Qbe.IdValExpr
                {
                  name = Get.Ast.name x.name;
                  reg = { name = exprId |> string_of_int };
                  type' = lowerType exprType;
                }
          | false -> xTODO uPOS "lower-expr"
        in
        exprIR :: []
    | _ ->
        print_endline (Ast.show_exprs expr);
        xTODO uPOS "lower-expr"
  in
  exprIRs

and canLowerType type' =
  match type' with
  | Ast.UnitType | Ast.IntType | Ast.FloatType | Ast.FunctionType _ -> true
  | _ -> false

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
