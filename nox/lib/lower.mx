open Core
open Get.X

let fmt = Printf.sprintf
let pmt = Printf.printf
let strOfInt = string_of_int

(* Utils *)
let isFunction x = match x with Ast.Function _ -> true | _ -> false

(** Lower a function's AST to QBE IR instructions *)
let rec lowerFunction file env entity =
  let types = Analyser.analyseFunction file env entity in
  match entity with
  | Ast.Function f ->
      let node =
        Qbe.Function
          {
            scope = lowerScope f.scope;
            name = getAstName f.name;
            args = lowerArgs file types f.args;
            stmts = lowerBlocks file types f.body "start";
            type' = lowerType f.type';
          }
      in
      node
  | _ -> xNEVER uPOS "expected-function"

and lowerBlocks file types body name =
  let stmtIRs = match body with Ast.Block b -> lowerStmts file types b.stmts in
  let node = Qbe.LabelStmt { name } :: stmtIRs in
  node

and lowerStmts file types stmts =
  let rec aux file types stmts acc =
    match stmts with
    | [] -> List.flatten (List.rev acc)
    | h :: t ->
        let node = lowerStmt file types h in
        aux file types t (node :: acc)
  in
  aux file types stmts []

and lowerStmt file types stmt =
  let stmtIRs =
    match stmt with
    | Ast.LetStmt s ->
        let exprIRs, exprReg = lowerExpr file types s.expr in
        let exprId = Get.Ast.idOfExpr s.expr in
        let exprType = types.(exprId) in
        let stmtIRs =
          List.map
            (fun expr ->
              match expr with
              | Qbe.FieldExpr e ->
                  Qbe.LetStmt { var = { name = fmt "%s.f%d" e.var.name e.idx }; expr }
              | Qbe.StoreExpr e -> Qbe.StoreStmt { expr }
              | Qbe.BlockExpr e -> Qbe.LotsOfStmt { stmts = e.stmts }
              | _ -> Qbe.LetStmt { var = { name = getQbeRegOfExpr expr }; expr })
            exprIRs
        in
        let varIRs =
          let rec lowerVars vars idx acc =
            match vars with
            | [] -> List.flatten (List.rev acc)
            | h :: t ->
                let name = getAstNameOfVar h in
                let varId = Get.Ast.idOfVar h in
                let varType = types.(varId) in
                let var =
                  match varType with
                  | Ast.UnitType -> []
                  | _ -> (
                      match exprType with
                      | Ast.TupleType _ ->
                          [
                            Qbe.LetStmt
                              {
                                var = { name };
                                expr =
                                  Qbe.LoadExpr
                                    {
                                      var = { name = exprReg |> strOfInt };
                                      type' = lowerType varType;
                                      from = { name = fmt "%d.f%d" exprReg idx };
                                    };
                              };
                          ]
                      | _ ->
                          [
                            Qbe.LetStmt
                              {
                                var = { name };
                                expr =
                                  Qbe.RegExpr
                                    {
                                      var = { name = exprReg |> strOfInt };
                                      type' = lowerType varType;
                                    };
                              };
                          ])
                in
                lowerVars t (idx + 1) (var :: acc)
          in
          lowerVars s.vars 0 []
        in
        let nodes = stmtIRs @ varIRs in
        nodes
    | Ast.ReturnStmt s ->
        let exprIRs, exprReg = lowerExpr file types s.expr in
        let exprType = types.(Get.Ast.idOfExpr s.expr) in
        let stmtIRs =
          List.map
            (fun expr ->
              match expr with
              | Qbe.FieldExpr e ->
                  Qbe.LetStmt { var = { name = fmt "%s.f%d" e.var.name e.idx }; expr }
              | Qbe.StoreExpr e -> Qbe.StoreStmt { expr }
              | Qbe.BlockExpr e -> Qbe.LotsOfStmt { stmts = e.stmts }
              | _ -> Qbe.LetStmt { var = { name = getQbeRegOfExpr expr }; expr })
            exprIRs
        in
        let nodes =
          stmtIRs
          @ [
              Qbe.ReturnStmt
                {
                  var =
                    (match exprType with
                    | Ast.UnitType -> None
                    | _ -> Some Qbe.{ name = exprReg |> strOfInt });
                };
            ]
        in
        nodes
    | Ast.SetStmt s ->
        let exprIRs, exprReg = lowerExpr file types s.expr in
        let exprType = types.(Get.Ast.idOfExpr s.expr) in
        let stmtIRs =
          List.map
            (fun expr ->
              match expr with
              | Qbe.FieldExpr e ->
                  Qbe.LetStmt { var = { name = fmt "%s.f%d" e.var.name e.idx }; expr }
              | Qbe.StoreExpr e -> Qbe.StoreStmt { expr }
              | Qbe.BlockExpr e -> Qbe.LotsOfStmt { stmts = e.stmts }
              | _ -> Qbe.LetStmt { var = { name = getQbeRegOfExpr expr }; expr })
            exprIRs
        in
        let nodes =
          stmtIRs
          @
          match exprType with
          | Ast.UnitType -> []
          | _ ->
              [
                Qbe.LetStmt
                  {
                    var = Qbe.{ name = strOfInt exprReg };
                    expr =
                      Qbe.RegExpr
                        { var = { name = strOfInt exprReg }; type' = lowerType exprType };
                  };
              ]
        in
        nodes
    | _ -> []
  in
  stmtIRs

and lowerExpr file types expr =
  let exprId = Get.Ast.idOfExpr expr in
  let exprType = types.(exprId) in
  let exprReg = exprId in
  let exprIRs =
    match expr with
    | Ast.BinOpExpr e ->
        let lexprId = Get.Ast.idOfExpr e.lexpr in
        let lexprType = types.(lexprId) in
        let op =
          match e.op with
          | Ast.AddOp -> Qbe.AddOp
          | Ast.SubOp -> Qbe.SubOp
          | Ast.MulOp -> Qbe.MulOp
          | Ast.DivOp -> (
              (* Since int is supposed to be unsigned, we need udiv *)
              (* For all unsigned types, such as u8, u16, u32, u64, etc. we need this. *)
              match lexprType with
              | Ast.IntType -> Qbe.UDivOp
              | _ ->
                  xTODO uPOS "lower-division";
                  Qbe.DivOp)
          | _ -> xTODO uPOS "lower-binop-code"
        in
        let lexprIRs, lexprReg = lowerExpr file types e.lexpr in
        let rexprIRs, rexprReg = lowerExpr file types e.rexpr in
        let nodes =
          (lexprIRs @ rexprIRs)
          @ [
              Qbe.BinOpExpr
                {
                  var = { name = exprId |> strOfInt };
                  lreg = { name = lexprReg |> strOfInt };
                  rreg = { name = rexprReg |> strOfInt };
                  type' = lowerType lexprType;
                  op;
                };
            ]
        in
        nodes
    | Ast.BlockExpr e ->
        let stmts = match e.block with Ast.Block b -> b.stmts in
        let stmtIRs = lowerStmts file types stmts in
        let nodes =
          [
            Qbe.BlockExpr
              {
                var = { name = exprId |> strOfInt };
                stmts = stmtIRs;
                type' = lowerType exprType;
              };
          ]
        in
        nodes
    | Ast.InvokeExpr e ->
        let argsIRs, argsRegs =
          List.split (List.map (fun arg -> lowerExpr file types arg) e.args)
        in
        let lastArgIRs =
          List.map
            (fun args ->
              let lastExprIR = last args in
              let type' = getQbeTypeOfExpr lastExprIR in
              let name = getQbeRegOfExpr lastExprIR in
              Qbe.RegExpr { var = { name }; type' })
            argsIRs
        in
        let nodes =
          List.flatten argsIRs
          @ [
              Qbe.CallExpr
                {
                  var = { name = exprId |> strOfInt };
                  name = getAstName e.name;
                  args = lastArgIRs;
                  type' = lowerType exprType;
                };
            ]
        in
        nodes
    | Ast.IdVal e ->
        let node =
          match canLowerType exprType with
          | true ->
              let node =
                [
                  Qbe.IdValExpr
                    {
                      name = getAstName e.name;
                      var = { name = exprId |> strOfInt };
                      type' = lowerType exprType;
                    };
                ]
              in
              node
          | false -> xTODO uPOS "lower-expr"
        in
        node
    | Ast.TupleVal e ->
        let align = alignOfType exprType in
        let size = sizeOfType exprType in
        let exprIRs, exprRegs =
          List.split (List.map (fun expr -> lowerExpr file types expr) e.value)
        in
        let alloc =
          [
            Qbe.AllocExpr
              {
                var = { name = exprId |> strOfInt };
                align;
                size;
                type' = lowerType exprType;
              };
          ]
        in
        let fields =
          let rec lowerFields fields offset idx acc =
            match fields with
            | [] -> List.rev acc
            | h :: t ->
                let align = alignOfType h in
                let size = sizeOfType h in
                let newOffset =
                  match h with
                  | Ast.UnitType -> offset
                  | _ -> (
                      match offset mod align = 0 with
                      | true -> offset
                      | false -> offset + align - (offset mod align))
                in
                let field =
                  match h with
                  | Ast.UnitType -> []
                  | _ ->
                      [
                        Qbe.FieldExpr
                          {
                            var = { name = exprId |> strOfInt };
                            align;
                            size;
                            offset = newOffset;
                            idx;
                            type' = Qbe.LongType;
                          };
                      ]
                in
                lowerFields t (newOffset + size) (idx + 1) (field @ acc)
          in
          lowerFields
            (match exprType with Ast.TupleType t -> t.types | _ -> xNEVER uPOS "wut?")
            0 0 []
        in
        let stores =
          let rec lowerStore types regs idx acc =
            match (types, regs) with
            | [], [] -> List.rev acc
            | h :: t, rh :: rt ->
                let store =
                  match h with
                  | Ast.UnitType -> []
                  | _ ->
                      [
                        Qbe.StoreExpr
                          {
                            var = { name = exprId |> strOfInt };
                            type' = lowerType h;
                            from = { name = rh |> strOfInt };
                            dest = { name = fmt "%d.f%d" exprId idx };
                          };
                      ]
                in
                lowerStore t rt (idx + 1) (store @ acc)
            | _ -> xNEVER uPOS "wut?"
          in
          lowerStore
            (match exprType with Ast.TupleType t -> t.types | _ -> xNEVER uPOS "wut?")
            exprRegs 0 []
        in
        let nodes = List.flatten exprIRs @ alloc @ fields @ stores in
        nodes
    | Ast.IntVal e ->
        let nodes =
          [
            Qbe.TermExpr
              {
                var = { name = exprId |> strOfInt };
                value = e.value;
                type' = lowerType exprType;
              };
          ]
        in
        nodes
    | Ast.UnitVal _ -> []
    | _ ->
        pmt "Unknown expression: %s\n" (Ast.show_exprs expr);
        xTODO uPOS "lower-expr"
  in
  (exprIRs, exprReg)

and lowerArgs file types args =
  let rec aux file types args acc =
    match args with
    | [] -> acc
    | h :: t ->
        let name = getAstNameOfVar h in
        let type' = lowerType (getAstTypeOfVar h |> xSOME uPOS) in
        let argIR = Qbe.{ name; type' } in
        aux file types t (argIR :: acc)
  in
  aux file types args []

and canLowerType type' =
  match type' with
  | Ast.UnitType | Ast.IntType | Ast.FloatType | Ast.FunctionType _ -> true
  | _ -> false

and lowerType type' =
  match type' with
  | Ast.TupleType _ -> Qbe.LongType
  | Ast.UnitType -> Qbe.VoidType
  | Ast.IntType -> Qbe.LongType
  | Ast.FloatType -> Qbe.DoubleType
  | Ast.FunctionType t -> lowerType t.type'
  | _ -> xTODO uPOS "lower-type"

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
              | Ast.UnitType -> offset
              | _ -> (
                  match offset mod align = 0 with
                  | true -> offset
                  | false -> offset + align - (offset mod align))
            in
            calcStructSize t (newOffset + size) (acc + (newOffset - offset) + size)
      in
      calcStructSize t.types 0 0
  | Ast.UnitType -> 0
  | Ast.IntType -> 8
  | Ast.FloatType -> 8
  | Ast.FunctionType t -> sizeOfType t.type'
  | _ -> xTODO uPOS "size-of-type"

and alignOfType type' =
  match type' with
  | Ast.TupleType t ->
      let aligns = List.map alignOfType t.types in
      List.fold_left max (List.hd aligns) (List.tl aligns)
  | Ast.UnitType -> 0
  | Ast.IntType -> 8
  | Ast.FloatType -> 8
  | Ast.FunctionType t -> sizeOfType t.type'
  | _ -> xTODO uPOS "align-of-type"

and lowerScope scope =
  match scope with
  | Ast.PublicScope | Ast.LocalScope -> Qbe.ExportScope
  | Ast.PrivateScope -> Qbe.PrivateScope
;;

(** Lower file's AST to QBE IR instructions *)
let lowerFile ast =
  let env = Env.initFileEnv ast in
  let defns =
    match ast with
    | Ast.File f ->
        List.map
          (fun entity -> lowerFunction f.file env entity)
          (List.filter isFunction f.entities)
  in
  (* List.iter (fun d -> print_endline (Qbe.show_defns d)) defns; *)
  defns
;;
