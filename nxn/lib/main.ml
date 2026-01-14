open Error.Failure
open Error.Unwrap
open Utils.String
open Utils.Tuple

(* Location *)
external loc : string = "%loc_LOC"

let write = print_endline
let unit = ()

(** Debug AST *)
let dbg ast = write (Ast.show_file ast)

(** User guide *)
let usage () =
  write "NxN: An elegant systems programming language.";
  write "";
  write "Usage:";
  write "    nxn <file.nxn>";
  write ""
;;

(** What file to compile? *)
let what () =
  let file =
    match Array.length Sys.argv < 2 with
    | true ->
        usage ();
        failwith "Expected file to compile."
    | false -> Sys.argv.(1)
  in
  file
;;

(** Evalulate error message *)
let emsg file pos msg =
  let lines = String.split_on_char '\n' (File.read_file_content file) in
  let lnum, cnum = pos in

  let line lnum =
    let line =
      match lnum > 0 && lnum < List.length lines with
      | true -> Format.sprintf "%*d" 8 lnum ^ " ┊  " ^ List.nth lines (lnum - 1)
      | false -> "       ~ ┊"
    in
    line
  in

  let l0 =
    msg ^ nl ^ sp
    ^ dot
        (" Around [Line Number: " ^ string_of_int lnum ^ ", Column Number: "
       ^ string_of_int cnum ^ "]")
    ^ nl
  in

  let l1 = line (lnum - 2) ^ nl in
  let l2 = line (lnum - 1) ^ nl in
  let l3 = line (lnum - 0) ^ red " <<< ERROR!" ^ nl in
  let l4 = line (lnum + 1) ^ nl in
  let l5 = line (lnum + 2) ^ nl in
  let l6 = sp ^ hot (" Source? [File: " ^ quote file ^ "]") ^ nl in

  l0 ^ l1 ^ l2 ^ l3 ^ l4 ^ l5 ^ l6
;;

(** Evalulate error message *)
let wmsg error info = error ^ nl ^ sp ^ cot (sp ^ info)

(** Parse nxn code and create AST *)
let parse code file =
  let buf = Lexing.from_string code in
  let ast =
    let epos (buf : Lexing.lexbuf) =
      let lnum = buf.lex_curr_p.pos_lnum in
      let cnum = buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol in
      (lnum, cnum)
    in
    try Parser.file Lexer.token buf with
    | Parser.Error ->
        let why =
          emsg file (epos buf)
            "Parser error.(Error could also be from lines above.)"
        in
        error loc why
    | Failure msg ->
        let why =
          emsg file (epos buf)
            "Parser error.(Error could also be from lines above.)"
        in
        error loc (why ^ ", With message: " ^ msg)
  in
  let ast =
    match ast with Ast.File f -> Ast.File { entities = f.entities; file }
  in
  ast
;;

(** Type infer and semantic analyze the AST (complex as hecc!)

    It's annoying to write AST visitors all the time. So, we will analyze,
    infer, and verify the AST in one horrifying lovecraftian monolithic step,
    without caring about anything else.

    May Gods save the poor soul who has to maintain this code in future.

    I apologize for this; but there was no other way.

    ~ oxcrow *)
let analyze ast =
  let file = GetAst.File.name ast in
  let func_name = ref "I DON'T HECCIN' KNOW!" in
  (* Create environment *)
  let envir ast =
    (* Create environment record list of functions *)
    let envfns entities =
      List.map
        (fun entity ->
          match entity with
          | Ast.Function f ->
              let xpos = GetAst.Entity.xpos entity in
              let name = GetAst.Id.value f.name in
              let type' = f.type' in
              (name, type', xpos)
          | _ -> never loc "Only functions are allowed.")
        (List.filter
           (fun e -> match e with Ast.Function _ -> true | _ -> false)
           entities)
    in

    (* Validate environment records *)
    let validate records =
      let rec count id n records =
        match records with
        | [] -> n
        | (x, _, _) :: tl ->
            if x = id then count id (n + 1) tl else count id n tl
      in
      (* We should report which identifier has multiple ocurrances *)
      let counts =
        List.map (fun (id, _, xpos) -> (count id 0 records, xpos)) records
      in
      List.iter
        (fun (n, xpos) ->
          if n > 1 then
            warn loc (emsg file xpos "Multiple occurances of identifier."))
        counts;
      (* Remove position from records *)
      let result = List.map (fun (id, type', _) -> (id, type')) records in
      result
    in
    (* Collect entities for environment *)
    let functions =
      match ast with Ast.File f -> envfns f.entities |> validate
    in
    let structs = [] in
    let enums = [] in
    (* Create enironment *)
    let env = Env.File { name = file; functions; structs; enums; vars = [] } in
    env
  in

  (* Extend environment with new variables *)
  let rec extend_env env vars =
    let extend env var =
      match var with
      | Ast.Var v ->
          let name = GetAst.Id.value v.name in
          let type' = v.type' in
          let state = v.state in
          let shadow = v.shadow in
          let found_prev_var, found_prev_shadow =
            let found_var =
              match GetEnv.File.var_type name env with
              | Some _ -> true
              | None -> false
            in
            let found_shadow = GetEnv.File.does_var_shadow name env in
            (found_var, found_shadow)
          in
          (* Only varibles previously marked to be shadowed, can be shadowed. *)
          if found_prev_var = true then
            assure loc (found_prev_shadow = true) (fun _ ->
                emsg file (GetAst.Id.xpos v.name)
                  "Shadowing variables without shadow mark is not allowed.");
          let env =
            Env.Add.File.var name (Env.Var { type'; state; shadow }) env
          in
          env
      | Ast.NoneVar -> never loc "NoneVar can not be added to the environment."
    in
    match vars with
    | [] -> env
    | hd :: tl ->
        let env = extend env hd in
        extend_env env tl
  in

  let infer_vars vars type' =
    let infer_var var type' =
      let expected_type = GetAst.Var.type' var in
      if expected_type <> Ast.NoneType && type' <> Ast.UndefinedType then
        assure loc (expected_type = type') (fun _ ->
            emsg file (GetAst.Var.xpos var)
              "Inferred expression type does not match expected variable type.");
      (* If type is specified by the user, then overwrite the infered type *)
      let type' =
        if expected_type <> Ast.NoneType then expected_type else type'
      in
      match var with
      | Ast.Var v ->
          Ast.Var { name = v.name; state = v.state; shadow = v.shadow; type' }
      | Ast.NoneVar -> never loc "Nonevar can not be inferred."
    in
    let vars =
      match vars with
      | [] -> never loc "No variables exist to infer."
      | [ var ] -> [ infer_var var type' ]
      | vars ->
          let data =
            List.combine vars
              (let types =
                 match type' with
                 | Ast.TupleType o -> o.types
                 | _ -> never loc "Only struct types can be combined?"
               in
               assure loc
                 (List.length vars = List.length types)
                 (fun _ ->
                   emsg file
                     (GetAst.Var.xpos (List.nth vars 0))
                     (wmsg "Unable to destructure and bind to variables."
                        "Number of variables don't match the number of \
                         expressions being assigned."));
               types)
          in
          let vars = List.map (fun (v, s) -> infer_var v s) data in
          vars
    in
    vars
  in

  (* Simplify types and remove differences, so they can be compared *)
  let simplify_type type' =
    let type' =
      match type' with
      | Ast.ConRefType t -> Ast.ConRefType { life = None; types = t.types }
      | Ast.MutRefType t -> Ast.MutRefType { life = None; types = t.types }
      | _ -> type'
    in
    type'
  in

  (* Infer expression type *)
  let rec infer_expr_type env expr =
    let type' =
      match expr with
      | Ast.TerminalExpr term -> (
          match term.value with
          | Ast.UnitVal -> Ast.UnitType
          | Ast.UndefinedVal -> Ast.UndefinedType
          | Ast.IntVal _ -> Ast.IntType
          | Ast.FloatVal _ -> Ast.FloatType
          | Ast.BoolVal _ -> Ast.BoolType
          | Ast.IdVal o ->
              let id = GetAst.Id.value o.value in
              let type' =
                match GetEnv.File.var_type id env with
                | Some type' -> type'
                | None ->
                    error loc
                      (emsg file (GetAst.Id.xpos o.value)
                         ("Identifier " ^ quote id
                        ^ " does not exist in environment."))
              in
              type'
          | Ast.TupleVal o ->
              let types = List.map (infer_expr_type env) o.value in
              let type' = Ast.TupleType { types } in
              type')
      | Ast.InvokeExpr o ->
          let name = GetAst.Id.value o.value in
          let type', argtypes =
            match GetEnv.File.function_type name env with
            | Some type' -> (
                match type' with
                | Ast.FunctionType t -> (t.type', t.args)
                | _ -> never loc "")
            | None ->
                error loc
                  (emsg file (GetAst.Id.xpos o.value)
                     ("Function " ^ quote name
                    ^ " does not exist in environment."))
          in
          (* Validate that the function argument types match *)
          assure loc
            (List.map (fun arg -> infer_expr_type env arg) o.args
            = List.map simplify_type argtypes)
            (fun _ ->
              emsg file (GetAst.Expr.xpos expr)
                "Function argument types don't match.");
          type'
      | Ast.BinOpExpr o ->
          let match_types env lvalue rvalue =
            let ltype = infer_expr_type env lvalue |> simplify_type in
            let rtype = infer_expr_type env rvalue |> simplify_type in
            assure loc (ltype = rtype) (fun _ ->
                emsg file (GetAst.Expr.xpos expr)
                  "Binary operator types mismatch.");
            ltype
          in
          let value_type = match_types env o.lvalue o.rvalue in
          let type' =
            match o.op with
            | Ast.EqOp -> Ast.BoolType
            | Ast.NeOp -> Ast.BoolType
            | Ast.LeOp -> Ast.BoolType
            | Ast.GeOp -> Ast.BoolType
            | Ast.LtOp -> Ast.BoolType
            | Ast.GtOp -> Ast.BoolType
            | _ -> value_type
          in
          type'
      | Ast.UnOpExpr o ->
          let value_type = infer_expr_type env o.value in
          let type' =
            match o.op with
            | Ast.PosOp -> value_type
            | Ast.NegOp -> value_type
            | Ast.NotOp -> value_type
            | Ast.TryOp -> value_type
            | Ast.ConRefOp -> Ast.ConRefType { life = None; types = value_type }
            | Ast.MutRefOp -> Ast.MutRefType { life = None; types = value_type }
            | Ast.DerefOp -> (
                match value_type with
                | Ast.ConRefType t -> t.types
                | Ast.MutRefType t -> t.types
                | _ ->
                    assure loc false (fun _ ->
                        emsg file (GetAst.Expr.xpos expr)
                          "Can not dereference this expression.");
                    never loc "wut?")
          in
          type'
      | Ast.IfExpr _ -> todo loc "Infer if expression"
      | Ast.ElseIfExpr _ -> todo loc "Infer else if expression"
      | Ast.ElseExpr _ -> todo loc "Infer else expression"
      | Ast.EntityExpr _ -> todo loc "Infer entity expression"
      | Ast.BlockExpr _ -> todo loc "Infer block expression"
    in
    type'
  (* Infer expression *)
  and infer_expr env expr =
    let type', expr =
      match expr with
      | Ast.TerminalExpr term -> (
          match term.value with
          | Ast.UndefinedVal ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.UnitVal ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.BoolVal _ ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.IntVal _ ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.FloatVal _ ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.IdVal _ ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.TupleVal o ->
              let result = List.map (infer_expr env) o.value in
              let types = List.map first result in
              let exprs = List.map second result in
              let value = Ast.TupleVal { value = exprs } in
              let type' = Ast.TupleType { types } in
              let pos = GetAst.Expr.pos expr in
              let expr = Ast.TerminalExpr { value; type'; pos } in
              (type', expr))
      | Ast.InvokeExpr o ->
          let type' = infer_expr_type env expr in
          let _, args = List.map (infer_expr env) o.args |> List.split in
          let expr = SetAst.Expr.with_args expr args in
          let expr = SetAst.Expr.with_type expr type' in
          (type', expr)
      | Ast.BinOpExpr o ->
          let expr =
            Ast.BinOpExpr
              {
                lvalue = second (infer_expr env o.lvalue);
                op = o.op;
                rvalue = second (infer_expr env o.rvalue);
                type' = o.type';
                pos = o.pos;
              }
          in
          let type' = infer_expr_type env expr in
          let expr = SetAst.Expr.with_type expr type' in
          (type', expr)
      | Ast.UnOpExpr o ->
          let expr =
            Ast.UnOpExpr
              {
                value = second (infer_expr env o.value);
                op = o.op;
                type' = o.type';
                pos = o.pos;
              }
          in
          let type' = infer_expr_type env expr in
          let expr = SetAst.Expr.with_type expr type' in
          (type', expr)
      | Ast.IfExpr o ->
          let validate_branch_type expr =
            let rec aux expr type' =
              match expr with
              | Ast.IfExpr o -> (
                  assure loc (o.type' = type') (fun _ ->
                      emsg file (GetAst.Expr.xpos expr)
                        "If branch type does not match with expected type.");
                  match o.other with Some x -> aux x type' | None -> unit)
              | Ast.ElseIfExpr o -> (
                  assure loc (o.type' = type') (fun _ ->
                      emsg file (GetAst.Expr.xpos expr)
                        "Else If branch type does not match with expected type.");
                  match o.other with Some x -> aux x type' | None -> unit)
              | Ast.ElseExpr o ->
                  assure loc (o.type' = type') (fun _ ->
                      emsg file (GetAst.Expr.xpos expr)
                        "Else branch type does not match with expected type.")
              | _ ->
                  never loc
                    (emsg file (GetAst.Expr.xpos expr)
                       "Only if/else expressions are allowed in conditionals.")
            in
            aux expr (GetAst.Expr.type' expr);
            expr
          in
          let validate_else_branch expr =
            let rec aux expr =
              match expr with
              | Ast.IfExpr o -> (
                  match o.is_stmt with
                  | true -> true
                  | false -> (
                      match o.other with Some x -> aux x | None -> false))
              | Ast.ElseIfExpr o -> (
                  match o.other with Some x -> aux x | None -> false)
              | Ast.ElseExpr _ -> true
              | _ -> never loc "Only if/else expressions are allowed."
            in
            let valid = aux expr in
            assure loc valid (fun _ ->
                emsg file (GetAst.Expr.xpos expr)
                  "Else branch not found for conditional expression");
            expr
          in
          let block = infer_block false env o.block in
          let type' = infer_block_type block in
          let expr =
            Ast.IfExpr
              {
                cond =
                  (let type', expr = infer_expr env o.cond in
                   assure loc (type' = Ast.BoolType) (fun _ ->
                       emsg file (GetAst.Expr.xpos o.cond)
                         "Only bool values can be conditions.");
                   expr);
                block;
                other =
                  (match o.other with
                  | Some x -> Some (infer_expr env x |> second)
                  | None -> None);
                is_stmt = o.is_stmt;
                type';
                pos = o.pos;
              }
            |> validate_branch_type |> validate_else_branch
          in
          (type', expr)
      | Ast.ElseIfExpr o ->
          let block = infer_block false env o.block in
          let type' = infer_block_type block in
          let expr =
            Ast.ElseIfExpr
              {
                cond =
                  (let type', expr = infer_expr env o.cond in
                   assure loc (type' = Ast.BoolType) (fun _ ->
                       emsg file (GetAst.Expr.xpos o.cond)
                         "Only bool values can be conditions.");
                   expr);
                block;
                other =
                  (match o.other with
                  | Some x -> Some (infer_expr env x |> second)
                  | None -> None);
                type';
                pos = o.pos;
              }
          in
          (type', expr)
      | Ast.ElseExpr o ->
          let block = infer_block false env o.block in
          let type' = infer_block_type block in
          let expr = Ast.ElseExpr { block; type'; pos = o.pos } in
          (type', expr)
      | Ast.BlockExpr o ->
          let block = infer_block false env o.block in
          let type' = infer_block_type block in
          let expr = Ast.BlockExpr { block; type'; pos = o.pos } in
          (type', expr)
      | Ast.EntityExpr _ -> todo loc "Infer entity expression"
    in
    (type', expr)
  (* Infer lvalue expressions *)
  and infer_lvalue_expr env vars type' =
    let validate_lvalue_term type' term =
      match term with
      | Ast.IdVal o ->
          let id = GetAst.Id.value o.value in
          let var =
            match GetEnv.File.var id env with
            | Some var -> var
            | None ->
                error loc
                  (emsg file (GetAst.Id.xpos o.value)
                     ("Identifier " ^ quote id
                    ^ " does not exist in environment."))
          in
          let state = match var with Env.Var v -> v.state in
          let type'' = match var with Env.Var v -> v.type' in
          assure loc (state = Ast.MutState) (fun _ ->
              emsg file (GetAst.Id.xpos o.value)
                "Can not assign since variable is not mutable.");
          assure loc (type' = type'') (fun _ ->
              emsg file (GetAst.Id.xpos o.value)
                "Can not assign since types do not match.");
          true
      | _ -> false
    in
    let validate_lvalue_unop op =
      match op with Ast.DerefOp -> true | _ -> false
    in
    let validate_lvalue_binop op =
      match op with Ast.DotOp -> true | _ -> false
    in
    let validate_lvalue_expr type' expr =
      let sucess =
        match expr with
        | Ast.TerminalExpr o -> validate_lvalue_term type' o.value
        | Ast.UnOpExpr o -> validate_lvalue_unop o.op
        | Ast.BinOpExpr o -> validate_lvalue_binop o.op
        | _ -> false
      in
      assure loc sucess (fun _ ->
          emsg file (GetAst.Expr.xpos expr)
            "Only lvalue expressions can be assinged values.")
    in
    let types =
      match type' with Ast.TupleType t -> t.types | _ -> [ type' ]
    in
    assure loc
      (List.length vars = 1 || List.length vars = List.length types)
      (fun _ ->
        emsg file
          (GetAst.Expr.xpos (List.nth vars 0))
          "Expression can not be destructured and assigned to variables.");
    (match List.length vars with
    | 1 -> List.iter (validate_lvalue_expr type') vars
    | _ -> List.iter2 (fun t v -> validate_lvalue_expr t v) types vars);
    let exprs = List.map (fun v -> second (infer_expr env v)) vars in
    exprs
  (* Infer statement *)
  and infer_stmt isfn env stmt =
    match stmt with
    | Ast.LetStmt s ->
        let type', expr = infer_expr env s.expr in
        let vars = infer_vars s.vars type' in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        let stmt = SetAst.Stmt.with_vars stmt vars in
        let env = extend_env env (GetAst.Stmt.vars stmt) in
        (env, stmt)
    | Ast.SetStmt s ->
        let _, expr = infer_expr env s.expr in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        assure loc (isfn = false) (fun _ ->
            emsg file (GetAst.Expr.xpos expr)
              "Set statement can not be used to return values from functions.");
        (env, stmt)
    | Ast.AssignStmt s ->
        let type', expr = infer_expr env s.expr in
        let vars = infer_lvalue_expr env s.vars type' in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        let stmt = SetAst.Stmt.with_assign_vars stmt vars in
        (env, stmt)
    | Ast.ReturnStmt s ->
        let type', expr = infer_expr env s.expr in
        assure loc
          (type'
          =
          let fntype =
            GetEnv.File.function_type !func_name env
            |> some loc |> simplify_type
          in
          match fntype with
          | Ast.FunctionType t -> t.type'
          | _ -> never loc "Only expected function type.")
          (fun _ ->
            emsg file (GetAst.Expr.xpos s.expr)
              "Returned expression does not match function's return type.");
        let stmt = SetAst.Stmt.with_expr stmt expr in
        (env, stmt)
    | Ast.InvokeStmt s ->
        let _, expr = infer_expr env s.expr in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        (env, stmt)
    | Ast.IfStmt s ->
        let _, expr = infer_expr env s.expr in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        (env, stmt)
    | Ast.BlockStmt s ->
        let block = infer_block false env s.block in
        (* Ensure we don't return values from block statements using set statements *)
        assure loc
          (infer_block_type block = Ast.UnitType)
          (fun _ ->
            emsg file
              (match block with
              | Ast.Block b -> (
                  let sets =
                    List.filter
                      (fun s ->
                        match s with Ast.SetStmt _ -> true | _ -> false)
                      b.stmts
                  in
                  match List.length sets with
                  | 1 -> GetAst.Stmt.xpos (List.nth sets 0)
                  | _ -> (0, 0)))
              "Block statements can not return values with set statements.");
        let stmt = Ast.BlockStmt { block; pos = s.pos } in
        (env, stmt)
    | _ -> todo loc "Infer statement."
  (* Infer list of statements *)
  and infer_stmts isfn env stmts =
    let rec aux env stmts acc =
      match stmts with
      | [] -> List.rev acc
      | hd :: tl ->
          let env, hd = infer_stmt isfn env hd in
          aux env tl (hd :: acc)
    in
    aux env stmts []
  (* Infer block type using set statements *)
  and infer_block_type block =
    match block with
    | Ast.Block b -> (
        let sets =
          List.filter
            (fun s -> match s with Ast.SetStmt _ -> true | _ -> false)
            b.stmts
        in
        match List.length sets with
        | 0 -> Ast.UnitType
        | 1 ->
            let first = List.nth sets 0 in
            let type' = GetAst.Stmt.type' first in
            type'
        | _ ->
            never loc
              (emsg file
                 (GetAst.Stmt.xpos (List.nth sets 1))
                 "A block expression can not have more than one set statement.")
        )
  (* Infer block *)
  and infer_block isfn env block =
    let block =
      match block with
      | Ast.Block b ->
          let stmts = infer_stmts isfn env b.stmts in
          Ast.Block { stmts; pos = b.pos }
    in
    block
  (* Infer entities *)
  and infer_entity env enty =
    match enty with
    | Ast.Function f ->
        func_name := GetAst.Id.value f.name;
        let env = extend_env env f.args in
        let block = infer_block true env f.block in
        Ast.Function
          { name = f.name; args = f.args; type' = f.type'; block; pos = f.pos }
    | Ast.Struct _ -> enty
  in

  (* Infer all entities in file *)
  let infer_entities env =
    let ast =
      match ast with Ast.File f -> List.map (infer_entity env) f.entities
    in
    ast
  in

  (* Create typed AST by infering its entities *)
  let ast =
    match ast with
    | Ast.File f ->
        Ast.File { entities = envir ast |> infer_entities; file = f.file }
  in

  ast
;;

(** Compile code and return the emitted result *)
let compile file =
  let code = File.read_file_content file in
  let ast = parse code file |> analyze in
  Some ""
;;

(** Verify a file can be compiled *)
let pass file =
  let _emitted =
    try compile file with
    | Failure msg ->
        write msg;
        None
    | Error.Report report ->
        write report.message;
        None
    | exn ->
        write (Printexc.to_string exn);
        None
  in
  unit
;;

(** Driver *)
let main =
  pass (what ());
  unit
;;
