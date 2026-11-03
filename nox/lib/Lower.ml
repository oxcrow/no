open Core

let getStringOfName = Ast.getStringOfName

let dump cfg =
  Store.BlockMap.iter
    (fun k (v : Mir.blocks) ->
      write (fmt "For blockIndex: %d" k);
      List.iter (fun s -> write (tabChar ^ Mir.show_stmts s)) v.stmts)
    cfg;
  write ""
;;

let rec lowerFile file =
  let entities = List.map (fun enty -> lowerEntity enty) (Ast.getEntyOfFile file) in
  entities

and lowerEntity enty =
  let lowEnty =
    match enty with
    | Ast.Function f ->
        Mir.Function
          {
            scope = lowerScope f.scope;
            name = getStringOfName f.name;
            types = lowerType f.types;
            block = lowerBlockMap f.block 0;
          }
  in
  lowEnty

and lowerBlockMap block blockIndex =
  let cfg = Store.BlockMap.empty in
  let cfg, lowYieldReg, blockIndex, accStmt = lowerBlock cfg block blockIndex [] in
  cfg

and lowerBlock cfg block blockIndex accStmt =
  let cfg, lowYieldReg, blockIndex, accStmt =
    match block with Ast.Block b -> lowerStmts cfg b.stmts blockIndex []
  in
  (cfg, lowYieldReg, blockIndex, accStmt)

and lowerStmts cfg stmts blockIndex accStmt =
  let cfg, lowStmts, lowYieldReg =
    let rec aux cfg stmts blockIndex lowYieldReg accStmt =
      match stmts with
      | [] -> (cfg, accStmt, lowYieldReg)
      | headStmt :: tailStmt ->
          let cfg, lowYieldReg, blockIndex, accStmt = lowerStmt cfg headStmt blockIndex accStmt in
          aux cfg tailStmt blockIndex lowYieldReg accStmt
    in
    aux cfg stmts blockIndex None accStmt
  in
  (cfg, lowYieldReg, blockIndex, lowStmts)

and lowerStmt cfg stmt blockIndex accStmt =
  let cfg, lowYieldReg, blockIndex, accStmt =
    match stmt with
    | Ast.ReturnStmt s ->
        let cfg, lowExprReg, blockIndex, accStmt = lowerExpr cfg s.expr blockIndex accStmt in
        let lowRet = Mir.RetStmt { from = lowExprReg } in
        let accStmt = List.rev (lowRet :: accStmt) in
        let cfg, blockIndex = addNewBlock cfg blockIndex accStmt in
        (cfg, None, blockIndex, [])
    | Ast.YieldStmt s ->
        let cfg, lowExprReg, blockIndex, accStmt = lowerExpr cfg s.expr blockIndex accStmt in
        (cfg, lowExprReg, blockIndex, accStmt)
    | _ -> todo source "lower-stmt"
  in
  (cfg, lowYieldReg, blockIndex, accStmt)

and lowerExpr cfg expr blockIndex accStmt =
  let cfg, lowExprReg, blockIndex, accStmt =
    match expr with
    | Ast.BlockExpr e ->
        let cfg, lowYieldReg, blockIndex, accStmt = lowerBlock cfg e.block blockIndex accStmt in
        (cfg, lowYieldReg, blockIndex, accStmt)
    | Ast.AddExpr e ->
        let cfg, lowExprRegL, blockIndex, accStmt = lowerExpr cfg e.lexpr blockIndex accStmt in
        let cfg, lowExprRegR, blockIndex, accStmt = lowerExpr cfg e.rexpr blockIndex accStmt in
        let lowExprReg = newReg e.exprId 0 (lowerType e.types) in
        let lowExpr =
          Mir.AddStmt
            {
              dest = lowExprReg;
              fromL = lowExprRegL |> some source;
              fromR = lowExprRegR |> some source;
            }
        in
        (cfg, Some lowExprReg, blockIndex, lowExpr :: accStmt)
    | Ast.SubExpr e ->
        let cfg, lowExprRegL, blockIndex, accStmt = lowerExpr cfg e.lexpr blockIndex accStmt in
        let cfg, lowExprRegR, blockIndex, accStmt = lowerExpr cfg e.rexpr blockIndex accStmt in
        let lowExprReg = newReg e.exprId 0 (lowerType e.types) in
        let lowExpr =
          Mir.SubStmt
            {
              dest = lowExprReg;
              fromL = lowExprRegL |> some source;
              fromR = lowExprRegR |> some source;
            }
        in
        (cfg, Some lowExprReg, blockIndex, lowExpr :: accStmt)
    | Ast.MulExpr e ->
        let cfg, lowExprRegL, blockIndex, accStmt = lowerExpr cfg e.lexpr blockIndex accStmt in
        let cfg, lowExprRegR, blockIndex, accStmt = lowerExpr cfg e.rexpr blockIndex accStmt in
        let lowExprReg = newReg e.exprId 0 (lowerType e.types) in
        let lowExpr =
          Mir.MulStmt
            {
              dest = lowExprReg;
              fromL = lowExprRegL |> some source;
              fromR = lowExprRegR |> some source;
            }
        in
        (cfg, Some lowExprReg, blockIndex, lowExpr :: accStmt)
    | Ast.DivExpr e ->
        let cfg, lowExprRegL, blockIndex, accStmt = lowerExpr cfg e.lexpr blockIndex accStmt in
        let cfg, lowExprRegR, blockIndex, accStmt = lowerExpr cfg e.rexpr blockIndex accStmt in
        let lowExprReg = newReg e.exprId 0 (lowerType e.types) in
        let lowExpr =
          match e.types with
          | Ast.IntType ->
              Mir.UDivStmt
                {
                  dest = lowExprReg;
                  fromL = lowExprRegL |> some source;
                  fromR = lowExprRegR |> some source;
                }
          | _ ->
              Mir.DivStmt
                {
                  dest = lowExprReg;
                  fromL = lowExprRegL |> some source;
                  fromR = lowExprRegR |> some source;
                }
        in
        (cfg, Some lowExprReg, blockIndex, lowExpr :: accStmt)
    | Ast.IntExpr e ->
        let lowExprReg = newReg e.exprId 0 (lowerType e.types) in
        let lowExpr = Mir.CopyStmt { dest = lowExprReg; value = e.value } in
        (cfg, Some lowExprReg, blockIndex, lowExpr :: accStmt)
    | Ast.UnitExpr e -> (cfg, None, blockIndex, accStmt)
    | _ -> todo source "lower-expr"
  in
  (cfg, lowExprReg, blockIndex, accStmt)

and lowerType types =
  match types with
  | Ast.IntType -> Mir.IntType
  | Ast.UnitType -> Mir.UnitType
  | _ -> todo source "lower-type"

and lowerScope scope = match scope with Ast.ExportScope -> Mir.ExportScope | _ -> Mir.ModuleScope

and addNewBlock cfg blockIndex accStmt =
  let cfg =
    Store.BlockMap.update blockIndex
      (function
        | Some (y : Mir.blocks) ->
            never source
              (fmt "Unable to insert CFG block of index: %d, since it already exists." blockIndex)
        | None -> Some (newBlock blockIndex accStmt))
      cfg
  in
  (cfg, blockIndex + 1)

(* *)
and newBlock blockId stmts : Mir.blocks = { blockId; stmts }
and newReg baseId otherId types : Mir.regs = { baseId; otherId; types }

let lowerFiles files =
  let entities = List.map (fun file -> lowerFile file) files in
  Mir.File { entities = List.flatten entities }
;;
