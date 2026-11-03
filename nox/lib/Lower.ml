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
  dump cfg;
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
        let cfg, blockIndex, accStmt = addNewBlock cfg blockIndex accStmt in
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
    | Ast.IfExpr e ->
        let cfg, lowYieldReg, blockIndex, accStmt = lowerIfElse cfg expr blockIndex accStmt [] in
        (cfg, lowYieldReg, blockIndex, accStmt)
    | Ast.ElseIfExpr e ->
        (* Add the condition expression and the first block *)
        (* %condition = lowerExpr();
         * jnz %condition, @trueBlock, @falseBlock *)
        never source "wtf?";
        let cfg, lowCondReg, selfBlockIndex, accStmt = lowerExpr cfg e.cond blockIndex accStmt in
        let trueBlockIndex = selfBlockIndex + 1 in
        let falseBlockIndex = selfBlockIndex + 2 in
        let mergeBlockIndex = selfBlockIndex + 3 in
        let lowCond =
          Mir.JnzStmt
            {
              condReg = some source lowCondReg;
              trueBlockId = trueBlockIndex;
              falseBlockId = falseBlockIndex;
            }
        in
        let accStmt = List.rev (lowCond :: accStmt) in
        let cfg, ifBlockIndex, accStmt = addNewBlock cfg selfBlockIndex accStmt in
        (* Add the second block for true branch then jump to the merge block *)
        let (cfg, lowTrueBlockReg, trueBlockIndex, accStmt), oldBlockIndex =
          (lowerBlock cfg e.block ifBlockIndex accStmt, ifBlockIndex)
        in
        let lowJmp =
          Mir.JmpStmt
            {
              fromBlockId = oldBlockIndex;
              toBlockId =
                (match trueBlockIndex == oldBlockIndex with
                | true -> trueBlockIndex + 1
                | false -> trueBlockIndex);
            }
        in
        let accStmt = List.rev (lowJmp :: accStmt) in
        let cfg, blockIndex, accStmt = addNewBlock cfg trueBlockIndex accStmt in
        (* TODO: Add the third block for false branch then jump to the merge block *)
        (* Add stmts for the merge branch *)
        let lowPhi =
          Mir.PhiStmt
            {
              dest = newReg e.exprId 0 (lowerType e.types);
              from = [ newPhi trueBlockIndex (some source lowTrueBlockReg) ];
            }
        in
        let accStmt = [ lowPhi ] in
        (cfg, lowTrueBlockReg, blockIndex, accStmt)
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
    | Ast.BoolExpr e ->
        let lowExprReg = newReg e.exprId 0 (lowerType Ast.BoolType) in
        let lowExpr =
          Mir.CopyStmt
            { dest = lowExprReg; value = (match e.value with Ast.True -> "1" | Ast.False -> "0") }
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

and lowerIfElse cfg expr blockIndex accStmt accPhi =
  match expr with
  | Ast.IfExpr e ->
      let cfg, lowCondReg, ifBlockIndex, accStmt = lowerExpr cfg e.cond blockIndex accStmt in
      (* Block indices for the three blocks we'll create *)
      let trueBlockIndex = ifBlockIndex + 1 in
      let mergeBlockIndex = ifBlockIndex + 2 in
      let falseBlockIndex = ifBlockIndex + 3 in
      let blockIndex = match e.rest with Some _ -> ifBlockIndex + 4 | None -> ifBlockIndex + 3 in
      (* Jump if not zero instruction to traverse to the other blocks *)
      let lowJnzStmt =
        match e.rest with
        (* If else-if/else branch exists, jump to the false branch. *)
        | Some _ ->
            Mir.JnzStmt
              {
                condReg = some source lowCondReg;
                trueBlockId = trueBlockIndex;
                falseBlockId = falseBlockIndex;
              }
        (* If else-if/else branch doesn't exist, then jump to merge. *)
        | None ->
            Mir.JnzStmt
              {
                condReg = some source lowCondReg;
                trueBlockId = trueBlockIndex;
                falseBlockId = mergeBlockIndex;
              }
      in
      let cfg, blockIndex, accStmt =
        addNewBlock cfg ifBlockIndex (List.rev (lowJnzStmt :: accStmt))
      in
      (* Add the second block for the true branch *)
      let (cfg, lowTrueBlockReg, blockIndex, accStmt), oldBlockIndex =
        (lowerBlock cfg e.block blockIndex accStmt, blockIndex)
      in
      let lowJmpStmt =
        Mir.JmpStmt
          {
            (* Is this correct? Maybe *)
            fromBlockId = oldBlockIndex;
            toBlockId = mergeBlockIndex;
          }
      in
      let cfg, blockIndex, accStmt =
        addNewBlock cfg trueBlockIndex (List.rev (lowJmpStmt :: accStmt))
      in
      let accStmt = [] in
      (* Add the third block for the false/merge branch *)
      let cfg, lowYieldReg, blockIndex, accStmt =
        match e.rest with
        (* We need a false branch *)
        | Some block ->
            let cfg, lowYieldReg, blockIndex, accStmt =
              lowerIfElse cfg block blockIndex accStmt accPhi
            in
            (cfg, None, blockIndex, accStmt)
        (* We need a merge branch *)
        | None -> (cfg, None, blockIndex, accStmt)
      in
      (cfg, None, blockIndex, accStmt)
  | Ast.ElseExpr e ->
      let (cfg, lowYieldReg, blockIndex, accStmt), oldBlockIndex =
        (lowerBlock cfg e.block blockIndex accStmt, blockIndex)
      in
      (cfg, None, blockIndex, accStmt)
  | _ -> never source "wut?"

and lowerType types =
  match types with
  | Ast.BoolType -> Mir.WordType
  | Ast.IntType -> Mir.LongType
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
  (cfg, blockIndex + 1, [])

(* *)
and newBlock blockId stmts : Mir.blocks = { blockId; stmts }
and newPhi blockId from : Mir.phis = { blockId; from }
and newReg baseId otherId types : Mir.regs = { baseId; otherId; types }

let lowerFiles files =
  let entities = List.map (fun file -> lowerFile file) files in
  Mir.File { entities = List.flatten entities }
;;
