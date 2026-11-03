open Core
open Hecc

let rec lowerFile file =
  let entities = List.map (fun enty -> lowerEntity enty) (Ast.getEntyOfFile file) in
  entities

and lowerEntity enty =
  let lowEnty =
    match enty with
    | Ast.Function f ->
        Mir.Function
          {
            name = getStringOfName f.name;
            types = f.types;
            block = lowerBlock f.block 0;
            ast = enty;
          }
  in
  lowEnty

and lowerBlock block blockIndexAcc =
  let lowStmts = match block with Ast.Block b -> lowerStmts b.stmts blockIndexAcc [] in
  Mir.Block { blockId = blockIndexAcc; stmts = lowStmts; ast = block }

and lowerStmts stmts blockIndex accStmt =
  let lowStmts =
    match stmts with
    | [] -> accStmt
    | headStmt :: tailStmt ->
        let x = lowerStmt headStmt blockIndex accStmt in
        lowerStmts tailStmt blockIndex accStmt
  in
  lowStmts

and lowerStmt stmt blockIndexAcc accStmt =
  unit;
  unit

and lowerScope scope = match scope with Ast.ExportScope -> Mir.ExportScope | _ -> Mir.ModuleScope
and getStringOfName = Ast.getStringOfName

let lowerFiles files =
  let entities = List.map (fun file -> lowerFile file) files in
  Mir.Module { entities = List.flatten entities }
;;
