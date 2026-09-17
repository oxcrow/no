open Core
open Hecc

(* Store the lifetimes of each variable *)
module LifeMap = Map.Make (Int)

type trees = { filePath : string; lifes : lifes LifeMap.t }
and lifes = { startIndex : int; endIndex : int }

let newLife startIndex endIndex : lifes = { startIndex; endIndex }

let printLifeMap (tree : trees) =
  LifeMap.iter
    (fun k v -> write (fmt "Variable of id: %d, has life: [%d,%d]" k v.startIndex v.endIndex))
    tree.lifes
;;

let rec growEntities tree entys =
  match entys with
  | [] -> tree
  | headEnty :: tailEnty ->
      (* We discard the tree returned! This cleans our local scope! *)
      let tree = growEntity tree headEnty in
      growEntities tree tailEnty

and growEntity tree enty =
  let tree =
    match enty with
    | Ast.Function o ->
        let lastStmtId = Ast.getIdOfStmt (lastOfList o.block |> some source) in
        let tree = growStmts tree o.block lastStmtId in
        tree
    | _ -> todo source "grow-entity"
  in
  tree

and growStmts tree stmts lastStmtId =
  match stmts with
  | [] -> tree
  | headStmt :: tailStmt ->
      let tree = growStmt tree headStmt lastStmtId in
      growStmts tree tailStmt lastStmtId

and growStmt tree stmt lastStmtId =
  let tree =
    match stmt with
    | Ast.LetStmt o ->
        let tree = growVars tree o.vars o.stmtId lastStmtId in
        tree
    | Ast.ReturnStmt o -> tree
    | Ast.YieldStmt o -> tree
    | Ast.AssignStmt o -> tree
    | Ast.IfStmt o ->
        let tree = growExpr tree o.expr o.stmtId lastStmtId in
        tree
    | _ -> todo source "grow-stmt"
  in
  tree

and growExpr tree expr stmtId lastStmtId =
  let lastStmtIdOfBlock block =
    let lastStmtId =
      match block with [] -> lastStmtId | _ -> Ast.getIdOfStmt (lastOfList block |> some source)
    in
    lastStmtId
  in
  let tree =
    match expr with
    | Ast.IfExpr o ->
        let lastStmtId = lastStmtIdOfBlock o.block in
        let tree = growStmts tree o.block lastStmtId in
        let tree =
          match o.rest with
          | Some (Ast.ElseIfExpr _) -> growExpr tree (o.rest |> some source) stmtId lastStmtId
          | Some (Ast.ElseExpr _) -> growExpr tree (o.rest |> some source) stmtId lastStmtId
          | _ -> never source "wut?"
        in
        tree
    | Ast.ElseIfExpr o ->
        let lastStmtId = lastStmtIdOfBlock o.block in
        let tree = growStmts tree o.block lastStmtId in
        let tree =
          match o.rest with
          | Some (Ast.ElseIfExpr _) -> growExpr tree (o.rest |> some source) stmtId lastStmtId
          | Some (Ast.ElseExpr _) -> growExpr tree (o.rest |> some source) stmtId lastStmtId
          | _ -> never source "wut?"
        in
        tree
    | Ast.ElseExpr o ->
        let lastStmtId = lastStmtIdOfBlock o.block in
        let tree = growStmts tree o.block lastStmtId in
        tree
    | _ -> todo source "grow-expr"
  in
  tree

and growVars tree vars stmtId lastStmtId =
  let tree =
    match vars with
    | [] -> tree
    | headVar :: tailVar ->
        (* Insert variables into memory one by one *)
        let varId = (Ast.getUuidOfVar headVar).varId in
        let tree = { tree with lifes = LifeMap.add varId (newLife stmtId lastStmtId) tree.lifes } in

        let name = Ast.getStringOfName (Ast.getNameOfVar headVar) in
        write (fmt "Variable %s, of id: %d, has life: [%d, %d]" name varId stmtId lastStmtId);

        growVars tree tailVar stmtId lastStmtId
  in
  tree
;;

let rec borrowEntities tree entys =
  match entys with
  | [] -> tree
  | headEnty :: tailEnty ->
      (* We discard the tree returned! This cleans our local scope! *)
      let _ = borrowEntity tree headEnty in
      borrowEntities tree tailEnty

and borrowEntity tree enty =
  let tree =
    match enty with
    | Ast.Function o ->
        let tree = borrowStmts tree o.block in
        tree
    | _ -> todo source "borrow-entity"
  in
  tree

and borrowStmts tree stmts =
  match stmts with
  | [] -> tree
  | headStmt :: tailStmt ->
      let tree = borrowStmt tree headStmt in
      borrowStmts tree tailStmt

and borrowStmt tree stmt =
  let tree =
    match stmt with
    | Ast.LetStmt o -> tree
    | Ast.ReturnStmt o -> tree
    | Ast.YieldStmt o -> tree
    | Ast.AssignStmt o -> tree
    | Ast.IfStmt o -> tree
    | _ -> todo source "borrow-stmt"
  in
  tree
;;

let borrowFile env file =
  let filePath = Ast.getStringNameOfFile file in
  let tree : trees = { filePath; lifes = LifeMap.empty } in

  (* Grow the alias tree to ensure that we know each variable's lifetime *)
  let tree = growEntities tree (Ast.getEntitiesOfFile file) in

  (* Analyse the alias tree to ensure memory safety *)
  let tree = borrowEntities tree (Ast.getEntitiesOfFile file) in

  file
;;
