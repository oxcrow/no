open Core
open Hecc

(* Store the lifetimes of each variable *)
module LifeMap = Map.Make (Int)

type loans =
  | AssignLoan of { loanVarId : int; stmtId : int; exprId : int; loc : Ast.loc }
  | UseLoan of { stmtId : int; exprId : int; loc : Ast.loc }
  | BranchLoan of { loans : loans list; loc : Ast.loc }
[@@deriving show { with_path = false }]

type trees = { filePath : string; lifes : lifes LifeMap.t }
and lifes = { name : string; varId : int; startIndex : int; endIndex : int; loans : loans list }

let rec printLifeMap (tree : trees) =
  LifeMap.iter
    (fun varId life ->
      write
        (fmt "Variable %s, of id: %d, has %d loans, with life: [%d, %d]" (quote life.name) varId
           (List.length life.loans) life.startIndex life.endIndex);
      List.iter (fun loan -> printLoan life.name varId loan) life.loans;
      write "")
    tree.lifes

and printLoan name varId loan =
  write
    (fmt "Variable %s, of id: %d, has loan: (%s)" (quote name) varId
       (match loan with
       (* Should we print more info? *)
       | AssignLoan _ -> "Assign"
       | UseLoan _ -> "Use"
       | BranchLoan _ -> "Branch"));
  match loan with
  | BranchLoan l ->
      write "///";
      List.iter (fun loan -> printLoan name varId loan) l.loans
  | _ -> ()
;;

let printLoans (tree : trees) =
  LifeMap.iter
    (fun varId life -> List.iter (fun loan -> printLoan life.name varId loan) life.loans)
    tree.lifes
;;

let newLife name varId startIndex endIndex : lifes =
  { name; varId; startIndex; endIndex; loans = [] }
;;

(* Create new tree branch that will be filled as we process the branch *)
let newBranch tree : trees = { filePath = tree.filePath; lifes = LifeMap.empty }

let addAssignLoan tree name varId loanVarId stmtId exprId loc =
  let loan = AssignLoan { loanVarId; stmtId; exprId; loc } in
  let tree =
    {
      tree with
      lifes =
        LifeMap.update varId
          (fun x ->
            match x with
            | Some oldLife -> Some { oldLife with loans = loan :: oldLife.loans }
            | None ->
                (* If the variable doesn't exist in tree, add a dummy *)
                let dummyLife = newLife name varId (-1) (-1) in
                Some { dummyLife with loans = loan :: dummyLife.loans })
          tree.lifes;
    }
  in
  tree
;;

let addUseLoan tree name varId stmtId exprId loc =
  let loan = UseLoan { stmtId; exprId; loc } in
  let tree =
    {
      tree with
      lifes =
        LifeMap.update varId
          (fun x ->
            match x with
            | Some oldLife -> Some { oldLife with loans = loan :: oldLife.loans }
            | None ->
                (* If the variable doesn't exist in tree, add a dummy *)
                let dummyLife = newLife name varId (-1) (-1) in
                Some { dummyLife with loans = loan :: dummyLife.loans })
          tree.lifes;
    }
  in
  tree
;;

(** Add elements from branches to the tree. *)
let addBranchLoan tree branch loanId loc =
  let tree =
    let rec aux (tree : trees) (varIds : int list) (lifes : lifes list) =
      let tree =
        match (varIds, lifes) with
        | [], [] -> tree
        | headVarId :: tailVarId, headLife :: tailLife ->
            (* Find the elements in the tree, and add them if they don't exist*)
            let tree =
              match LifeMap.find_opt headVarId tree.lifes with
              | Some oldVar ->
                  (* If old variable exists in scope, we need to add branch. *)
                  let loan = BranchLoan { loans = headLife.loans; loc } in
                  let tree =
                    {
                      tree with
                      lifes =
                        LifeMap.update headVarId
                          (fun x ->
                            match x with
                            | Some oldLife -> Some { oldLife with loans = loan :: oldLife.loans }
                            | None ->
                                (* If the variable doesn't exist in tree, then we messed up *)
                                never source "wut?")
                          tree.lifes;
                    }
                  in
                  tree
              | None ->
                  (* If this is a new variable, then add it directly *)
                  let loans =
                    match headLife.loans with
                    | [] -> []
                    | _ -> [ BranchLoan { loans = headLife.loans; loc } ]
                  in
                  let tree = { tree with lifes = LifeMap.add headVarId headLife tree.lifes } in
                  let tree =
                    {
                      tree with
                      lifes =
                        LifeMap.update headVarId
                          (fun x ->
                            match x with
                            | Some newLife -> Some { newLife with loans }
                            | None -> never source "wut?")
                          tree.lifes;
                    }
                  in
                  tree
            in
            aux tree tailVarId tailLife
        | _ -> never source "wut?"
      in
      tree
    in
    (* Extract keys and values from the map as list, so we can recurse on them *)
    let varIds, lifes = LifeMap.bindings branch.lifes |> List.split in
    let tree = aux tree varIds lifes in
    tree
  in
  tree
;;

let rec growEntities tree entys =
  match entys with
  | [] -> tree
  | headEnty :: tailEnty ->
      (* We discard the tree returned! This cleans our local scope! *)
      let oldTree = growEntity tree headEnty in

      printLifeMap oldTree;

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
    | Ast.AssignStmt o ->
        let tree = growLvals tree o.vars [ o.expr ] o.stmtId lastStmtId in
        tree
    | Ast.IfStmt o ->
        let tree = growExpr tree o.expr o.stmtId lastStmtId in
        tree
    | _ -> todo source "grow-stmt"
  in
  tree

and growLvals tree lvals exprs stmtId lastStmtId =
  let tree =
    match (lvals, exprs) with
    | [], [] -> tree
    | headLval :: tailLval, headExpr :: tailExpr ->
        let tree, exprs =
          match headExpr with
          (* If we find a complex expression, expand it *)
          | Ast.TupleExpr e -> todo source "assign-tuple"
          | Ast.ArrayExpr e -> todo source "assign-array"
          (* If we find a reference, create an assign loan *)
          | Ast.ConRefExpr e ->
              let name, varId =
                match headLval with
                | Ast.NameExpr n -> (Ast.getStringOfName n.value, Ast.getVarIdOfName n.value)
                | _ -> todo source "wut?"
              in
              let loanVarId =
                match e.expr with
                | Ast.NameExpr n -> Ast.getVarIdOfName n.value
                | _ -> todo source "wut?"
              in
              let exprId = Ast.getIdOfExpr headExpr in
              let tree = addAssignLoan tree name varId loanVarId stmtId exprId e.loc in
              (tree, [])
          (* If we find anything else, create an use loan *)
          | _ -> (tree, [])
        in
        growLvals tree tailLval exprs stmtId lastStmtId
    | _ -> never source "wut?"
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
        let branch = growStmts (newBranch tree) o.block lastStmtId in
        let restBranch, loanId =
          match o.rest with
          | Some (Ast.ElseIfExpr _) ->
              (Some (growExpr (newBranch tree) (o.rest |> some source) stmtId lastStmtId), 102)
          | Some (Ast.ElseExpr _) ->
              (Some (growExpr (newBranch tree) (o.rest |> some source) stmtId lastStmtId), 103)
          | _ -> (None, 0)
        in
        let tree =
          match restBranch with
          | Some rest -> addBranchLoan tree (addBranchLoan branch rest loanId o.loc) 101 o.loc
          | None -> addBranchLoan tree branch 104 o.loc
        in
        tree
    | Ast.ElseIfExpr o ->
        let lastStmtId = lastStmtIdOfBlock o.block in
        let branch = growStmts (newBranch tree) o.block lastStmtId in
        let restBranch, loanId =
          match o.rest with
          | Some (Ast.ElseIfExpr _) ->
              (Some (growExpr (newBranch tree) (o.rest |> some source) stmtId lastStmtId), 202)
          | Some (Ast.ElseExpr _) ->
              (Some (growExpr (newBranch tree) (o.rest |> some source) stmtId lastStmtId), 203)
          | _ -> (None, 0)
        in
        let tree =
          match restBranch with
          | Some rest -> addBranchLoan tree (addBranchLoan branch rest loanId o.loc) 201 o.loc
          | None -> addBranchLoan tree branch 204 o.loc
        in
        tree
    | Ast.ElseExpr o ->
        let lastStmtId = lastStmtIdOfBlock o.block in
        let branch = growStmts (newBranch tree) o.block lastStmtId in
        branch
    | _ -> todo source "grow-expr"
  in
  tree

and growVars tree vars stmtId lastStmtId =
  let tree =
    match vars with
    | [] -> tree
    | headVar :: tailVar ->
        (* Insert variables into memory one by one *)
        let name = Ast.getStringOfName (Ast.getNameOfVar headVar) in
        let varId = (Ast.getUuidOfVar headVar).varId in
        let tree =
          { tree with lifes = LifeMap.add varId (newLife name varId stmtId lastStmtId) tree.lifes }
        in
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
