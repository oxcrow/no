open Core
open Hecc

(* Store the lifetimes of each variable *)
module LifeMap = Map.Make (Int)

type loans =
  | AssignLoan of { loanVarId : int; stmtId : int; exprId : int; loc : Ast.loc }
  | YieldLoan of { loanVarId : int; stmtId : int; exprId : int; loc : Ast.loc }
  | UseLoan of { stmtId : int; exprId : int; loc : Ast.loc }
  | BranchLoan of { loans : loans list list; loc : Ast.loc }
[@@deriving show { with_path = false }]

type trees = { filePath : string; lifes : lifes LifeMap.t }
and lifes = { name : string; varId : int; startIndex : int; endIndex : int; loans : loans list }

let rec printLifeMap (tree : trees) =
  LifeMap.iter
    (fun varId life ->
      (* Print variable lifetime *)
      write
        (fmt "Variable %s, of id: %d, has %d loans, with life: [%d, %d]" (quote life.name) varId
           (List.length life.loans) life.startIndex life.endIndex);
      (* Print variable loans *)
      List.iter (fun loan -> printLoan life.name varId loan) life.loans;
      write "")
    tree.lifes

and printLoan name varId loan =
  write
    (fmt "Variable %s, of id: %d, has: (%s Loan)" (quote name) varId
       (match loan with
       (* Should we print more info? *)
       | AssignLoan _ -> "Assign"
       | YieldLoan _ -> "Yield"
       | UseLoan _ -> "Use"
       | BranchLoan _ -> "Branch"));
  match loan with
  | BranchLoan l ->
      write "///";
      List.iter
        (fun loanList ->
          match List.length loanList with
          | 0 -> write (fmt "Variable %s, of id: %d, has: (Empty Branch)" (quote name) varId)
          | _ -> List.iter (fun loan -> printLoan name varId loan) loanList)
        l.loans
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
let addBranchLoan tree (branches : trees list) =
  (* Add all new variables from each branch, to the tree *)
  let rec addBranchVars tree branches varIdAcc =
    match branches with
    | [] -> (tree, List.sort_uniq compare (List.flatten varIdAcc))
    | headBranch :: tailBranch ->
        (* Insert each branch to the tree *)
        let rec addVars tree varIds lifes =
          match (varIds, lifes) with
          | [], [] -> tree
          | headVarId :: tailVarId, headLife :: tailLife ->
              (* Insert each variable to the tree *)
              let tree =
                match LifeMap.find_opt headVarId tree.lifes with
                | Some oldLife ->
                    (* If this is an old variable, then collect its loans for use *)
                    tree
                | None ->
                    (* If this is a new variable, then add it directly *)
                    let tree = { tree with lifes = LifeMap.add headVarId headLife tree.lifes } in
                    tree
              in
              addVars tree tailVarId tailLife
          | _ -> never source "wut?"
        in

        let varIds, lifes = LifeMap.bindings headBranch.lifes |> List.split in
        let tree = addVars tree varIds lifes in
        addBranchVars tree tailBranch (varIds :: varIdAcc)
  in

  (* Add all loans for each variable assigned in each branch *)
  let addLoans (tree : trees) (branches : trees list) (varIds : int list) =
    let rec aux tree (loans : loans list list list) (varIds : int list) =
      match (varIds, loans) with
      | [], [] -> tree
      | headVarId :: tailVarId, headLoan :: tailLoan ->
          (* Insert the loans to each variable *)
          let loan = BranchLoan { loans = headLoan; loc = Ast.Nowhere } in
          let tree =
            {
              tree with
              lifes =
                LifeMap.update headVarId
                  (fun x ->
                    match x with
                    | Some oldLife -> Some { oldLife with loans = loan :: oldLife.loans }
                    | None -> never source "wut?")
                  tree.lifes;
            }
          in
          aux tree tailLoan tailVarId
      | _ -> never source "wut?"
    in

    (* Find loans for each variable from each branch *)
    let rec getLoans (branches : trees list) (varIds : int list) accLoans =
      match varIds with
      | [] -> accLoans
      | headVarId :: tailVarId ->
          let loans =
            List.map
              (fun branch ->
                let lifes = LifeMap.find_opt headVarId branch.lifes in
                match lifes with Some life -> life.loans | None -> [])
              branches
          in
          getLoans branches tailVarId (loans :: accLoans)
    in

    let tree = aux tree (getLoans branches varIds []) varIds in
    tree
  in

  let tree, varIds =
    addBranchVars tree branches
      (let varIds, _ = LifeMap.bindings tree.lifes |> List.split in
       [ varIds ])
  in
  let tree = addLoans tree branches varIds in
  tree
;;

let lastStmtIdOfBlock block lastStmtId =
  let lastStmtId =
    match block with [] -> lastStmtId | _ -> Ast.getIdOfStmt (lastOfList block |> some source)
  in
  lastStmtId
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
        (* WARNING: THIS WILL NOT WORK FOR ALIASES! WE NEED TO MATCH ON EXPRTYPE! *)
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

and growIfElse tree expr stmtId lastStmtId =
  let branches =
    match expr with
    | Ast.IfExpr o ->
        let lastStmtId = lastStmtIdOfBlock o.block lastStmtId in
        let mainBranch = growStmts (newBranch tree) o.block lastStmtId in
        let restBranches =
          match o.rest with
          | Some (Ast.ElseIfExpr e) ->
              growIfElse (newBranch tree) (o.rest |> some source) stmtId lastStmtId
          | Some (Ast.ElseExpr e) ->
              growIfElse (newBranch tree) (o.rest |> some source) stmtId lastStmtId
          | _ -> []
        in
        [ mainBranch ] @ restBranches
    | Ast.ElseIfExpr o ->
        let lastStmtId = lastStmtIdOfBlock o.block lastStmtId in
        let mainBranch = growStmts (newBranch tree) o.block lastStmtId in
        let restBranches =
          match o.rest with
          | Some (Ast.ElseIfExpr e) ->
              growIfElse (newBranch tree) (o.rest |> some source) stmtId lastStmtId
          | Some (Ast.ElseExpr e) ->
              growIfElse (newBranch tree) (o.rest |> some source) stmtId lastStmtId
          | _ -> []
        in
        [ mainBranch ] @ restBranches
    | Ast.ElseExpr o ->
        let lastStmtId = lastStmtIdOfBlock o.block lastStmtId in
        let mainBranch = growStmts (newBranch tree) o.block lastStmtId in
        [ mainBranch ]
    | _ -> never source "wut?"
  in
  branches

and growExpr tree expr stmtId lastStmtId =
  let tree =
    match expr with
    | Ast.IfExpr o ->
        let branches = growIfElse tree expr stmtId lastStmtId in
        let tree = addBranchLoan tree branches in
        tree
    | Ast.ElseIfExpr o -> never source "wut?"
    | Ast.ElseExpr o -> never source "wut?"
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
