open Core

(* *)
module Ast = struct
  let name x = match x with Ast.Name n -> n.name
  let locOfName x = match x with Ast.Name n -> n.loc

  let locofEntity x =
    match x with
    | Ast.Function y -> y.loc
    | Ast.Struct y -> y.loc
    | Ast.Use y -> y.loc
    | _ -> xTODO uPOS "loc-of-entity"
  ;;
end
