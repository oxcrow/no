open Core

(* *)
module Ast = struct
  let name x = match x with Ast.Name n -> n.name
  let locOfName x = match x with Ast.Name n -> n.loc
end
