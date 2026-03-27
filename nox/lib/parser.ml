open Core

(* Aliases *)
let nextToken lexer pos = Lexer.nextToken lexer |> xSOME pos
let skipToken lexer expected pos = Lexer.skipToken lexer expected |> xSOME pos
let tokenLocation = Ast.tokenLocation
let printf = Printf.printf

(** Parse entire source code of a file *)
let rec parseFile (file : string) =
  let code = File.readFileContent file in
  let entities, lexer = parseEntities (Lexer.init code) in
  let node = Ast.File { entities; file } in
  ()

(** Parse all entities in scope (functions, structs, unions, enums, etc.) *)
and parseEntities (lexer : Lexer.uLexer) =
  let rec aux lexer acc =
    let token, _ = nextToken lexer uPOS in
    match token.kind with
    | Token.EOF -> (acc, lexer)
    | _ ->
        let entity, lexer = parseEntity lexer in
        aux lexer (entity :: acc)
  in
  aux lexer []

(** Parse entity (functions, structs, unions, enums, etc.) *)
and parseEntity (lexer : Lexer.uLexer) =
  let scope, lexer = parseScope lexer uPOS in
  let token, lexer = nextToken lexer uPOS in
  let loc = tokenLocation token in
  let node, lexer =
    match token.kind with
    | Token.FUNCTION -> (Ast.NoneEnty, lexer)
    | Token.USE ->
        let name, lexer = parseName lexer uPOS in
        let lexer = skipToken lexer Token.SEMICOLON uPOS in
        (Ast.Use { name; scope; loc }, lexer)
    | _ ->
        raise
          (Report
             {
               message = "Unknown token found while parsing entities.";
               source = xSOURCE uPOS;
               error = None;
             })
  in
  (node, lexer)

and parseScope lexer pos =
  let scopeToken, newLexer = nextToken lexer pos in
  match scopeToken.kind with
  | Token.PUBLIC -> (Ast.PublicScope, newLexer)
  | _ -> (Ast.PrivateScope, lexer)

and parseName lexer pos =
  let nameToken, newLexer = nextToken lexer pos in
  ("FIXME!", newLexer)
;;
