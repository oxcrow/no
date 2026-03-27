open Core

(* Aliases *)
let nextToken lexer pos = Lexer.nextToken lexer |> xSOME pos

let skipToken lexer expected pos =
  let peekToken, _ = nextToken lexer pos in
  let newLexer = Lexer.skipToken lexer expected in
  match newLexer with
  | Some lexer -> lexer
  | None ->
      raise
        (Report
           {
             message = "Unable to unwrap optional value. (Value is None)";
             source = xSOURCE pos;
             error = Some (Any (Error.UnwrapNone (lexer.file, peekToken)));
           })
;;

let tokenLocation = Ast.tokenLocation
let printf = Printf.printf
let write = print_endline

(* Debug utils *)
let dbgToken lexer id =
  write (Token.show_uToken (fst @@ nextToken lexer uPOS) ^ string_of_int id)
;;

(** Parse entire source code of a file using external menhir parser *)
let parseFileExt (file : string) =
  let code = File.readFileContent file in
  let buf = Lexing.from_string code in
  let ast =
    let epos (buf : Lexing.lexbuf) =
      let lineIdx = buf.lex_curr_p.pos_lnum in
      let colIdx = buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol in
      (lineIdx, colIdx)
    in
    try ExtParser.file ExtLexer.token buf with
    | ExtParser.Error ->
        raise
          (Report
             {
               message = "Unable to parse code. (Error could also be from lines above)";
               source = xSOURCE uPOS;
               error = Some (Any (Error.UnknownParserError (file, epos buf)));
             })
    | Failure msg -> xTODO uPOS "parser-error-with-message"
  in
  let ast = match ast with Ast.File f -> Ast.File { entities = f.entities; file } in
  ast
;;

(** Parse entire source code of a file *)
let rec parseFile (file : string) =
  let code = File.readFileContent file in
  let uses, lexer = parseUseList (Lexer.init file code) in
  let ents, lexer =
    match 0 = 0 with
    | true -> parseEntityList lexer (* XKEEP *)
    | false ->
        (* XDELETE *)
        ([], lexer)
  in
  let node = Ast.File { entities = uses @ ents; file } in
  ()

(** Parse module uses in scope *)
and parseUseList (lexer : Lexer.uLexer) : Ast.entities list * Lexer.uLexer =
  let rec aux lexer acc =
    let peekToken, _ = nextToken lexer uPOS in
    match peekToken.kind with
    | Token.USE ->
        let entity, lexer = parseUse lexer in
        aux lexer (entity :: acc)
    | _ -> (acc, lexer)
  in
  aux lexer []

(** Parse all entities in scope (functions, structs, unions, enums, etc.) *)
and parseEntityList (lexer : Lexer.uLexer) : Ast.entities list * Lexer.uLexer =
  let rec aux lexer acc =
    let peekToken, _ = nextToken lexer uPOS in
    match peekToken.kind with
    | Token.FUNCTION ->
        let entity, lexer = parseEntity lexer in
        aux lexer (entity :: acc)
    | Token.EOF -> (acc, lexer)
    | _ ->
        raise
          (Report
             {
               message = "Unknown token found while parsing entities.";
               source = xSOURCE uPOS;
               error = Some (Any (Error.UnknownToken (lexer.file, peekToken)));
             })
  in
  aux lexer []

(** Parse module use *)
and parseUse (lexer : Lexer.uLexer) : Ast.entities * Lexer.uLexer =
  let scope, lexer = parseScope lexer uPOS in
  let token, lexer = nextToken lexer uPOS in
  let loc = tokenLocation token in
  let node, lexer =
    match token.kind with
    | Token.USE ->
        let name, lexer = parseName lexer uPOS in
        let lexer = skipToken lexer Token.SEMICOLON uPOS in
        (Ast.Use { name; scope; import = true; loc }, lexer)
    | _ ->
        raise
          (Report
             {
               message = "Unknown token found while parsing entities.";
               source = xSOURCE uPOS;
               error = Some (Any (Error.UnknownToken (lexer.file, token)));
             })
  in
  (node, lexer)

(** Parse entity (functions, structs, unions, enums, etc.) *)
and parseEntity (lexer : Lexer.uLexer) : Ast.entities * Lexer.uLexer =
  let scope, lexer = parseScope lexer uPOS in
  let token, lexer = nextToken lexer uPOS in
  let loc = tokenLocation token in
  let node, lexer =
    match token.kind with
    | Token.FUNCTION ->
        let name, lexer = parseName lexer uPOS in
        let lexer = skipToken lexer Token.LPAREN uPOS in
        let lexer = skipToken lexer Token.RPAREN uPOS in
        let type', lexer = parseReturnType lexer uPOS in
        let lexer = skipToken lexer Token.LBRACE uPOS in
        let body, lexer = parseBlockBody lexer uPOS in
        let lexer = skipToken lexer Token.RBRACE uPOS in
        (Ast.NoneEnty, lexer)
    | _ ->
        raise
          (Report
             {
               message = "Unknown token found while parsing entities.";
               source = xSOURCE uPOS;
               error = Some (Any (Error.UnknownToken (lexer.file, token)));
             })
  in
  (node, lexer)

and parseBlockBody lexer pos =
  let rec parseStmtList lexer acc =
    let peekToken, _ = nextToken lexer uPOS in
    match peekToken.kind with
    | Token.RBRACE -> (acc, lexer)
    | _ ->
        let stmt, lexer = parseStmt lexer in
        parseStmtList lexer (stmt :: acc)
  in
  parseStmtList lexer []

and parseStmt lexer =
  let token, lexer = nextToken lexer uPOS in
  match token.kind with
  | Token.LET ->
      let vars, lexer = parseVarGroup lexer uPOS in
      let lexer = skipToken lexer Token.EQ uPOS in
      let expr, lexer = parseExpr lexer uPOS in
      xTODO uPOS "parse-let-stmt"
  | Token.RETURN -> xTODO uPOS "parse-return-stmt"
  | Token.IF -> xTODO uPOS "parse-if-stmt"
  | _ -> xTODO uPOS "parse-lval-expr?"

and parseExpr lexer pos =
  xTODO uPOS "parse-expr";
  ((), lexer)

and parseVarGroup lexer pos =
  let peekToken, _ = nextToken lexer pos in
  match peekToken.kind with
  | Token.LPAREN ->
      let lexer = skipToken lexer Token.LPAREN pos in
      let vars, lexer = parseVarList lexer pos in
      let lexer = skipToken lexer Token.RPAREN pos in
      (vars, lexer)
  | _ -> parseVarList lexer pos

and parseVarList lexer pos =
  let rec parseSepList lexer pos acc =
    let peekToken, _ = nextToken lexer pos in
    match peekToken.kind with
    | Token.MUT | Token.NAMEVAL _ ->
        let node, lexer = parseVar lexer pos in
        let lexer =
          let peekToken, newLexer = nextToken lexer uPOS in
          match peekToken.kind with
          | Token.COMMA -> newLexer
          | Token.EQ -> lexer
          | _ ->
              raise
                (Report
                   {
                     message = "Unable to parse variables.";
                     source = xSOURCE uPOS;
                     error = Some (Any (Error.InvalidGrammar (lexer.file, peekToken)));
                   })
        in
        parseSepList lexer pos (node :: acc)
    | Token.EQ -> (acc, lexer)
    | _ ->
        raise
          (Report
             {
               message = "Unable to parse variables.";
               source = xSOURCE uPOS;
               error = Some (Any (Error.InvalidGrammar (lexer.file, peekToken)));
             })
  in
  parseSepList lexer pos []

and parseVar lexer pos =
  let state, lexer = parseState lexer uPOS in
  let shadow = false in
  let name, lexer = parseName lexer uPOS in
  let type', lexer = parseTypeOption lexer uPOS in
  (Ast.Var { name; state; shadow; type' }, lexer)

and parseState lexer pos =
  let peekToken, _ = nextToken lexer pos in
  match peekToken.kind with
  | Token.MUT ->
      let lexer = skipToken lexer Token.MUT pos in
      (Ast.MutableState, lexer)
  | _ -> (Ast.ImmutableState, lexer)

and parseReturnType lexer pos =
  let peekToken, _ = nextToken lexer pos in
  let type', lexer =
    match peekToken.kind with
    | Token.LBRACE -> (Ast.UnitType, lexer)
    | _ -> parseType lexer pos
  in
  (type', lexer)

and parseType lexer pos =
  let type', lexer =
    match parseTypeOption lexer pos with
    | Some type', lexer -> (type', lexer)
    | _ -> xNEVER uPOS "parse-type"
  in
  (type', lexer)

and parseTypeOption lexer pos =
  let peekToken, newLexer = nextToken lexer pos in
  let type', lexer =
    match peekToken.kind with
    | Token.FLOAT -> (Some Ast.FloatType, newLexer)
    | Token.INT -> (Some Ast.IntType, newLexer)
    | _ -> (None, lexer)
  in
  (type', lexer)

and parseScope lexer pos =
  let scopeToken, newLexer = nextToken lexer pos in
  match scopeToken.kind with
  | Token.PUBLIC -> (Ast.PublicScope, newLexer)
  | _ -> (Ast.PrivateScope, lexer)

and parseName lexer pos =
  let nameToken, lexer = nextToken lexer pos in
  let name =
    match nameToken.kind with
    | Token.NAMEVAL name -> Ast.Name { name; loc = Ast.tokenLocation nameToken }
    | _ ->
        raise
          (Report
             {
               message = "Identifier name was expected; but not found.";
               source = xSOURCE pos;
               error = Some (Any (Error.InvalidIdentifier (lexer.file, nameToken)));
             })
  in
  (name, lexer)
;;
