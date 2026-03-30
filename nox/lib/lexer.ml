open Core

(* Aliases *)
let printf = Printf.printf

(* Is a character digit, alphabet, or whitespace? *)
let isAlpha (c : char) : bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let isWhite (c : char) : bool = c = ' ' || c = '\t' || c = '\r' || c = '\n'
let isDigit (c : char) : bool = c >= '0' && c <= '9'
let isAlNum (c : char) : bool = isAlpha c || isDigit c
let isDel (c : char) : bool = not (isAlNum c)

(* Context to store data while we tokenize code *)
type uLexer = { code : string; codeLen : int; charIdx : int; lineIdx : int; colIdx : int }

(* Tokenize the next token *)
let rec nextToken (lexer : uLexer) : (Token.uToken * uLexer) option =
  (* Identify the current character, then the token it's part of *)
  let c = lexer.code.[lexer.charIdx] in
  (* Identify the primary reserved tokens (or if they don't exist) *)
  let primaryToken, lexer =
    (* Does substring match given target? *)
    let substringMatches (text : string) (target : string) (start : int) : bool =
      let targetLen = String.length target in
      let textLen = String.length text in
      if targetLen > textLen then
        raise
          (Report
             {
               message =
                 "Unable to compare substrings (Target is larger than given string)";
               source = xSOURCE uPOS;
               error = None;
             });
      let segment = String.sub text start targetLen in
      String.equal segment target
    in
    (* Does substring match given keyword? *)
    (* And does the substring end with a valid delimiter? *)
    let wordMatches (text : string) (word : string) (start : int) : bool =
      let lastChar text word =
        let wordLen = String.length word in
        text.[start + wordLen]
      in
      substringMatches text word start && isDel (lastChar text word)
    in
    (* Extract reserved keyword token if it exists *)
    let lexWord (lexer : uLexer) (kind : Token.uTokenKind) (word : string) :
        Token.uToken option * uLexer =
      let wordLen = String.length word in
      match wordMatches lexer.code word lexer.charIdx with
      | true ->
          ( Some
              {
                kind;
                charIdx = lexer.charIdx;
                lineIdx = lexer.lineIdx;
                colIdx = lexer.colIdx;
              },
            {
              lexer with
              charIdx = lexer.charIdx + wordLen;
              colIdx = lexer.charIdx + wordLen;
            } )
      | false -> (None, lexer)
    in
    let lexMany (lexer : uLexer) (kinds : Token.uTokenKind list) (words : string list) fx
        : Token.uToken option * uLexer =
      List.map2 (fun kind word -> fx lexer kind word) kinds words
      |> List.filter (fun tokenLexer -> isSome (fst tokenLexer))
      |> fun tokens -> List.nth tokens 0
    in
    (* Extract whitespace tokens *)
    let lexWhite (lexer : uLexer) : Token.uToken option * uLexer =
      ( Some
          {
            kind = Token.WHITESPACE;
            charIdx = lexer.charIdx;
            lineIdx = lexer.lineIdx;
            colIdx = lexer.colIdx;
          },
        { lexer with charIdx = lexer.charIdx + 1; colIdx = lexer.colIdx + 1 } )
    in
    let lexLine (lexer : uLexer) : Token.uToken option * uLexer =
      ( Some
          {
            kind = Token.NEWLINE;
            charIdx = lexer.charIdx;
            lineIdx = lexer.lineIdx;
            colIdx = lexer.colIdx;
          },
        {
          lexer with
          charIdx = lexer.charIdx + 1;
          lineIdx = lexer.lineIdx + 1;
          colIdx = 1;
        } )
    in
    (* Extract reserved atomic token if it exists *)
    let lexAtom (lexer : uLexer) (kind : Token.uTokenKind) (atom : string) :
        Token.uToken option * uLexer =
      let atomLen = String.length atom in
      match substringMatches lexer.code atom lexer.charIdx with
      | true ->
          ( Some
              {
                kind;
                charIdx = lexer.charIdx;
                lineIdx = lexer.lineIdx;
                colIdx = lexer.colIdx;
              },
            {
              lexer with
              charIdx = lexer.charIdx + atomLen;
              lineIdx = lexer.lineIdx;
              colIdx = lexer.colIdx + atomLen;
            } )
      | false -> (None, lexer)
    in
    (* Words tokenized one at a time *)
    let token, lexer =
      match c with
      | ' ' | '\t' -> lexWhite lexer
      | '\n' -> lexLine lexer
      | 'e' -> lexWord lexer Token.ELSE "else"
      | 'f' -> lexWord lexer Token.FUNCTION "fn"
      | 'i' -> lexMany lexer [ Token.INT; Token.IF ] [ "int"; "if" ] lexWord
      | 'l' -> lexWord lexer Token.LET "let"
      | 'm' ->
          lexMany lexer
            [ Token.MATCH; Token.MUT; Token.MOD ]
            [ "match"; "mut"; "mod" ] lexWord
      | 'p' -> lexWord lexer Token.PUBLIC "pub"
      | 'r' -> lexWord lexer Token.RETURN "return"
      | 'u' -> lexWord lexer Token.USE "use"
      (* Mathematical *)
      | '+' -> lexAtom lexer Token.PLUS "+"
      | '-' -> lexAtom lexer Token.MINUS "-"
      | '*' -> lexAtom lexer Token.STAR "*"
      | '/' ->
          let token, lexer =
            match
              lexMany lexer
                [ Token.DOCUMENT; Token.COMMENT; Token.SLASH ]
                [ "///"; "//"; "/" ] lexAtom
            with
            | Some token, lexer ->
                let token, lexer =
                  match token.kind with
                  | Token.DOCUMENT | Token.COMMENT ->
                      let lexLineLen lexer =
                        let rec nextNewLine text idx =
                          let c = text.[idx] in
                          match c = '\n' with
                          | true -> idx
                          | false -> nextNewLine text (idx + 1)
                        in
                        nextNewLine lexer.code lexer.charIdx - lexer.charIdx
                      in
                      let lineLen = lexLineLen lexer in
                      printf "YYY: Lexing /// //  (LineLength: %d)!\n" lineLen;
                      ( Some token,
                        {
                          lexer with
                          charIdx = lexer.charIdx + (lineLen - 1);
                          colIdx = lexer.colIdx + (lineLen - 1);
                        } )
                  | Token.SLASH -> (Some token, lexer)
                  | _ -> failwith "wut?"
                in
                (token, lexer)
            | None, lexer -> (None, lexer)
          in
          (token, lexer)
      (* Conditionals *)
      | '=' -> lexMany lexer [ Token.EQEQ; Token.EQ ] [ "=="; "=" ] lexAtom
      | '>' -> lexMany lexer [ Token.GE; Token.GT ] [ ">="; ">" ] lexAtom
      | '<' -> lexMany lexer [ Token.LE; Token.LT ] [ "<="; "<" ] lexAtom
      (* Symbols *)
      | ';' -> lexAtom lexer Token.SEMICOLON ";"
      | '?' -> lexAtom lexer Token.QUESTION "?"
      | '&' -> lexAtom lexer Token.AMPERSAND "&"
      | ':' -> lexAtom lexer Token.COLON ":"
      | ',' -> lexAtom lexer Token.COMMA ","
      | '.' -> lexAtom lexer Token.DOT "."
      | '|' -> lexAtom lexer Token.BAR "|"
      | '(' -> lexAtom lexer Token.LPAREN "("
      | ')' -> lexAtom lexer Token.RPAREN ")"
      | '{' -> lexAtom lexer Token.LBRACE "{"
      | '}' -> lexAtom lexer Token.RBRACE "}"
      | '[' -> lexAtom lexer Token.LBRACK "["
      | ']' -> lexAtom lexer Token.RBRACK "]"
      | _ ->
          printf "Unknown character: %c\n" c;
          failwith "wut?"
    in
    (token, lexer)
  in
  (* Extract identifier and digit tokens *)
  let lastToken, lastLexer =
    (* Extract length of identifier token *)
    let lexNameLen lexer =
      let rec aux text idx =
        let c = text.[idx] in
        match isDel c with true -> idx | false -> aux text (idx + 1)
      in
      aux lexer.code lexer.charIdx - lexer.charIdx
    in
    (* Extract identifier tokens *)
    let lexName lexer : Token.uToken * uLexer =
      let name = String.sub lexer.code lexer.charIdx (lexNameLen lexer) in
      let nameLen = String.length name in
      ( {
          kind = Token.NAMEVAL name;
          charIdx = lexer.charIdx;
          lineIdx = lexer.lineIdx;
          colIdx = lexer.colIdx;
        },
        {
          lexer with
          charIdx = lexer.charIdx + nameLen;
          lineIdx = lexer.lineIdx;
          colIdx = lexer.colIdx + nameLen;
        } )
    in
    (* Extract digit tokens *)
    let lexDigit lexer =
      let digit = "I DON'T KNOW!" in
      (digit, lexer)
    in
    match primaryToken with
    | Some token -> (Some token, lexer)
    | None ->
        if isAlpha c then
          let token, lexer = lexName lexer in
          (Some token, lexer)
        else if isDigit c then (
          let token, lexer = lexDigit lexer in
          failwith "Implement digit lexer";
          (None, lexer))
        else (None, lexer)
  in
  printf "XXX: Token: %s\n" (Token.show_uToken (lastToken |> xSOME uPOS));
  match lastToken with
  | Some token -> (
      match token.kind with
      | Token.WHITESPACE | Token.NEWLINE -> nextToken lastLexer
      | _ -> Some (token, lastLexer))
  | None -> None
;;

(* Consume the expected token *)
let skipToken (lexer : uLexer) (expected : Token.uTokenKind) : uLexer option =
  match nextToken lexer with
  | Some (token, newLexer) -> (
      match token.kind = expected with true -> Some newLexer | false -> None)
  | _ -> None
;;

(* Initialize lexer from start of the code *)
let init (code : string) : uLexer =
  { code; codeLen = String.length code; charIdx = 0; lineIdx = 1; colIdx = 1 }
;;
