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
type uLexer = {
  code : string; [@opaque]
  codeLen : int;
  charIdx : int;
  lineIdx : int;
  colIdx : int;
  file : string; [@opaque]
}
[@@deriving show { with_path = false }]

(* Tokenize the next token *)
let rec nextToken (lexer : uLexer) : (Token.uToken * uLexer) option =
  (* Identify the current character (or if we're at the end of file) *)
  let c =
    match lexer.charIdx < lexer.codeLen with
    | true -> Some lexer.code.[lexer.charIdx]
    | false -> None
  in
  (* Identify the primary reserved tokens (or if they don't exist) *)
  let primaryToken, lexer =
    (* Does substring match given target? *)
    let substrMatches (text : string) (target : string) (start : int) : bool =
      let targetLen = String.length target in
      let textLen = String.length text in
      if targetLen > textLen then begin
        raise
          (Report
             {
               message =
                 "Unable to compare substrings (Target is larger than given string)";
               source = xSOURCE uPOS;
               error = None;
             })
      end;
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
      substrMatches text word start && isDel (lastChar text word)
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
              colIdx = lexer.colIdx + wordLen;
            } )
      | false -> (None, lexer)
    in
    (* Extract reserved atomic token if it exists *)
    let lexAtom (lexer : uLexer) (kind : Token.uTokenKind) (atom : string) :
        Token.uToken option * uLexer =
      let atomLen = String.length atom in
      match substrMatches lexer.code atom lexer.charIdx with
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
    let lexMany (lexer : uLexer) (kinds : Token.uTokenKind list) (words : string list)
        lexMethod : Token.uToken option * uLexer =
      List.map2 (fun kind word -> lexMethod lexer kind word) kinds words
      |> List.filter (fun tokens -> isSome (fst tokens))
      |> fun tokens ->
      match List.nth_opt tokens 0 with
      | Some (token, lexer) -> (token, lexer)
      | None -> (None, lexer)
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
    (* Reserved keywords and atoms tokenized one at a time *)
    let token, lexer =
      match c with
      | Some ' ' | Some '\t' -> lexWhite lexer
      | Some '\n' -> lexLine lexer
      | Some 'a' -> (None, lexer)
      | Some 'b' -> (None, lexer)
      | Some 'c' -> (None, lexer)
      | Some 'd' -> (None, lexer)
      | Some 'e' -> lexWord lexer Token.ELSE "else"
      | Some 'f' -> lexWord lexer Token.FUNCTION "fn"
      | Some 'g' -> (None, lexer)
      | Some 'h' -> (None, lexer)
      | Some 'i' -> lexMany lexer [ Token.INT; Token.IF ] [ "int"; "if" ] lexWord
      | Some 'j' -> (None, lexer)
      | Some 'k' -> (None, lexer)
      | Some 'l' -> lexWord lexer Token.LET "let"
      | Some 'm' ->
          lexMany lexer
            [ Token.MATCH; Token.MUT; Token.MOD ]
            [ "match"; "mut"; "mod" ] lexWord
      | Some 'n' -> (None, lexer)
      | Some 'o' -> (None, lexer)
      | Some 'p' -> lexWord lexer Token.PUBLIC "pub"
      | Some 'q' -> (None, lexer)
      | Some 'r' -> lexWord lexer Token.RETURN "return"
      | Some 's' -> (None, lexer)
      | Some 't' -> (None, lexer)
      | Some 'u' -> lexWord lexer Token.USE "use"
      | Some 'v' -> (None, lexer)
      | Some 'w' -> (None, lexer)
      | Some 'x' -> (None, lexer)
      | Some 'y' -> (None, lexer)
      | Some 'z' -> (None, lexer)
      (* Mathematical *)
      | Some '+' -> lexAtom lexer Token.PLUS "+"
      | Some '-' -> lexAtom lexer Token.MINUS "-"
      | Some '*' -> lexAtom lexer Token.STAR "*"
      | Some '/' ->
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
                      ( Some token,
                        {
                          lexer with
                          charIdx = lexer.charIdx + lineLen;
                          colIdx = lexer.colIdx + lineLen;
                        } )
                  | Token.SLASH -> (Some token, lexer)
                  | _ -> xNEVER uPOS "wut?"
                in
                (token, lexer)
            | None, lexer -> (None, lexer)
          in
          (token, lexer)
      (* Conditionals *)
      | Some '=' -> lexMany lexer [ Token.EQEQ; Token.EQ ] [ "=="; "=" ] lexAtom
      | Some '>' -> lexMany lexer [ Token.GE; Token.GT ] [ ">="; ">" ] lexAtom
      | Some '<' -> lexMany lexer [ Token.LE; Token.LT ] [ "<="; "<" ] lexAtom
      (* Symbols *)
      | Some ';' -> lexAtom lexer Token.SEMICOLON ";"
      | Some '?' -> lexAtom lexer Token.QUESTION "?"
      | Some '&' -> lexAtom lexer Token.AMPERSAND "&"
      | Some ':' -> lexAtom lexer Token.COLON ":"
      | Some ',' -> lexAtom lexer Token.COMMA ","
      | Some '.' -> lexAtom lexer Token.DOT "."
      | Some '|' -> lexAtom lexer Token.BAR "|"
      | Some '(' -> lexAtom lexer Token.LPAREN "("
      | Some ')' -> lexAtom lexer Token.RPAREN ")"
      | Some '{' -> lexAtom lexer Token.LBRACE "{"
      | Some '}' -> lexAtom lexer Token.RBRACE "}"
      | Some '[' -> lexAtom lexer Token.LBRACK "["
      | Some ']' -> lexAtom lexer Token.RBRACK "]"
      (* Unknown character that can not be lexed as a primary token. *)
      | Some c ->
          raise
            (Report
               {
                 message = "Unkown character that can not be lexed.";
                 source = xSOURCE uPOS;
                 error = None;
               })
      (* End of file *)
      | None ->
          ( Some
              {
                kind = Token.EOF;
                charIdx = lexer.charIdx;
                lineIdx = lexer.lineIdx;
                colIdx = lexer.colIdx;
              },
            lexer )
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
    match (c, primaryToken) with
    | Some c, Some token -> (Some token, lexer)
    | Some c, None ->
        if isAlpha c then begin
          let token, lexer = lexName lexer in
          (Some token, lexer)
        end
        else if isDigit c then begin
          let token, lexer = lexDigit lexer in
          failwith "Implement digit lexer";
          (None, lexer)
        end
        else (None, lexer)
    | None, _ -> (None, lexer)
  in
  match lastLexer.charIdx < lastLexer.codeLen with
  | true -> (
      match lastToken with
      | Some token -> (
          match token.kind with
          | Token.WHITESPACE | Token.NEWLINE -> nextToken lastLexer
          | Token.DOCUMENT | Token.COMMENT -> nextToken lastLexer
          | _ -> Some (token, lastLexer))
      | None -> None)
  | false ->
      Some
        ( {
            kind = Token.EOF;
            charIdx = lastLexer.charIdx;
            lineIdx = lastLexer.lineIdx;
            colIdx = lastLexer.colIdx;
          },
          lastLexer )
;;

(* Consume the expected token *)
let skipToken (lexer : uLexer) (expected : Token.uTokenKind) : uLexer option =
  match nextToken lexer with
  | Some (token, newLexer) -> (
      match token.kind = expected with true -> Some newLexer | false -> None)
  | _ -> None
;;

(* Initialize lexer from start of the code *)
let init (file : string) (code : string) : uLexer =
  { code; codeLen = String.length code; charIdx = 0; lineIdx = 1; colIdx = 1; file }
;;
