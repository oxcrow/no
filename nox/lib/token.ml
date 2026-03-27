type uToken = { kind : uTokenKind; charIdx : int; lineIdx : int; colIdx : int }
[@@deriving show { with_path = false }]

and uTokenKind =
  (* Reserved *)
  | AS
  | AND
  | BREAK
  | CONTINUE
  | ELSE
  | FALSE
  | FLOAT
  | FUNCTION
  | IF
  | INT
  | LET
  | MATCH
  | MOD
  | MUT
  | NOT
  | OR
  | PUBLIC
  | RETURN
  | UNDEFINED
  | USE
  (* Mathematical *)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  (* Conditionals *)
  | EQEQ
  | EQ
  | GE
  | LE
  | GT
  | LT
  (* Symbols *)
  | SEMICOLON
  | QUESTION
  | AMPERSAND
  | COLON
  | COMMA
  | DOTDOT
  | DOT
  | BAR
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  (* Values *)
  | NAMEVAL of string
  | INTVAL
  (* Other *)
  | WHITESPACE
  | DOCUMENT
  | COMMENT
  | EOF
