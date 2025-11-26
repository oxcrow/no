let whitespace = [' ''\t']
let newline = ['\n']
let digit = ['0'-'9']
let integer = digit['0'-'9''_']*
let float = digit+(['.']digit+)?
let id = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
let document = "///"[^'\n']*newline
let comment = "//"[^'\n']*newline

rule token = parse
  (* Simple symbols *)
  | document { Lexing.new_line lexbuf; token lexbuf }
  | comment { Lexing.new_line lexbuf; token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | whitespace { token lexbuf }
  | "/*" { comment 1 lexbuf }
  | "*/" { failwith "Can not terminate multi-line comment that doesn't exist." }

  (* Terminals *)
  | integer as lexeme { Parser.INTVAL(int_of_string lexeme) }
  | float as lexeme { Parser.FLOATVAL(float_of_string lexeme) }

  | "break" { Parser.BREAK }
  | "case" { Parser.CASE }
  | "channel" { Parser.CHANNEL }
  | "continue" { Parser.CONTINUE }
  | "defer" { Parser.DEFER }
  | "else" { Parser.ELSE }
  | "enum" { Parser.ENUM }
  | "for" { Parser.FOR }
  | "fn" { Parser.FN }
  | "go" { Parser.GO }
  | "goto" { Parser.GOTO }
  | "if" { Parser.IF }
  | "import" { Parser.IMPORT }
  | "implement" { Parser.IMPLEMENT }
  | "interface" { Parser.INTERFACE }
  | "let" { Parser.LET }
  | "map" { Parser.MAP }
  | "package" { Parser.PACKAGE }
  | "range" { Parser.RANGE }
  | "return" { Parser.RETURN }
  | "select" { Parser.SELECT }
  | "struct" { Parser.STRUCT }
  | "switch" { Parser.SWITCH }
  | "type" { Parser.TYPE }
  | "var" { Parser.VAR }
  | "while" { Parser.WHILE }

  | ".." { Parser.DOTDOT }
  | "==" { Parser.EQ }
  | "!=" { Parser.NE }
  | "<=" { Parser.LE }
  | ">=" { Parser.GE }
  | "<" { Parser.LT }
  | ">" { Parser.GT }

  | ";" { Parser.SEMICOLON }
  | ":" { Parser.COLON }
  | "," { Parser.COMMA }
  | "." { Parser.DOT }
  | "?" { Parser.QUESTION }
  | "&" { Parser.AMPERSAND }
  | "#" { Parser.HASH }
  | "~" { Parser.TILDE }
  | "{" { Parser.LBRACE }
  | "}" { Parser.RBRACE }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "[" { Parser.LBRACK }
  | "]" { Parser.RBRACK }
  | "<" { Parser.LANGLE }
  | ">" { Parser.RANGLE }
  | "+" { Parser.PLUS }
  | "-" { Parser.MINUS }
  | "*" { Parser.STAR }
  | "/" { Parser.SLASH }
  | "^" { Parser.CARET }
  | "|" { Parser.BAR }
  | "=" { Parser.EQUAL }
  | "!" { Parser.EXCLAMATION }

  (* Identifiers *)
  | id as lexeme { Parser.IDVAL(lexeme) }

  (* Catch'em all! *)
  | eof { Parser.EOF }
  | _ { raise (Failure ("Unknown character: " ^ Lexing.lexeme lexbuf)) }

and comment level = parse
  | "/*" { comment (level+1) lexbuf }
  | "*/" { if level = 1 then token lexbuf else comment (level-1) lexbuf }
  | "\n" { Lexing.new_line lexbuf; comment level lexbuf }
  | eof { failwith "Failed to terminate multi-line commment before end of file." }
  | _ { comment level lexbuf }
