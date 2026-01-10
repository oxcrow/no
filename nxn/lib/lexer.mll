let document = "///"[^'\n']*
let comment = "//"[^'\n']*
let nospace = ['\n']['\t']*' '
let indent = ['\n']['\t']*
let newline = ['\n']
let white = [' ''\t']
let digit = ['0'-'9']
let integer = digit['0'-'9''_']*
let float = digit+(['.']digit+)?
let id = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*

rule token = parse
  (* Simple symbols *)
  | document    { Lexing.new_line lexbuf; token lexbuf }
  | comment     { Lexing.new_line lexbuf; token lexbuf }
  | nospace     { Lexing.new_line lexbuf; failwith "Spaces will never be allowed for indentation! Use tabs!" }
  | indent      { Lexing.new_line lexbuf; token lexbuf }
  | ' '         { token lexbuf }
  | '\t'        { failwith "Tabs will never be allowed for allignment! Use spaces!" }
  | "/*"        { comment 1 lexbuf }
  | "*/"        { failwith "Can not terminate multi-line comment that doesn't exist." }

  (* Terminals *)
  | integer as lexeme { Parser.INTVAL(int_of_string lexeme) }
  | float as lexeme { Parser.FLOATVAL(float_of_string lexeme) }

  (* Reserved tokens *)
  | "as"        { Parser.AS }
  | "and"       { Parser.AND }
  | "con"       { Parser.CON }
  | "else"      { Parser.ELSE }
  | "false"     { Parser.FALSE }
  | "float"     { Parser.FLOAT }
  | "fn"        { Parser.FN }
  | "if"        { Parser.IF }
  | "int"       { Parser.INT }
  | "let"       { Parser.LET }
  | "mut"       { Parser.MUT }
  | "not"       { Parser.NOT }
  | "or"        { Parser.OR }
  | "return"    { Parser.RETURN}
  | "set"       { Parser.SET }
  | "true"      { Parser.TRUE }
  | "undefined" { Parser.UNDEFINED }

  | "=="        { Parser.EQEQ }
  | "="         { Parser.EQ }
  | "/="        { Parser.NE }
  | "<="        { Parser.LE }
  | ">="        { Parser.GE }
  | "<"         { Parser.LT }
  | ">"         { Parser.GT }

  | ";"         { Parser.SEMICOLON }
  | ":"         { Parser.COLON }
  | ","         { Parser.COMMA }
  | "@"         { Parser.AT }
  | ".."        { Parser.DOTDOT }
  | "."         { Parser.DOT }
  | "?"         { Parser.QUESTION }
  | "`"         { Parser.TICK }
  | "!"         { Parser.EXCLAMATION }
  | "&"         { Parser.AMPERSAND }
  | "#"         { Parser.HASH }
  | "~"         { Parser.TILDE }
  | "%"         { Parser.PERCENT }
  | "$"         { Parser.DOLLAR }
  | "{"         { Parser.LBRACE }
  | "}"         { Parser.RBRACE }
  | "("         { Parser.LPAREN }
  | ")"         { Parser.RPAREN }
  | "["         { Parser.LBRACK }
  | "["         { Parser.LBRACK }
  | "<"         { Parser.LANGLE }
  | ">"         { Parser.RANGLE }
  | "+"         { Parser.PLUS }
  | "-"         { Parser.MINUS }
  | "*"         { Parser.STAR }
  | "/"         { Parser.SLASH }
  | "^"         { Parser.CARET }
  | "|"         { Parser.BAR }

  (* Identifiers *)
  | id as lexeme { Parser.IDVAL(lexeme) }

  (* Catch'em all! *)
  | eof         { Parser.EOF }
  | _           { raise (Failure ("Unknown character: " ^ Lexing.lexeme lexbuf)) }

and comment level = parse
  | "/*"        { comment (level+1) lexbuf }
  | "*/"        { if level = 1 then token lexbuf else comment (level-1) lexbuf }
  | "\n"        { Lexing.new_line lexbuf; comment level lexbuf }
  | eof         { failwith "Failed to terminate multi-line commment before end of file." }
  | _           { comment level lexbuf }
