let document = "///"[^'\n']*['\n']
let comment = "//"[^'\n']*['\n']
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
  | newline     { Lexing.new_line lexbuf; token lexbuf }
  | white       { token lexbuf }
  | "/*"        { comment 1 lexbuf }
  | "*/"        { failwith "Unable to terminate multi-line comment that doesn't exist." }

  (* Terminals *)
  | integer as lexeme   { ExtParser.INTVAL(lexeme) }
  | float as lexeme     { ExtParser.FLOATVAL(lexeme) }

  (* Reserved tokens *)
  | "as"        { ExtParser.AS }
  | "and"       { ExtParser.AND }
  | "else"      { ExtParser.ELSE }
  | "export"    { ExtParser.EXPORT }
  | "false"     { ExtParser.FALSE }
  | "float"     { ExtParser.FLOAT }
  | "fn"        { ExtParser.FN }
  | "if"        { ExtParser.IF }
  | "int"       { ExtParser.INT }
  | "let"       { ExtParser.LET }
  | "mod"       { ExtParser.MOD }
  | "mut"       { ExtParser.MUT }
  | "not"       { ExtParser.NOT }
  | "or"        { ExtParser.OR }
  | "return"    { ExtParser.RETURN}
  | "set"       { ExtParser.SET }
  | "struct"    { ExtParser.STRUCT }
  | "true"      { ExtParser.TRUE }
  | "undefined" { ExtParser.UNDEFINED }
  | "use"       { ExtParser.USE }

  | "=="        { ExtParser.EQEQ }
  | "!="        { ExtParser.NE }
  | "="         { ExtParser.EQ }
  | "<="        { ExtParser.LE }
  | ">="        { ExtParser.GE }
  | "<"         { ExtParser.LT }
  | ">"         { ExtParser.GT }

  | ";"         { ExtParser.SEMICOLON }
  | ":"         { ExtParser.COLON }
  | ","         { ExtParser.COMMA }
  | "@"         { ExtParser.AT }
  | ".."        { ExtParser.DOTDOT }
  | "."         { ExtParser.DOT }
  | "?"         { ExtParser.QUESTION }
  | "`"         { ExtParser.TICK }
  | "'"         { ExtParser.APOSTROPHE }
  | "!"         { ExtParser.EXCLAMATION }
  | "&"         { ExtParser.AMPERSAND }
  | "#"         { ExtParser.HASH }
  | "%"         { ExtParser.PERCENT }
  | "$"         { ExtParser.DOLLAR }
  | "{"         { ExtParser.LBRACE }
  | "}"         { ExtParser.RBRACE }
  | "("         { ExtParser.LPAREN }
  | ")"         { ExtParser.RPAREN }
  | "["         { ExtParser.LBRACK }
  | "["         { ExtParser.LBRACK }
  | "<"         { ExtParser.LANGLE }
  | ">"         { ExtParser.RANGLE }
  | "+"         { ExtParser.PLUS }
  | "-"         { ExtParser.MINUS }
  | "*"         { ExtParser.STAR }
  | "/"         { ExtParser.SLASH }
  | "^"         { ExtParser.CARET }
  | "|"         { ExtParser.BAR }

  (* Identifiers *)
  | id as lexeme { ExtParser.IDVAL(lexeme) }

  (* Catch'em all! *)
  | eof         { ExtParser.EOF }
  | _           { raise (Failure ("Unknown character: " ^ Lexing.lexeme lexbuf)) }

and comment level = parse
  | "/*"        { comment (level+1) lexbuf }
  | "*/"        { if level = 1 then token lexbuf else comment (level-1) lexbuf }
  | "\n"        { Lexing.new_line lexbuf; comment level lexbuf }
  | eof         { failwith "Failed to terminate multi-line commment before end of file." }
  | _           { comment level lexbuf }
