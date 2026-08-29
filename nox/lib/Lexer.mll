let document = "///"[^'\n']*['\n']
let comment = "//"[^'\n']*['\n']
let newline = ['\n']
let white = [' ']
let digit = ['0'-'9']
let integer = digit['0'-'9''_']*
let float = digit+(['.']digit+)?
let id = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*

rule token = parse
    (* Simple symbols *)
    | document              { Lexing.new_line lexbuf; token lexbuf }
    | comment               { Lexing.new_line lexbuf; token lexbuf }
    | newline               { Lexing.new_line lexbuf; token lexbuf }
    | white                 { token lexbuf }
    | '\t'                  { raise (Failure ("Tabs are not allowed!")) }

    (* Terminals *)
    | integer as lexeme     { Parser.XINT(lexeme) }
    | float as lexeme       { Parser.XFLOAT(lexeme) }

    (* Reserved tokens *)
    | "as"                  { Parser.AS }
    | "and"                 { Parser.AND }
    | "else"                { Parser.ELSE }
    | "enum"                { Parser.ENUM }
    | "export"              { Parser.EXPORT }
    | "false"               { Parser.FALSE }
    | "float"               { Parser.FLOAT }
    | "fn"                  { Parser.FN }
    | "if"                  { Parser.IF }
    | "int"                 { Parser.INT }
    | "later"               { Parser.LATER }
    | "let"                 { Parser.LET }
    | "local"               { Parser.LOCAL }
    | "macro"               { Parser.MACRO }
    | "mod"                 { Parser.MOD }
    | "mut"                 { Parser.MUT }
    | "never"               { Parser.NEVER }
    | "none"                { Parser.NONE }
    | "not"                 { Parser.NOT }
    | "ok"                  { Parser.OK }
    | "or"                  { Parser.OR }
    | "return"              { Parser.RETURN}
    | "some"                { Parser.SOME }
    | "struct"              { Parser.STRUCT }
    | "todo"                { Parser.TODO }
    | "true"                { Parser.TRUE }
    | "type"                { Parser.TRUE }
    | "undefined"           { Parser.UNDEFINED }
    | "use"                 { Parser.USE }
    | "yield"               { Parser.YIELD }

    | "=="                  { Parser.EQEQ }
    | "!="                  { Parser.NOTEQ }
    | "="                   { Parser.EQ }
    | "<="                  { Parser.LTEQ }
    | ">="                  { Parser.GTEQ }
    | "<"                   { Parser.LT }
    | ">"                   { Parser.GT }

    | ";"                   { Parser.SEMICOLON }
    | ":"                   { Parser.COLON }
    | ","                   { Parser.COMMA }
    | "@"                   { Parser.AT }
    | ".."                  { Parser.DOTDOT }
    | "."                   { Parser.DOT }
    | "?"                   { Parser.QUESTION }
    | "`"                   { Parser.TICK }
    | "!"                   { Parser.EXCLAMATION }
    | "&"                   { Parser.AMPERSAND }
    | "#"                   { Parser.HASH }
    | "%"                   { Parser.PERCENT }
    | "$"                   { Parser.DOLLAR }
    | "{"                   { Parser.LBRACE }
    | "}"                   { Parser.RBRACE }
    | "("                   { Parser.LPAREN }
    | ")"                   { Parser.RPAREN }
    | "["                   { Parser.LBRACK }
    | "]"                   { Parser.RBRACK }
    | "<"                   { Parser.LANGLE }
    | ">"                   { Parser.RANGLE }
    | "+"                   { Parser.PLUS }
    | "-"                   { Parser.MINUS }
    | "*"                   { Parser.STAR }
    | "/"                   { Parser.SLASH }
    | "\\"                  { Parser.BACKSLASH }
    | "^"                   { Parser.CARET }
    | "|"                   { Parser.BAR }

    (* Identifiers *)
    | id as lexeme          { Parser.XNAME(lexeme) }

    (* Catch'em all! *)
    | eof                   { Parser.EOF }
    | _                     { raise (Failure ("Unknown character: " ^ Lexing.lexeme lexbuf)) }
