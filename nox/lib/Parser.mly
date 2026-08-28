%{
%}

%token <string> XINT
%token <string> XFLOAT
%token <string> XNAME

%token AS AND ELSE ENUM EXPORT FALSE FLOAT FN IF LET LOCAL MACRO MOD MUT NOT OR RETURN STRUCT TRUE TYPE UNDEFINED USE YIELD
%token EQEQ NOTEQ EQ LTEQ GTEQ LT GT

%token SEMICOLON COLON COMMA AT DOTDOT DOT QUESTION TICK EXCLAMATION AMPERSAND HASH
%token PERCENT DOLLAR LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK LANGLE RANGLE
%token PLUS MINUS STAR SLASH CARET BAR

%token EOF

(* Start parsing from the file node *)
%start      file
%type       <Ast.file> file

%%

file:
    | EOF { Ast.File {entities=[]; file="I DON'T KNOW!"} }
    | e=nonempty_list(entities); EOF { Ast.File {entities=e; file="I DON'T KNOW!"} }

entities:
    | XINT { 0 }
