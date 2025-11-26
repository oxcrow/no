%token <int> INTVAL
%token <float> FLOATVAL
%token <string> IDVAL

%token BREAK
%token CASE
%token CHANNEL
%token CONTINUE
%token DEFER
%token ELSE
%token ENUM
%token FOR
%token FN
%token GO
%token GOTO
%token IF
%token IMPORT
%token IMPLEMENT
%token INTERFACE
%token LET
%token MAP
%token PACKAGE
%token RANGE
%token RETURN
%token SELECT
%token STRUCT
%token SWITCH
%token TYPE
%token VAR
%token WHILE

%token DOTDOT
%token EQ
%token NE
%token LE
%token GE
%token LT
%token GT

%token SEMICOLON
%token COLON
%token COMMA
%token DOT
%token QUESTION
%token AMPERSAND
%token HASH
%token TILDE
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LANGLE
%token RANGLE
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token CARET
%token BAR
%token EQUAL
%token EXCLAMATION

%token EOF

%start file
%type  <Ast.file> file

%%

file:
    | EOF { Ast.File { entities = []; filename = "_" } }
    | e = nonempty_list(entities) EOF {Ast.File { entities = e; filename = "_" }}

entities:
    | INTVAL { Ast.NoneEntity }
