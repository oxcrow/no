%{
#include <stdio.h>
#include "parser.tab.h"

/* Report errors */
void yyerror(const char * msg);
int yylex(void);
%}

%expect 0 /* FUCK Shift-Reduce conflicts! I hate them so much! */

%locations
%define parse.error verbose

%token AS ELSE EXPORT FLOAT FN INT IF LET LOCAL MATCH MUT MOD NEW RETURN USE
%token SEMICOLON QUESTION MUTAMPERSAND AMPERSAND COLON COMMA DOT BAR APOSTROPHE
%token NOTEQ EQEQ EQ AT LARROW RARROW LE GE LT GT NOT
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token IDVAL INTVAL STRVAL
%token UNDERSCORE

// Precedence examples to help debug issues in future.
// a + b <  c + d is parsed as (a + b) <  (c + d) not as a + (b < c)  + d
// a + b != c + d is parsed as (a + b) != (c + d) not as a + (b != c) + d
// a + b == c + d is parsed as (a + b) == (c + d) not as a + (b == c) + d
// &mut&a is parsed as &(mut&a)
// &&a is parsed as &(&a)
// --a is parsed as -(-a)
// +-a is parsed as +(-a)

// Lowest precedence at top
%nonassoc LE GE LT GT
%nonassoc NOTEQ EQEQ
%precedence NOT
%left PLUS MINUS
%left STAR SLASH
%precedence UMINUS UPLUS UCONREF UMUTREF
%left DOT
// Highest precedence at bottom (ARRAY and DOT will always be highest)

%start program

%%

program: entityList

// List of entities
entityList
    : entityList entity
    | %empty
    ;

// Top level entities (functions, structs, enums, etc.)
entity
    : scope FN IDVAL LPAREN argList RPAREN returnType block
    | use
    ;

// List of imported modules
useList
    : useList use
    | %empty
    ;

// Used imports
use
    : scope USE IDVAL usePathOption SEMICOLON
    | scope MOD IDVAL usePathOption SEMICOLON
    ;

usePathOption
    : LPAREN STRVAL RPAREN
    | %empty
    ;

// Block of code (used both as statements and expressions)
block
    : LBRACE blockBody RBRACE
    ;
blockBody
    : useList stmtList endExpr
    ;

// Expressions at the end of a block of code
endExpr
    : COLON expr
    | %empty
    ;

// List of statements
stmtList
    : stmtList stmt
    | %empty
    ;

// Statements
stmt
    : LBRACE blockBody RBRACE
    | LET varGroup EQ expr SEMICOLON
    | LPAREN lvalList RPAREN EQ expr SEMICOLON
    | lvalList EQ expr SEMICOLON
    | lvalExpr SEMICOLON
    | RETURN expr SEMICOLON
    | ifStmt
    ;

// Expressions
expr
    : LPAREN expr RPAREN
    | postExpr
    | unopExpr
    | biopExpr
    | condExpr
    ;

// If else statements
ifStmt
    : IF condExpr LBRACE blockBody RBRACE
    | IF condExpr LBRACE blockBody RBRACE elseStmt
    ;
elseStmt
    : ELSE ifStmt
    | ELSE LBRACE blockBody RBRACE
    ;

// Postfix expressions
postExpr
    : compExpr
    | postExpr QUESTION  // %prec UTRY
    | postExpr AMPERSAND // %prec UDEREF
    ;

// Compound expressions
compExpr
    : IDVAL LPAREN exprList RPAREN
    | IDVAL LBRACK exprList RBRACK
    | DOT LBRACE exprList RBRACE
    | LBRACK exprList RBRACK
    | LPAREN exprTuple RPAREN
    | LBRACE blockBody RBRACE
    | matchExpr
    | ifExpr
    | INTVAL
    | IDVAL
    ;

// Unary operation expressions
unopExpr
    : MUTAMPERSAND expr %prec UMUTREF
    | AMPERSAND expr %prec UCONREF
    | MINUS expr %prec UMINUS
    | PLUS expr %prec UPLUS
    ;

// Binary operation expressions
biopExpr
    : expr DOT expr %prec DOT
    | expr PLUS expr
    | expr MINUS expr
    | expr STAR expr
    | expr SLASH expr
    ;

// Conditional expressions
condExpr
    : expr NOTEQ expr %prec NOTEQ
    | expr EQEQ expr %prec EQEQ
    | expr LE expr %prec LE
    | expr GE expr %prec GE
    | expr LT expr %prec LT
    | expr GT expr %prec GT
    | NOT expr %prec NOT
    ;

// Match expressions
matchExpr
    : MATCH expr LBRACE matchBodyList RBRACE
    ;

matchBodyList
    : matchBodyList matchCase
    | matchCase
    ;

matchCase
    : BAR patExprList RARROW matchBody
    ;

matchBody
    : blockBody
    ;

// List of pattern expressions
patExprList
    : patExprList patExpr commaOption
    | patExpr COMMA
    | patExpr
    ;

// Pattern expressions
patExpr
    : LPAREN patExprList RPAREN
    | STRVAL
    | INTVAL
    | UNDERSCORE
    ;

// If else expressions
ifExpr
    : IF condExpr LBRACE blockBody RBRACE elseExpr
    ;
elseExpr
    : ELSE ifExpr
    | ELSE LBRACE blockBody RBRACE
    ;

// Left value expressions
lvalExpr
    : lvalPostExpr
    | lvalUnopExpr
    | lvalBiopExpr
    ;

// Left value postfix expressions
lvalPostExpr
    : lvalComp
    | lvalPostExpr QUESTION  // %prec UTRY
    | lvalPostExpr AMPERSAND // %prec UDEREF
    ;

// Left value compound expressions
lvalComp
    : IDVAL LPAREN exprList RPAREN
    | IDVAL
    ;

// Left value unary operation expressions
lvalUnopExpr
    : MUTAMPERSAND lvalExpr %prec UMUTREF
    | AMPERSAND lvalExpr %prec UCONREF
    ;

// Left value binary operation expressions
lvalBiopExpr
    : lvalExpr DOT lvalExpr %prec DOT
    ;

// List of expressions
exprList
    : exprNonEmptyList commaOption
    | %empty
    ;
exprNonEmptyList
    : exprNonEmptyList COMMA expr
    | expr
    ;

exprTuple
    : exprNonEmptyTuple commaOption
    ;

exprNonEmptyTuple
    : exprNonEmptyTuple COMMA expr
    | expr COMMA expr
    ;

lvalList
    : lvalNonEmptyList commaOption
    ;

lvalNonEmptyList
    : lvalNonEmptyList COMMA lvalExpr
    | lvalExpr
    ;

argList
    : argNonEmptyList commaOption
    | %empty
    ;

argNonEmptyList
    : argNonEmptyList COMMA arg
    | arg
    ;

// Arguments
arg: IDVAL type

varGroup
    : LPAREN varList RPAREN
    | varList
    ;

varList
    : varNonEmptyList commaOption
    ;

varNonEmptyList
    : varNonEmptyList COMMA var
    | var
    ;

// Variables
var
    : state shadow IDVAL typeOption
    ;

// States
state
    : MUT
    | %empty
    ;

// Shadows
shadow
    : PLUS
    | %empty
    ;

// Scopes
scope
    : %empty
    | EXPORT
    | LOCAL
    ;

// Return types
returnType
    : %empty
    | type
    ;

// Types
type
    : LPAREN RPAREN
    | MUTAMPERSAND type
    | AMPERSAND type
    | STAR type
    | simpType
    ;

// Simple types
simpType
    : FLOAT
    | INT
    ;

typeOption
    : type
    | %empty
    ;

commaOption
    : COMMA
    | %empty
    ;

%%

void yyerror(const char * msg) {
    fprintf(
        stderr,
        "Syntax error. (Around (Line: %d, Column: %d))\n",
        yylloc.first_line, yylloc.first_column
    );
}
