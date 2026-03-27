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

%token AS ELSE FN INT IF LET MATCH MUT MOD NEW RETURN OX USE
%token SEMICOLON QUESTION AMPERSAND COLON COMMA DOT BAR
%token NOTEQ EQEQ EQ AT LE GE LT GT NOT
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token IDVAL INTVAL STRVAL
%token UNDERSCORE

%nonassoc LE GE LT GT
%nonassoc NOTEQ EQEQ
%precedence NOT
%left PLUS MINUS
%left STAR SLASH
%precedence UMINUS UPLUS
%left DOT

%start program

%%

program: useList entityList

// List of imported modules
useList
    : useList use
    | %empty
    ;

// List of entities
entityList
    : entityList entity
    | %empty
    ;

// Used imports
use
    : USE IDVAL EQ modPath SEMICOLON
    | MOD IDVAL EQ modPath SEMICOLON
    | USE usePath SEMICOLON
    | MOD modPath SEMICOLON
    ;

// Unused imports
modPath
    : modPath DOT IDVAL
    | STRVAL
    | IDVAL
    ;
usePath
    : usePath DOT IDVAL
    | IDVAL
    ;

// Top level entities (functions, structs, enums, etc.)
entity
    : FN IDVAL LPAREN argList RPAREN returnType LBRACE blockBody RBRACE
    ;

// Block of code (used both as statements and expressions)
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
    | postExpr QUESTION
    | postExpr AMPERSAND
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

// Binary operation expressions
biopExpr
    : expr DOT expr %prec DOT
    | MINUS expr %prec UMINUS
    | PLUS expr %prec UPLUS
    | STAR expr
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
    : BAR patExprList COLON matchBody
    | BAR UNDERSCORE COLON matchBody
    ;

matchBody
    : expr
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
    | lvalBiopExpr
    ;

// Left value postfix expressions
lvalPostExpr
    : lvalComp
    | lvalPostExpr QUESTION
    | lvalPostExpr AMPERSAND
    ;

// Left value compound expressions
lvalComp
    : IDVAL LPAREN exprList RPAREN
    | IDVAL
    ;

// Left value binary operation expressions
lvalBiopExpr
    : lvalExpr DOT lvalExpr %prec DOT
    ;

// List of expressions
exprList
    : exprNonEmptyList commaOption
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
    : varMut
    | varCon
    ;

varMut
    : MUT IDVAL typeOption
    ;

varCon
    : IDVAL typeOption
    ;

// Return types
returnType
    : %empty
    | type
    ;

// Types
type
    : LPAREN RPAREN
    | INT
    | STAR type
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
