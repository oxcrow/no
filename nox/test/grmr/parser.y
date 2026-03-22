%{
#include <stdio.h>
#include "parser.tab.h"

/* Report errors */
void yyerror(const char * msg);
int yylex(void);
%}

%expect 0 /* FUCK Shift-Reduce conflicts! There shall be none in my grammar! */

%locations
%define parse.error verbose

%token AS ELSE FN INT IF LET MUT MOD NEW OK RETURN USE
%token SEMICOLON QUESTION AMPERSAND COMMA DOT
%token NOTEQ EQEQ EQ AT LE GE LT GT NOT 
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token IDVAL INTVAL

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

useList
    : useList use
    | %empty
    ;

use
    : USE IDVAL AS IDVAL SEMICOLON
    | MOD IDVAL AS IDVAL SEMICOLON
    | USE IDVAL SEMICOLON
    | MOD IDVAL SEMICOLON
    ;

entityList
    : entityList entity
    | %empty
    ;

// Top level entities (functions, structs, enums, etc.)
entity
    : FN IDVAL LPAREN argList RPAREN returnType LBRACE blockBody RBRACE
    ;

// Block of code (used both as statements and expressions)
blockBody
    : stmtList okOption
    ;

stmtList
    : stmtList stmt
    | %empty
    ;

// Statements
stmt
    : LBRACE blockBody RBRACE
    | LET varGroup EQ expr SEMICOLON
    | LPAREN lvalList RPAREN EQ expr SEMICOLON
    | lvalExpr EQ expr SEMICOLON
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
    | DOT LBRACK exprList RBRACK
    | AT LBRACE blockBody RBRACE
    | AT ifExpr
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

exprList
    : exprNonEmptyList commaOption
    ;

exprNonEmptyList
    : exprNonEmptyList COMMA expr
    | expr
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
    | var
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

okOption
    : ok
    | %empty
    ;

ok
    : OK expr
    ;

%%

void yyerror(const char * msg) {
    fprintf(
        stderr,
        "Syntax error. (Around (Line: %d, Column: %d))\n",
        yylloc.first_line, yylloc.first_column
    );
}
