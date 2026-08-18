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

%code requires {
    #ifndef YYLTYPE_IS_DECLARED
    #define YYLTYPE_IS_DECLARED 1
    typedef struct YYLTYPE {
        int first_line;
        int first_column;
        int last_line;
        int last_column;
        const char *filename;
    } YYLTYPE;
    #endif
}

%token AS DEF ELSE EXPORT FN IF LET LIB MATCH MOD MUT RUN RETURN SET USE
%token SEMICOLON QUESTION AMPERSAND COLON COMMA DOLLAR HASH DOTDOT DOT BAR
%token NOTEQ EQEQ EQ AT LTEQ GTEQ LT GT NOT
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK RARROW
%token XNICK XNAME XSTR XINT
%token UNDERSCORE

// %nonassoc LTEQ GTEQ LT GT
// %nonassoc NOTEQ EQEQ
// %precedence NOT
%left PLUS MINUS
%left STAR SLASH
%precedence UMINUS UPLUS
%left DOT

%start program

%%

program: useList entityList

////////////////////////////////////////////////////////////////////////////////
// Imports (lib, mod, use) and entities (functions, structs, enums)
////////////////////////////////////////////////////////////////////////////////

useList
    : useList use
    | %empty
    ;

use
    : MOD XNAME useOption useAliasOption SEMICOLON
    ;

useOption
    : DOT LBRACE nameList RBRACE
    | %empty
    ;

useAliasOption
    : AS XNAME
    | %empty
    ;

entityList
    : entityList entity
    | %empty
    ;

entity
    : scope FN XNAME LPAREN RPAREN returnType block
    ;

block
    : LBRACE stmtList endExpr RBRACE
    ;

////////////////////////////////////////////////////////////////////////////////
// Statements and expressions
////////////////////////////////////////////////////////////////////////////////

stmtList
    : stmtList stmt
    | %empty
    ;

stmt
    : LET varGroup EQ expr SEMICOLON
    ;

endExpr
    : COLON expr
    | %empty
    ;

expr
    : LPAREN expr RPAREN
    | postExpr
    | biopExpr
    // | condExpr
    ;

postExpr
    : compExpr
    | postExpr QUESTION
    | postExpr AMPERSAND
    ;

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

compExpr
    : XNICK LPAREN exprList RPAREN
    | XNAME LPAREN exprList RPAREN
    | block
    | tupleExpr
    | matchExpr
    | XNAME
    | XSTR
    | XINT
    ;

exprList
    : exprNonEmptyList commaOption
    ;
exprNonEmptyList
    : exprNonEmptyList COMMA expr
    | expr
    ;

matchExpr
    : MATCH expr LBRACE matchBodyList RBRACE
    ;

matchBodyList
    : matchBodyList matchCase
    | matchCase
    ;

matchCase
    : BAR patExpr RARROW expr commaOption
    | UNDERSCORE RARROW expr commaOption
    ;

patExpr
    : XNAME
    ;

tupleExpr
    : LPAREN exprNonEmptyTuple commaOption RPAREN
    ;

exprNonEmptyTuple
    : exprNonEmptyTuple COMMA expr
    | expr COMMA expr
    ;

////////////////////////////////////////////////////////////////////////////////
// Variables and arguments
////////////////////////////////////////////////////////////////////////////////

varGroup
    : LPAREN varGroup RPAREN
    | LBRACK varGroup RBRACK
    | varList
    ;

varList
    : varNonEmptyList commaOption
    ;

varNonEmptyList
    : varNonEmptyList COMMA var
    | var
    ;

var
    : state XNAME typeOption
    ;

// arg
//     : XNAME type
//     ;

////////////////////////////////////////////////////////////////////////////////
// Utilities
////////////////////////////////////////////////////////////////////////////////

nameList
    : nameNonEmptyList commaOption
    ;

nameNonEmptyList
    : nameNonEmptyList COMMA XNAME
    | XNAME
    ;

returnType
    : %empty
    | type
    ;

type
    : LPAREN RPAREN
    | LBRACK RBRACK
    | AMPERSAND type
    | XNAME
    ;

typeOption
    : type
    | %empty
    ;

state
    : MUT
    | SET
    | %empty
    ;

scope
    : %empty
    | EXPORT
    ;

commaOption
    : COMMA
    | %empty
    ;

%%

void yyerror(const char * msg) {
    fprintf(
        stderr,
        "Syntax error. (Around (Line: %d, Column: %d, File: '%s'))\n",
        yylloc.first_line, yylloc.first_column, yylloc.filename
    );
}
