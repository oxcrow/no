%{
#include "parser.tab.h"
int yycolumn = 1;

/* Update token location */
#define YY_USER_ACTION \
    yylloc.first_line = yylloc.last_line = yylineno; \
    yylloc.first_column = yycolumn; \
    yylloc.last_column = yycolumn + yyleng - 1; \
    yycolumn += yyleng;
%}

%option yylineno
%option noyywrap

%%

[ \t]+                      { /* Ignore whitespaces */ }
\r                          { perror("Unable to parse Carraige Return. (\\r).\n"); }
\n                          { yycolumn = 1; }

"//".*                      { /* Ignore comments */ }

"as"                        { return AS; }
"def"                       { return DEF; }
"else"                      { return ELSE; }
"export"                    { return EXPORT; }
"fn"                        { return FN; }
"if"                        { return IF; }
"let"                       { return LET; }
"lib"                       { return LIB; }
"match"                     { return MATCH; }
"mod"                       { return MOD; }
"mut"                       { return MUT; }
"run"                       { return RUN; }
"return"                    { return RETURN; }
"set"                       { return SET; }
"use"                       { return USE; }

";"                         { return SEMICOLON; }
"?"                         { return QUESTION; }
"&"                         { return AMPERSAND; }
":"                         { return COLON; }
","                         { return COMMA; }
"$"                         { return DOLLAR; }
"#"                         { return HASH; }
".."                        { return DOTDOT; }
"."                         { return DOT; }
"|"                         { return BAR; }

"!="                        { return NOTEQ; }
"=="                        { return EQEQ; }
"="                         { return EQ; }
">="                        { return GTEQ; }
"<="                        { return LTEQ; }
">"                         { return GT; }
"<"                         { return LT; }
"!"                         { return NOT; }

"+"                         { return PLUS; }
"->"                        { return RARROW; }
"-"                         { return MINUS; }
"*"                         { return STAR; }
"/"                         { return SLASH; }

"("                         { return LPAREN; }
")"                         { return RPAREN; }
"{"                         { return LBRACE; }
"}"                         { return RBRACE; }
"["                         { return LBRACK; }
"]"                         { return RBRACK; }

"_"                         { return UNDERSCORE; }

[@][a-zA-Z_][a-zA-Z0-9_]*   { return XNICK; }
[a-zA-Z_][a-zA-Z0-9_]*      { return XNAME; }
[0-9]+                      { return XINT; }
["][^"]*["]                 { return XSTR; }

. {
    fprintf(stderr,
        "Illegal character '%s'. (Around (Line: %d, Column: %d))\n",
        yytext,
        yylineno,
        yycolumn
    );
    exit(EXIT_FAILURE);
}

%%
