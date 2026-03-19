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

[ \t]+      { /* Ignore whitespaces */ }
\n          { yycolumn = 1; }

"//".*      { /* Ignore comments */ }

"else"      { return ELSE; }
"fn"        { return FN; }
"int"       { return INT; }
"if"        { return IF; }
"let"       { return LET; }
"mut"       { return MUT; }
"ok"        { return OK; }
"return"    { return RETURN; }

";"         { return SEMICOLON; }
"?"         { return QUESTION; }
"&"         { return AMPERSAND; }
","         { return COMMA; }
"."         { return DOT; }

"!="        { return NOTEQ; }
"=="        { return EQEQ; }
"="         { return EQ; }
"@"         { return AT; }
">="        { return GE; }
"<="        { return LE; }
">"         { return GT; }
"<"         { return LT; }
"!"         { return LT; }

"+"         { return PLUS; }
"-"         { return MINUS; }
"*"         { return STAR; }
"/"         { return SLASH; }

"("         { return LPAREN; }
")"         { return RPAREN; }
"{"         { return LBRACE; }
"}"         { return RBRACE; }
"["         { return LBRACK; }
"]"         { return RBRACK; }

[a-zA-Z_][a-zA-Z0-9_]* { return IDVAL; }
[0-9]+      { return INTVAL; }

. {
    fprintf(
        stderr,
        "Illegal character '%s'. (Around (Line: %d, Column: %d))\n",
        yytext,
        yylineno,
        yycolumn
    );
    exit(EXIT_FAILURE);
}

%%
