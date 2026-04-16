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
%option stack

%x comment

%%

[ \t]+      { /* Ignore whitespaces */ }
\n          { yycolumn = 1; }

"//".*      { /* Ignore comments */ }

"/*"                { yy_push_state(comment); }
<comment>"/*"       { yy_push_state(comment); }
<comment>"*/"       { yy_pop_state(); }
<comment>.|\n       { /* Ignore lines */  }
<comment><<EOF>>    {
    fprintf(stderr,
        "Unable to terminate nested comment. (Around (Line: %d, Column: %d))\n",
        yylineno,
        yycolumn
    );
    exit(EXIT_FAILURE);
}

"as"        { return AS; }
"else"      { return ELSE; }
"export"    { return EXPORT; }
"fn"        { return FN; }
"int"       { return INT; }
"if"        { return IF; }
"let"       { return LET; }
"match"     { return MATCH; }
"mut"       { return MUT; }
"mod"       { return MOD; }
"new"       { return NEW; }
"return"    { return RETURN; }
"use"       { return USE; }

";"         { return SEMICOLON; }
"?"         { return QUESTION; }
"&"         { return AMPERSAND; }
":"         { return COLON; }
","         { return COMMA; }
"."         { return DOT; }
"|"         { return BAR; }

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

"_"         { return UNDERSCORE; }

[a-zA-Z_][a-zA-Z0-9_]* { return IDVAL; }
[0-9]+      { return INTVAL; }
["][^"]*["] { return STRVAL; }

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
