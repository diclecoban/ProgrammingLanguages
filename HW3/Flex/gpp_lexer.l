%{
#include "y.tab.h"
#include <stdio.h>
#include <stdlib.h>
int yylval;
%}

%option noyywrap

%%

[aA][nN][dD]        { return KW_AND; }
[oO][rR]            { return KW_OR; }
[nN][oO][tT]        { return KW_NOT; }
[eE][qQ][uU][aA][lL] { return KW_EQUAL; }
[lL][eE][sS][sS]    { return KW_LESS; }
[nN][iI][lL]        { return KW_NIL; }
[lL][sS][tT]        { return KW_LIST; }
[aA][pP][pP][eE][nN][dD] { return KW_APPEND; }
[cC][oO][nN][cC][aA][tT] { return KW_CONCAT; }
[sS][eE][tT]        { return KW_SET; }
[dD][eE][fF][fF][uU][nN] { return KW_DEFFUN; }
[fF][oO][rR]        { return KW_FOR; }
[iI][fF]            { return KW_IF; }
[eE][xX][iI][tT]    { return KW_EXIT; }
[lL][oO][aA][dD]    { return KW_LOAD; }
[pP][rR][iI][nN][tT] { return KW_PRINT; }
[tT][rR][uU][eE]    { return KW_TRUE; }
[fF][aA][lL][sS][eE] { return KW_FALSE; }

"("                 { return OP_OP; }
")"                 { return OP_CP; }
"+"                 { return OP_PLUS; }
"-"                 { return OP_MINUS; }
"*"                 { return OP_MULT; }
"/"                 { return OP_DIV; }
;;.*                { /* Yorum satırlarını yok say */ }
;.*                { /* Noktalı virgülü görmezden gel */ }

[ \t\n]+            ; // Ignore whitespaces
[0-9]+              { yylval = atoi(yytext); return VALUEI; }
[0-9]+\.[0-9]+       { return VALUEF; }
[a-zA-Z][a-zA-Z0-9_]* { return IDENTIFIER; }

.                   { printf("SYNTAX_ERROR: %s\n", yytext); }

%%

