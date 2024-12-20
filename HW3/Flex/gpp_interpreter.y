%{
#include <stdio.h>
#include <stdlib.h>

// Lexer fonksiyonunun bildirimi
extern int yylex();
extern int yylval;
void yyerror(const char *s);
%}


%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL
%token KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN
%token KW_FOR KW_IF KW_EXIT KW_LOAD KW_PRINT KW_TRUE KW_FALSE
%token OP_OP OP_CP OP_PLUS OP_MINUS OP_MULT OP_DIV
%token <intval> VALUEI
%token <floatval> VALUEF
%token <strval> IDENTIFIER


%start start

%%

start: input;

input: expression
     | expression_list
     | function_definition
     | control_statement
     | variable_definition
     | exit_statement
     | print_statement
     | load_statement;

expression_list: OP_OP valid_expression_list OP_CP ;

valid_expression_list: expression_list expression
                     | /* empty */ ;

expression: OP_OP KW_AND expression expression OP_CP
          | OP_OP KW_OR expression expression OP_CP
          | OP_OP KW_NOT expression OP_CP
          | OP_OP KW_EQUAL expression expression OP_CP
          | OP_OP KW_LESS expression expression OP_CP
          | OP_OP KW_LIST expression_list OP_CP
          | OP_OP KW_APPEND expression expression OP_CP
          | OP_OP KW_CONCAT expression expression OP_CP 
          |OP_OP OP_PLUS expression expression OP_CP
          | OP_OP OP_MINUS expression expression OP_CP
          | OP_OP OP_MULT expression expression OP_CP
          | OP_OP OP_DIV expression expression OP_CP
          | VALUEI
          | VALUEF
          | IDENTIFIER
          | KW_TRUE
          | KW_FALSE
          | KW_NIL;

function_definition: OP_OP KW_DEFFUN IDENTIFIER parameter_list expression_list OP_CP;

parameter_list: OP_OP parameter_list IDENTIFIER OP_CP
              | IDENTIFIER;

control_statement: OP_OP KW_IF expression expression_list OP_CP
                 | OP_OP KW_IF expression expression_list expression_list OP_CP
                 | OP_OP KW_FOR OP_OP IDENTIFIER expression expression OP_CP expression_list OP_CP;

variable_definition: OP_OP KW_SET IDENTIFIER expression OP_CP;

exit_statement: OP_OP KW_EXIT OP_CP;

print_statement: OP_OP KW_PRINT expression OP_CP;

load_statement: OP_OP KW_LOAD IDENTIFIER OP_CP;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}

int main() {
    printf("Enter your expressions:\n");
    yyparse();
    return 0;
}