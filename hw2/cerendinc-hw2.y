%{
#include <stdio.h>
void yyerror (const char *s) /* Called by yyparse on error */ {
    
}
%}
%token tSTRING tGET tSET tFUNCTION tPRINT tIF tRETURN tADD tSUB tMUL tDIV tINC tGT tEQUALITY tDEC tLT tLEQ tGEQ tIDENT tNUM tLBRAC tRBRAC tCOMMA
%start program

%% /* Grammar rules and actions follow */ 

program:   tLBRAC statement tRBRAC
;

statement:  
            | tLBRAC tSET tCOMMA tIDENT tCOMMA sExpression tRBRAC statement
            | tLBRAC tPRINT tCOMMA sExpression tRBRAC statement
            | tLBRAC tINC tCOMMA tIDENT tRBRAC statement
            | tLBRAC tDEC tCOMMA tIDENT tRBRAC statement
            | tLBRAC tIF tCOMMA condition tCOMMA tLBRAC statement tRBRAC optionalElse tRBRAC statement
            | sExpression statement
            | tLBRAC tRETURN optionalReturn tRBRAC statement
;

sExpression:    tNUM
                | tSTRING
                | operator
                | condition
                | function
                | get
;

get:    tLBRAC tGET tCOMMA tIDENT tRBRAC
        | tLBRAC tGET tCOMMA tIDENT tCOMMA tLBRAC lExpression tRBRAC tRBRAC
;

lExpression:    
                | sExpression
                | sExpression tCOMMA lExpression
;

condition:      tLBRAC tLEQ tCOMMA sExpression tCOMMA sExpression tRBRAC
                | tLBRAC tGEQ tCOMMA sExpression tCOMMA sExpression tRBRAC
                | tLBRAC tEQUALITY tCOMMA sExpression tCOMMA sExpression tRBRAC
                | tLBRAC tLT tCOMMA sExpression tCOMMA sExpression tRBRAC
                | tLBRAC tGT tCOMMA sExpression tCOMMA sExpression tRBRAC
;

operator:   tLBRAC tADD tCOMMA sExpression tCOMMA sExpression tRBRAC
            | tLBRAC tSUB tCOMMA sExpression tCOMMA sExpression tRBRAC
            | tLBRAC tMUL tCOMMA sExpression tCOMMA sExpression tRBRAC
            | tLBRAC tDIV tCOMMA sExpression tCOMMA sExpression tRBRAC
;

function:   tLBRAC tFUNCTION tCOMMA tLBRAC parameter tRBRAC tCOMMA tLBRAC statement tRBRAC tRBRAC
;

parameter:  
            | tIDENT
            | tIDENT tCOMMA parameter
;

optionalElse:     
                | tLBRAC statement tRBRAC
;

optionalReturn:     
                    | tCOMMA sExpression
;

%%
int main ()
{
    if (yyparse()) {
        // yyparse returns 1 if there is an error 
        // parse error
        printf("ERROR\n"); 
        return 1;
    }
    else {
        // yyparse returns 0 if the parsing is completed successfully 
        // successful parsing
        printf("OK\n");
        return 0;
    }
}