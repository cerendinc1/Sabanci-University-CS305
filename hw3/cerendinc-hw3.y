%{
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "chw.h"
void yyerror (const char *s) 
{
}

Attr * node;

Attr * cr_node();
ExprNode cnvs(char * mystr);
ExprNode cnvi(IntNode myi);
ExprNode cnvr(RealNode myr);
ExprNode esum(ExprNode exn, ExprNode exnn, int curlin);
ExprNode edec(ExprNode exn, ExprNode exnn, int curlin);
ExprNode emlt(ExprNode exn, ExprNode exnn, int curlin);
ExprNode edv(ExprNode exn, ExprNode exnn, int curlin);
char *unitee(const char * lstr, const char * rstr);
void getAttr(struct Attr *head, ExprNode mynode,int att);
void pfunc(struct Attr *head);

%}


%token tPRINT tGET tSET tFUNCTION tRETURN tIDENT tEQUALITY tIF tGT tLT tGEQ tLEQ tINC tDEC

%union {
char * string_value;
IntNode intNode;
RealNode realNode;
ExprNode exprNode;
int curLine;
}

%type <exprNode> operation
%type <exprNode> expr
%token <string_value> tSTRING
%token <curLine> tADD tSUB tMUL tDIV
%token <intNode> tINT
%token <realNode> tREAL

%start prog

%%
prog:		'[' stmtlst ']'
;

stmtlst:	stmtlst stmt |
;

stmt:		setStmt | if | print | unaryOperation | expr {getAttr(node, $1,0); } 
		| returnStmt

;

getExpr:	'[' tGET ',' tIDENT ',' '[' exprList ']' ']'
		| '[' tGET ',' tIDENT ',' '[' ']' ']'
		| '[' tGET ',' tIDENT ']'
;

setStmt:	'[' tSET ',' tIDENT ',' expr ']' { getAttr(node, $6 ,0); }

;

if:		'[' tIF ',' condition ',' '[' stmtlst ']' ']'
		| '[' tIF ',' condition ',' '[' stmtlst ']' '[' stmtlst ']' ']'
;

print:		'[' tPRINT ',' '[' expr ']' ']' { getAttr(node, $5,0); }
;

operation:	'[' tADD ',' expr ',' expr ']' { $$ = esum($4, $6, $2);}
		| '[' tSUB ',' expr ',' expr ']' { $$ = edec($4, $6, $2); }
		| '[' tMUL ',' expr ',' expr ']' { $$ = emlt($4, $6, $2); } 
		| '[' tDIV ',' expr ',' expr ']' { $$ = edv($4, $6, $2); }
;	

unaryOperation: '[' tINC ',' tIDENT ']'
		| '[' tDEC ',' tIDENT ']'
;

expr:		tINT {$$ = cnvi($1);}  
			| tSTRING {$$ = cnvs($1);} 
			| tREAL {$$ = cnvr($1);}
			| getExpr{
						ExprNode ures;
						ures.nodeType = UNDEF;
						$$ = ures;
					} 
			| function {
						ExprNode ures;
						ures.nodeType = UNDEF;
						$$ = ures;
					}
			| operation 
			| condition {
						ExprNode ures;
						ures.nodeType = UNDEF;
						$$ = ures;
					}

function:	 '[' tFUNCTION ',' '[' parametersList ']' ',' '[' stmtlst ']' ']'
		| '[' tFUNCTION ',' '[' ']' ',' '[' stmtlst ']' ']'
;

condition:	'[' tEQUALITY ',' expr ',' expr ']'{ getAttr(node, $4,0); getAttr(node, $6,0);}
						     
		
		| '[' tGT ',' expr ',' expr ']'{   getAttr(node, $4,0); getAttr(node, $6,0);}

		| '[' tLT ',' expr ',' expr ']'{    getAttr(node, $4,0); getAttr(node, $6,0);}
                                                     

		| '[' tGEQ ',' expr ',' expr ']'{    getAttr(node, $4,0); getAttr(node, $6,0);
                                             
                                                     }

		| '[' tLEQ',' expr ',' expr ']'{     getAttr(node, $4,0); getAttr(node, $6,0);
                                                     }


;

returnStmt:	'[' tRETURN ',' expr ']'  { getAttr(node, $4,0);}
		| '[' tRETURN ']'
;

parametersList: parametersList ',' tIDENT | tIDENT
;

exprList:	exprList ',' expr {    getAttr(node, $3,0);}
		| expr { getAttr(node, $1,0);}
;

%%

Attr * cr_node(){
	Attr *head = malloc(sizeof(Attr));
	ExprNode cr;
	head->step = NULL;
	head->exprNode = cr;
	return head;
}

void getAttr (struct Attr *head, ExprNode node, int err_or_not){
	Attr *ptr = malloc(sizeof(head));
	Attr *q = malloc(sizeof(head));
	ptr->exprNode = node;
	ptr->curlin = node.curlin;
	ptr->err = err_or_not;
	q = head;
	while (q->step != NULL){
		q = q->step;
	}
	q->step = ptr;
}

void pfunc(struct Attr *head){
	while (head != NULL){
		ExprNode node = head->exprNode;
		int curlin = head->curlin;
		int err_or_not = head->err;

		if (node.newn == 1 && err_or_not == 0){

			if(node.nodeType == REAL){

				double rreal = node.realNode.real_value;
				if(rreal < 0){
					double rrreal;
					rrreal = (int)(rreal *10 - 0.5);
					rreal = (double) rrreal /10;
				}
				else{
					double rrreal;
					rrreal = (int)(rreal *10 + 0.5);
					rreal = (double) rrreal /10;
				}
                printf("Result of expression on %d is (%.1f)\n", curlin, rreal);
			}
			else if(node.nodeType == INT){
				int number = node.intNode.int_value;
                printf("Result of expression on %d is (%d)\n", curlin, number);
			}
			else if(node.nodeType == STRING){
				printf("Result of expression on %d is (%s)\n", curlin, node.string_value);
			}
		}

		if (node.newn == 1 && err_or_not == 1){

			if(node.nodeType == REAL){

				double rreal = node.realNode.real_value;
				if(rreal < 0){
					double rrreal;
					rrreal = (int)(rreal *10 - 0.5);
					rreal = (double) rrreal /10;
				}
				else{
					double rrreal;
					rrreal = (int)(rreal *10 + 0.5);
					rreal = (double) rrreal /10;
				}
                printf("Type mismatch on %d\n", curlin);
                printf("Result of expression on %d is (%.1f)\n", curlin, rreal);
			}
			else if(node.nodeType == INT){
				int number = node.intNode.int_value;
                printf("Type mismatch on %d\n", curlin);
                printf("Result of expression on %d is (%d)\n", curlin, number);
			}
			else if(node.nodeType == STRING){
                printf("Type mismatch on %d\n", curlin);
				printf("Result of expression on %d is (%s)\n", curlin, node.string_value);
			}
		}
        
 	
		else if (err_or_not == 1){
			printf("Type mismatch on %d\n", curlin);
		}		
		head = head->step;
	}
}



char *unitee(const char *lstr, const char *rstr) {
	const size_t s1 = strlen(lstr);
	const size_t s2 = strlen(rstr);
	char *res = malloc(s1+s2+1);
   	memcpy(res, lstr, s1);
    memcpy(res+s1, rstr, s2+1);
    return res;
}


ExprNode cnvs(char * mys) {
	ExprNode sres;
    sres.nodeType = STRING;
	sres.string_value = mys;
    sres.newn = 0;
	return sres;
}

ExprNode cnvi(IntNode myi){
	ExprNode ires;
    ires.nodeType = INT;		
	ires.intNode.int_value = myi.int_value;
	ires.newn = 0;
	return ires;
}


ExprNode cnvr(RealNode myr) {
	ExprNode rres;
    rres.nodeType = REAL;		
	rres.realNode.real_value = myr.real_value;
	rres.newn = 0;
	return rres;
}


ExprNode esum(ExprNode left, ExprNode right, int curlin) {
	if( left.nodeType == REAL && right.nodeType == REAL){
			RealNode realNode;
			realNode.real_value = left.realNode.real_value + right.realNode.real_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
			}
	else if(left.nodeType == REAL && right.nodeType == INT){
			RealNode realNode;
			realNode.real_value = left.realNode.real_value + right.intNode.int_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == INT && right.nodeType == REAL){
			RealNode realNode;
			realNode.real_value = left.intNode.int_value + right.realNode.real_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == INT && right.nodeType == INT){
			IntNode intNode;
			intNode.int_value = left.intNode.int_value + right.intNode.int_value;
			ExprNode resnum;
			resnum.nodeType = INT;
			resnum.intNode.int_value = intNode.int_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == STRING && right.nodeType == STRING){	

		char *string_value = unitee(left.string_value, right.string_value);
		ExprNode resstr;
		resstr.nodeType = STRING;
		resstr.string_value = string_value;
		resstr.newn = 1;
		resstr.curlin = curlin;
		return resstr;
	}
	else if((left.nodeType == STRING && right.nodeType == INT) || (left.nodeType == STRING && right.nodeType == REAL) || (left.nodeType == INT && right.nodeType == STRING) || (left.nodeType == REAL && right.nodeType == STRING)){
		ExprNode resns;
		resns.curlin = curlin;
		getAttr(node, resns, 1);
		ExprNode resns2;
		resns2.nodeType = UNDEF;
		return resns2;
	}
	else{
		ExprNode resns2;
		resns2.nodeType = UNDEF;
		return resns2;
	}
	
}


ExprNode edec(ExprNode left, ExprNode right, int curlin) {
	if( left.nodeType == REAL && right.nodeType == REAL){
			RealNode realNode;
			realNode.real_value = left.realNode.real_value - right.realNode.real_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
			}
	else if(left.nodeType == REAL && right.nodeType == INT){
			RealNode realNode;
			realNode.real_value = left.realNode.real_value - right.intNode.int_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == INT && right.nodeType == REAL){
			RealNode realNode;
			realNode.real_value = left.intNode.int_value - right.realNode.real_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == INT && right.nodeType == INT){
			IntNode intNode;
			intNode.int_value = left.intNode.int_value - right.intNode.int_value;
			ExprNode resnum;
			resnum.nodeType = INT;
			resnum.intNode.int_value = intNode.int_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if((left.nodeType == STRING && right.nodeType == INT) || (left.nodeType == STRING && right.nodeType == REAL) || (left.nodeType == INT && right.nodeType == STRING) || (left.nodeType == REAL && right.nodeType == STRING) || (left.nodeType == STRING && right.nodeType == STRING)){
		ExprNode resns;
		resns.curlin = curlin;
		getAttr(node, resns, 1);
		ExprNode resns2;
		resns2.nodeType = UNDEF;
		return resns2;
	}
	else{
		ExprNode resns2;
		resns2.nodeType = UNDEF;
		return resns2;
	}
         
}


ExprNode emlt(ExprNode left, ExprNode right, int curlin) {
	if( left.nodeType == REAL && right.nodeType == REAL){
			RealNode realNode;
			realNode.real_value = left.realNode.real_value * right.realNode.real_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
			}
	else if(left.nodeType == REAL && right.nodeType == INT){
			RealNode realNode;
			realNode.real_value = left.realNode.real_value * right.intNode.int_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == INT && right.nodeType == REAL){
			RealNode realNode;
			realNode.real_value = left.intNode.int_value * right.realNode.real_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == INT && right.nodeType == INT){
			IntNode intNode;
			intNode.int_value = left.intNode.int_value * right.intNode.int_value;
			ExprNode resnum;
			resnum.nodeType = INT;
			resnum.intNode.int_value = intNode.int_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == INT && right.nodeType == STRING){
		if(left.intNode.int_value == 0){
			char * nullstr = "";
			ExprNode resstr;
			resstr.nodeType = STRING;
			resstr.string_value = nullstr;
			resstr.newn = 1;
			resstr.curlin = curlin;
			return resstr;
		}
		else if(left.intNode.int_value > 0){
			int mulnum;
			mulnum = left.intNode.int_value;
			char *resm = malloc(mulnum * strlen(right.string_value));
			memcpy(resm, right.string_value, strlen(right.string_value));
			while (mulnum > 1) { 
				resm = unitee(resm, right.string_value);	
				mulnum = mulnum -1;
			}
			ExprNode resstr;
			resstr.nodeType = STRING;
			resstr.string_value = resm;
			resstr.newn = 1;
			resstr.curlin = curlin;
			return resstr;
		}
		else{
			ExprNode resns;
			resns.curlin = curlin;
			getAttr(node, resns, 1);
			ExprNode resns2;
			resns2.nodeType = UNDEF;
			return resns2;
		}
	}
	else if((left.nodeType == STRING && right.nodeType == STRING) || (left.nodeType == STRING && right.nodeType == REAL) || (left.nodeType == STRING && right.nodeType == INT) || (left.nodeType == REAL && right.nodeType == STRING)){
			ExprNode resns;
			resns.curlin = curlin;
			getAttr(node, resns, 1);
			ExprNode resns2;
			resns2.nodeType = UNDEF;
			return resns2;
	}
	else{
		ExprNode resns2;
		resns2.nodeType = UNDEF;
		return resns2;
	}
}


ExprNode edv(ExprNode left, ExprNode right, int curlin) {
	if( left.nodeType == REAL && right.nodeType == REAL){
			RealNode realNode;
			realNode.real_value = left.realNode.real_value / right.realNode.real_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
			}
	else if(left.nodeType == REAL && right.nodeType == INT){
			RealNode realNode;
			realNode.real_value = left.realNode.real_value / right.intNode.int_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == INT && right.nodeType == REAL){
			RealNode realNode;
			realNode.real_value = left.intNode.int_value / right.realNode.real_value;
			ExprNode resnum;
			resnum.nodeType = REAL;
			resnum.realNode.real_value = realNode.real_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if(left.nodeType == INT && right.nodeType == INT){
			IntNode intNode;
			intNode.int_value = left.intNode.int_value / right.intNode.int_value;
			ExprNode resnum;
			resnum.nodeType = INT;
			resnum.intNode.int_value = intNode.int_value;
			resnum.newn = 1;
			resnum.curlin = curlin;
			return resnum;
	}
	else if((left.nodeType == STRING && right.nodeType == STRING) || (left.nodeType == STRING && right.nodeType == REAL) || (left.nodeType == STRING && right.nodeType == INT) || (left.nodeType == REAL && right.nodeType == STRING) || (left.nodeType == INT && right.nodeType == STRING)){
			ExprNode resns;
			resns.curlin = curlin;
			getAttr(node, resns, 1);
			ExprNode resns2;
			resns2.nodeType = UNDEF;
			return resns2;
	}

	else{
		ExprNode resns2;
		resns2.nodeType = UNDEF;
		return resns2;
	}
}

int main ()
{
node = cr_node();
if (yyparse()) {
printf("ERROR\n");
return 1;
}
else {
pfunc(node);
return 0;
}
}
