grammar BMC;



POW	:	('^'|'**');
SPECIAL	:	'%'+ ~('%'|' '|'\t'|'\r'|'\n')* '%';

COMMENT
    :   '#' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    |   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
    ;

WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) {$channel=HIDDEN;}
    ;

DOUBLE
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
    |   ('0'..'9')+ EXPONENT
    ;

NAME:	('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'.')*
    ;



fragment
EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;


input: var_stmt? data_stmt? model_stmt?
;

var_stmt: 'var' dec_list ';'?
;

dec_list: node_dec (',' node_dec)*
;

node_dec: NAME ( '[' dim_list ']' )?
;

dim_list: expression (',' expression)*
;

data_stmt: 'data' '{' relation_list '}'
;

model_stmt: 'model' '{' relation_list '}'
;
 
relations: '{' relation_list '}'
;

relation_list:	(relation)+
;

relation: for_loop
| ( var '~' )=> stoch_relation ';'?
| determ_relation ';'?
;

for_loop: counter relations
;

counter: 'for' '(' NAME 'in' range_element ')'
;

determ_relation: var '<-' expression
| NAME '(' var ')' '<-' expression
;

stoch_relation:	var '~' distribution (truncated | interval)?
;
/*
product: expression ('*' expression)+
;

sum: expression ('+' expression)+
;

neg: '-' expression
;

expression
: 
(
    var 
  | DOUBLE 
  | 'length' '(' var ')' 
  | 'dim' '(' var ')' 
  | NAME '(' expression_list ')' 
  | product 
  | sum 
  | neg 
  | '(' expression ')'
) 
(
    '/' expression 
  | '-' expression 
  | '>' expression 
  | '>=' expression 
  | '<' expression 
  | '<=' expression 
  | '==' expression 
  | '!=' expression 
  | '&&' expression 
  | '||' expression 
  | '^' expression 
  | SPECIAL expression
)*
;
*/
expression
	:	and_expression ('||' and_expression)*
	;

and_expression
	:	not_expression ('&&' not_expression)*
	;

not_expression
	:	'!'? comparison_expression
	;

comparison_expression
	:	sum_expression ( ('>'|'>='|'<'|'<='|'=='|'!=') sum_expression)?
	;

sum_expression
	:	product_expression ( ('+'|'-') product_expression)*
	;

product_expression
	:	special_expression ( ('*'|'/') special_expression)*
	;

special_expression
	:	neg_expression (SPECIAL neg_expression)*
	;
	
neg_expression
	:	'-'? power_expression
	;

power_expression
	:	atom ('^' power_expression)? // right-associative via tail recursion
	;

atom	:	var
	|	DOUBLE
	|	'length' '(' var ')'
	|	'dim' '(' var ')'
	|	NAME '(' expression_list ')'
	|	'(' expression ')'
	;
	

expression_list: (expression) (',' expression)*
;

range_list: (range_element) (',' range_element)*
;

range_element: (expression (':' expression)? )?
;

distribution: NAME '(' expression_list? ')'
;

truncated: 'T' '(' expression? ','  expression? ')'
;

interval: 'I' '(' expression? ','  expression? ')'
;

var: NAME ( '[' range_list ']' )?
;
