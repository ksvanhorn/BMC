grammar BMC;


@members {
	public String getErrorMessage(RecognitionException e,
			String[] tokenNames)
	{
		List stack = getRuleInvocationStack(e, this.getClass().getName());
		String msg = null;
		if ( e instanceof NoViableAltException ) {
			NoViableAltException nvae = (NoViableAltException)e;
			msg = " no viable alt; token="+e.token+
				" (decision="+nvae.decisionNumber+
				" state "+nvae.stateNumber+")"+
				" decision=<<"+nvae.grammarDecisionDescription+">>";
		}
		else {
			msg = super.getErrorMessage(e, tokenNames);
		}
		return stack+" "+msg;
	}
	public String getTokenErrorDisplay(Token t) {
		return t.toString();
	}
}

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
    :   DIGIT+ '.' DIGIT* EXPONENT?
    |   '.' DIGIT+ EXPONENT?
    |   DIGIT+ EXPONENT
    ;

INT: 	DIGIT+
    ;

NAME:	('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'.')*
    ;



fragment EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;
fragment DIGIT : ('0'..'9') ;


input: {System.out.print("(:model"); } var_stmt? data_stmt? model_stmt? {System.out.print("\n)\n");}
;

var_stmt: 'var' {System.out.print("\n  (:vars");} dec_list ';'? {System.out.print("\n  )");}
;

dec_list: node_dec (',' node_dec)*
;

node_dec: type=NAME name=NAME {System.out.print("\n    (" + $name.text + " (" + $type.text);} ( '[' dim_list ']' )? {System.out.print("))");}
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
	:	exps += product_expression ( ('+'|'-') exps += product_expression)*
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

atom	:	var { System.out.print(" " + $var.text); }
	|	DOUBLE { System.out.print(" " + $DOUBLE.text); }
	|	INT { System.out.print(" " + $INT.text); }
	|	'length' '(' var ')'
	|	'dim' '(' var ')'
	|	( NAME '(' )=> NAME '(' expression_list ')'
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


