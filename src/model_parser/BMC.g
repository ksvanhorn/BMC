grammar BMC;
options {
	//output=AST;
	output=template;
}

@header {
import org.antlr.stringtemplate.*;
}

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

input: args_section reqs_section vars_section model_section
	-> template(args={$args_section.st},
		reqs={$reqs_section.st},
		vars={$vars_section.st},
		model={$model_section.st}) 
"(:model
<args>
<reqs>
<vars>
<model>
)"
;

args_section: 'args' '{' declaration_list? '}'
-> template(declarations={$declaration_list.st})
"(:args
<declarations>
)"
;

declaration_list: nodes+=node_dec (nodes+=node_dec)*
-> template(nodes={$nodes}) "<nodes; separator=\"\n\">"
;

node_dec: name=NAME ':' type=NAME ( p='[' dim_list ']' )?
-> {p != null}? template(name={$name.text}, type={$type.text}, dim_list={$dim_list.st})
"(<name> (<type> <dim_list>))"
-> template(name={$name.text},type={$type.text}) "(<name> <type>)"
;

dim_list: exps+=expression (',' exps+=expression)*
-> template(exps={$exps}) "<exps; separator=\" \">"
;

reqs_section: 'reqs' '{' requirements_list? '}'
-> template(requirements={$requirements_list.st}) 
"(:reqs 
<requirements>
)"
;

requirements_list: exps+=expression (',' exps+=expression)*
-> template(exps={$exps}) "<exps; separator=\"\n\">"
;

vars_section: 'vars' '{' declaration_list? '}'
-> template(declarations={$declaration_list.st})
"(:vars
<declarations>
)"
;

model_section: 'model' '{' relation_list? '}'
-> template(relations={$relation_list.st})
"(:body
<relations>
)"
;

relations: '{' relation_list '}'
-> template(relation_list={$relation_list.st})
"<relation_list>"
;

relation_list: (rels+=relation)+
-> template(relations={$rels}) "<relations; separator=\"\n\">"
;

relation: for_loop -> template(for_loop={$for_loop.st}) "<for_loop>"
| if_stmt -> template(if_stmt={$if_stmt.st}) "<if_stmt>"
| ( var '~' )=> stoch_relation ';'? -> template(stoch_relation={$stoch_relation.st}) "<stoch_relation>"
| determ_relation ';'? -> template(determ_relation={$determ_relation.st}) "<determ_relation>"
;

for_loop: counter relations
-> template(counter={$counter.st},relation={$relations.st})
"(:for <counter>
<relation>
)"
;

counter: 'for' '(' NAME 'in' range_element ')'
-> template(name={$NAME.text}, range_element={$range_element.st})
"<name> <range_element>"
;

if_stmt: if_condition relations
-> template(if_condition={$if_condition.st}, relation={$relations.st})
"(:if <if_condition>
<relation>
)"
;

if_condition: 'if' '(' expression ')'
-> template(expression={$expression.st})
"<expression>"
;

determ_relation: var '<-' expression -> template(var={$var.st}, expression={$expression.st}) "(\<- <var> <expression>)"
| NAME '(' var ')' '<-' expression
;

stoch_relation:	var '~' distribution (truncated_=truncated | interval_=interval)?
-> {truncated_ != null}? template(var={$var.st}, distribution={$distribution.st}, truncated={$truncated_.st})
"(~ <var> <distribution> <truncated>)"
-> {interval_ != null}? template(var={$var.st}, distribution={$distribution.st}, interval={$interval_.st})
"(~ <var> <distribution> <interval>)"
-> template(var={$var.st}, distribution={$distribution.st})
"(~ <var> <distribution>)"
;

expression
	:	(a=and_expression -> {$a.st})
		('||' b=and_expression -> template(and_left={$expression.st}, and_right={$b.st}) "(|| <and_left> <and_right>)"
		)*
	;

and_expression
	:	(a=not_expression -> {$a.st})
		('&&' b=not_expression -> template(or_left={$and_expression.st}, or_right={$b.st}) "(&& <or_left> <or_right>)"
		)*
	;

not_expression
	:	ex='!'? comparison_expression
-> {ex != null}? template(comparison_expression={$comparison_expression.st}) "(! <comparison_expression>)"
-> {$comparison_expression.st}
	;

comparison_expression
	:	a=range_expression
		(	
			(  '>'  b=range_expression -> template(gt_left={$a.st}, gt_right={$b.st}) "(\> <gt_left> <gt_right>)"
			| '>=' b=range_expression -> template(ge_left={$a.st}, ge_right={$b.st}) "(\>= <ge_left> <ge_right>)"
			| '<'  b=range_expression -> template(lt_left={$a.st}, lt_right={$b.st}) "(\< <lt_left> <lt_right>)"
			| '<=' b=range_expression -> template(le_left={$a.st}, le_right={$b.st}) "(\<= <le_left> <le_right>)"
			| '==' b=range_expression -> template(eq_left={$a.st}, eq_right={$b.st}) "(== <eq_left> <eq_right>)"
			| '!=' b=range_expression -> template(ne_left={$a.st}, ne_right={$b.st}) "(!= <ne_left> <ne_right>)"
			)
		|	-> {$a.st}
		)
	;

range_expression
	:	a=sum_expression
		(
			(':') => (':' b=sum_expression -> template(rng_left={$a.st}, rng_right={$b.st}) "(<rng_left> <rng_right>)"
			)
		|	-> {$a.st}
		)
	;

sum_expression
	:	(a=product_expression -> {$a.st})
		( 
			 '+' b=product_expression -> template(plus_left={$sum_expression.st}, plus_right={$b.st}) "(+ <plus_left> <plus_right>)"
			|'-' b=product_expression -> template(minus_left={$sum_expression.st}, minus_right={$b.st}) "(- <minus_left> <minus_right>)"
		)*
	;

product_expression
	:	(a=special_expression -> {$a.st})
		( 
			'*' b=special_expression -> template(times_left={$product_expression.st}, times_right={$b.st}) "(* <times_left> <times_right>)"
			| ('/' b=special_expression -> template(divide_left={$product_expression.st}, divide_right={$b.st}) "(/ <divide_left> <divide_right>)")
		)*
	;

special_expression
	:	(a=neg_expression -> {$a.st})
		(SPECIAL neg_expression)*
	;
	
neg_expression
	:	neg='-'? power_expression
-> {neg != null}? template(power_expression={$power_expression.st}) "(- <power_expression>)"
-> {$power_expression.st}
	;

power_expression
	:	a=atom
		(('^' b=power_expression -> template(pow_left={$a.st}, pow_right={$b.st}) "(^ <pow_left> <pow_right>" )
		| -> {$a.st}
		) // right-associative via tail recursion
	;

atom	:	var -> template(value={$var.st}) "<value>"
	|	DOUBLE -> template(value={$DOUBLE.text}) "<value>"
	|	INT -> template(value={$INT.text}) "<value>"
	|	'length' '(' var ')' -> template(value={$var.st}) "(:length <value>)"
	|	'dim' '(' var ')' -> template(value={$var.st}) "(:dim <value>)"
	|	( NAME '(' )=> NAME '(' expression_list ')' -> template(name={$NAME.text.toLowerCase()},exp_list={$expression_list.st}) "(<name> <exp_list>)"
	|	'(' expression ')' -> template(exp={$expression.st}) "<exp>"
	;
	

expression_list: (exps+=expression) (',' exps+=expression)*
-> template(exps={$exps}) "<exps; separator=\" \">"
;

range_list: (range_elements+=range_element) (',' range_elements+=range_element)*
-> template(range_elements={$range_elements}) "<range_elements; separator=\" \">"
;

range_element: (expressions+=expression (':' expressions+=expression)? )?
-> {expressions != null}? template(expressions={$expressions}) "<expressions; separator=\" \">"
-> template() ":all"
;

distribution: NAME '(' expressions=expression_list? ')'
-> {expressions != null}? template(name={$NAME.text}, exps_list={$expressions.st})
"(<name> <exps_list>)"
-> template(name={$NAME.text})
"(<name>)"
;

truncated: 'T' '(' expression? ','  expression? ')'
;

interval: 'I' '(' expression? ','  expression? ')'
;

var: NAME ( op='[' range_list ']' )?
-> {op != null}? template(name={$NAME.text}, range_list={$range_list.st}) "(@ <name> <range_list>)"
-> template(name={$NAME.text}) "<name>"
;


