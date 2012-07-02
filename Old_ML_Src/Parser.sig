local
in
datatype token =
    AND
  | BODY
  | BOOL
  | BOOL_LIT of bool
  | COLON
  | COMMA
  | DIV
  | DIVIDE
  | END
  | EOF
  | EQUALS
  | FOR
  | GREATER
  | GREATEREQ
  | IN
  | INPUTS
  | INT
  | INT_LIT of int
  | LBRACK
  | LCURLY
  | LEFTARROW
  | LESS
  | LESSEQ
  | LPAREN
  | MINUS
  | MOD
  | NAME of string
  | NOT
  | NOTEQUAL
  | OR
  | PLUS
  | POWER
  | RBRACK
  | RCURLY
  | REAL
  | REAL_LIT of AST.real_struct
  | RPAREN
  | SPECIAL of string
  | TILDE
  | TIMES
  | TRUNC
  | VARS
end;

val Model :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> AST.model;
val Expr0 :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> AST.expr;
val Relation0 :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> AST.relation;
