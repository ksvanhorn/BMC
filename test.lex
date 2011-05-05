
datatype lexresult =
  Identifier of string
| NumLiteral of int
| RealLiteral of real
| LeftParen | RightParen | LeftSquare | RightSquare | LeftCurly | RightCurly
| Comma | SemiColon
| Equals | LeftArrow | Tilde | Eof
| IllegalChar

fun eof _ = Eof

val linenum = ref 1

%%

alpha=[A-Za-z];
digit=[0-9];
alnum=[A-Za-z0-9_];
symbol=[!@#%z^&*+=|\\:<>?/-];
ws=[\ \t];
eol=\r|\r|\r\n;

%%

{eol}    => (linenum := !linenum + 1; continue());
{ws}+    => (continue());
"("      => (LeftParen);
")"      => (RightParen);
"["      => (LeftSquare);
"]"      => (RightSquare);
"{"      => (LeftCurly);
"}"      => (RightCurly);
","      => (Comma);
";"      => (SemiColon);
"<-"     => (LeftArrow);
"~"      => (Tilde);
{alpha}{alnum}*|{symbol}+
         => (Identifier yytext);
{digit}+ => (NumLiteral (valOf (Int.fromString yytext)));
{digit}+'.'{digit}*((e|E)[+-]?{digit}+)?
         => (RealLiteral (valOf (Real.fromString yytext)));
_        => (IllegalChar);
