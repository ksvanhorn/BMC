{
open Lexing Parser String Char;

exception LexicalError of string * int * int (* (message, loc1, loc2) *)

fun lexerError lexbuf s = 
    raise LexicalError (s, getLexemeStart lexbuf, getLexemeEnd lexbuf)

fun keyword s =
  case s of
    "inputs" => INPUTS
  | "vars"   => VARS
  | "body"   => BODY
  | "for"    => FOR
  | "in"     => IN
  | "real"   => REAL
  | "int"    => INT
  | "bool"   => BOOL
  | "true"   => BOOL_LIT true
  | "false"  => BOOL_LIT false
  | "div"    => DIV
  | "mod"    => MOD
  | "T"      => TRUNC
  | _        => NAME s

fun int s = getOpt(Int.fromString s, 0)

fun realStructFromString s =
  let 
    fun is_dot c = (c = #".")
    fun exp_char c = (c = #"e" orelse c = #"E")
    fun has_dot s = List.exists is_dot (explode s)
    val mant_exp = fields exp_char s
    val [mant,exp] =
      if length(mant_exp) = 1 then mant_exp @ ["0"] else mant_exp
    val [int_part, frac_part] =
      if has_dot mant
        then fields is_dot mant
        else [mant, ""]
    val e = int exp - size frac_part
    val m = int (int_part ^ frac_part)
  in
    { mantissa=m, exponent=e }
  end
}

let WHITESPACE = [` ` `\t` `\n` `\r`]
let RCOMMENT = `#`[^`\r` `\n`]*[`\r` `\n`]
let CCOMMENT = "/*" ([^`*`]* `*`)* [^`*`]* "*/"
let EXPONENT = [`e` `E`][`+` `-`]?[`0`-`9`]+
let DIGITS = [`0`-`9`]+
let FLOAT = (DIGITS EXPONENT |
             (DIGITS "." [`0`-`9`]* | "." DIGITS) EXPONENT?)
rule Token = parse
  WHITESPACE+    { Token lexbuf }
| RCOMMENT       { Token lexbuf }
| CCOMMENT       { Token lexbuf }
| [`a`-`z` `A`-`Z`][`a`-`z` `A`-`Z` `0`-`9` `.` `_`]*
                 { keyword (getLexeme lexbuf) }
| DIGITS         { INT_LIT(getOpt(Int.fromString (getLexeme lexbuf), 0)) }
| FLOAT          { REAL_LIT(realStructFromString (getLexeme lexbuf)) }
| `%`+ [^`%` ` ` `\t` `\r` `\n`]* `%` 
                 { SPECIAL (getLexeme lexbuf) }
| `~`  { TILDE }
| "<-" { LEFTARROW }
| `,`  { COMMA }
| `:`  { COLON }
| `;`  { END }
| eof  { EOF }
| `[`  { LBRACK }
| `]`  { RBRACK }
| `{`  { LCURLY }
| `}`  { RCURLY }
| `(`  { LPAREN }
| `)`  { RPAREN }
| "||" { OR }
| "&&" { AND }
| `!`  { NOT }
| "=" { EQUALS }
| "!=" { NOTEQUAL }
| "<=" { LESSEQ }
| "<"  { LESS }
| ">=" { GREATEREQ }
| ">"  { GREATER }
| "+"  { PLUS }
| "-"  { MINUS }
| "*"  { TIMES }
| "/"  { DIVIDE }
| "^"  { POWER }
| "**" { POWER }
| _    { raise lexerError lexbuf "Illegal character" }
;
