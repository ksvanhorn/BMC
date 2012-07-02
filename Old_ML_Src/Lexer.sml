local open Obj Lexing in


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

fun action_34 lexbuf = (
 raise lexerError lexbuf "Illegal character" )
and action_33 lexbuf = (
 POWER )
and action_32 lexbuf = (
 POWER )
and action_31 lexbuf = (
 DIVIDE )
and action_30 lexbuf = (
 TIMES )
and action_29 lexbuf = (
 MINUS )
and action_28 lexbuf = (
 PLUS )
and action_27 lexbuf = (
 GREATER )
and action_26 lexbuf = (
 GREATEREQ )
and action_25 lexbuf = (
 LESS )
and action_24 lexbuf = (
 LESSEQ )
and action_23 lexbuf = (
 NOTEQUAL )
and action_22 lexbuf = (
 EQUALS )
and action_21 lexbuf = (
 NOT )
and action_20 lexbuf = (
 AND )
and action_19 lexbuf = (
 OR )
and action_18 lexbuf = (
 RPAREN )
and action_17 lexbuf = (
 LPAREN )
and action_16 lexbuf = (
 RCURLY )
and action_15 lexbuf = (
 LCURLY )
and action_14 lexbuf = (
 RBRACK )
and action_13 lexbuf = (
 LBRACK )
and action_12 lexbuf = (
 EOF )
and action_11 lexbuf = (
 END )
and action_10 lexbuf = (
 COLON )
and action_9 lexbuf = (
 COMMA )
and action_8 lexbuf = (
 LEFTARROW )
and action_7 lexbuf = (
 TILDE )
and action_6 lexbuf = (
 SPECIAL (getLexeme lexbuf) )
and action_5 lexbuf = (
 REAL_LIT(realStructFromString (getLexeme lexbuf)) )
and action_4 lexbuf = (
 INT_LIT(getOpt(Int.fromString (getLexeme lexbuf), 0)) )
and action_3 lexbuf = (
 keyword (getLexeme lexbuf) )
and action_2 lexbuf = (
 Token lexbuf )
and action_1 lexbuf = (
 Token lexbuf )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_22 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_22 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_16 lexbuf
 else case currChar of
    #"\n" => state_3 lexbuf
 |  #"\t" => state_3 lexbuf
 |  #"\r" => state_3 lexbuf
 |  #" " => state_3 lexbuf
 |  #"~" => action_7 lexbuf
 |  #"}" => action_16 lexbuf
 |  #"|" => state_27 lexbuf
 |  #"{" => action_15 lexbuf
 |  #"^" => action_32 lexbuf
 |  #"]" => action_14 lexbuf
 |  #"[" => action_13 lexbuf
 |  #">" => state_21 lexbuf
 |  #"=" => action_22 lexbuf
 |  #"<" => state_19 lexbuf
 |  #";" => action_11 lexbuf
 |  #":" => action_10 lexbuf
 |  #"/" => state_15 lexbuf
 |  #"." => state_14 lexbuf
 |  #"-" => action_29 lexbuf
 |  #"," => action_9 lexbuf
 |  #"+" => action_28 lexbuf
 |  #"*" => state_10 lexbuf
 |  #")" => action_18 lexbuf
 |  #"(" => action_17 lexbuf
 |  #"&" => state_7 lexbuf
 |  #"%" => state_6 lexbuf
 |  #"#" => state_5 lexbuf
 |  #"!" => state_4 lexbuf
 |  #"\^@" => action_12 lexbuf
 |  _ => action_34 lexbuf
 end)
and state_3 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => state_55 lexbuf
 |  #"\t" => state_55 lexbuf
 |  #"\r" => state_55 lexbuf
 |  #" " => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_4 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_21);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_23 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_5 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_34);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_1 lexbuf
 |  #"\r" => action_1 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_52 lexbuf
 end)
and state_6 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_34);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  #"\t" => backtrack lexbuf
 |  #"\r" => backtrack lexbuf
 |  #" " => backtrack lexbuf
 |  #"%" => state_50 lexbuf
 |  _ => state_49 lexbuf
 end)
and state_7 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_34);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"&" => action_20 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_10 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_30);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => action_33 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_34);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_46 lexbuf
 else backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_31);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => state_43 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_4);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_36 lexbuf
 else case currChar of
    #"E" => state_37 lexbuf
 |  #"e" => state_37 lexbuf
 |  #"." => state_35 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_24 lexbuf
 |  #"-" => action_8 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_21 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_26 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_22 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_3);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_31 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_31 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_31 lexbuf
 else case currChar of
    #"." => state_31 lexbuf
 |  #"_" => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_34);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"|" => action_19 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_31 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_3);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_31 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_31 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_31 lexbuf
 else case currChar of
    #"." => state_31 lexbuf
 |  #"_" => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_35 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_35 lexbuf
 else case currChar of
    #"E" => state_40 lexbuf
 |  #"e" => state_40 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_36 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_4);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_36 lexbuf
 else case currChar of
    #"E" => state_37 lexbuf
 |  #"e" => state_37 lexbuf
 |  #"." => state_35 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_37 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_39 lexbuf
 else case currChar of
    #"+" => state_38 lexbuf
 |  #"-" => state_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_38 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_39 lexbuf
 else backtrack lexbuf
 end)
and state_39 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_39 lexbuf
 else backtrack lexbuf
 end)
and state_40 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_42 lexbuf
 else case currChar of
    #"+" => state_41 lexbuf
 |  #"-" => state_41 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_41 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_42 lexbuf
 else backtrack lexbuf
 end)
and state_42 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_42 lexbuf
 else backtrack lexbuf
 end)
and state_43 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => state_44 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_43 lexbuf
 end)
and state_44 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_45 lexbuf
 |  #"*" => state_44 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_43 lexbuf
 end)
and state_45 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_2);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => state_44 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_43 lexbuf
 end)
and state_46 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_46 lexbuf
 else case currChar of
    #"E" => state_40 lexbuf
 |  #"e" => state_40 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_49 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  #"\t" => backtrack lexbuf
 |  #"\r" => backtrack lexbuf
 |  #" " => backtrack lexbuf
 |  #"%" => action_6 lexbuf
 |  _ => state_49 lexbuf
 end)
and state_50 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  #"\t" => backtrack lexbuf
 |  #"\r" => backtrack lexbuf
 |  #" " => backtrack lexbuf
 |  #"%" => state_50 lexbuf
 |  _ => state_49 lexbuf
 end)
and state_52 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_1 lexbuf
 |  #"\r" => action_1 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_52 lexbuf
 end)
and state_55 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => state_55 lexbuf
 |  #"\t" => state_55 lexbuf
 |  #"\r" => state_55 lexbuf
 |  #" " => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_34, action_33, action_32, action_31, action_30, action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
