open Parser BasicIO;

fun createLexerStream (is : instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

fun parse_report file stream lexbuf =
let
  fun errmsg pos1 pos2 msg =
        Location.errMsg (file, stream, lexbuf) (Location.Loc(pos1,pos2)) msg
  val model =
        Parser.Model Lexer.Token lexbuf
        handle
          Parsing.ParseError f =>
            let val pos1 = Lexing.getLexemeStart lexbuf
                val pos2 = Lexing.getLexemeEnd lexbuf
            in
              errmsg pos1 pos2 "Syntax error."
            end
          | Lexer.LexicalError(msg, pos1, pos2) =>
              if pos1 >= 0 andalso pos2 >= 0 then
                errmsg pos1 pos2 ("Lexical error: " ^ msg)
              else
                (Location.errPrompt ("Lexical error: " ^ msg ^ "\n\n");
                raise Fail "Lexical error")
in
  Parsing.clearParser();
  model
end
handle exn => (Parsing.clearParser(); raise exn)

fun parse_model path =
let
  val is     = Nonstdio.open_in_bin path
  val lexbuf = createLexerStream is
  val model  = parse_report path is lexbuf
in
  BasicIO.close_in is;
  model
end
