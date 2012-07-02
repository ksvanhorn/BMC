open Lexer Parser AST Lexing;

exception FailedAssert of string

local
  val prefixes = ref ([] : string list)
  fun push_pfx s = (prefixes := s :: !prefixes)
  fun pop_pfx () = (prefixes := tl (!prefixes))
  fun add_prefixes [] s = s
    | add_prefixes (s1 :: rest) s = add_prefixes rest (s1 ^ ": " ^ s)
  fun fullmsg s = add_prefixes (!prefixes) s
in
  fun assert(b,msg) =
    if not b then
      raise(FailedAssert(fullmsg msg))
    else ()
  fun runtest f name =
    (push_pfx name; f(); pop_pfx())
end

fun lexall s =
  let val lexbuf = createLexerString s
      fun f () =
        let val tok = Token lexbuf
        in
          if tok = EOF then [tok] else tok :: f()
        end
  in f() end

fun punct_and_ops_test () =
  let
    val inp = "<-,~**^/ *-+>!<||>=&&<=(=)!={;}[:]div mod %% %*% %foo%"
    val toks = lexall inp
    val expected = 
      [LEFTARROW, COMMA, TILDE, POWER, POWER, DIVIDE, TIMES, MINUS, PLUS, GREATER,
       NOT, LESS, OR, GREATEREQ, AND, LESSEQ, LPAREN, EQUALS, RPAREN, NOTEQUAL,
       LCURLY, END, RCURLY, LBRACK, COLON, RBRACK, DIV, MOD, 
       SPECIAL "%%", SPECIAL "%*%", SPECIAL "%foo%", EOF]
  in
    assert(toks = expected, "Lexing of punctuation and operators")
  end

fun keywords_test () =
  let
    val inp = "T true false bool int real in for body vars inputs"
    val toks = lexall inp
    val expected =
      [TRUNC, BOOL_LIT true, BOOL_LIT false, BOOL, INT, REAL, IN, FOR,
       BODY, VARS, INPUTS, EOF]
  in
    assert(toks = expected, "Lexing of keywords")
  end

fun names_test () =
  let
    val inp = "foo up_down a7 Some.thing"
    val toks = lexall inp
    val expected = 
      [NAME "foo", NAME "up_down", NAME "a7", NAME "Some.thing", EOF]
  in
    assert(toks = expected, "Lexing of names")
  end

fun numbers_test () =
  let
    val inp = "37 058 2.45 67. .351 967e18 6.82e-5 .74e+245 85.E53 " ^
              "381E3 4.921E-43 .128E+82"
    val toks = lexall inp
    val expected =
      [INT_LIT 37, INT_LIT 58, REAL_LIT {mantissa=245, exponent= ~2},
       REAL_LIT {mantissa=67, exponent=0},
       REAL_LIT {mantissa=351, exponent= ~3},
       REAL_LIT {mantissa=967, exponent=18},
       REAL_LIT {mantissa=682, exponent= ~7},
       REAL_LIT {mantissa=74, exponent=243},
       REAL_LIT {mantissa=85, exponent=53},
       REAL_LIT {mantissa=381, exponent=3},
       REAL_LIT {mantissa=4921, exponent= ~46},
       REAL_LIT {mantissa=128, exponent=79},
       EOF]
  in
    assert(toks = expected, "Lexing of numbers")
  end

fun whitespace_test () =
  let
    val inp = "in # this is a comment\n\
              \for \t# this is another comment\r\n\
              \,# and yet another\r\
              \:/* here * is * another * comment */"
    val toks = lexall inp
    val expected = [IN, FOR, COMMA, COLON, EOF]
  in
    assert(toks = expected, "Lexing of comments")
  end

(*
fun cmpexp (e1 as ApplyFunc(f1,a1), e2 as ApplyFunc(f2,a2)) =
    if f1 = f2 then
      if length a1 = length a2 then
        ListPair.all cmpexp (a1,a2)
      else
        (printVal(e1,e2); false)
    else
      (printVal(e1,e2); false)
|   cmpexp (e1 as VarExpr a1, e2 as VarExpr a2) =
    if a1 = a2 then true else (printVal(e1, e2); false)
|   cmpexp (e1, e2) =
    (printVal(e1, e2); false)
*)

fun parseexpr s = Parser.Expr0 Token (createLexerString s)

fun precedence_test() =
let
  val inp =
    "c=d && c != d || c<=d && c >=d || !b && c < d && c > d || \
    \!bar(x[n]) && a = b+c*d/a^b^foo(c) %.% -n - a div b mod c"
  val e = parseexpr inp
  val x = VarExpr(Var("x"))
  val n = VarExpr(Var("n"))
  val a = VarExpr(Var("a"))
  val b = VarExpr(Var("b"))
  val c = VarExpr(Var("c"))
  val d = VarExpr(Var("d"))
  val x_n = VarExpr(ArrayVar("x", [PointRange n]))
  val expected =
    ApplyFunc("OR", [
      ApplyFunc("OR", [
        ApplyFunc("OR", [
          ApplyFunc("AND", [ApplyFunc("=", [c,d]), ApplyFunc("!=", [c,d])]),
          ApplyFunc("AND", [ApplyFunc("<=", [c,d]), ApplyFunc(">=", [c,d])])
        ]),
        ApplyFunc("AND", [
          ApplyFunc("AND", [
            ApplyFunc("NOT", [b]), ApplyFunc("<", [c,d])
          ]),
          ApplyFunc(">", [c,d])
        ])
      ]),
      ApplyFunc("AND", [
        ApplyFunc("NOT", [ApplyFunc("bar", [x_n])]),
        ApplyFunc("=", [
          a,
          ApplyFunc("-", [
            ApplyFunc("+", [
              b,
              ApplyFunc("/", [
                ApplyFunc("*", [c,d]),
                ApplyFunc("%.%", [
                  ApplyFunc("^", [a, ApplyFunc("^", [b, ApplyFunc("foo", [c])])]),
                  ApplyFunc("MINUS", [n])
                ])
              ])
            ]),
            ApplyFunc("MOD", [ApplyFunc("DIV", [a,b]), c])
          ])
        ])
      ])
    ])
in
  assert(e = expected, "Parsing precedences")
end

fun arrayvar_expr_test () =
let
  val inp = "x[3, m+5 : n div 2 * k[], ]"
  val e = parseexpr inp
  val expected =
    VarExpr(
      ArrayVar("x",
        [PointRange(IntLiteral 3),
         Range(ApplyFunc("+", [VarExpr(Var("m")), IntLiteral 5]),
               ApplyFunc("*",
                         [ApplyFunc("DIV", [VarExpr(Var("n")), IntLiteral 2]),
                          VarExpr(ArrayVar("k", [FullRange]))])),
         FullRange]))
in
  assert(e = expected, "Parsing array variables")
end

fun function_app_test() =
let
  val inp = "foo(x, y()+z)"
  val e = parseexpr inp
  val expected =
    ApplyFunc("foo", [
      VarExpr(Var "x"),
      ApplyFunc("+", [ApplyFunc("y", []), VarExpr(Var "z")])])
in
  assert(e = expected, "Parsing function calls")
end

fun parserel s = Parser.Relation0 Token (createLexerString s)
fun v s = VarExpr(Var s)
fun ilit n = IntLiteral n

fun determ_relation_test () =
let
  val inp = "x <- m + n"
  val e = parserel inp
  val expected = 
    DetermRelation(Var "x", ApplyFunc("+", [v "m", v "n"]))
in
  assert(e = expected, "Parsing deterministic relation")
end

fun stoch_relation_test () =
let
  val inp = "y[i,j] ~ dnorm(mu+1, sigma^(-2))"
  val e = parserel inp
  val expected = 
    StochRelation(
      ArrayVar("y", [PointRange(v "i"), PointRange(v "j")]),
      Distr("dnorm", [
        ApplyFunc("+", [v "mu", ilit 1]),
        ApplyFunc("^", [v "sigma", ApplyFunc("MINUS", [ilit 2])])]))
in
  assert(e = expected, "Parsing stochastic relation")
end

fun arrayvar_relation_test () =
let
  val inp = "x[3, m+5 : n div 2 * k[], ] ~ dfoo()"
  val e = parserel inp
  val lhs =
      ArrayVar("x",
        [PointRange(IntLiteral 3),
         Range(ApplyFunc("+", [VarExpr(Var("m")), IntLiteral 5]),
               ApplyFunc("*",
                         [ApplyFunc("DIV", [VarExpr(Var("n")), IntLiteral 2]),
                          VarExpr(ArrayVar("k", [FullRange]))])),
         FullRange])
  val expected = StochRelation(lhs, Distr("dfoo", []))
in
  assert(e = expected, "Parsing array variable on LHS of stochastic relation")
end

fun for_loop_test () =
let
  val inp = "for (i in n^2:y+z) {\n\
            \  for (j in a:b) { w[i,j] <- false }\
            \  x[i] ~ dexp(7);\n\
            \  y[i] <- i+3\n\
            \  z[i] ~ dfoo()\n\
            \}"
  val e = parserel inp
  val rng_i = PointRange(v "i")
  val rng_j = PointRange(v "j")
  val expected =
    ForLoop(
      "i",
      ApplyFunc("^", [v "n", ilit 2]),
      ApplyFunc("+", [v "y", v "z"]),
      [ForLoop("j", v "a", v "b",
         [DetermRelation(
            ArrayVar("w", [rng_i, rng_j]), BoolLiteral false)]),
       StochRelation(
         ArrayVar("x", [rng_i]), Distr("dexp", [ilit 7])),
       DetermRelation(
         ArrayVar("y", [rng_i]), ApplyFunc("+", [v "i", ilit 3])),
       StochRelation(
         ArrayVar("z", [rng_i]), Distr("dfoo", []))])
in
  assert(e = expected, "Parsing for loop")
end

fun trunc_twosided_test () =
let
  val inp = "x ~ dfoo(alpha) T(a+b, 2*c)"
  val e = parserel inp
  val expected =
    StochRelation(Var "x",
      TruncDistr(Distr("dfoo", [v "alpha"]),
        SOME(ApplyFunc("+", [v "a", v "b"])),
        SOME(ApplyFunc("*", [ilit 2, v "c"]))))
in
  assert(e = expected, "Parsing truncated distribution (two-sided)")
end

fun trunc_upperbound_test () =
let
  val inp = "x ~ dfoo(alpha) T(, 2*c)"
  val e = parserel inp
  val expected =
    StochRelation(Var "x",
      TruncDistr(Distr("dfoo", [v "alpha"]),
        NONE,
        SOME(ApplyFunc("*", [ilit 2, v "c"]))))
in
  assert(e = expected, "Parsing truncated distribution (upper bound)")
end

fun trunc_lowerbound_test () =
let
  val inp = "x ~ dfoo(alpha) T(a+b, )"
  val e = parserel inp
  val expected =
    StochRelation(Var "x",
      TruncDistr(Distr("dfoo", [v "alpha"]),
        SOME(ApplyFunc("+", [v "a", v "b"])),
        NONE))
in
  assert(e = expected, "Parsing truncated distribution (lower bound)")
end

fun link_function_test () =
let
  val inp = "logit(y[i]) <- a + b * x[i]"
  val e = parserel inp
  val expected =
    DetermLinkRelation(
      "logit",
      ArrayVar("y", [PointRange (v "i")]),
      ApplyFunc("+", [
        v "a",
        ApplyFunc("*", [
          v "b", VarExpr(ArrayVar("x", [PointRange (v "i")]))])]))
in
  assert(e = expected, "Parsing link function relation")
end

fun model_test() =
let
  val inp =
    "inputs m : int, n : int, x : real[m * n]\n\
    \vars b : bool, y: int[m, n+1]\n\
    \body\n\
    \ b <- (m < n)\n\
    \ y[1:m, n+1] ~ dbar(x[m - 1]);\n\
    \ y[1:m, 1:n] ~ dfoo(37, n)"
  val e = Parser.Model Token (createLexerString inp)
  val expected =
    { inp = [ 
        { name="m", typ=PrimitiveType Int },
        { name="n", typ=PrimitiveType Int },
        { name="x", typ=ArrayType(Real, [ApplyFunc("*", [v "m", v "n"])]) } ],
      var = [
        { name="b", typ=PrimitiveType Bool },
        { name="y",
          typ=ArrayType(Int, [v "m", ApplyFunc("+", [v "n", ilit 1])]) }],
      rel = [
        DetermRelation(Var "b", ApplyFunc("<", [v "m", v "n"])),
        StochRelation(
          ArrayVar("y", [Range(ilit 1, v "m"),
                         PointRange(ApplyFunc("+", [v "n", ilit 1]))]),
          Distr("dbar", [
            VarExpr(ArrayVar("x",
              [PointRange(ApplyFunc("-", [v "m", ilit 1]))]))])),
        StochRelation(
          ArrayVar("y", [Range(ilit 1, v "m"), Range(ilit 1, v "n")]),
          Distr("dfoo", [ilit 37, v "n"]))]
    }
in
  assert(e = expected, "Parsing complete model")
end

val main = (
  punct_and_ops_test();
  keywords_test();
  names_test();
  numbers_test();
  whitespace_test();

  precedence_test();
  arrayvar_expr_test();
  function_app_test();
  determ_relation_test();
  stoch_relation_test();
  arrayvar_relation_test();
  for_loop_test();
  trunc_twosided_test();
  trunc_upperbound_test();
  trunc_lowerbound_test();
  link_function_test();
  model_test();
  ()
)
