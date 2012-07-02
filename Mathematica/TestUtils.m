(* ::Package:: *)

BeginPackage["TestUtils`"]

SetAttributes[assert, HoldAllComplete]
assert[x_, okval_, failedPfx_] :=
  If[x, okval, Throw[failedPfx <> ToString[Unevaluated[x], InputForm]]]
assert[x_] :=
  assert[x, Null, "Assertion failed: "]
assert[x_, name_] :=
  assert[x, Null, "Assertion " <> name <> " failed: "]

SetAttributes[assertEq, HoldAllComplete]
assertEq[x_, y_, testName_] :=
	If[x === y, Null,
		Module[{xstr0 = ToString[Unevaluated[x], InputForm],
				ystr0 = ToString[Unevaluated[y], InputForm],
				xstr1 = ToString[x, InputForm],
				ystr1 = ToString[y, InputForm],
				msg},
			msg = "ERROR (" <> testName <> "): Expected\n" <>
				  "  " <> xstr0 <> " === " <> ystr0 <> ";\n" <>
				  "got LHS === " <> xstr1 <> "\n" <>
				  "and RHS === " <> ystr1;
			Throw[msg]]]		

EndPackage[]


