(* ::Package:: *)

BeginPackage["BMC`"]
  
  model::usage = "Define a joint pdf."

  logDensity::usage =
  	"logDensity[m] gives the log of the joint density for the model m."
  
  fullConditional::usage =
  	"fullCondition[ld,v] gives the full conditional for v in model m
  	if ld is logDensity[m]."
  
  domain::usage =
  	"domain[m] is a list of pairs {v,p}, where v is a symbol (the name
  	of a model variable) and p is a list of properties of variable v,
  	that is, restrictions on possible values of v." 

  Begin["`Private`"]
	model[x___] := Module[{decls, groups, defs},
		decls = Select[{x}, isDecl];
		declgroups = Split[Sort[decls], #1[[1]]===#2[[1]] &];
		multiDeclError /@ Select[declgroups, Length[#] > 1 &];
		defs = Select[{x}, isDef];
		defgroups = Split[Sort[defs], #1[[1]]===#2[[1]] &];
		multiDefError /@ Select[defgroups, Length[#] > 1 &];
		Null]

	isDecl[_] := False
	isDecl[_ \[Colon] _] := True

	isDef[_] := False
	isDef[_ \[Tilde] _] := True
	isDef[_ \[LeftArrow] _] := True

	multiDeclError[grp_] := Module[{v, types, msg},
		v = ToString[varFromDecl[grp[[1]]]];
		types = typeFromDecl /@ grp;
		msg = "Multiple declarations for " <> v  <> ": " <>
			StringJoin[Riffle[ToString[#, InputForm] & /@ types, ", "]];
		Throw[msg]]

	varFromDecl[v_ \[Colon] _] := v
	typeFromDecl[_ \[Colon] typ_] := typ

	multiDefError[grp_] := Module[{v, rhss, msg},
		v = varFromDef[grp[[1]]];
		rhss = rhsFromDef /@ grp;
		msg = "Multiple definitions for " <> v <> ":" <>
			StringJoin["\n  " <> # & /@ rhss];
		Throw[msg]]

	varFromDef[v_ \[Tilde] _] := ToString[v, InputForm]
	varFromDef[v_ \[LeftArrow] _] := ToString[v, InputForm]
	rhsFromDef[_ \[Tilde] rhs_] := "\[Tilde] " <> ToString[rhs, InputForm]
	rhsFromDef[_ \[LeftArrow] rhs_] := "\[LeftArrow] " <> ToString[rhs, InputForm]

  	logDensity[x_] := lpdExpand[decls[x], Log[pd[x]]]

	  fullConditional[lpd_, x_] := Select[lpd, !FreeQ[#,x]&]

	  gibbsUpdate[lfc_, v_] := 
		Module[{result, i, n = Length[gibbsUpdateRules]},
			For[result = Null; i = 1, result === Null && i <= n, ++i,
				result = gibbsUpdateRules[[i]][lfc, v]];
			result]

	  gibbsUpdateRules = {
		normalGibbsUpdate,
		gammaGibbsUpdate
	  }

	  normalGibbsUpdate[lfc_, v_] :=
		Module[{e, c},
			e = Collect[lfc, v];
			c = Cases[{e}, a_ v^2 + b_ v :> {a, b}];
			If [c == { }, Null,
				With[{a = c[[1]][[1]], b = c[[1]][[2]]},
				With[{\[Mu] = Simplify[-b/(2 a)], \[Lambda] = Simplify[-2 a]},
					NormalDistribution[\[Mu], \[Lambda]^(-1/2)]]]]]

	  gammaGibbsUpdate[lfc_, v_] :=
		Module[{e, c},
			e = Collect[lfc, {v, Log[v]}];
			c = Cases[{e}, a_ Log[v] + b_ v :> {a, b}];
			If [c == { }, Null,
				With[{a = c[[1]][[1]], b = c[[1]][[2]]},
				With[{\[Alpha] = Simplify[a + 1], \[Theta] = Simplify[1/b]},
					GammaDistribution[\[Alpha], \[Theta]]]]]]			
  
  (*
  	decls[model[x___]] := Select[{x}, isDecl]
  *)
  	notDecl[x_] := \[Not]isDecl[x]
  	pd[v_ \[Tilde] distr_] := PDF[distr, v]
	  pd[{}] := 1
	  pd[{x_}] := pd[x]
  	pd[{x_,y__}] := pd /@ Times[x,y]
  	pd[for[{v_, beg_, end_}, body___]] := 
  		Product[pd[{body}], {v, beg, end}]
(*
  	pd[model[x___]] := pd[Select[{x}, notDecl]]
  *)
  	lpdExpand[p_, x_] :=
  		FixedPoint[lpdec[p], x]
  	lpdec[p_][x_] :=
		  Expand[lpde[p, x]]

	  lpde[p_, x_] :=
		  x
	  lpde[p_, x_ + y__] :=
  		lpdec[p] /@ (x + y)

  	lpde[p_, HoldPattern[Sum[x_ + y__, r:{v_, beg_, end_}]]] :=
  		(Sum[#, r] &) /@ (x + y) /;
  		ensures[p, beg \[Element] Integers \[And] end \[Element] Integers]

	  lpde[p_, HoldPattern[Sum[e:(x_ * y__), r:{v_, beg_, end_}]]] :=
		  With[{out = Select[e, FreeQ[#, v]&],
                in = Select[e, !FreeQ[#, v]&]},
			out * Sum[in, r]]
		  (* PROBLEM!!! TOO STRONG OF A CONDITION! *)

  	lpde[p_, HoldPattern[Sum[e_, r:{v_, beg_, end_}]]] :=
  		With[{p1 = addProp[p, v \[Element] intRng[beg, end]]},
  			Sum[lpdec[p1][e], r]] /;
  		ensures[p, beg \[Element] Integers \[And] end \[Element] Integers]

	  lpde[p_, x_ * y__] :=
  		lpdec[p] /@ (x * y)
  		
  		(* Product? *)
  
  	lpde[p_, Log[x_ * y__]] :=
		  Log[x] + Log[Times[y]] /;
		  ensures[p, 0 < x < \[Infinity]]

  	lpde[p_, HoldPattern[Log[Product[e_, {v_, beg_, end_}]]]] :=
  		Sum[Log[e], {v, beg, end}] /;
		  ensures[p, beg \[Element] Integers \[And] end \[Element] Integers] \[And]
  		ensures[addProp[p, v \[Element] intRng[beg,end]], 0 < e < \[Infinity]]

  	lpde[p_, Log[Exp[x_]]] :=
  		x /;
  		ensures[p, Im[x] == 0]
  
  	lpde[p_, Log[a_^b_]] :=
  		b Log[a] /;
  		ensures[p, a >= 0 \[And] Im[b] == 0] \[And]
  		ensures[p,
  			(b != 0 \[And] b != -Infinity) \[Or]
  			(a != 0 \[And] b != 0) \[Or]
  			(a != 0 \[And] a != Infinity)]
  
  	addProp[p_, q:(v_ \[Element] dom_)] :=
		  With[{u = Unique[v]},
			Append[subst[u, v, p], q]]
  
  	ensures[p_, e_] := 
		implies[And @@ (decl2prop[#, e] &) /@ p, e]

	  implies[p_, e_] := Assuming[Simplify[p], Simplify[e]]
  
  	decl2prop[x_, _] :=
  		x
  	decl2prop[x_ \[Element] preals, _] :=
  		Infinity > x > 0
  	decl2prop[x_ \[Element] nats, _] :=
  		x \[Element] Integers \[And] x >= 0
  	decl2prop[x_ \[Element] Reals[n_], e_] :=
		With[{matches = Extract[e, Position[e, x[_]]]},
			And @@ ((inIntRng[#[[1]], 1, n] \[Implies] # \[Element] Reals &) /@ matches)]
  	decl2prop[x_ \[Element] intRng[beg_, end_], _] :=
  		inIntRng[x, beg, end]
  
  	inIntRng[x_, beg_, end_] :=
  		x \[Element] Integers \[And] beg <= x <= end

	(* PROBLEM!!!! With[...] does not respect bound variables
       in Sum[...]!
       SOLUTION? Use Module instead of With.
 *)
	  subst[val_, i_, e_] :=
		ReleaseHold[ReplacePart[
			Hold[With[{idup = inew}, e]],
			{{1,1,1,1} -> i, {1,1,1,2} -> val}
		]]

(*
	  mymodel = model[
		n \[Element] nats,
		x \[Element] Reals[n],
		\[Mu] \[Element] Reals,
		\[Lambda] \[Element] preals,
		\[Mu]mean \[Element] Reals,
		\[Mu]sd \[Element] preals,
		\[Alpha] \[Element] preals,
		\[Beta] \[Element] preals,
		\[Lambda] \[Tilde] GammaDistribution[\[Alpha], \[Beta]],
		\[Mu] \[Tilde] NormalDistribution[\[Mu]mean, \[Mu]sd],
		for[{i, 1, n},
			x[i] \[Tilde] NormalDistribution[\[Mu], \[Lambda]^(-1/2)]
		]
	  ]  
*)
  End[]
EndPackage[]





















