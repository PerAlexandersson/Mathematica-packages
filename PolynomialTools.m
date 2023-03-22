(* ::Package:: *)

ClearAll["PolynomialTools`*"];

BeginPackage["PolynomialTools`"];

InterleavingRootsQ;
HStarPolynomial;


EulerianA;
EulerianAPolynomial;
BinomialEulerianPolynomial;

RealRootedQ;

SamePhaseStableQ;
StablePolynomialQ;
FindStablePolynomialCounterexample;

FindPolynomialRecurrence;
VariableDegree;
IndexDegree;
DifferentialDegree;
RecurrenceLength;
Homogeneous;


Begin["`Private`"];

Clear[RealRootedQ];
RealRootedQ::usage = "RealRootedQ[poly] returns true if the poly is a real-rooted univariate polynomial, or a constant.";
RealRootedQ::poly = "Argument `1` should be a polynomial,";
RealRootedQ[0,t_]:=True;
RealRootedQ[poly_/;NumberQ,t_] := True; 
RealRootedQ[poly_, t_] := Module[{d},
   d = Exponent[poly, t];
   d == CountRoots[poly, t]
];
RealRootedQ[0] := True;
RealRootedQ[poly_/;NumberQ] := True; (* Constants are considered real-rooted. *)
RealRootedQ[poly_] := If[
   PolynomialQ[poly, Variables[poly][[1]]],
   RealRootedQ[poly, Variables[poly][[1]]]
   ,
   Message[RealRootedQ::poly, poly]
];

SamePhaseStableQ::usage = "SamePhaseStableQ[poly, [samples] ] randomly checks some stuff. ";
SamePhaseStableQ[poly_, samples_: 20]:=Module[{t, vars=Variables[poly]},
	Catch[
		Do[
			With[{vals = RandomReal[{0,1},Length@vars]},
				If[ !RealRootedQ[ poly/.  MapThread[#1 -> t*#2 &, {vars, vals},1], t], 
				(* Todo, send error here, if we want to get counter-example. *)
					Print["Not same phase stabe. Counterexamle: ", {vars, vals}];
					Throw[False]
				]
			]
		,{k,samples}];
	True]
];

StablePolynomialQ::usage = "StablePolynomialQ[poly, [samples] ] randomly checks some stuff. ";
StablePolynomialQ[poly_, samples_: 20]:=Module[{t, vars=Variables[poly]},
	Catch[
		Do[
			With[{lam = RandomReal[{0,1},Length@vars], mu = RandomReal[{0,1},Length@vars]},
				If[ !RealRootedQ[ poly/.  MapThread[#1 -> t*#2 + #3 &, {vars, lam, mu},1], t], 
				(* Todo, send error here, if we want to get counter-example. *)
				Print["Not stabe. Counterexamle: ", {vars, lam, mu}];
				Throw[False]
				]
			]
		,{k,samples}];
	True]
];

FindStablePolynomialCounterexample::usage = "FindStablePolynomialCounterexample[poly] uses
Mathematicas FindInstance method to look for counterexample.";
FindStablePolynomialCounterExample[poly_]:=With[{vv=Variables@poly},
	FindInstance[ 
		And @@ Thread[(Im /@vv) > 0] && poly == 0, vv]
];


(*
Use this technique instead.
https://mathoverflow.net/questions/403708/b%c3%a9zout-matrices-and-interlacing-roots
*)

InterleavingRootsQ::usage = "InterleavingRootsQ[P,Q,t] returns true 
if the roots interleave (weakly). In particular, largest root of Q is greater than largest root of P.";
(*
InterleavingRootsQ[pp_, qq_]:=InterleavingRootsQ[ pp, qq, First@Variables[{pp,qq}]];
*)

InterleavingRootsQ[pp1_, qq1_, t_Symbol] := Module[{pp, qq, gcd, rootsPP, rootsQQ, interleavesQ},
	
	(* Factor out common roots. *)
	gcd = PolynomialGCD[pp1, qq1];
	pp = Together[pp1/gcd];
	qq = Together[qq1/gcd];
	
	
	rootsPP = If[NumberQ[pp], {},
		Sort[t /. NSolve[pp == 0, t, WorkingPrecision -> 160]]
	];
	rootsQQ = If[NumberQ[qq], {},
		Sort[t /. NSolve[qq == 0, t, WorkingPrecision -> 160]]
	];
	
	interleavesQ[lstF_, {}] := True;
	interleavesQ[{}, lstG_] := True;
	interleavesQ[lstF_, lstG_] := ((Last[lstF] <= Last[lstG]) && interleavesQ[Most@lstG, lstF]);
	
	(0<=Exponent[qq,t]-Exponent[pp,t]<=1) && interleavesQ[rootsPP, rootsQQ]
];

InterleavingRootsQ[polys_List, t_Symbol]:=Module[{pp},
	And@@Table[
		InterleavingRootsQ[ pp[[1]], pp[[2]] , t]
		,{pp, Subsets[polys,{2}]}]
];



(* Eulerian numbers. *)
EulerianA[n_Integer, m_Integer] := EulerianA[n, m] = Sum[(-1)^k Binomial[n + 1, k] (m + 1 - k)^(n), {k, 0, m + 1}];

EulerianAPolynomial[0, t_]:=1;
EulerianAPolynomial[n_Integer, t_]:=EulerianAPolynomial[n,t] = Expand[
Sum[ Binomial[n , k] EulerianAPolynomial[k,t] (t-1)^(n-1-k), {k, 0, n-1}]];


BinomialEulerianPolynomial::usage = "BinomialEulerianPolynomial[n,t] returns the Binomial Eulerian polynomial, introduced by Athanasiadis.";
BinomialEulerianPolynomial[n_Integer, t_] := BinomialEulerianPolynomial[n,t] = Expand[
1 + t Sum[Binomial[n, m] EulerianAPolynomial[m, t], {m, n}]];


HStarPolynomial::usage="HStarPolynomial[pol,x] returns the h*-polynomial.";

HStarPolynomial[poly_, k_] := Expand@Module[
	{cl = CoefficientList[poly, k],j},
		cl.Table[ (1 - k)^(Length[cl] - 1 - j) If[j == 0, 1, k] EulerianAPolynomial[j, k]
	,{j, 0, Length[cl] - 1}]];


VariableDegree::usage = "Option for FindPolynomialRecurrence. Non-negative integer value.";
IndexDegree::usage = "Option for FindPolynomialRecurrence. Non-negative integer value.";
DifferentialDegree::usage = "Option for FindPolynomialRecurrence. Non-negative integer value.";
RecurrenceLength::usage = "Option for FindPolynomialRecurrence. Non-negative integer value.";
Homogeneous::usage = "Option for FindPolynomialRecurrence. True or False.";

Options[FindPolynomialRecurrence] = {
	VariableDegree -> 2,
	IndexDegree -> 1,
	DifferentialDegree -> 1,
	RecurrenceLength -> 1,
	Homogeneous -> True
};

FindPolynomialRecurrence::usage = "FindPolynomialRecurrence[{p1,p2,...,pk},{t,n}] returns 
a linear recursion involving the polynomials (in t) and their derivatives, and coefficients in 
t and n. Options are used to specify maximal length of recursion, maximal degree of differentiation,
and maximal degree in t and degree in n.";
FindPolynomialRecurrence[polys_List, {t_Symbol, n_Symbol}, 
	opts : OptionsPattern[]] := Module[{c, coeffSum, vDeg, iDeg, rDeg,
	dDeg, eqns, vars,
	sol, solFormat, formatSingleCoeff, mm = Length@polys,
	ts = ToString@t,
	ns = ToString@n,
	homo
	},
	vDeg = OptionValue[VariableDegree];
	iDeg = OptionValue[IndexDegree];
	dDeg = OptionValue[DifferentialDegree];
	rDeg = OptionValue[RecurrenceLength];
	homo = OptionValue[Homogeneous];
	
	(* General coefficient. *)
	coeffSum[r_Integer, d_Integer] := Sum[ c[r, d, i, j] n^i t^j, {i, 0, iDeg}, {j, 0, vDeg}];

	formatSingleCoeff[r_, d_, s_] := Module[
		{coeff, polyPart},
		
		coeff = Sum[
			(c[r, d, i, j] /. s) n^i t^j
			, {j, 0, vDeg}, {i, 0, iDeg}];
		
		polyPart = Which[
			d == 0,
			Subscript["P", ns <> "-" <> ToString@r]
			,
			r == -1 && d == -1, (* Non-homogeneous part *)
			1
			,
			True,
			Subsuperscript["P", ns <> "-" <> ToString@r, 
			"(" <> ToString[d] <> ")"]
			];
		Which[
		coeff === 0, 0,
		coeff === 1, polyPart,
		True, Factor[coeff]*polyPart
		]
		];
	
	(* Just add all coeffs. *)
	solFormat[s_] := Total[
		Join[
			Join @@
				Table[formatSingleCoeff[r, d, s], {d, 0, dDeg}, {r, rDeg}]
			,
			If[! homo, {formatSingleCoeff[-1, -1, s]}, {}]
		]
		];
	
	eqns = Table[
		(*To be zero*)
		polys[[nn]] -
		Sum[
			(coeffSum[r, d] /. n -> nn)*D[polys[[nn - r]], {t, d}]
			, {d, 0, dDeg}, {r, rDeg}]
		-
		If[!homo, (coeffSum[-1, -1] /. n -> nn), 0]
		, {nn, rDeg + 1, mm}];
	
	(* All unknowns *)
	vars = Cases[eqns, c[_, _, _, _], Infinity];
	sol = Solve[Thread[CoefficientList[eqns, t] == 0], vars];
	
	If[Length@sol == 0,
		None,
		Row[{Subscript["P", ns], "=", solFormat[sol[[1]]]}]
	]
];


End[(* End private *)];
EndPackage[];



