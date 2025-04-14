(* ::Package:: *)

ClearAll["PolynomialTools`*"];

BeginPackage["PolynomialTools`"];

InterleavingRootsQ;


GammaPolynomial;
HStarPolynomial;
HStarVectorToEhrhart;
HVectorInequalitiesQ;

SymmetricDecomposition;

EulerianA;
EulerianAPolynomial;
BinomialEulerianPolynomial;
RefinedEulerian;
RefinedEulerianCoefficients;
SEulerianPolynomial;

LogConcaveQ;
UltraLogConcaveQ;
RealRootedQ;

SamePhaseStableQ;
StablePolynomialQ;
FindStablePolynomialCounterExample;

FindPolynomialRecurrence;
VariableDegree;
IndexDegree;
DifferentialDegree;
RecurrenceLength;
Homogeneous;

CompleteHomogeneousPolynomial;
ElementarySymmetricPolynomial;

Begin["`Private`"];



LogConcaveQ::usage="LogConcaveQ[poly,t] returns true if coefficients form a log-concave sequence.";
LogConcaveQ[poly_, t_] := Module[{c},
   c = CoefficientList[poly, t];
   If[Length[c] < 3, True,
    And @@ Table[c[[i]]^2 >= c[[i - 1]] c[[i + 1]], {i, 2, Length[c] - 1}]
    ]
   ];
UltraLogConcaveQ::usage="LogConcaveQ[poly,t] returns true if coefficients form an ultra log-concave sequence.";
UltraLogConcaveQ[poly_, t_] := Module[{c},
   c = CoefficientList[poly, t];
   If[Length[c] < 3, True,
    And @@
     And @@ Table[ i c[[i]]^2 >= (i + 1) c[[i - 1]] c[[i + 1]], {i, 2, Length[c] - 1}]
	]
];


Clear[RealRootedQ];
RealRootedQ::usage = "RealRootedQ[poly] returns true if the poly is a real-rooted univariate polynomial, or a constant.";
RealRootedQ::poly = "Argument `1` should be a polynomial,";
RealRootedQ[0,t_]:=True;
RealRootedQ[poly_,t_] := True/;NumberQ[poly]; 
RealRootedQ[poly_, t_] := Module[{d},
   d = Exponent[poly, t];
   d == CountRoots[poly, t]
];
RealRootedQ[0] := True;
RealRootedQ[poly_] := True/;NumberQ[poly]; (* Constants are considered real-rooted. *)
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
					Print["Not same phase stable. Counterexample: ", {vars, vals}];
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
				Print["Not stable. Counterexample: ", {vars, lam, mu}];
				Throw[False]
				]
			]
		,{k,samples}];
	True]
];

FindStablePolynomialCounterExample::usage = "FindStablePolynomialCounterexample[poly] uses
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

(* By convention *)
Options[InterleavingRootsQ]={WorkingPrecision -> 30};

InterleavingRootsQ[0, qq1_, t_Symbol,opts:OptionsPattern[]]:=True;
InterleavingRootsQ[pp1_, 0, t_Symbol,opts:OptionsPattern[]]:=True;

InterleavingRootsQ[pp1_, qq1_, t_Symbol,opts:OptionsPattern[]] := Module[
	{pp, qq, gcd, rootsPP, rootsQQ, interleavesQ,wp},
	
	(* Factor out common roots. *)
	gcd = PolynomialGCD[pp1, qq1];
	pp = Together[pp1/gcd];
	qq = Together[qq1/gcd];
	
	wp = OptionValue[WorkingPrecision];
	
	rootsPP = If[NumberQ[pp], {},
		Sort[t /. NSolve[pp == 0, t, WorkingPrecision -> wp]]
	];
	rootsQQ = If[NumberQ[qq], {},
		Sort[t /. NSolve[qq == 0, t, WorkingPrecision -> wp]]
	];
	
	interleavesQ[lstF_, {}] := True;
	interleavesQ[{}, lstG_] := True;
	interleavesQ[lstF_, lstG_] := ((Last[lstF] <= Last[lstG]) && interleavesQ[Most@lstG, lstF]);
	
	(0<=Exponent[qq,t]-Exponent[pp,t]<=1) && interleavesQ[rootsPP, rootsQQ]
];

InterleavingRootsQ[polys_List, t_Symbol,opts:OptionsPattern[]]:=Module[{pp},
	And@@Table[
		InterleavingRootsQ[ pp[[1]], pp[[2]] , t,opts]
		,{pp, Subsets[polys,{2}]}]
];


(*
https://arxiv.org/pdf/1808.04141.pdf
*)
SymmetricDecomposition::usage = "SymmetricDecomposition[p,x] returns two palindromic polynomials, a,b, such that p=a+xb";
SymmetricDecomposition[pp_, x_] := Module[{d = Exponent[pp, x], ii},
   If[d < 0, {0, 0},
    ii = Together[x^d (pp /. (x -> 1/x))];
    Together /@ {(pp - x ii)/(1 - x), (ii - pp)/(1 - x)}
    ]
   ];

(* Eulerian numbers. *)
EulerianA[n_Integer, m_Integer] := EulerianA[n, m] = Sum[(-1)^k Binomial[n + 1, k] (m + 1 - k)^(n), {k, 0, m + 1}];

EulerianAPolynomial[0, t_]:=1;
EulerianAPolynomial[n_Integer, t_]:=EulerianAPolynomial[n,t] = Expand[
Sum[ Binomial[n , k] EulerianAPolynomial[k,t] (t-1)^(n-1-k), {k, 0, n-1}]];


BinomialEulerianPolynomial::usage = "BinomialEulerianPolynomial[n,t] returns the Binomial Eulerian polynomial, introduced by Athanasiadis.";
BinomialEulerianPolynomial[n_Integer, t_] := BinomialEulerianPolynomial[n,t] = Expand[
1 + t Sum[Binomial[n, m] EulerianAPolynomial[m, t], {m, n}]];


RefinedEulerian::usage ="RefinedEulerian[n,j,t] is the des-generating polynomial for permutations in Sn with pi(1)=j.";
RefinedEulerian[1, 1, t_] := 1;
RefinedEulerian[n_Integer, j_Integer, t_] := RefinedEulerian[n, j, t] =
   If[1 <= j <= n,
    Expand[Sum[
      If[j <= k, 1, t] RefinedEulerian[n - 1, k, t]
      , {k, n}]], 0];


RefinedEulerianCoefficients::usage = "RefinedEulerianCoefficients[poly,t] returns the coefficients in the refined Eulerian basis.";
RefinedEulerianCoefficients[poly_, t_] := Module[{n, tbz, c, vars},
   n = Max[0, Exponent[poly, t]];
   tbz = poly - Sum[c[j] RefinedEulerian[n + 1, j, t], {j, n + 1}];
   vars = c /@ Range[n + 1];
   vars /. Solve[Thread[CoefficientList[tbz, t] == 0], vars]
];

SEulerianPolynomial::usage = "SEulerianPolynomial[s,t] returns the s-Eulerian polynomial associated with positive vector s.";
SEulerianPolynomial[{},t_]:=0;
SEulerianPolynomial[s_List, t_] := SEulerianPolynomial[s,t]=Module[{i, ascE, ranges},
   ascE[ee_List] := Sum[
     If[i == 0, Boole[ee[[1]] > 0],
      Boole[ee[[i]] s[[i + 1]] < ee[[i + 1]] s[[i]]]
      ]
     , {i, 0, Length[ee] - 1}];

   ranges = Range[0, # - 1] & /@ s;
   Total[Flatten@Outer[t^ascE[List@##] &, Sequence @@ ranges, 1]]
   ];


GammaPolynomial::usage = "GammaPolynomial[poly,x] expands a palindromic polynomial in the gamma-basis. See Athanasiadis for background.";
GammaPolynomial[poly_, x_] := Module[{c, inGamma, i, n = Exponent[poly, x], vars},
   inGamma = Sum[c[i] x^i (1 + x)^(n - 2 i), {i, 0, Floor[n/2]}];
   vars = Table[c[i], {i, 0, Floor[n/2]}];
   Expand[
    Sum[c[i] x^i, {i, 0, Floor[n/2]}] /. 
     First[Solve[Thread[CoefficientList[poly - inGamma, x] == 0], 
       vars]]]
];

      
HStarPolynomial::usage="HStarPolynomial[pol,x] returns the h*-polynomial.";

HStarPolynomial[poly_]:=HStarPolynomial[poly,First@Variables@poly];
HStarPolynomial[poly_, k_] := Expand@Module[
	{cl = CoefficientList[poly, k],j},
		cl.Table[ (1 - k)^(Length[cl] - 1 - j) If[j == 0, 1, k] EulerianAPolynomial[j, k]
	,{j, 0, Length[cl] - 1}]];

HStarVectorToEhrhart::usage = "HStarVectorToEhrhart[vec,t] returns the Ehrhart polynomial associated with the h-vector.";
HStarVectorToEhrhart[vec_List, t_] := Module[{d = Length[vec] - 1},
   Expand@Sum[Binomial[t + d - j, d] vec[[j + 1]], {j, 0, d}]
];

HVectorInequalitiesQ::usage = "HVectorInequalitiesQ[{1,h1,h2,..,hd}] returns true if it satisfies the inequalities expected of an h*-vector. These are only neccesary, not sufficient.";
HVectorInequalitiesQ[hh_List] :=
  Module[{h, d = Length[hh] - 1, s, hibiQ, hibi2Q, stanleyQ,
    ballettiQ},
   (* h is indexed 0,...,d *)
   h[j_] := hh[[j + 1]];
   (* If h[d]>0, then polytope has interior pts *)

   s = Last@Select[Range[0, d], h[#] > 0 &];
   hibiQ = And @@ Table[
      Total[h /@ Range[d - i, d]]
       <= Total[h /@ Range[1, i + 1]]
      , {i, 0, Floor[d/2] - 1}];
   stanleyQ = And @@ Table[
      Total[h /@ Range[0, i]]
       <= Total[h /@ Range[s - i, s]]
      , {i, 0, Floor[s/2] - 1}];
   hibi2Q = Or[h[d] == 0, And @@ Table[h[1] <= h[i], {i, d-1}]];
   (* Scotts inequality, for general polytopes *)

   ballettiQ =
    Or[d < 3, h[3] > 0, h[2] == 0, h[1] <= 3 h[2] + 3,
     h[1] == 7 && h[2] == 1];
   And[hibiQ, stanleyQ, hibi2Q, ballettiQ]
];

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


CompleteHomogeneousPolynomial::usage = "CompleteHomogeneousPolynomial[d,{a,b},x] returns the degree-d complete homogeneous polynomial in variables x[a]...x[b].";
CompleteHomogeneousPolynomial[d_Integer, {a_Integer, b_Integer}, x_] := 
  CompleteHomogeneousPolynomial[d, {a, b}, x] = Expand[
    Which[
     Not[1 <= a <= b], 0,
     a == b, x[a]^d,
     a < b, 
     Sum[x[a]^k CompleteHomogeneousPolynomial[d - k, {a + 1, b}, x], {k, 0, d}]
     ]];

ElementarySymmetricPolynomial::usage = "ElementarySymmetricPolynomial[d,{a,b},x] returns the degree-d elementary symmetric polynomial in variables x[a]...x[b].";
ElementarySymmetricPolynomial[d_Integer, {a_Integer, b_Integer}, x_] := 
  ElementarySymmetricPolynomial[d, {a, b}, x] = Expand[
    Which[
     Not[1 <= a <= b && (d <= a - b + 1)], 0,
     a == b && d == 1, x[a],
     a < b, 
     x[a] ElementarySymmetricPolynomial[d - 1, {a + 1, b}, x] + ElementarySymmetricPolynomial[d, {a + 1, b}, x]
     ]];

End[(* End private *)];
EndPackage[];



