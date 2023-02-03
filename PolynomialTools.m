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

FindFirstOrderRecurrence;
FindSecondOrderRecurrence;

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
				Throw[False]
				]
			]
		,{k,samples}];
	True]
];


(*
Use this technique instead.
https://mathoverflow.net/questions/403708/b%c3%a9zout-matrices-and-interlacing-roots
*)

InterleavingRootsQ::usage = "InterleavingRootsQ[P,Q,t] returns true 
if the roots interleave (weakly). In particular, largest root of Q is greater than largest root of P.";
(*
InterleavingRootsQ[pp_, qq_]:=InterleavingRootsQ[pp,qq, First@Variables[{pp,qq}]];
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

InterleavingRootsQ[polys_List,t_Symbol]:=Module[{pp},
	And@@Table[
		RootInterleavingQ[ pp[[1]], pp[[2]] ,t]
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

	
	
(* TODO: Make nicer, two params, derivative order, and recurrence length *)
(* 
Finds a recurrence in terms of the polynomial and its derivative.
*)
FindSecondOrderRecurrence::usage = "FindSecondOrderRecurrence[polys,t] finds a recurrence, if there is one, 
expressing P[n] in terms of P[n-1] and P'[n-1], P[n-2] and P'[n-2].";
FindSecondOrderRecurrence[polys_List, t_Symbol] := Module[
	{a, b, c, d, e, n, coeff, mm = Length@polys, eqns, vars, sol},
	
	(* General coefficient. *)
	
	coeff[n_, i_] := (a[i] t n + b[i] t + c[i] t^2 + d[i] n + e[i]);
	
	eqns = Table[
		(* To be zero *)
		polys[[n]] - Plus[
			coeff[n, 1] polys[[n - 1]],
			coeff[n, 2] D[polys[[n - 1]], t]
			
			,
			coeff[n,3]polys[[n-2]],
			coeff[n,4]D[polys[[n-2]],t]
			
			]
		, {n, 3, mm}];
	vars = Join @@ Table[v[i], {v, {a, b, c, d, e}}, {i, 4}];
	sol = Solve[Thread[CoefficientList[eqns, t] == 0], vars];

	If[Length@sol==0,
		None
		,
	
		Row[{
			Subscript["P", "n"], "=",
			Plus[
				((#[1] & /@ {a, b, c, d, e}).{t "n", t, t^2, "n", 
						1} ) Subscript["P", "n-1"]
				,
				((#[2] & /@ {a, b, c, d, e}).{t "n", t, t^2, "n", 
						1} ) Subscript["P'", "n-1"]
				
				,
				((#[3]&/@{a,b,c,d,e}).{t "n",t,t^2,"n",1} )Subscript["P","n-2"]
				,
				((#[4]&/@{a,b,c,d,e}).{t "n",t,t^2,"n",1} )Subscript["P'","n-2"]

				] /. sol[[1]]
			}]
	]
];


FindFirstOrderRecurrence::usage = "FindFirstOrderRecurrence[polys,t] finds a recurrence, if there is one, 
expressing P[n] in terms of P[n-1] and P'[n-1].";
FindFirstOrderRecurrence[polys_List, t_Symbol] := Module[
	{a, b, c, d, e, n, coeff, mm = Length@polys, eqns, vars, sol},
	
	(* General coefficient. *)
	
	coeff[n_, i_] := (a[i] t n + b[i] t + c[i] t^2 + d[i] n + e[i]);
	
	eqns = Table[
		(* To be zero *)
		polys[[n]] - Plus[
			coeff[n, 1] polys[[n - 1]],
			coeff[n, 2] D[polys[[n - 1]], t]
			]
		, {n, 3, mm}];
	vars = Join @@ Table[v[i], {v, {a, b, c, d, e}}, {i, 2}];
	sol = Solve[Thread[CoefficientList[eqns, t] == 0], vars];
	
	If[Length@sol==0,
		None
		,
		Row[{
			Subscript["P", "n"], "=",
			Plus[
				((#[1] & /@ {a, b, c, d, e}).{t "n", t, t^2, "n", 
						1} ) Subscript["P", "n-1"]
				,
				((#[2] & /@ {a, b, c, d, e}).{t "n", t, t^2, "n", 
						1} ) Subscript["P'", "n-1"]			
				] /. sol[[1]]
			}]
	]
];

End[(* End private *)];
EndPackage[];



