
(* ::Package:: *)

ClearAll["PolynomialTools`*"];

BeginPackage["PolynomialTools`"];




RootInterleavingQ;
HStarPolynomial;


EulerianA;
EulerianAPolynomial;

RealRootedQ;

Begin["`Private`"];

Clear[RealRootedQ];
RealRootedQ::poly = "Argument `1` should be a polynomial";
RealRootedQ[poly_, t_] := Module[{d},
   d = Exponent[poly, t];
   d == CountRoots[poly, t]
   ];
RealRootedQ[0] := True;
RealRootedQ[poly_] := Which[
   NumberQ[poly], False,
   PolynomialQ[poly, Variables[poly][[1]]],
   RealRootedQ[poly, Variables[poly][[1]]],
   True, Message[RealRootedQ::poly, poly]
];

(*
Use this technique instead.
https://mathoverflow.net/questions/403708/b%c3%a9zout-matrices-and-interlacing-roots
*)

RootInterleavingQ::usage = "RootInterleavingQ[P,Q,t] returns true 
if the roots interleave (weakly). In particular, largest root of Q is greater than largest root of P.";
RootInterleavingQ[pp_, qq_, t_Symbol] := 
  Module[{rootsPP, rootsQQ, interleavesQ},
   rootsPP = If[NumberQ[pp], {},
     Sort[t /. NSolve[pp == 0, t, WorkingPrecision -> 160]]
     ];
   rootsQQ = If[NumberQ[qq], {},
     Sort[t /. NSolve[qq == 0, t, WorkingPrecision -> 160]]
     ];
   
   interleavesQ[lstF_, {}] := True;
   interleavesQ[{}, lstG_] := True;
   interleavesQ[lstF_, 
     lstG_] := ((Last[lstF] <= Last[lstG]) && interleavesQ[Most@lstG, lstF]);
	 
	(0<=Exponent[qq,t]-Exponent[pp,t]<=1) && interleavesQ[rootsPP, rootsQQ]
];

RootInterleavingQ[polys_List,t_]:=Module[{pp},
	And@@Table[
		RootInterleavingQ[ pp[[1]], pp[[2]] ,t]
		,{pp,Subsets[polys,{2}]}]
];



(* Eulerian numbers. *)
EulerianA[n_Integer, m_Integer] := EulerianA[n, m] = Sum[(-1)^k Binomial[n + 1, k] (m + 1 - k)^(n), {k, 0, m + 1}];

EulerianAPolynomial[0, t_]:=1;
EulerianAPolynomial[n_Integer, t_]:=EulerianAPolynomial[n,t] = Expand[
Sum[ Binomial[n , k] EulerianAPolynomial[k,t] (t-1)^(n-1-k), {k, 0, n-1}]];

	 

HStarPolynomial::usage="HStarPolynomial[pol,x] returns the h*-polynomial.";

HStarPolynomial[poly_, k_] := Expand@Module[
	{cl = CoefficientList[poly, k],j},
		cl.Table[ (1 - k)^(Length[cl] - 1 - j) If[j == 0, 1, k] EulerianAPolynomial[j, k]
	,{j, 0, Length[cl] - 1}]];

(*
HStarPolynomial[poly_, k_] := With[{cl = CoefficientList[poly, k]},
	Together[(1 - k)^(Length[cl]) (cl.Table[HurwitzLerchPhi[k, 1 - j, 0], {j, Length[cl]}])]
];
*)
(*
HStarPolynomial[poly_,x_] := Module[{bb, c, d,y},
	bb[xx_, i_, s_] := 1/i! Product[(xx - k + s), {k, 0, i - 1}];
	
	d = Exponent[poly, x];
	If[d<0, 0,
		tbz = CoefficientList[poly - Sum[ c[i] bb[x, d, i], {i, 0, d}], x];
		vars = Variables[tbz];
		Reverse[vars] /. Solve[Thread[tbz == 0], vars][[1]]
	].(x^Range[0,d])
];
*)


End[(* End private *)];
EndPackage[];



