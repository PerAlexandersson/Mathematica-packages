
(* ::Package:: *)

Clear["PolynomialTools`*"];

BeginPackage["PolynomialTools`"];



RootInterleaveProperty;



Begin["Private`"];

RootInterleaveProperty::usage = "RootInterleaveProperty[P,Q,t] returns true 
if the roots interleave (weakly). In particular, largest root of Q is greater than largest root of P.";
RootInterleaveProperty[pp_, qq_, t_Symbol] := 
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
     lstG_] := ((Last[lstF] <= Last[lstG]) && 
      interleavesQ[Most@lstG, lstF]);
   
   interleavesQ[rootsPP, rootsQQ]
   ];



End[(* End private *)];
EndPackage[];



