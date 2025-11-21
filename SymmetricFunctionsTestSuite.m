(* ::Package:: *)


(* TODO 

--- Create a test suite.

--- Make converting to new bases easier. 
		Might be an issue if these are not expressed in the monomial basis, but should be rare.

--- Use ExpandM instead of automatic.

--- Use the e-basis as intermediate basis instead(?)
		
--- Allow standard basis to handle Hall inner product in a more efficient way?

--- Improve Sn-character code(?)
	
--- Hard-code algorithms for computing e-to-m and h-to-m using Kostka coeffs / other formulas instead?
			
--- Make change-of-basis accept several/All alphabets at once?

--- See what attributes can be used (listable, orderless, etc).

--- Add DeclarePackage instead of loading NewTableaux.

--- Update documentation.

*)

ClearAll["SymmetricFunctionsTestSuite`*"];
BeginPackage["SymmetricFunctionsTestSuite`"];

Get["SymmetricFunctions`"];


(* Compute the basic functions in terms of monomials. *)

TimingTest[MonomialExpansion]:=
Table[
	func[la]
	,{la,IntegerPartitions[8]}
	,{func,{SchurSymmetric,ElementaryESymmetric,CompleteHSymmetric,PowerSumSymmetric}}
];


TimingTest[MonomialProducts]:= ({
	Expand[(MonomialSymbol[{6,5,1,1}]^2 + MonomialSymbol[{6,5,2}]^2 + MonomialSymbol[{4,1,1,1,1,1,1,1,1}]^2)]
	,
	Expand[Times[
		Sum[First[lam] MonomialSymbol[lam],{lam,IntegerPartitions[8]}]
		,
		Sum[Last[lam] MonomialSymbol[lam],{lam,IntegerPartitions[8]}]
	]]
});


UnitTest[SchurKostka]:=(
	ToMonomialBasis[SchurSymmetric[{4,3,2}]-SkewSchurSymmetric[{4,3,2}]] === 0 );


UnitTest[LRRule]:=Table[
Expand[
	LRExpand[SchurSymbol[la]*SchurSymbol[mu]] - 
	(SchurSymbol[la]SchurSymbol[mu]//ToMonomialBasis//ToSchurBasis) 
]
,{la,IntegerPartitions[4]},{mu,IntegerPartitions[3]}];


UnitTest[ElementaryESymmetric] := With[{m = 5},
MExpand[Sum[
	(-1)^i ElementaryESymmetric[i] CompleteHSymmetric[m - i], {i, 0,
		m}]] === 0
];


UnitTest[OmegaInvolution] := And[
ToMonomialBasis@OmegaInvolution@OmegaInvolution@SchurSymmetric[{4, 2, 1}] === 
	SchurSymmetric[{4, 2, 1}],
ToMonomialBasis@OmegaInvolution@ElementaryESymmetric[{4, 2, 1}] === ToMonomialBasis@CompleteHSymmetric[{4, 2, 1}]
];

UnitTest[HallLittlewoodTSymmetric] := And[
Block[{q, ss},
	SameQ[
	Coefficient[
	ToSchurBasis[HallLittlewoodTSymmetric[{2, 2, 2, 2}, q]], 
	SchurSymbol[{3, 3, 2}]],
	(q^3 + q^4 + q^5)]]
];


(* Add plethysm relation also, and test. *)
UnitTest[HallLittlewoodPSymmetric] := Module[{t, lam = {3, 2}}, And[
  ToMonomialBasis@HallLittlewoodPSymmetric[lam, t] ==
   ToMonomialBasis@MacdonaldPSymmetric[lam, 0, t]]
];


EndPackage[];
