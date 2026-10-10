(* ::Package:: *)

BeginPackage["QuasiSymmetricFunctions`",{"AlgebraicBases`","CombinatoricTools`"}];


Unprotect["`*"]
ClearAll["`*"]

PartitionedCompositionCoarsenings;

MonomialQSymmetric;
FundamentalQSymmetric;
PowerSumQSymmetric;
PowerSumAltQSymmetric;
ZPowerSumQSymmetric;

MonomialQSymbol;
FundamentalQSymbol;
PowerSumQSymbol;
ZPowerSumQSymbol;


ToOtherQSymmetricBasis; (* Use sparingly *)

ToFundamentalBasis;
ToPowerSumQSymBasis;
ToZPowerSumQSymBasis;


Begin["`Private`"];

(* Pattern for list of integers *)
iList = {RepeatedNull[_Integer]};

(*This takes a list of lists!*)
(* This is used for quasisymmetric power-sums. *)
(*
PartitionedCompositionCoarsenings[{{a_Integer}}] := {{{a}}};
PartitionedCompositionCoarsenings[alpha_List] := 
Module[{rest, first},
	rest = Rest[alpha];
	first = alpha[[1]];
	Join[
	(* Either first block is separate, or joined with the thing on the right. *)
	Prepend[#, first] & /@ 
		PartitionedCompositionCoarsenings[rest], 
			{Join[first, #1], ##2} & @@@ PartitionedCompositionCoarsenings[rest]]
];
*)

PartitionedCompositionCoarsenings::usage = "PartitionedCompositionCoarsenings[alpha] returns all coarsenings of the partitioned composition alpha, represented as a list of lists of compositions.";
PartitionedCompositionCoarsenings[alpha_List] := Module[{v, n = Length@alpha},
	Map[Join @@ # &, ListSplits[alpha], {2}]
];

(*********************************************************)




QMonomialProduct[alpha_List, beta_List, x_: None] := Module[
	{specPaths, m = Length@alpha, n = Length@beta, ap, alphaPad, betaPad},
	
	alphaPad = Append[alpha, 0];
	betaPad = Append[beta, 0];
	
	(* Using p.33 of https://www.math.ubc.ca/~steph/papers/
	QuasiSchurBook.pdf *)
	
	specPaths[0, b_] := {ConstantArray[{0, 1}, b]};
	specPaths[a_, 0] := {ConstantArray[{1, 0}, a]};
	specPaths[a_, b_] := specPaths[a, b] = Join[
			Append[#, {1, 0}] & /@ specPaths[a - 1, b],
			Append[#, {0, 1}] & /@ specPaths[a, b - 1],
			Append[#, {1, 1}] & /@ specPaths[a - 1, b - 1]];
	
	Sum[
		ap = Prepend[Accumulate[p], {0, 0}];
		MonomialQSymbol[
		Table[
			p[[i]].{ alphaPad[[ 1 + ap[[i, 1]]]], betaPad[[ 1 + ap[[i, 2]]]]}
			, {i, Length@p}]
		, x]
	, {p, specPaths[m, n]}]
];


(* Basis symbols are created with AlgebraicBases`CreateBasis, with composition indices. *)

MonomialQSymbol::usage = "MonomialQSymbol[alpha, x] represents the monomial quasisymmetric-function basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
CreateBasis[MonomialQSymbol, "M", IndexType -> "Composition",
	MultiplicationFunction -> QMonomialProduct,
	PowerFunction -> "Mult"
];


FundamentalQSymbol::usage = "FundamentalQSymbol[alpha, x] represents the fundamental quasisymmetric-function basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
CreateBasis[FundamentalQSymbol, "F", IndexType -> "Composition",
	MultiplicationFunction -> None,
	PowerFunction->None
];

PowerSumQSymbol::usage = "PowerSumQSymbol[alpha, x] represents the quasisymmetric power-sum basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
CreateBasis[PowerSumQSymbol, "\[Psi]", IndexType -> "Composition",
	MultiplicationFunction -> None,
	PowerFunction->None
];

ZPowerSumQSymbol::usage = "ZPowerSumQSymbol[alpha, x] represents the z-normalized quasisymmetric power-sum basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
CreateBasis[ZPowerSumQSymbol, "z\[Psi]", IndexType -> "Composition",
	MultiplicationFunction -> None,
	PowerFunction->None
];

MonomialQSymmetric::usage = "MonomialQSymmetric[alpha, x] returns the monomial quasisymmetric function indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
MonomialQSymmetric[alpha_List, x_: None] := MonomialQSymbol[alpha, x];


FundamentalQSymmetric::usage = "FundamentalQSymmetric[alpha, x] returns the fundamental quasisymmetric function indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
FundamentalQSymmetric[alpha_List, x_: None] := 
FundamentalQSymmetric[alpha, x] = Sum[
	MonomialQSymbol[beta, x]
	, {beta, Join@@@CompositionRefinements[alpha]}];

	
(* The fundamental Qsym fulfills the shuffle product rule. *)

UnitTest[FundamentalQSymmetric] := And[
	Expand[FundamentalQSymmetric[{1, 2}] FundamentalQSymmetric[{1}]] ==
					FundamentalQSymmetric[{1, 3}] + 
			FundamentalQSymmetric[{1, 2, 1}] +
			FundamentalQSymmetric[{2, 2}] + FundamentalQSymmetric[{1, 1, 2}]
];


PowerSumQSymmetric::usage = "PowerSumQSymmetric[alpha, x] returns the quasisymmetric power-sum function indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
PowerSumQSymmetric[alpha_List, x_: None] := PowerSumQSymmetric[alpha, x] = Module[{pi},
	pi[comp_List] := Times @@ Accumulate[comp];
	Expand[
		ZCoefficient[alpha]
		Sum[
			1/(Times @@ (pi /@ beta)) MonomialQSymbol[Total /@ beta, x]
			, {beta, PartitionedCompositionCoarsenings[List /@ alpha]}]
		]
];

(* With a constant *)
ZPowerSumQSymmetric::usage = "ZPowerSumQSymmetric[alpha, x] returns the z-normalized quasisymmetric power-sum function indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
ZPowerSumQSymmetric[alpha_List, x_: None] := ZPowerSumQSymmetric[alpha, x] = 
		Expand[PowerSumQSymmetric[alpha,x]/ZCoefficient[alpha]];


PowerSumAltQSymmetric::usage = "PowerSumAltQSymmetric[alpha, x] returns the alternate quasisymmetric power-sum function indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
PowerSumAltQSymmetric[alpha_List, x_: None] := PowerSumAltQSymmetric[alpha, x] = Module[{spi},
	spi[comp_List] := Length[comp]! (Times @@ comp);
	Expand[
		ZCoefficient[alpha]
		Sum[
			1/(Times @@ (spi /@ beta)) MonomialQSymbol[Total /@ beta, x]
			, {beta, PartitionedCompositionCoarsenings[List /@ alpha]}]
		]
];


Clear[CompositionIndexedBasisRule];
CompositionIndexedBasisRule[size_Integer, bb_, toBasis_, monom_: MonomialQSymbol, x_: None] := 
	CompositionIndexedBasisRule[size, bb, toBasis, monom, x] =
	Module[{parts, mat, imat},
	parts = IntegerCompositions[size];
	
	
	(* Matrix with the toBasis expanded in monomials. *)
	(* 
	Here we use the "None" alphabet *)
	mat = Table[
		Table[
			Coefficient[toBasis[p], monom[q] ], {q, parts}]
	, {p, parts}];
	
	(* 
		TODO:
		Can be made much faster if we use floats instead.
		Works well if we know entries are integers in the output.
	*)
	(*
	imat = Inverse[0.0 + mat];
	*)
	imat = Inverse[mat];
	
	Table[
		monom[parts[[p]], x] -> Sum[
			bb[ parts[[q]], x ]*imat[[p, q]]
			, {q, Length@parts}]
		, {p, Length@parts}]
];


ToOtherQSymmetricBasis::usage = "ToOtherQSymmetricBasis[basis, pol, newSymb, x, mm] converts pol from the monomial basis to the basis specified by basis and newSymb. The alphabet x defaults to None and the monomial symbol mm defaults to MonomialQSymbol.";
ToOtherQSymmetricBasis[basis_, pol_, newSymb_, x_: None, mm_: MonomialQSymbol] := Module[
	{mmVars, deg, maxDegree, monomList},
	
	mmVars = Cases[Variables[pol], mm[__, x], {0, Infinity}];
	maxDegree = Max[0, mmVars /. mm[lam__, x] :> Tr[lam]];
	If[maxDegree == 0, pol,
	monomList = MonomialList[pol, mmVars];
	
	
	Expand@Sum[
		deg = Max@Cases[mon, mm[lam__, x] :> Tr[lam], {0, Infinity}];
		If[deg <= 0, mon, 
			mon /. CompositionIndexedBasisRule[deg, newSymb, basis, mm, 
				x]]
		, {mon, monomList}]]
];


ToFundamentalBasis::usage = "ToFundamentalBasis[poly, x] converts poly to the fundamental quasisymmetric basis. The alphabet x defaults to None.";
ToFundamentalBasis[poly_, x_: None] := 
	ToOtherQSymmetricBasis[FundamentalQSymmetric, poly, FundamentalQSymbol, x];

ToPowerSumQSymBasis::usage = "ToPowerSumQSymBasis[poly, x] converts poly to the quasisymmetric power-sum basis. The alphabet x defaults to None.";
ToPowerSumQSymBasis[poly_,x_: None] := 
	ToOtherQSymmetricBasis[PowerSumQSymmetric, poly, PowerSumQSymbol, x];
	
ToZPowerSumQSymBasis::usage = "ToZPowerSumQSymBasis[poly, x] converts poly to the z-normalized quasisymmetric power-sum basis. The alphabet x defaults to None.";
ToZPowerSumQSymBasis[poly_,x_: None] := 
	ToOtherQSymmetricBasis[ZPowerSumQSymmetric, poly, ZPowerSumQSymbol, x];


End[(* End private *)];
EndPackage[];
