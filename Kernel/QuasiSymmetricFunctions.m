(* ::Package:: *)

BeginPackage["QuasiSymmetricFunctions`",{"AlgebraicBases`","CombinatoricTools`","SymmetricFunctions`"}];


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
QuasiSchurQSymbol;


ToOtherQSymmetricBasis; (* Use sparingly *)

ToFundamentalBasis;
ToPowerSumQSymBasis;
ToZPowerSumQSymBasis;
QuasiSymmetricFunctionToPolynomial;
PolynomialToQuasiSymmetricFunction;
ToQuasiSymmetric;
QuasiSchurQSymmetric;


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


QuasiSymmetricFunctionToPolynomial::usage = "QuasiSymmetricFunctionToPolynomial[expr, x, n] expresses a quasisymmetric function in the variables x[1] through x[n].";
QuasiSymmetricFunctionToPolynomial[expr_, x_, n_Integer] := Module[{mExpression},
	mExpression = Expand[expr /. {
		HoldPattern[FundamentalQSymbol[alpha_List, _]] :> FundamentalQSymmetric[alpha],
		HoldPattern[PowerSumQSymbol[alpha_List, _]] :> PowerSumQSymmetric[alpha],
		HoldPattern[ZPowerSumQSymbol[alpha_List, _]] :> ZPowerSumQSymmetric[alpha],
		HoldPattern[QuasiSchurQSymbol[alpha_List, _]] :> QuasiSchurQSymmetric[alpha],
		HoldPattern[MonomialQSymbol[alpha_List, _]] :>
			Total[Times @@ MapThread[Power, {x /@ #, alpha}] & /@ Subsets[Range[n], {Length[alpha]}]]
	}];
	Expand[mExpression /. HoldPattern[MonomialQSymbol[alpha_List, _]] :>
		Total[Times @@ MapThread[Power, {x /@ #, alpha}] & /@ Subsets[Range[n], {Length[alpha]}]]]
];

qSymmetricPolynomialVariableIndices[poly_, x_] := Union@Cases[Variables[Expand[poly]],
	HoldPattern[x[i_]] :> i, Infinity];

(* Coefficients of poly in x[1], ..., x[n], grouped by the composition of nonzero exponents.
   poly is quasisymmetric iff each group has all Binomial[n, l] placements with equal
   coefficients. *)
qSymmetricOrbitGroups[poly_, x_, n_Integer] :=
	GroupBy[CoefficientRules[Expand[poly], x /@ Range[n]], DeleteCases[First[#], 0] & -> Last];

qSymmetricPolynomialQ[groups_Association, n_Integer] := And @@ KeyValueMap[
	Function[{alpha, cs},
		Length[cs] == Binomial[n, Length[alpha]] &&
		AllTrue[cs, Expand[# - First[cs]] === 0 &]],
	groups];

PolynomialToQuasiSymmetricFunction::usage = "PolynomialToQuasiSymmetricFunction[poly, x, basisSymbol] converts a quasisymmetric polynomial in x[1], x[2], ... to the MonomialQSymbol or FundamentalQSymbol basis; the basis defaults to MonomialQSymbol.";
PolynomialToQuasiSymmetricFunction::nonquasisymmetric = "The polynomial is not quasisymmetric in the variables `1`[1], ..., `1`[`2`].";
PolynomialToQuasiSymmetricFunction::basis = "Unknown quasisymmetric-function basis symbol `1`.";
PolynomialToQuasiSymmetricFunction[poly_, x_, n_Integer] :=
	PolynomialToQuasiSymmetricFunction[poly, x, MonomialQSymbol, n];
PolynomialToQuasiSymmetricFunction[poly_, x_, basisSymbol_: MonomialQSymbol] := Module[
	{n = Max[0, Select[qSymmetricPolynomialVariableIndices[poly, x], IntegerQ]]},
	PolynomialToQuasiSymmetricFunction[poly, x, basisSymbol, n]
];
PolynomialToQuasiSymmetricFunction[poly_, x_, basisSymbol_, n_Integer] := Module[
	{groups, monomial},
	groups = qSymmetricOrbitGroups[poly, x, n];
	If[! SubsetQ[Range[n], qSymmetricPolynomialVariableIndices[poly, x]] ||
			! qSymmetricPolynomialQ[groups, n],
		Message[PolynomialToQuasiSymmetricFunction::nonquasisymmetric, x, n]; Return[$Failed]];
	monomial = Total[KeyValueMap[
		If[#1 === {}, First[#2], First[#2] MonomialQSymbol[#1, None]] &, groups]];
	Switch[basisSymbol,
		MonomialQSymbol, monomial,
		FundamentalQSymbol, ToFundamentalBasis[monomial],
		_, Message[PolynomialToQuasiSymmetricFunction::basis, basisSymbol]; $Failed]
];


ToQuasiSymmetric::usage = "ToQuasiSymmetric[expr] embeds a SymmetricFunctions expression in QSym by sending m_lambda to the sum of M_alpha over all distinct rearrangements alpha of lambda.";
toQuasiSymmetricAlphabet[expr_] := Module[{alphabets},
	 alphabets = DeleteDuplicates@Join[
		Cases[expr, HoldPattern[MonomialSymbol[_, a_]] :> a, {0, Infinity}],
		Cases[expr, HoldPattern[SchurSymbol[_, a_]] :> a, {0, Infinity}],
		Cases[expr, HoldPattern[ElementaryESymbol[_, a_]] :> a, {0, Infinity}],
		Cases[expr, HoldPattern[CompleteHSymbol[_, a_]] :> a, {0, Infinity}],
		Cases[expr, HoldPattern[PowerSumSymbol[_, a_]] :> a, {0, Infinity}],
		Cases[expr, HoldPattern[ForgottenSymbol[_, a_]] :> a, {0, Infinity}]];
	If[alphabets === {}, None, First[alphabets]]
];
ToQuasiSymmetric[expr_, x_: None] := Module[{source, target},
	source = toQuasiSymmetricAlphabet[expr];
	target = If[x === None, source, x];
	Expand[ToMonomialBasis[expr, source] /. HoldPattern[MonomialSymbol[lam_List, _]] :>
		Total[MonomialQSymbol[#, target] & /@ DeleteDuplicates[Permutations[lam]]]]
];


(* The quasisymmetric Schur function S_alpha restricted to x[1], ..., x[n] is the sum of the
   Demazure atoms A_gamma over weak compositions gamma of length n whose nonzero parts form
   alpha (Haglund-Luoto-Mason-van Willigenburg), with the standard indexing of
   NonsymmetricPolynomials (CONVENTIONS.md). Taking n = |alpha| variables determines S_alpha.
   The atoms are generated by operators. NonsymmetricPolynomials is loaded here rather than
   in BeginPackage so that its names are not put on $ContextPath of every QSym user (the
   legacy MacdonaldPolynomials package defines some of the same names). *)
Needs["NonsymmetricPolynomials`"];

quasiSchurMonomialExpansion[{}] := 1;
quasiSchurMonomialExpansion[alpha_List] := quasiSchurMonomialExpansion[alpha] = Module[
	{n = Total[alpha], z},
	PolynomialToQuasiSymmetricFunction[
		Total[NonsymmetricPolynomials`AtomPolynomial[
			ReplacePart[ConstantArray[0, n], Thread[# -> alpha]], z] & /@
			Subsets[Range[n], {Length[alpha]}]],
		z, MonomialQSymbol, n]
];

QuasiSchurQSymbol::usage = "QuasiSchurQSymbol[alpha, x] represents the HLMW quasisymmetric Schur basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.";
CreateBasis[QuasiSchurQSymbol, "S", IndexType -> "Composition",
	MultiplicationFunction -> None,
	PowerFunction -> None
];

QuasiSchurQSymmetric::usage = "QuasiSchurQSymmetric[alpha, basisSymbol] returns the HLMW quasisymmetric Schur function indexed by composition alpha in the MonomialQSymbol basis, or in the FundamentalQSymbol basis when requested.";
QuasiSchurQSymmetric[alpha_List, basisSymbol_: MonomialQSymbol] := Module[{monomial},
	monomial = quasiSchurMonomialExpansion[alpha];
	Switch[basisSymbol,
		MonomialQSymbol, monomial,
		FundamentalQSymbol, ToFundamentalBasis[monomial],
		_, Message[PolynomialToQuasiSymmetricFunction::basis, basisSymbol]; $Failed]
];


End[(* End private *)];
EndPackage[];
