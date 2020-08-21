(* ::Package:: *)


(* TODO 

--- Make change-of-basis accept several alphabets
--- Allow abstract symbols for P, E, H and S bases, and add code to handle multiplication, basis conversion, Hall inner prod, etc.
--- Some symmetric functions are more natural to express in other bases, so do that.

--- See what attributes can be used (listable, orderless, etc).

--- Add DeclarePackage instead of loading NewTableaux.

*)
(* TODO Make formatting compatible with TEX *)

ClearAll["SymmetricFunctions`*"];
BeginPackage["SymmetricFunctions`"];
Needs["CombinatoricsUtil`"];
Needs["NewTableaux`"];

KostkaCoefficient;
InverseKostkaCoefficient;

PositiveCoefficientsQ;

(* Symbols for denoting the common bases. *)
ElementaryESymbol;
PowerSumSymbol;
CompleteHSymbol;
MonomialSymbol;
SchurSymbol;
ForgottenSymbol;


ChangeSymmetricAlphabet;

MonomialSymmetric;
AugmentedMonomialSymmetric;
CompleteHSymmetric;
ElementaryESymmetric;
PowerSumSymmetric;
SchurSymmetric;
ForgottenSymmetric;

ToSchurBasis;
ToPowerSumBasis;
ToPowerSumZBasis;
ToElementaryEBasis;
ToCompleteHBasis;

OmegaInvolution;
HallInnerProduct;

SymmetricFunctionDegree;
SymmetricFunctionToPolynomial;

PrincipalSpecialization;
Plethysm;


JackPSymmetric;
JackJSymmetric;

HallLittlewoodMSymmetric;
HallLittlewoodTSymmetric;

SchursQSymmetric;
SchursPSymmetric;

SnCharacter;

MacdonaldPSymmetric;
MacdonaldHSymmetric;
SkewMacdonaldESymmetric;

ToMacdonaldHBasis;

LLTSymmetric;

SymplecticSchurSymmetric;
OrthogonalSchurSymmetric;

PetrieSymmetric;
CylindricSchurSymmetric;

LahSymmetricFunction;
LahSymmetricFunctionNegative;



DeltaOperator;
NablaOperator;
DeltaPrimOperator;


Begin["Private`"];



(* Abstract function for defining a symmetric function basis,
	with formatting and basic multiplication. *)
Options[createBasis] = {
		MultiplicationFunction -> None, 
		SortFunction -> "Standard",
		PowerFunction -> "Mult"
};
createBasis[bb_, symb_String, opts:OptionsPattern[]] := Module[{sort,mult,pow},
	
	bb[lam_List] := bb[lam, None];
	bb[i_Integer] := bb[{i}, None];
	bb[i_Integer,x_] := bb[{i}, x];
	
	bb[{lam__, 0}, x_: None] := bb[{lam}, x];
	
	sort = OptionValue[SortFunction];
	
	Which[
		sort === "Standard",
				bb[lam_List, x_: None] := bb[Sort[lam,Greater], x] /; Not[OrderedQ[Reverse@lam]];
		,
		sort =!= None,
			bb[lam_List, x_: None] :=  (sort[lam, x]) /; Not[OrderedQ[Reverse@lam]];
		,
		True, Null
	];
	
	mult = OptionValue[MultiplicationFunction];
	If[mult =!= None,
		bb /: Times[bb[a_List, x_], bb[b_List, x_]] := mult[a, b, x];
	];
	
	
	bb /: Power[bb[a_List, x_], 0] := 1;
	bb /: Power[bb[a_List, x_], 1] := bb[a, x];
	
	pow = OptionValue[PowerFunction];
	Which[
		(* Use recursion, via multiplication *)
		pow === "Mult", 
			bb /: Power[bb[a_List, x_], n_Integer] := 
				Expand[Power[bb[a, x], n - 2] bb[a, a, x]];
		,
		(* Use custom power function *)
		pow =!= None,
			bb /: Power[bb[a_List, x_], n_Integer] := pow[a,n,x];
		,
		True, Null
	];
	
	
	bb /: Format[bb[a_List, x_]] := 
		With[{r = Row[a /. i_Integer :> If[i > 9, OverBar@i, i]]}, 
		If[x === None, Subscript[symb, r], 
			Row[{Subscript[symb, r], "(", ToString@x, ")"}]]];
	
	bb /: HoldPattern[MakeBoxes[bb[a_List, x_], TraditionalForm]] := 
		With[{r = 
			If[Max[a] <= 9, RowBox[ToString /@ a], 
				RowBox[Riffle[ToString /@ a, ","]]]}, 
		If[x === None, SubscriptBox[symb, r], 
			RowBox[{SubscriptBox[symb, r], "(", ToString@x, ")"}]]];
];


(* This is a private helper function, for utilizing memoization. *)
(* TODO-MAKE IT KEEP TRACK OF DUPLICATES AUTOMATICALLY, and reuse *)
monomialProducts[lam_List, {}] := {lam};
monomialProducts[lam_List, mut_List] := 
		monomialProducts[lam, mut] = 
		Module[{addToPartition, done, lamp, new},
			addToPartition[gam_List, {k_, m_Integer}] := 
			With[{ll = Length@gam},
				Table[
				{k + gam[[ii]], gam[[Complement[Range@ll, ii]]]}
				, {ii, Subsets[Range[ll], {m}]}]
				];
			Join @@ Table[
				{done, lamp} = new;
				Join[done, #] & /@ monomialProducts[lamp, Rest@mut]
				, {new, addToPartition[lam, First@mut]}]
	];

(* private helper function. *)
monomialProduct[lam_List, mu_List, x_: None] := Module[
	{n = Length[lam] + Length[mu], dim, lamp, mup, products, nu, mult, dimCoeff},
	
	(*
	https://math.stackexchange.com/questions/395842/
	decomposition-of-products-of-monomial-symmetric-polynomials-into-
	sums-of-them
	*)
	
	dim[p_List] := Times @@ (Last[#]! & /@ Tally[PadRight[p, n]]);
	
	lamp = PadRight[lam, n];
	mup = PadRight[mu, n];
	
	(* Here, decide which partition has fewest permutations? *)
	If[(Multinomial@@(Last /@ Tally[lamp])) > (Multinomial@@(Last /@ Tally[mu])),
		dimCoeff = (dim@lam);
	,
		{lamp,mup} = {mup,lamp};
		dimCoeff = (dim@mu);
	];
	
	(* TODO: THIS OLD CODE IS NOT THAT EFFICIENT IT SEEMS! *)
	(*
	products = 
		Table[
			Sort[DeleteCases[lamp + muPerm, 0], Greater]
		, {muPerm, Permutations@mup}];
	*)
	
	products = monomialProducts[lamp,DeleteCases[Tally@mup,0]];
	
	Total[
		((dim[#1]/dimCoeff)*#2*MonomialSymbol[#1,x])  & @@@ (Tally@products)
	]
];


(* Create the three classical multiplicative bases. *)
createBasis[ElementaryESymbol, "e", 
	MultiplicationFunction -> (ElementaryESymbol[PartitionJoin[#1, #2], #3]&),
	PowerFunction-> 
		Function[{a,n,x},
			ElementaryESymbol[Join@@(ConstantArray[#1, n]& /@a), x]]
];

createBasis[PowerSumSymbol, "p", 
	MultiplicationFunction -> (PowerSumSymbol[PartitionJoin[#1, #2], #3]&),
	PowerFunction-> 
		Function[{a,n,x},
			PowerSumSymbol[Join@@(ConstantArray[#1, n]& /@a), x]]
];

createBasis[CompleteHSymbol, "h", 
	MultiplicationFunction -> (CompleteHSymbol[PartitionJoin[#1, #2], #3]&),
	PowerFunction->
		Function[{a,n,x},
			CompleteHSymbol[Join@@(ConstantArray[#1, n]& /@a), x]]
];

createBasis[MonomialSymbol, "m", 
	MultiplicationFunction -> (monomialProduct),
	PowerFunction->"Mult"
];

createBasis[ForgottenSymbol, "f", 
	MultiplicationFunction -> (monomialProduct),
	PowerFunction->"Mult"
];


(* We use slinky rule for the Schur functions. *)
createBasis[SchurSymbol, "s", 
	MultiplicationFunction -> None,
	PowerFunction -> None,
	SortFunction -> (With[{slr=CompositionSlinky[#1]},
		If[slr[[2]]==0, 0, 
			slr[[2]]*SchurSymbol[slr[[1]], #2]
			]
	]&)
];


(* These are the same, for compatability reasons. *)
MonomialSymmetric=MonomialSymbol;

(* This is based on Macdonald, p.327, and is extremely fast. *)
KostkaCoefficient::usage = "KostkaCoefficient[lam,mu,[a=1]] returns the Kostka coefficient. For general a, this gives the JackP symmetric function coefficients.";

KostkaCoefficient[mu_List, mu_List, a_: 1] := 1;
KostkaCoefficient[mu_List, nu_List, a_: 1] := 0 /; (! PartitionDominatesQ[nu, mu]);
KostkaCoefficient[lam_List, mu_List, a_: 1] := KostkaCoefficient[lam, mu, a] = Together[With[
	{n = Length@mu,
	ee = Function[{ll}, 
		a PartitionN[ConjugatePartition@ll] - PartitionN[ll]
		]
	},
	
	1/(ee[lam]-ee[mu])
	Sum[
		With[{mu2 =
			Sort[DeleteCases[
			ReplacePart[mu, {i -> mu[[i]] + r, j -> mu[[j]] - r}], 0],
			Greater]
			},
			
			(mu[[i]] - mu[[j]] + 2 r) KostkaCoefficient[lam, mu2, a]
		]
	, {i, n}
	,{j, i + 1, n}
	,{r, mu[[j]]}
	]
]];


InverseKostkaCoefficient::usage = "InverseKostkaCoefficient[lam,mu] returns the inverse Kostka coefficient.";

InverseKostkaCoefficient[lam_List, mu_List] := (0 /; PartitionDominatesQ[lam, mu]);
InverseKostkaCoefficient[lam_List, mu_List] := InverseKostkaKHelper[Reverse@lam, Reverse@mu];

(* The convention in https://doi.org/10.1080/03081089008817966
uses partitions with parts ordered increasingly, which makes notation easier.
*)

InverseKostkaKHelper[mu_List, mu_List] := 1;
InverseKostkaKHelper[mu_List, {i_Integer}] := Boole[mu === {i}];
InverseKostkaKHelper[lam_List, mu_List] := InverseKostkaKHelper[lam, mu] =
	-Sum[
		With[{pos = Position[lam, mu[[j]] + j - 1, 1, 1]},
			If[pos == {}, 0
			, (-1)^(j)
				InverseKostkaKHelper[
				(* Remove a part equal to p *)
				
				ReplacePart[lam, pos -> Nothing]
				,
				(* Subtract 1 from the first j-1 entries, 
				and drop jth entry *)
				
				DeleteCases[
					MapIndexed[#1 - Boole[#2[[1]] < j] &, Drop[mu, {j}]], 0]
				]
			]
			]
		, {j, Length@mu}];
(********************************************************************)


ChangeSymmetricAlphabet[expr_, to_, from_: None] := If[ 
	to === from,
	expr,
	(expr /. MonomialSymbol[mu__, from] :> MonomialSymbol[mu, to])];
	
	


AugmentedMonomialSymmetric[lam_List] := Times @@ (PartitionPartCount[lam]!) MonomialSymbol[lam];


CompleteHSymmetric[{}, x_: None] := 1;
CompleteHSymmetric[d_Integer, x_: None] := If[d < 0, 0, Sum[MonomialSymbol[mu, x], {mu, IntegerPartitions[d]}]];
CompleteHSymmetric[lam_List, x_: None] := CompleteHSymmetric[lam, x] = Expand[Product[CompleteHSymmetric[k, x], {k, lam}]];

ElementaryESymmetric[{}, x_: None] := 1;
ElementaryESymmetric[d_Integer, x_: None] := If[d < 0, 0, MonomialSymbol[ConstantArray[1, d], x]];
ElementaryESymmetric[lam_List, x_: None] := ElementaryESymmetric[lam, x] = Expand[Product[ElementaryESymmetric[k, x], {k, lam}]];

UnitTest[ElementaryESymmetric] := With[{m = 5},
Expand[Sum[
	(-1)^i ElementaryESymmetric[i] CompleteHSymmetric[m - i], {i, 0,
		m}]] === 0
];


PowerSumSymmetric[{}, x_: None] := 1;
PowerSumSymmetric[0, x_: None] := 1;
PowerSumSymmetric[d_Integer, x_: None] := If[d < 0, 0, MonomialSymbol[{d}, x]];
PowerSumSymmetric[lam_List, x_: None] := PowerSumSymmetric[lam, x] = Expand[Product[PowerSumSymmetric[k, x], {k, lam}]];


PositiveCoefficientsQ::notnumber = "The coefficient `1` is not a number.";
PositiveCoefficientsQ::usage = "PositiveCoefficientsQ[expr, basis] returns true if all coeffs of basis[..] are positive. Only works for numbers.";
PositiveCoefficientsQ[expr_, basisSymbol_: ss, extraSymbols_:{}] := 
	With[{exp = Expand@expr},
		And @@ 
			Map[
			With[{c=Last[#]},
				If[ NumberQ[c], 
					c>=0,
					Message[PositiveCoefficientsQ::notnumber,c];
					Abort[]
				]
			]
			&
			, CoefficientRules[exp, 
				Join[ Cases[exp, basisSymbol[___], Infinity], extraSymbols] ]
			]
];


(******************************************************)

HSYMBOLIC::usage = "HSYMBOLIC represents the complete homogeneous symmetric function.";

(* TODO -- Use this for Schur. The ideas is to reuse for different alphabets. *)
SymbolicJacobiTrudi[{lam_, mu_}, hh_:HSYMBOLIC] := SymbolicJacobiTrudi[{lam, mu}, hh] =
Module[{mup = PadRight[mu, Length@lam], i, j, jtDet},
		jtDet = Expand@If[Length[DeleteCases[mu, 0]] > Length[lam],
					0,
					Det@Table[
						hh[lam[[i]] - mup[[j]] - i + j],
					{i, Length@lam}, {j, Length@lam}]];
		Expand[jtDet]
];


(* Uses the Jacobi--Trudi identity, and quick Kostka coeffs. Also includes skew version. *)
SchurSymmetric[lam_List, x_: None] := SchurSymmetric[{lam, {}}, x];

(* Slinky rule. *)
SchurSymmetric[{lam_List, {}}, x_: None] := With[{slr=CompositionSlinky[lam]},
	If[slr[[2]]==0,
	0, 
	slr[[2]]*SchurSymmetric[{slr[[1]], {}}, x]
	]
] /; Not[OrderedQ[Reverse@lam]];


(* Memoization seem to have an issue when using default parameters(?) *)
SchurSymmetric[{lam_List, mu_List}]:=SchurSymmetric[{lam, mu}, None];
SchurSymmetric[{lam_List, mu_List}, x_:None] := SchurSymmetric[{lam, mu}, x] = Module[
	{i, j, jtDet, bb, mup = PadRight[mu, Length@lam]},
	
	(* Jacobi--Trudi determinant *)
	jtDet[ll_, mm_, 0] := 1;
	jtDet[ll_, mm_, d_] := Expand[Det@Table[
		bb[ll[[i]] - mm[[j]] - i + j],
		{i, d},
		{j, d}]];

(* Use the smallest matrix. *)
Expand@Which[
	!PartitionLessEqualQ[mu, lam],
		0
	,
	
	Length[lam]==0, 
		1
	,
	
	(* In the non-skew case, use the quick monomial recursion for Kostka coefficients. *)
	Tr[mu]==0,
		Sum[
			KostkaCoefficient[lam, nu,1] MonomialSymbol[nu,x]
		,{nu,IntegerPartitions[Tr@lam]}]
	,
	Length[lam] >= lam[[1]],
		With[{d = lam[[1]]},
		jtDet[
			PadRight[ConjugatePartition[lam], d],
			PadRight[ConjugatePartition[mu], d], d]
		] /. bb[v_] :> ElementaryESymmetric[v, x]
	,
	True,
		With[{d = Length[lam]},
			jtDet[PadRight[lam, d], PadRight[mu, d], d]
		] /. bb[v_] :> CompleteHSymmetric[v, x]
	]
];


(* Private Util function for converting between bases. *)

PartitionIndexedBasisRule[size_Integer, basisSymb_, toBasis_, 
	monom_: MonomialSymbol, x_: None] := 
	PartitionIndexedBasisRule[size, basisSymb, toBasis, monom, x] = Module[{parts, mat, imat,p,q},
	parts = IntegerPartitions[size];
	
	(*
	Print["PartitionIndexedBasisRule for ", {size, basisSymb, toBasis, monom, x}]; 
	*)
	
	(* Matrix with the toBasis expanded in monomials. *)
	(* Here we use the "None" alphabet *)
	
	mat = Table[
		Table[
		Coefficient[toBasis[p], monom[q] ], {q, parts}]
		, {p, parts}];
	
	(* Here we compute the matrix. *)
	imat = Inverse[mat];
	
	(* Use dispatch for quicker rule. *)
	Dispatch@Table[
		monom[parts[[p]], x] -> Expand@Together@Sum[ basisSymb[parts[[q]]]*imat[[p, q]]
			, {q, Length@parts}]
	, {p, Length@parts}]
];

ToOtherSymmetricBasis[basis_, pol_, newSymb_, x_: None, mm_: MonomialSymbol] :=
Module[{mmVars, deg, maxDegree, monomList},
	mmVars = Cases[Variables[pol], mm[__, x], {0, Infinity}];
	maxDegree = Max[0, mmVars /. mm[lam__, x] :> Tr[lam]];
	If[maxDegree == 0,
	
		pol,
		
		(* If degree > 0 *)
		monomList = MonomialList[pol, mmVars];
		
		Expand@Sum[
			deg = Max@Cases[mon, mm[lam__, x] :> Tr[lam], {0, Infinity}];
		If[deg <= 0, 
			mon, 
			mon /. PartitionIndexedBasisRule[deg, newSymb, basis, mm, x]]
		, {mon, monomList}]]
];

ToSchurBasis[poly_, ss_, x_: None] := 
ToOtherSymmetricBasis[SchurSymmetric, poly, ss, x];
ToPowerSumBasis[poly_, pp_, x_: None] := 
ToOtherSymmetricBasis[PowerSumSymmetric, poly, pp, x];
ToElementaryEBasis[poly_, ee_, x_: None] := 
ToOtherSymmetricBasis[ElementaryESymmetric, poly, ee, x];
ToCompleteHBasis[poly_, hh_, x_: None] := 
ToOtherSymmetricBasis[CompleteHSymmetric, poly, hh, x];

(* A normalized version, where we multiply by the z-coefficient. *)
ToPowerSumZBasis[poly_, pp_, x_: None] := (ToPowerSumBasis[poly,pp,x]/.pp[lam_] :> ZCoefficient[lam] pp[lam]);

(* The forgotten basis is given by omega(monomial) *)

ForgottenSymmetric[mu_List, x_: None] := 
ForgottenSymmetric[mu, x] = Module[{pp},
	Expand[
	ToPowerSumBasis[MonomialSymbol[mu, x], pp] /. 
	pp[lam_] :> (-1)^(Tr[lam] - Length[lam]) PowerSumSymmetric[lam, 
		x]]];

OmegaInvolution[poly_, x_: None] := Module[{pp},
Expand[
	Expand[poly] /. 
	MonomialSymbol[mu_, x] :> ForgottenSymmetric[mu, x]]
];
UnitTest[OmegaInvolution] := And[
OmegaInvolution@OmegaInvolution@SchurSymmetric[{4, 2, 1}] === 
	SchurSymmetric[{4, 2, 1}],
OmegaInvolution@ElementaryESymmetric[{4, 2, 1}] === 
	CompleteHSymmetric[{4, 2, 1}]
];




(***********************************************************************************)

SymmetricFunctionDegree::usage = "SymmetricFunctionDegree[expr] returns the degree of the symmetric function.";
SymmetricFunctionDegree[expr_, yy_: None] := Max[
	Cases[expr, MonomialSymbol[mu_List, yy] :> Tr[mu],{0,Infinity}]];

SymmetricFunctionToPolynomial::usage = "SymmetricFunctionToPolynomial[expr,x,[n]] expresses the function as a polynomial in n variables.";

SymmetricFunctionToPolynomial[MonomialSymbol[mu_List, None], x_, 0] := 0;
SymmetricFunctionToPolynomial[MonomialSymbol[mu_List, None], x_, n_Integer] := 
Which[
Length[mu]>n,0,
True, Sum[
   Times @@ ((x /@ Range[n])^p)
   , {p, Permutations[PadRight[mu, n]]}]
];

SymmetricFunctionToPolynomial[expr_, x_] := SymmetricFunctionToPolynomial[expr, x, SymmetricFunctionDegree[expr]];

SymmetricFunctionToPolynomial[expr_, x_, n_Integer, yy_: None] := 
  ReplaceAll[
   expr,
   MonomialSymbol[mu_List, yy] :> 
    SymmetricFunctionToPolynomial[MonomialSymbol[mu, None], x, n]];


(***********************************************************************************)





(* Todo --- Make more efficient *)

PrincipalSpecialization::usage = "PrincipalSpecialization[poly,q,[k],[x]], 
gives the principal specialization. The parameter k tells how many 
variables to replace - this can be infinity.";

PrincipalSpecialization[poly_, q_, k_: Infinity, x_: None] := Module[{psMon, pp},
	psMon[mu_List] := With[{
		ppc = PartitionPartCount[mu],
		ell = Length@mu},
		Product[k - j, {j, 0, ell - 1}]/(Times @@ (ppc!))
	];
	
	Which[
		k === Infinity,
			ToPowerSumBasis[poly, pp, x] /. 
			pp[lam_] :> 1/Product[1 - q^i, {i, lam}],
		
		
		(* This is quite efficient. *)
		q === 1,
			Expand[poly] /. MonomialSymbol[lam_, x] :> psMon[lam],
		
		True,
			ToPowerSumBasis[poly, pp, x] /. 
			pp[lam_] :> Product[Sum[q^(i (j - 1)), {j, k}], {i, lam}]
		]
];


SnCharacter[lam_List, nu_List] := SnCharacter[{lam, {}}, nu];
SnCharacter[{lam_List, mu_List}, nu_List] := Module[{schur, pp, n = Tr[lam] - Tr[mu], inP},
schur = SchurSymmetric[{lam, mu}];
inP = ToPowerSumBasis[schur, pp];
(* Memoize all *)
Do[
	SnCharacter[{lam, mu}, tau] = 
	ZCoefficient[tau] Coefficient[inP, pp[tau]];
	, {tau, IntegerPartitions@n}];
SnCharacter[{lam, mu}, nu]
];

HallInnerProduct[f_, g_] := HallInnerProduct[f, g, {1, 1}, None];
HallInnerProduct[f_, g_, {q_, t_}, x_: None] := Module[{
	ff, gg, pp, lam,
	vars, rulesF, rulesG, vF, vG, rF, rG
	},
	
	
	(* Convert to powersum *)
	
	ff = ToPowerSumBasis[f, pp, x];
	gg = ToPowerSumBasis[g, pp, x];
	
	vars = Cases[Variables[{ff, gg}], pp[__], {0, Infinity}];
	rulesF = CoefficientRules[ff, vars];
	rulesG = CoefficientRules[gg, vars];
	

(* Combine all power-sum terms. *)
Sum[
	vF = First@rF;
	vG = First@rG;
	
	(* Special cases depending on constant term. *)
	If[ vF === {}, 
		lam = {1}
		,
		lam = Pick[vars, vF, 1][[1, 1]]
	];
	
	If[vF != vG, 
		0,
		Last[rF] * Last[rG] * ZCoefficient[lam] *
		If[q === t, 1, Product[(1 - q^p)/(1 - t^p), {p, lam}]]
	]
	, {rF, rulesF}, {rG, rulesG}]
];

UnitTest[HallInnerProduct] := And[
1 == HallInnerProduct[SchurSymmetric[{2, 1}], 
	SchurSymmetric[{2, 1}]],
1 == HallInnerProduct[MonomialSymbol[{2, 1}], 
	CompleteHSymmetric[{2, 1}]],
40 == HallInnerProduct[5 SchurSymmetric[{2, 1}], 
	8 SchurSymmetric[{2, 1}]],
0 == HallInnerProduct[5 SchurSymmetric[{2, 1}], 
	8 SchurSymmetric[{3}]],
Block[{a, b, c, d},
	Expand[5 (c + d) - 10 (a + b)] == 
	Expand[HallInnerProduct[
	5 SchurSymmetric[{2, 1}] - (a + b) SchurSymmetric[{3}]
	, (c + d) SchurSymmetric[{2, 1}] + (10) SchurSymmetric[{3}]]]
	]
];



(* Computes f(-x1,-x2,...) *)
NegateAlphabet[f_, x_: None] := Expand[f] /. MonomialSymbol[mu__, x] :> (-1)^(Tr@mu) MonomialSymbol[mu, x];


(* Here, f is in the None alphabet, and plethysm act on ALL given \
alphabets in the G-expression. *)
Clear[Plethysm];
Plethysm[f_, g_, alphabets_: {None}] := 
Module[{PkPlethysmWithG, fpp, gpp, fInP, gInP,
	fVars, gVars, auxVars, cF, k, v, vF
	},
fInP = ToPowerSumBasis[f, fpp, None];
gInP = Fold[ToPowerSumBasis[#1, gpp[#2], #2 ] &, g, alphabets];

fVars = Cases[Variables[fInP], fpp[__], {0, Infinity}];
gVars = Cases[Variables[gInP], gpp[_][__], {0, Infinity}];
auxVars = Complement[Variables[{fInP, gInP}], fVars, gVars];

	PkPlethysmWithG[
	k_] :=
	(gInP /. 
	Table[v -> v^k, {v, auxVars}]) /. {gpp[x_][mu__] :> 
	gpp[x][k mu]};

Expand@(Sum[
	cF = Coefficient[fInP, vF];
	cF Product[ PkPlethysmWithG[k], {k, First@vF}]
	, {vF, fVars}] /. gpp[x_][mu__] :> PowerSumSymmetric[mu, x])
];

UnitTest[Plethysm] := And[
Plethysm[PowerSumSymmetric[{7}], PowerSumSymmetric[{1}]/(1 - q)] ==

		PowerSumSymmetric[{7}]/(1 - q^7),
ToSchurBasis[Plethysm[SchurSymmetric[{4}], SchurSymmetric[{2}]], 
	ss] ==
	ss[{8}] + ss[{4, 4}] + ss[{6, 2}] + ss[{4, 2, 2}] + 
	ss[{2, 2, 2, 2}]
];



JackPSymmetric::usage = "JackPSymmetric[lam,a] returns the Jack P normalization of Jack functions.";
JackPSymmetric[lam_List,a_, x_: None] := JackPSymmetric[lam,a,x] = Sum[
	KostkaCoefficient[lam,mu,a] MonomialSymbol[mu,x]
,{mu,IntegerPartitions[Tr@lam]}];

JackJSymmetric::usage = "JackJSymmetric[lam,a] returns the Jack J normalization of Jack functions.";
JackJSymmetric[lam_List,a_, x_: None] := Together[JackPSymmetric[lam,a,x] Product[
	a*PartitionArm[lam,s] + PartitionLeg[lam,s]+1
,{s,ShapeBoxes[lam]}]];


(* This is not the modified Hall-Littlewood! *)
(*
http://igm.univ-mlv.fr/~fpsac/FPSAC02/ARTICLES/Tudose.pdf
*)
HallLittlewoodTSymmetric[lam_List, q_, x_: None] := 
HallLittlewoodTSymmetric[lam, q, x] = Module[{Rij,
	res, operators, n = Tr@lam, applyIJ, hh, qq},
	
	Rij[vec_List, i_Integer, j_Integer, k_Integer] := 
	ReplacePart[vec,
	{i -> vec[[i]] + k, j -> vec[[j]] - k}];
	
	(* Applies (1-Rij)/(1-qR_ij) to the polynomial *)
	
	applyIJ[i_, j_][poly_] := Module[{max, out},
	max = (n - j) + Max@Cases[poly, hh[a_] :> a[[j]], {0, Infinity}];
	out = poly +
		Sum[
		poly /. 
		hh[a_List] :> (qq - 1) qq^(k - 1) hh[Rij[a, i, j, k]]
		, {k, max}];
	Expand[out /. {hh[a_] /; Min[a] < 0 :> 0}]
	];
	
	(* Operators must appear in this order, in order to optimize *)

	operators = 
	Reverse[Join @@ 
	Table[applyIJ[i, j], {j, n, 1, -1}, {i, j - 1, 1, -1}]];
	
	res = Composition[Sequence @@ operators][ hh[PadRight[lam, n]] ];
	(* Replace with complete homogeneous sym funcs.*)
	
	Expand[res /. {hh[a_] :> CompleteHSymmetric[a, x], qq -> q}]
	];

(* Example 3.1 in https://www.mat.univie.ac.at/~slc/s/s32leclerc.pdf *)

UnitTest[HallLittlewoodTSymmetric] := And[
Block[{q, ss},
	SameQ[
	Coefficient[
	ToSchurBasis[HallLittlewoodTSymmetric[{2, 2, 2, 2}, q], ss], 
	ss[{3, 3, 2}]],
	(q^3 + q^4 + q^5)]]
];



SchursQSymmetric::usage = "SchursQSymmetric[lam]";
SchursQSymmetric[lam_List, x_: None]:=SchursQSymmetric[lam,x] = Plethysm[
	HallLittlewoodTSymmetric[lam, q,x]
	, (1 - q) PowerSumSymmetric[{1},x],{x}] /. q -> -1;

SchursPSymmetric[lam_List, x_: None]:=Together[SchursQSymmetric[lam,x]/2^Length[lam]];

(* This is the modified Hall-Littlewood polynomial. *)

HallLittlewoodMSymmetric[lam_List, q_, x_: None] := 
Together[q^PartitionN[lam] HallLittlewoodTSymmetric[lam, 1/q, x]];


MacdonaldPSymmetric[lam_List, q_, t_, x_: None] := Module[{},
ChangeSymmetricAlphabet[
	MacdonaldPSymmetricHelper[lam, SPECIALQ, SPECIALT]
	, x] /. {SPECIALQ -> q, SPECIALT -> t}
];

(* 7.13' p. 346 in Macdonald's book. *)
MacdonaldPSymmetricHelper[mu_List, q_, t_, x_: None] :=MacdonaldPSymmetricHelper[mu,q,t,x]=
Together@Sum[
   MonomialSymbol[YoungTableauWeight[ssyt],x]
    *
    With[{
      ribbons = Table[
        YoungTableauShape[ssyt, i]
        , {i, Max[ssyt]}]}
     ,
     Product[
      MacdonaldPsi[rib, q, t]
      , {rib, Partition[ Reverse@ribbons , 2, 1]}]
     ]
   , {ssyt,
    SemiStandardYoungTableaux[{mu, {}}, Tr@mu]
 }];



(* We always compute with the None alphabet *)
(* Here, we need to have some private variables. *)
MacdonaldPSymmetricOld[lam_List, q_, t_, x_: None] := Module[{},
ChangeSymmetricAlphabet[
	MacdonaldPPolynomialHelper[lam, SPECIALQ, SPECIALT]
	, x] /. {SPECIALQ -> q, SPECIALT -> t}
];

MacdonaldPPolynomialHelper[lam_List, q_, t_] := Module[
{uu, n = Tr@lam, ips, PP, polys, ppExpr, eqns1, eqns2, vars, sol},
ips = IntegerPartitions[n];

(* Generic expression for Macdonald polynomial. *)

ppExpr[nu_] :=
	MonomialSymbol[nu] + Sum[
	uu[nu, mu] MonomialSymbol[mu]
	, {mu, Select[ips, PartitionStrictDominatesQ[#, nu] &]}];

eqns1 = Table[PP[nu] == ppExpr[nu], {nu, ips}];
eqns2 = Join @@ Table[
	If[mu == nu,
	Sequence @@ {},
	HallInnerProduct[ppExpr[mu], ppExpr[nu], {q, t}] == 0
	]
	, {mu, ips}, {nu, ips}];

vars = Join[
	Table[PP[mu], {mu, ips}],
	Join @@ Table[uu[nu, mu]
	, {nu, ips}
	, {mu, Select[ips, PartitionStrictDominatesQ[#, nu] &]}]
	];

sol = Solve[Join[eqns1, eqns2], vars][[1]];
Do[
	MacdonaldPPolynomialHelper[mu, q, t] = Together[(PP[mu] /. sol)];
	, {mu, ips}];
MacdonaldPPolynomialHelper[lam, q, t]
];



(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)




MacdonaldHSymmetric::usage = "MacdonaldHSymmetric[lam,q,t] is the modified Macdonald polynomial.";
MacdonaldHSymmetric[lam_List, q_, t_] := MacdonaldHSymmetric[{lam, {}}, q, t];


MacdonaldHSymmetric[{lam_List, mu_List}, q_, t_] := MacdonaldHSymmetric[{lam,mu},q,t]=
MacdonaldHSymmetric[{lam, mu}, SPECIALQ, SPECIALT] /. {SPECIALQ -> q, SPECIALT -> t};

(* This is computed via the F-expansion, using the slinky rule *)
MacdonaldHSymmetric[{lam_List, mu_List}, SPECIALQ, SPECIALT] := MacdonaldHSymmetric[{lam,mu}, SPECIALQ, SPECIALT] = Module[
	{template, n = Tr[lam] - Tr[mu], tab, p, inv, maj, rw, desSet, alpha},
 
	template = SuperStandardTableau[{lam, mu}];
	
	Expand@Sum[
		tab = template /. Thread[Range[n] -> p];
		{inv, maj} = InvMajStatistic[tab];
		rw = SYTReadingWord@(Transpose[tab]);
		desSet = DescentSet@Ordering[rw];
		alpha = DescentSetToComposition[desSet, n];
		
		SchurSymmetric[alpha]
		SPECIALQ^inv SPECIALT^maj
	, {p, Permutations[Range[n]]}]
];



ToMacdonaldHBasis[poly_, mh_, q_, t_, x_: None] :=  Expand@Together@ToOtherSymmetricBasis[ MacdonaldHSymmetric[#,q,t]&,  poly, mh, x];


PostfixedCharge[mu_List, w_List] := Module[{postFix, decomp},
   postFix = 
    Reverse[Join @@ 
      MapIndexed[ConstantArray[#2[[1]], #1] &, ConjugatePartition@mu]];
   decomp = ChargeWordDecompose[Join[w, postFix]];
   Total[PermutationCharge /@ decomp]
   ];

   
   
   
SkewMacdonaldESymmetric::usage = "SkewMacdonaldESymmetric[{lam,mu},q] gives the skew Macdonald polynomial.";

SkewMacdonaldESymmetric[lam_List, q_] := SkewMacdonaldESymmetric[{lam,{}}, q];

SkewMacdonaldESymmetric[{lam_List, mu_List}, q_] := 
SkewMacdonaldESymmetric[{lam, mu}, SPECIALQ] /. {SPECIALQ -> q};

SkewMacdonaldESymmetric[{lam_List, mu_List}, SPECIALQ] := SkewMacdonaldESymmetric[{lam,mu},SPECIALQ]=
  Module[{alpha, n = Tr[lam] - Tr[mu],t,
    nn = Tr[lam], muc = ConjugatePartition[mu]},
   alpha = PadRight[ConjugatePartition[lam], nn] - PadRight[muc, nn];
   Sum[
    SchurSymmetric[nu]
     Sum[
      SPECIALQ^PostfixedCharge[mu, SYTReadingWord[t]]
      , {t, 
       SemiStandardYoungTableaux[{ConjugatePartition@nu, {}}, 
        alpha]}]
    , {nu, IntegerPartitions[n]}]
   ];


(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)




LLTSymmetric::usage = "LLTSymmetric[nu,q] returns the LLT polynomial associated with the tuple of skew shapes.";

LLTSymmetric[nu_List, q_] := LLTSymmetric[nu,q] = Module[
{rwWithContent, ttInv, sizes, lam, mu, templateList, tableauList,
	tabValues, perms, p, contentRW, rwPerm, n,
	tabTuples, desSet, alpha, inv
	},

(* Returns a list (entry, c-r,r,i) *)
(* 
Used to construct the reading word. *)

rwWithContent[YoungTableau[tab_], i_: 1] :=
	
	Join @@ MapIndexed[
	If[IntegerQ[#1], {#1, #2.{-1, 1}, #2[[1]], i}, Nothing] &,
	tab, {2}];

(* 
Computes inversions between two tableaux.
	*)
ttInv[tab1_YoungTableau, tab2_YoungTableau] := With[
	{rw1 = rwWithContent[tab1],
	rw2 = rwWithContent[tab2]},
	Sum[
	Boole[Or[
		a[[2]] == b[[2]] && a[[1]] > b[[1]],
		a[[2]] - 1 == b[[2]] && a[[1]] < b[[1]]
		]
	], {a, rw1}, {b, rw2}]
	];

(* Number of boxes in each (skew) shape. *)
sizes = Table[
		If[ IntegerQ[nui[[1]]], 
			Tr[nui],
			Tr[nui[[1]]] - Tr[nui[[2]]]
		]
	, {nui, nu}];

n = Tr[sizes];

(* All combinations of SYT on the shapes. *)

templateList = Flatten[
	Outer[List, Sequence @@ (StandardYoungTableaux /@ nu), 1],
	Length[nu] - 1];

(* All ways to distribute entries amongst the shapes. *)

perms = Permutations@(Join @@ 
	MapIndexed[ConstantArray[#2[[1]], #1] &, sizes]);
tabValues = PartitionList[Ordering[#], sizes] & /@ perms;

(* Combine all SYT with distributing entries. *)

tabTuples = Join @@ Table[
	MapThread[#2 /. (Thread[Range[#3] -> #1]) &
	, {tv, template, sizes}, 1]
	, {tv, tabValues},
	{template, templateList}];

Expand@Sum[
	contentRW = Join @@ MapIndexed[rwWithContent[#1, #2[[1]]] &, tt];
	rwPerm = 
	First /@ SortBy[contentRW, {#[[2]] &, #[[4]] &, #[[3]] &}];
	desSet = DescentSet@Ordering[rwPerm];
	alpha = DescentSetToComposition[desSet, n];
	
	inv = Total[ttInv @@@ Subsets[tt, {2}]];
	q^inv SchurSymmetric[alpha]
	, {tt, tabTuples}]
];



(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)

SymplecticSchurSymmetric::usage = "SymplecticSchurSymmetric[mu] gives the symplectic Schur function.";
SymplecticSchurSymmetric[{}] := 1;
SymplecticSchurSymmetric[lam_List] := Expand[(1/2) Det@Table[
      CompleteHSymbol[lam[[i]] - i + j] + 
       CompleteHSymbol[lam[[i]] - i - j + 2], {i, Length@lam}, 
		{j, Length@lam}]];


OrthogonalSchurSymmetric::usage = "OrthogonalSchurSymmetric[mu] gives the orthogonal Schur function.";
OrthogonalSchurSymmetric[{}] := 1;
OrthogonalSchurSymmetric[lam_List] := Det@Table[
    CompleteHSymbol[lam[[i]] - i + j] - 
     CompleteHSymbol[lam[[i]] - i - j], 
	{i, Length@lam}, {j, Length@lam}];


	
	
PetrieSymmetric::usage = "PetrieSymmetric[k,m] gives the degree-m part of the kth Petrie symmetric function.";
PetrieSymmetric[k_Integer,0,x_:None]:= 1;
PetrieSymmetric[k_Integer,m_Integer,x_:None]:= Sum[
		MonomialSymbol[lam,x]
,{lam, Select[IntegerPartitions[m], #[[1]]<k &] }];




CylindricSchurSymmetric::usage = "CylindricSchurSymmetric[{lam,mu}, d] gives a cylindric Schur function.";

CylindricSchurSymmetric[{lam_List, mu_List}, d_Integer: 0,x_:None]:=Sum[
	MonomialSymbol[YoungTableauWeight@ssyt,x]
,{ssyt, CylindricTableaux[{lam, mu}, d]}];



(* Based on 7.10c in https://arxiv.org/pdf/1907.02645.pdf *)
LahSymmetricFunction[n_Integer, k_Integer] := LahSymmetricFunction[n, k] = Expand[
	((n - 1)!/(k - 1)!) Sum[
			With[{ll = Table[Count[alpha, j], {j, n - k}]},
				(-1)^Tr[ll - 1] (Multinomial @@ Append[ll, n - 1])
				Product[(CompleteHSymbol[j]/(j + 1))^ll[[j]], {j, n - k}]
				]
			, {alpha, IntegerPartitions[n - k]}]
];

LahSymmetricFunctionNegative[n_Integer, k_Integer] := LahSymmetricFunctionNegative[n, k] = Expand[
	((n - 1)!/(k - 1)!) Sum[
			With[{ll = Table[Count[alpha, j], {j, n - k}]},
				(-1)^Tr[ll - 1] (Multinomial @@ Append[ll, n - 1])
				Product[(ElementaryESymbol[j]/(j + 1))^ll[[j]], {j, n - k}]
				]
			, {alpha, IntegerPartitions[n - k]}]
];



(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)


	
	
DeltaOperator::usage = "DeltaOperator[f,g,q,t] is the Garsia Delta operator.";

DeltaOperator[f_, g_, q_, t_] := DeltaOperator[f, g, q, t] = Module[{x, val, inH, monoms},
		(* Monomials defined by shape. *)
		
		monoms[mu_List] := (q^(#1 - 1) t^(#2 - 1) & @@@ ShapeBoxes[mu]);
		
		(* This correspond to f[B(mu)] -- it is quicker than plethysm *)

		val[mu_List] := SymmetricFunctionToPolynomial[f, x, Tr[mu]] /. x[i_] :> monoms[mu][[i]];
		
		inH = Expand[ToMacdonaldHBasis[g, "hh", q, t]];
		Together[inH /. "hh"[lam_List] :> val[lam] MacdonaldHSymmetric[lam, q, t]]
];

NablaOperator[g_, q_, t_] :=
DeltaOperator[ElementaryESymmetric[SymmetricFunctionDegree[g]], g, q, t];

DeltaPrimOperator[f_, g_, q_, t_] := DeltaPrimOperator[f, g, q, t] = Module[{x, val, inH, monoms},
		(* Monomials defined by shape. *)
		
		monoms[mu_List] := 
		Rest[q^(#1 - 1) t^(#2 - 1) & @@@ ShapeBoxes[mu]];
		
		(* This correspond to f[B(mu)-1] -- it is quicker than plethysm *)

				val[mu_List] := 
		SymmetricFunctionToPolynomial[f, x, Tr[mu] - 1] /. 
			x[i_] :> monoms[mu][[i]];
		
		inH = Expand[ToMacdonaldHBasis[g, "hh", q, t]];
		Together[
		inH /. "hh"[lam_List] :> 
			val[lam] MacdonaldHSymmetric[lam, q, t]]
];


End[(* End private *)];
EndPackage[];
