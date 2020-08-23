(* ::Package:: *)


(* TODO 

--- Make converting to new bases easier. 
		Might be an issue if these are not expressed in the monomial basis, but should be rare.

--- Split Schur into Schur and SkewSchur, the latter returning expression in e.
		
--- Allow standard basis to handle
		-Hall inner prod

--- Improve Sn-character code
	
--- Hard-code algorithms for computing e-to-m and h-to-m using Kostka coeffs / other formulas instead?
			
--- Make change-of-basis accept several alphabets at once?

--- See what attributes can be used (listable, orderless, etc).

--- Add DeclarePackage instead of loading NewTableaux.

--- Make a unit-test file.

--- Update documentation.

*)

ClearAll["SymmetricFunctions`*"];
BeginPackage["SymmetricFunctions`"];
Needs["CombinatoricsUtil`"];
Needs["NewTableaux`"];


KostkaCoefficient;
InverseKostkaCoefficient;
LRCoefficient;
LRExpand;

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

(* This expands into e's *)
SkewSchurSymmetric;

ToSchurBasis;
ToPowerSumBasis;
ToPowerSumZBasis;
ToElementaryEBasis;
ToCompleteHBasis;
ToMonomialBasis;

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
MacdonaldHSymbol;

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

coreBasesList=(MonomialSymbol|SchurSymmetric|ElementaryESymbol|CompleteHSymbol|ForgottenSymbol|PowerSumSymbol);

(* Abstract function for defining a symmetric function basis,
	with formatting and basic multiplication. *)
Options[createBasis] = {
		MultiplicationFunction -> None, 
		SortFunction -> "Standard",
		PowerFunction -> "Mult"
};


(* TODO: Break this into smaller pieces, so that formatting code is separate *)

createBasis[bb_, symb_String, opts:OptionsPattern[]] := Module[{sort,mult,pow},
	
	bb[lam_List] := bb[lam, None];
	bb[i_Integer] := Which[i>0, bb[{i}, None], i==0, 1, True, 0];
	bb[i_Integer, x_] := Which[i>0, bb[{i}, x], i==0, 1, True, 0];
	
	bb[{}, x_: None] := 1;
	bb[{0}, x_: None] := 1;
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
		(* Use recursion, via multiplication. *)
		pow === "Mult",
			bb /: Power[bb[a_List, x_], 2] := mult[a, a, x];
			bb /: Power[bb[a_List, x_], n_Integer] := Expand@Which[
			EvenQ[n],
				Power[bb[a, x], n/2] * Power[bb[a, x], n/2],
			True,
				Power[bb[a, x], (n-1)/2] * Power[bb[a, x], (n-1)/2]*bb[a, x]
			];
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
		
		
		
		
(* (0.3) in Okounkov-Olshanski, https://arxiv.org/pdf/q-alg/9605042.pdf *)

shiftedSchurDet[lambda_List, mu_List] := Module[
	{ff, ll, xx, n = Max[Length@lambda, Length@mu]},
	ff[a_Integer, 0] := 1;
	ff[a_Integer, k_Integer] := Product[a - j, {j, 0, k - 1}];
	ll = PadRight[lambda, n];
	xx = PadRight[mu, n];
	Divide[
		Det[Table[ff[xx[[i]] + n - i, ll[[j]] + n - j], {i, n}, {j, n}]]
		,
		Det[Table[ff[xx[[i]] + n - i, n - j], {i, n}, {j, n}]]
	]
];

(* Base cases are values of shifted Schur functions. *)
LRCoefficient[lambda_List, mu_List, mu_List] := LRCoefficient[lambda, mu, mu] = shiftedSchurDet[lambda, mu];
LRCoefficient[nu_List, mu_List, nu_List] := LRCoefficient[nu, mu, nu] = shiftedSchurDet[mu, nu];

(* General recursion - Based on recursion by Molev-Sagan, (1999) *)
LRCoefficient[lambda_List, mu_List, nu_List] := LRCoefficient[lambda, mu, nu] =
	Which[
	! PartitionLessEqualQ[mu, nu] || ! 
		PartitionLessEqualQ[lambda, nu], 0
	,
	(* Interchange accoring to size. *)
	Tr[lambda] > Tr[mu], 
	LRCoefficient[mu, lambda, nu]
	,
	(* Recursion *)
	True,
	1/(Tr@nu - Tr@mu) (
		Sum[LRCoefficient[lambda, mup, nu], {mup, PartitionAddBox[mu]}] -
		Sum[
		LRCoefficient[lambda, mu, num], {num, 
			PartitionRemoveBox[nu]}]
		)
];



LRExpand[expr_, x_: None] := Module[{schurProduct, schurPower, rule},
	schurProduct[lam_List, mu_List] := Sum[
		LRCoefficient[lam, mu, nu] SchurSymbol[nu, x]
		, {nu, IntegerPartitions[Tr[lam] + Tr[mu]]}];

	schurPower[lam_List, 0] := 1;
	schurPower[lam_List, 1] := SchurSymbol[lam, x];
	schurPower[lam_List, d_Integer] := Which[
		d == 2, schurProduct[lam, lam],
		EvenQ[d], LRExpand[schurPower[lam, (d/2)]*schurPower[lam, (d/2)]],
		True, 
		LRExpand[
		schurPower[lam, (d - 1)/2]*schurPower[lam, 1 + (d - 1)/2]]
		];
	(* This expands products and powers with the LR-rule. *)
	rule = {
		Times[SchurSymbol[lam_List, x], SchurSymbol[mu_List, x]] :> 
		schurProduct[lam, mu]
		,
		Power[SchurSymbol[lam_List, x], d_] :> schurPower[lam, d]
		};
	(* Repeatedly expand and use LR-rule. *)

	FixedPoint[Expand[#] /. rule &, expr]
];
		
		
(********************************************************************)



ChangeSymmetricAlphabet[expr_, to_, from_: None] := If[ 
	to === from,
	expr,
	(expr /. (bb:coreBasesList)[mu__, from] :> bb[mu, to])];


AugmentedMonomialSymmetric[lam_List] := Times @@ (PartitionPartCount[lam]!) MonomialSymbol[lam];


(* This setup is for making sure memoization works. *)
CompleteHSymmetric[a_]:=CompleteHSymmetric[a,None];
CompleteHSymmetric[{}, None] := 1;
CompleteHSymmetric[d_Integer, None] :=  Which[
	d < 0, 0, 
	d==0, 1,
	True, Sum[MonomialSymbol[mu], {mu, IntegerPartitions[d]}]];
CompleteHSymmetric[lam_List, None] := CompleteHSymmetric[lam, None] = Expand[
	CompleteHSymmetric[First@lam, None]*CompleteHSymmetric[Rest@lam, None]
];
CompleteHSymmetric[{}, x_] := 1;
CompleteHSymmetric[lam_List, x_] := ChangeSymmetricAlphabet[CompleteHSymmetric[lam,None],x];


ElementaryESymmetric[a_]:=ElementaryESymmetric[a,None];
ElementaryESymmetric[{}, None] := 1;
ElementaryESymmetric[d_Integer, None] := Which[
	d < 0, 0,
	d ==0, 1,
	True, MonomialSymbol[ConstantArray[1, d]]];

ElementaryESymmetric[lam_List, None] := ElementaryESymmetric[lam, None] = Expand[
	ElementaryESymmetric[First@lam, None]*ElementaryESymmetric[Rest@lam, None]
];
ElementaryESymmetric[{}, x_] := 1;
ElementaryESymmetric[lam_List, x_] := ChangeSymmetricAlphabet[ElementaryESymmetric[lam,None],x];



UnitTest[ElementaryESymmetric] := With[{m = 5},
Expand[Sum[
	(-1)^i ElementaryESymmetric[i] CompleteHSymmetric[m - i], {i, 0,
		m}]] === 0
];


PowerSumSymmetric[a_]:=PowerSumSymmetric[a,None];
PowerSumSymmetric[{}, None] := 1;
PowerSumSymmetric[d_Integer, None] :=  Which[
	d < 0, 0, 
	d==0, 1, 
	True, MonomialSymbol[{d}, None]];
PowerSumSymmetric[lam_List, None] := PowerSumSymmetric[lam, None] = Expand[
	PowerSumSymmetric[First@lam, None]*PowerSumSymmetric[Rest@lam, None]
];
PowerSumSymmetric[{}, x_] := 1;
PowerSumSymmetric[lam_List, x_] := ChangeSymmetricAlphabet[PowerSumSymmetric[lam,None],x];



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



(* Uses Kostka coeffs. *)

SchurSymmetric[lam_List]:=SchurSymmetric[lam, None];
SchurSymmetric[lam_List,None] := SchurSymmetric[lam,None] = Module[
	{i, j, jtDet, bb},
	
	(* Jacobi--Trudi determinant *)
	jtDet[ll_, mm_, 0] := 1;
	jtDet[ll_, mm_, d_] := Expand[Det@Table[
		bb[ll[[i]] - mm[[j]] - i + j],
		{i, d},
		{j, d}]];

	Which[
		Length[lam]==0, 
		1
	,

		True,
			Expand@Sum[
				KostkaCoefficient[lam, nu,1] MonomialSymbol[nu]
			,{nu,IntegerPartitions[Tr@lam]}]
	]
];

(* Slinky rule. *)
SchurSymmetric[lam_List, x_] := With[{slr=CompositionSlinky[lam]},
	If[slr[[2]]==0,
	0, 
	slr[[2]]*SchurSymmetric[slr[[1]], x]
	]
] /; Not[OrderedQ[Reverse@lam]];

SchurSymmetric[lam_List, x_] := ChangeSymmetricAlphabet[ SchurSymmetric[lam, None], x];



SkewSchurSymmetric[lam_List]:=SkewSchurSymmetric[{lam, {}}, None];
SkewSchurSymmetric[{lam_List, mu_List}]:=SkewSchurSymmetric[{lam, mu}, None];
SkewSchurSymmetric[{lam_List, mu_List},None] := SkewSchurSymmetric[{lam, mu}, None] = Module[
	{i, j, jtDet, bb, mup = PadRight[mu, Length@lam]},
	
	(* Jacobi--Trudi determinant *)
	jtDet[ll_, mm_, 0] := 1;
	jtDet[ll_, mm_, d_] := Expand[Det@Table[
		bb[ll[[i]] - mm[[j]] - i + j],
		{i, d},
		{j, d}]];

Expand@Which[
	!PartitionLessEqualQ[mu, lam],
		0
	,
	Length[lam]==0, 
		1
	,
	
	True,
		With[{d = lam[[1]]},
		jtDet[
			PadRight[ConjugatePartition[lam], d],
			PadRight[ConjugatePartition[mu], d], d]
		] /. bb[v_] :> ElementaryESymbol[v]
	,
	False, (* This never happens, we do not use this formula.*)
		With[{d = Length[lam]},
			jtDet[PadRight[lam, d], PadRight[mu, d], d]
		] /. bb[v_] :> CompleteHSymbol[v]
	]
];

SkewSchurSymmetric[lam_List,x_]:=SkewSchurSymmetric[{lam, {}}, x];
SkewSchurSymmetric[{lam_List, mu_List}, x_] := ChangeSymmetricAlphabet[ SkewSchurSymmetric[{lam, mu}, None],  x];





(* Private Util function for converting between bases. *)

PartitionIndexedBasisRule[size_Integer, basisSymb_, toBasis_, 
	monom_: MonomialSymbol, x_: None] := 
	PartitionIndexedBasisRule[size, basisSymb, toBasis, monom, x] = Module[{parts, mat, imat,p,q},
	parts = IntegerPartitions[size];
	
	
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



(* Code to generate transition matrices *)

(* If from and to are the same, we have identity. *)
fromToBasis[fromBasisFunc_, fromBasisFunc_, deg_Integer]:=IdentityMatrix[PartitionsP@deg];


fromToBasis[fromBasisFunc_, MonomialSymmetric, deg_Integer]:=
	fromToBasis[fromBasisFunc, MonomialSymmetric, deg]=Table[
		Coefficient[ fromBasisFunc[lam], MonomialSymbol[mu,None] ]
	,{lam, IntegerPartitions[deg]},
	{mu, IntegerPartitions[deg]}
];

fromToBasis[MonomialSymmetric, toBasisFunc_, deg_Integer]:=
	fromToBasis[MonomialSymmetric,toBasisFunc,deg]=Inverse@fromToBasis[toBasisFunc,MonomialSymmetric, deg];

(* Simply matrix multiplication *)
(* These functions are not used.... *)
fromToBasis[fromBasisFunc_, toBasisFunc_, deg_Integer] := 
fromToBasis[toBasisFunc,MonomialSymmetric, deg].fromToBasis[fromBasisFunc, MonomialSymmetric,deg];


(* 
Given a function for producing the monomial expansion,
and a symbol for the new basis, produce a replacement rule to do this conversion. 
It maps from alphabet x, to alphabet y.
*)
fromMonomialToCoreBasisRule[{toBasisFunc_,toBasisSymb_}, deg_Integer, {x_, y_}]:=
	MapThread[
		Rule,
		{
			Table[
				MonomialSymbol[lam,x]
			,{lam,IntegerPartitions[deg]}]
		,
			fromToBasis[MonomialSymmetric,toBasisFunc,deg].Table[
				toBasisSymb[lam,y]
			,{lam,IntegerPartitions[deg]}]

		}
	,
	1
];


toOtherSymmetricBasis[{basisFunc_, basisSymbol_}, poly_, x_: None] := Module[
	{bases,replaceRule,degrees,inMonomBasis},
	
	bases = {
		{MonomialSymmetric, MonomialSymbol},
		{PowerSumSymmetric, PowerSumSymbol},
		{ElementaryESymmetric,ElementaryESymbol},
		{CompleteHSymmetric,CompleteHSymbol},
		{SchurSymmetric,SchurSymbol},
		{ForgottenSymmetric, ForgottenSymbol}
	}; 
	
	(* 
		Rule for doing conversion to monomial basis in expression.
	*)
	replaceRule = (#2[lam_,x]->#1[lam,x])& @@@ Select[bases, Last[#] =!= basisSymbol &];
	
	
	(* Convert everything to monomial basis, except expressions in the basis itself. *)
	inMonomBasis = Expand[ Expand[poly] /. replaceRule ];
	
	
	(* Which degrees appear *)
	degrees = Union[Cases[inMonomBasis, MonomialSymbol[lam__, x] :> Tr[lam], {0, Infinity}]];
	
	(* Replace everything *)
	Do[
		inMonomBasis =  inMonomBasis /. fromMonomialToCoreBasisRule[{basisFunc,basisSymbol},d,{x,x}]
	, {d, degrees}];
	
	Expand[inMonomBasis]
];


ToSchurBasis[poly_, x_: None] := toOtherSymmetricBasis[{SchurSymmetric,SchurSymbol}, poly, x];
ToPowerSumBasis[poly_, x_: None] := toOtherSymmetricBasis[{PowerSumSymmetric,PowerSumSymbol}, poly, x];
ToElementaryEBasis[poly_, x_: None] := toOtherSymmetricBasis[{ElementaryESymmetric,ElementaryESymbol}, poly, x];
ToCompleteHBasis[poly_, x_: None] := toOtherSymmetricBasis[{CompleteHSymmetric, CompleteHSymbol}, poly, x];
ToMonomialBasis[poly_, x_: None] := toOtherSymmetricBasis[{MonomialSymmetric,MonomialSymbol}, poly, x];


(* A normalized version, where we multiply by the z-coefficient. *)
ToPowerSumZBasis[poly_, x_: None] := (ToPowerSumBasis[poly,x]/.PowerSumSymbol[lam_,x] :> ZCoefficient[lam] PowerSumSymbol[lam,x]);

(* The forgotten basis is given by omega(monomial) *)
(* We compute it in the monomial basis here. *)
ForgottenSymmetric[mu_List, x_: None] := 
ForgottenSymmetric[mu, x] = Module[{},
	Expand[
		ToPowerSumBasis[MonomialSymbol[mu, x]] /. 
	PowerSumSymbol[lam_,x] :> (-1)^(Tr[lam] - Length[lam]) PowerSumSymmetric[lam, x]]
];


OmegaInvolution::usage = "OmegaInvolution[expr, [x]] applies the omega involution on the expression.";
OmegaInvolution[poly_, x_: None] := (
	poly /. {
		MonomialSymbol[mu_, x] :> ForgottenSymbol[mu, x],
		ForgottenSymbol[mu_, x] :> MonomialSymbol[mu, x],
		PowerSumSymbol[mu_,x] :> (-1)^(Tr[mu] - Length[mu])PowerSumSymbol[mu, x],
		ElementaryESymbol[mu_,x]:>CompleteHSymbol[mu,x],
		CompleteHSymbol[mu_,x]:>ElementaryESymbol[mu,x],
		SchurSymbol[mu_,x]:>SchurSymbol[ConjugatePartition@mu,x]
	}
);

UnitTest[OmegaInvolution] := And[
OmegaInvolution@OmegaInvolution@SchurSymmetric[{4, 2, 1}] === 
	SchurSymmetric[{4, 2, 1}],
OmegaInvolution@ElementaryESymmetric[{4, 2, 1}] === 
	CompleteHSymmetric[{4, 2, 1}]
];



(***********************************************************************************)

(* Todo - update so that it takes all bases into account. *)
SymmetricFunctionDegree::usage = "SymmetricFunctionDegree[expr] returns the degree of the symmetric function.";
SymmetricFunctionDegree[expr_, yy_: None] := Max[
	Cases[expr, MonomialSymbol[mu_List, yy] :> Tr[mu],{0,Infinity}]];

SymmetricFunctionToPolynomial::usage = "SymmetricFunctionToPolynomial[expr,x,[n]] expresses the function as a polynomial in n variables.";

SymmetricFunctionToPolynomial[MonomialSymbol[mu_List, None], x_, 0] := 0;
SymmetricFunctionToPolynomial[MonomialSymbol[mu_List, None], x_, n_Integer] := 
	If[
		Length[mu]>n, 0
		,
		Sum[ Times @@ ((x /@ Range[n])^p)
	, {p, Permutations[PadRight[mu, n]]}]
	];

SymmetricFunctionToPolynomial[expr_, x_] := SymmetricFunctionToPolynomial[expr, x, SymmetricFunctionDegree[expr]];

(* TODO - This can be made more efficient if we replace expressions directly for some bases. *)
SymmetricFunctionToPolynomial[expr_, x_, n_Integer, yy_: None] := 
	ReplaceAll[
		ToMonomialBasis[expr,yy]
		,
		MonomialSymbol[mu_List, yy] :> 
		SymmetricFunctionToPolynomial[MonomialSymbol[mu, None], x, n]
	];


(***********************************************************************************)



(* TODO -- Optimize so that known formulas for princ. spec. is used. *)

PrincipalSpecialization::usage = "PrincipalSpecialization[poly,q,[k],[x]], 
gives the principal specialization. The parameter k tells how many 
variables to replace - this can be infinity.";

PrincipalSpecialization[poly_, q_, k_: Infinity, x_: None] := Module[{psMon},
	psMon[mu_List] := With[{
		ppc = PartitionPartCount[mu],
		ell = Length@mu},
		Product[k - j, {j, 0, ell - 1}]/(Times @@ (ppc!))
	];
	
	Which[
		k === Infinity,
			ToPowerSumBasis[poly, x] /. 
			PowerSumSymbol[lam_,x] :> 1/Product[1 - q^i, {i, lam}],
		
		
		(* This is quite efficient. *)
		q === 1,
			Expand[poly] /. MonomialSymbol[lam_, x] :> psMon[lam],
		
		True,
			ToPowerSumBasis[poly,  x] /. 
			PowerSumSymbol[lam_,x] :> Product[Sum[q^(i (j - 1)), {j, k}], {i, lam}]
		]
];


(* TODO -- Use the change of basis matrix directly instead. *)
SnCharacter[lam_List, nu_List] := SnCharacter[{lam, {}}, nu];
SnCharacter[{lam_List, mu_List}, nu_List] := Module[{schur, n = Tr[lam] - Tr[mu], inP},
	schur = SchurSymbol[{lam, mu}];
	inP = ToPowerSumBasis[schur];
	
	(* Memoize all *)
	Do[
		SnCharacter[{lam, mu}, tau] = 
		ZCoefficient[tau] Coefficient[inP, PowerSumSymbol[tau]];
		, {tau, IntegerPartitions@n}];
	SnCharacter[{lam, mu}, nu]
];


HallInnerProduct[f_, g_] := HallInnerProduct[f, g, {1, 1}, None];
HallInnerProduct[f_, g_, {q_, t_}, x_: None] := Module[{
	ff, gg, lam,
	vars, rulesF, rulesG, vF, vG, rF, rG
	},
	
	(* Convert to powersum *)
	ff = ToPowerSumBasis[f, x];
	gg = ToPowerSumBasis[g, x];
	
	vars = Cases[Variables[{ff, gg}], PowerSumSymbol[__,x], {0, Infinity}];
	
	rulesF = CoefficientRules[ff, vars];
	rulesG = CoefficientRules[gg, vars];

(* Combine all power-sum terms. *)
Expand@Sum[
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
(* TODO-UPDATE with rules for all bases. *)
NegateAlphabet[f_, x_: None] := ToMonomialBasis[f] /. MonomialSymbol[mu__, x] :> (-1)^(Tr@mu) MonomialSymbol[mu, x];


(* TODO - UPDATE! *)
(* Here, f is in the None alphabet, and plethysm act on ALL given alphabets in the G-expression. *)
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
	
	PkPlethysmWithG[k_] :=
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
HallLittlewoodTSymmetric::usage = "HallLittlewoodTSymmetric is the transformed Hall-Littlewood polynomial.";
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
	
	Expand[res /. {hh[a_] :> CompleteHSymbol[a, x], qq -> q}]
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

MacdonaldPSymmetricHelper[mu_List, q_, t_] :=MacdonaldPSymmetricHelper[mu,q,t]=
Together@Sum[
   MonomialSymbol[YoungTableauWeight[ssyt]]
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



(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)



(* TODO: allow for optional alphabet. *)

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
		
		SchurSymbol[alpha]
		SPECIALQ^inv SPECIALT^maj
	, {p, Permutations[Range[n]]}]
];


ToMacdonaldHBasis[poly_, q_, t_, mh_:MacdonaldHSymbol, x_: None] := 
Expand@Together@toOtherSymmetricBasis[ {MacdonaldHSymmetric[#,q,t]&, mh}, poly, x];


PostfixedCharge[mu_List, w_List] := Module[{postFix, decomp},
   postFix = 
    Reverse[Join @@ 
      MapIndexed[ConstantArray[#2[[1]], #1] &, ConjugatePartition@mu]];
   decomp = ChargeWordDecompose[Join[w, postFix]];
   Total[PermutationCharge /@ decomp]
];


(* TODO: allow for optional alphabet. *)

SkewMacdonaldESymmetric::usage = "SkewMacdonaldESymmetric[{lam,mu},q] gives the skew Macdonald polynomial.";

SkewMacdonaldESymmetric[lam_List, q_] := SkewMacdonaldESymmetric[{lam,{}}, q];

SkewMacdonaldESymmetric[{lam_List, mu_List}, q_] := 
SkewMacdonaldESymmetric[{lam, mu}, SPECIALQ] /. {SPECIALQ -> q};

SkewMacdonaldESymmetric[{lam_List, mu_List}, SPECIALQ] := SkewMacdonaldESymmetric[{lam,mu},SPECIALQ]=
  Module[{alpha, n = Tr[lam] - Tr[mu],t,
    nn = Tr[lam], muc = ConjugatePartition[mu]},
   alpha = PadRight[ConjugatePartition[lam], nn] - PadRight[muc, nn];
   Sum[
    SchurSymbol[nu]
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

LLTSymmetric[nu_List, q_]:=LLTSymmetric[nu,q,None];
LLTSymmetric[nu_List, q_,None] := LLTSymmetric[nu,q,None] = Module[
{rwWithContent, ttInv, sizes, lam, mu, templateList, tableauList,
	tabValues, perms, p, contentRW, rwPerm, n,
	tabTuples, desSet, alpha, inv
	},

(* Returns a list (entry, c-r,r,i) *)
(* Used to construct the reading word. *)

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
	q^inv SchurSymbol[alpha]
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
       CompleteHSymbol[lam[[i]] - i - j + 2], 
	{i, Length@lam}, 
	{j, Length@lam}]];


OrthogonalSchurSymmetric::usage = "OrthogonalSchurSymmetric[mu] gives the orthogonal Schur function.";
OrthogonalSchurSymmetric[{}] := 1;
OrthogonalSchurSymmetric[lam_List] := Det@Table[
    CompleteHSymbol[lam[[i]] - i + j] - 
     CompleteHSymbol[lam[[i]] - i - j], 
	{i, Length@lam}, 
	{j, Length@lam}];


	
	
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

NablaOperator[g_, q_, t_] := DeltaOperator[ElementaryESymmetric[SymmetricFunctionDegree[g]], g, q, t];

DeltaPrimOperator[f_, g_, q_, t_] := DeltaPrimOperator[f, g, q, t] = Module[{x, val, inH, monoms},
		(* Monomials defined by shape. *)
		
		monoms[mu_List] := 
		Rest[q^(#1 - 1) t^(#2 - 1) & @@@ ShapeBoxes[mu]];
		
		(* This correspond to f[B(mu)-1] -- it is quicker than plethysm *)

		val[mu_List] := SymmetricFunctionToPolynomial[f, x, Tr[mu] - 1] /. x[i_] :> monoms[mu][[i]];
		
		inH = Expand[ToMacdonaldHBasis[g, "hh", q, t]];
		Together[
		inH /. "hh"[lam_List] :> 
			val[lam] MacdonaldHSymmetric[lam, q, t]]
];



End[(* End private *)];
EndPackage[];
