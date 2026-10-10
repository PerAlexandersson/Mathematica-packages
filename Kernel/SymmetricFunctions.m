(* ::Package:: *)

(* TODO 


=== MAKE A MORE CONSISTENT SYSTEM FOR CREATING NEW BASES! 
	One needs to map BasisSymmetric to BasisSymbol and vice versa 
	Make a general function that takes the arguments {BasisSymmetric, BasisSymbol} and 
	possibly additional parameters ({q,t}) and sets up everything. 

--- Ask Mike Zabrocki about sage and symfuns

--- Can we make an external C program that compute transition matrices? Is it worth it?
--- Use Local objects to store transition matrices(?) https://reference.wolfram.com/language/ref/LocalObject.html

--- Update documentation.

--- Use https://reference.wolfram.com/language/ref/$Failed.html for failures

--- https://reference.wolfram.com/language/ref/SyntaxInformation.html

--- Check out https://github.com/jkuczm/MathematicaCellsToTeX for how to write packages.

--- Update test suite?

--- Use protect/unprotect with memoization: 
	https://mathematica.stackexchange.com/questions/39014/is-it-possible-to-protect-a-dynamic-programming-process

*)

BeginPackage["SymmetricFunctions`",{"AlgebraicBases`","CombinatoricTools`","NewTableaux`"}];

Unprotect["`*"]
ClearAll["`*"]


LRCoefficient;
LRExpand;
MExpand;
SkewKostkaCoefficient;

PositiveCoefficientsQ;
CoefficientsSum;

(* Symbols for denoting the common bases. *)
ElementaryESymbol;
PowerSumSymbol;
CompleteHSymbol;
MonomialSymbol;
SchurSymbol;
ForgottenSymbol;

FunctionAlphabets;
ChangeFunctionAlphabet;
SymmetricMonomialList;


(* Transition matrices. This is a wrapper for memoized internal function. *)
SymFuncTransMat;


ToSchurBasis;
ToPowerSumBasis;
ToPowerSumZBasis;
ToElementaryEBasis;
ToCompleteHBasis;
ToMonomialBasis;

MonomialSymmetric;
AugmentedMonomialSymmetric;
CompleteHSymmetric;
ElementaryESymmetric;
PowerSumSymmetric;
SchurSymmetric;
ForgottenSymmetric;

toOtherSymmetricBasis;

OmegaInvolution;
HallInnerProduct;
JackInnerProduct;
JackLowerHook;
JackUpperHook;

SymmetricFunctionDegree;
SymmetricFunctionToPolynomial;
PolynomialToSymmetricFunction;

PrincipalSpecialization;
Plethysm;
KroneckerCoefficient;
InternalProduct;

(* This expands into e:s *)
SkewSchurSymmetric;

JackPSymmetric;
JackJSymmetric;
ShiftedJackPSymmetric;

HallLittlewoodMSymmetric;
HallLittlewoodTSymmetric;
HallLittlewoodPSymmetric;

ToHallLittlewoodPBasis
HallLittlewoodPSymbol;

kSchurSymmetric;


SchursQSymmetric;
SchursPSymmetric;
BigSchurSymmetric;

MacdonaldPSymmetric;
MacdonaldJSymmetric;
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

LyndonSymmetric;

DeltaOperator;
NablaOperator;
DeltaPrimOperator;

HookFilterMap;


PrecomputeBasisMatrices;
LoadBasisMatrices;
ClearSymmetricFunctionsCache;


SnModuleCharacters;


Begin["`Private`"];

sortToPartition[mu_List]:=Sort[Select[mu,Positive],Greater];


(* Results of memoized public functions are stored here rather than as down-values,
   since the public symbols are protected. Keys start with the function symbol. *)
sfCache = <||>;

SetAttributes[cached, HoldRest];
cached[key_, expr_] := If[KeyExistsQ[sfCache, key], sfCache[key], sfCache[key] = expr];

ClearSymmetricFunctionsCache::usage = "ClearSymmetricFunctionsCache[] removes all stored results of memoized symmetric functions, such as SkewSchurSymmetric, MacdonaldHSymmetric and KroneckerCoefficient. Transition matrices between the classical bases are kept.";
ClearSymmetricFunctionsCache[] := (sfCache = <||>;);


coreBasesList=(MonomialSymbol|SchurSymbol|ElementaryESymbol|CompleteHSymbol|ForgottenSymbol|PowerSumSymbol);


(* This returns a permutation, which sends the list of integer partitions, to the same list,
 but rearranged as conjugates. Useful for change-of-basis matrices. *)
conjugatePermutation[n_Integer]:=conjugatePermutation[n]=With[{ip=IntegerPartitions@n},
	(Ordering[ip])[[Ordering@(Ordering[ConjugatePartition /@ ip])]]
];





(* Basis symbols are created with AlgebraicBases`CreateBasis (shared with
   QuasiSymmetricFunctions). *)


(* This is a private helper function, for utilizing memoization. *)
(* TODO-MAKE IT KEEP TRACK OF DUPLICATES AUTOMATICALLY, and reuse? *)
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

(* private helper function, use Partition-part-count instead *)
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
	
	products = monomialProducts[lamp,DeleteCases[Tally@mup,0]];
	
	Total[
		((dim[#1]/dimCoeff)*#2*MonomialSymbol[#1,x])  & @@@ (Tally@products)
	]
];


MExpand::usage = "MExpand[expr] expands products and powers of monomial symmetric functions in expr.";
MExpand[expr_]:=Module[{lam, mm,multRule,bb=MonomialSymbol},
	
	monomPower[lam_List, 0,x_] := 1;
	monomPower[lam_List, 1,x_] := bb[lam, x];
	monomPower[lam_List, 2,x_] := monomialProduct[lam, lam,x];
	monomPower[lam_List, d_Integer,x_] := monomPower[lam,d,x]=Which[
		EvenQ[d],
				monomPower[lam, (d/2),x]^2,
		True, 
			monomPower[lam, (d - 1)/2,x]*monomPower[lam, 1 + (d - 1)/2,x]
		];
	FixedPoint[
				Expand[#] /. {
					Times[bb[a_List, x_], bb[b_List, x_]] :> monomialProduct[a, b, x]
					,
					Power[bb[a_List, x_], n_Integer] :> monomPower[a,n,x]
					
				}&
	, expr]
	
];


(* Create the three classical multiplicative bases. *)
ElementaryESymbol::usage = "ElementaryESymbol[lam, x] represents the elementary symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.";
CreateBasis[ElementaryESymbol, "e", 
	MultiplicationFunction -> (ElementaryESymbol[PartitionJoin[#1, #2], #3]&),
	PowerFunction-> 
		Function[{a,n,x},
			ElementaryESymbol[Join@@(ConstantArray[#1, n]& /@a), x]]
];

PowerSumSymbol::usage = "PowerSumSymbol[lam, x] represents the power-sum symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.";
CreateBasis[PowerSumSymbol, "p", 
	MultiplicationFunction -> (PowerSumSymbol[PartitionJoin[#1, #2], #3]&),
	PowerFunction-> 
		Function[{a,n,x},
			PowerSumSymbol[Join@@(ConstantArray[#1, n]& /@a), x]]
];

CompleteHSymbol::usage = "CompleteHSymbol[lam, x] represents the complete homogeneous symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.";
CreateBasis[CompleteHSymbol, "h", 
	MultiplicationFunction -> (CompleteHSymbol[PartitionJoin[#1, #2], #3]&),
	PowerFunction->
		Function[{a,n,x},
			CompleteHSymbol[Join@@(ConstantArray[#1, n]& /@a), x]]
];


(* Use this if monomials should automatically multiply together. *)
(*
CreateBasis[MonomialSymbol, "m", 
	MultiplicationFunction -> (monomialProduct),
	PowerFunction->"Mult"
];

CreateBasis[ForgottenSymbol, "f", 
	MultiplicationFunction -> (monomialProduct),
	PowerFunction->"Mult"
];
*)

MonomialSymbol::usage = "MonomialSymbol[lam, x] represents the monomial symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.";
CreateBasis[MonomialSymbol, "m", 
	MultiplicationFunction -> None,
	PowerFunction->None
];

ForgottenSymbol::usage = "ForgottenSymbol[lam, x] represents the forgotten symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.";
CreateBasis[ForgottenSymbol, "f", 
	MultiplicationFunction -> None,
	PowerFunction->None
];


(* We use slinky rule for the Schur functions. *)
SchurSymbol::usage = "SchurSymbol[lam, x] represents the Schur symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.";
CreateBasis[SchurSymbol, "s", 
	MultiplicationFunction -> None,
	PowerFunction -> None,
	SortFunction -> (With[{slr=CompositionSlinky[#1]},
		If[slr[[2]]==0, 0, 
			slr[[2]]*SchurSymbol[slr[[1]], #2]
			]
	]&)
];


(* Jacobi--Trudi determinants *)
jacobiTrudiDet[ll_List, bb_, 0] := 1;
jacobiTrudiDet[ll_List, bb_, d_Integer] := Expand@Det@Table[
		bb[ll[[i]] - i + j]
		,{i, d},{j, d}];
		
jacobiTrudiDet[{ll_List, mm_List}, bb_, 0] := 1;
jacobiTrudiDet[{ll_List, mm_List}, bb_, d_Integer] := Expand@Det@Table[
		bb[ll[[i]] - mm[[j]] - i + j]
		,{i, d},{j, d}];


(* (0.3) in Okounkov-Olshanski, https://arxiv.org/pdf/q-alg/9605042.pdf *)
(* Used to compute LR-coefficients *)
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


LRCoefficient::usage="LRCoefficient[lam, mu, nu] gives the Littlewood-Richardson coefficient of Schur function S_nu in S_lam*S_mu. The arguments are partitions.";
LRCoefficient[lambda_List, mu_List, nu_List] := Module[
	{cleanLambda = DeleteCases[lambda, 0], cleanMu = DeleteCases[mu, 0],
		cleanNu = DeleteCases[nu, 0]},
	If[Tr[cleanNu] != Tr[cleanLambda] + Tr[cleanMu], 0,
		lrCoefficientInternal[cleanLambda, cleanMu, cleanNu]]
];

(* Base cases are values of shifted Schur functions. *)
lrCoefficientInternal[lambda_List, mu_List, mu_List] := lrCoefficientInternal[lambda, mu, mu] = shiftedSchurDet[lambda, mu];
lrCoefficientInternal[nu_List, mu_List, nu_List] := lrCoefficientInternal[nu, mu, nu] = shiftedSchurDet[mu, nu];

(* General recursion - Based on recursion by Molev-Sagan, (1999) *)
lrCoefficientInternal[lambda_List, mu_List, nu_List] := lrCoefficientInternal[lambda, mu, nu] =
	Which[
	! PartitionLessEqualQ[mu, nu] || ! 
		PartitionLessEqualQ[lambda, nu], 0
	,
	(* Interchange according to size. *)
	Tr[lambda] > Tr[mu], 
	lrCoefficientInternal[mu, lambda, nu]
	,
	(* Recursion *)
	True,
	1/(Tr@nu - Tr@mu) (
		Sum[lrCoefficientInternal[lambda, mup, nu], {mup, PartitionAddBox[mu]}] -
		Sum[
		lrCoefficientInternal[lambda, mu, num], {num, 
			PartitionRemoveBox[nu]}]
		)
];


LRExpand::usage = "LRExpand[expr, x] expands products and powers of Schur symmetric functions in expr using the Littlewood-Richardson rule. The alphabet x defaults to None.";
LRExpand[expr_, x_: None] := Module[{schurProduct, schurPower, rule},
	schurProduct[lam_List, mu_List] := Sum[
		lrCoefficientInternal[lam, mu, nu] SchurSymbol[nu, x]
		, {nu, IntegerPartitions[Tr[lam] + Tr[mu]]}];

	schurPower[lam_List, 0] := 1;
	schurPower[lam_List, 1] := SchurSymbol[lam, x];
	schurPower[lam_List, d_Integer] := Which[
		d == 2, schurProduct[lam, lam],
		EvenQ[d], LRExpand[schurPower[lam, (d/2)]^2],
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

SkewKostkaCoefficient::usage = "SkewKostkaCoefficient[lam, mu, w] returns the skew Kostka coefficient for skew shape lam/mu and weight w. The weight w is sorted to a partition.";
SkewKostkaCoefficient[lam_List, mu_List, w_List] :=
  With[{nu = sortToPartition[w], n = Tr[lam] - Tr[mu]},
   If[Tr[nu] != n, 0,
    cached[{SkewKostkaCoefficient, lam, mu, nu},
     With[{inM = ToMonomialBasis[SkewSchurSymmetric[{lam, mu}]]},
      (* Store the coefficients for all partitions of n. *)
      Do[
       sfCache[{SkewKostkaCoefficient, lam, mu, kappa}] =
         Coefficient[inM, MonomialSymbol[kappa, None]];
       , {kappa, IntegerPartitions[n]}];
      sfCache[{SkewKostkaCoefficient, lam, mu, nu}]
      ]]]
   ];

(********************************************************************)


FunctionAlphabets::usage = "FunctionAlphabets[expr] returns the list of alphabets occurring in a symmetric-function expression.";
FunctionAlphabets[expr_]:=Cases[expr, (bb:coreBasesList)[mu__, x_] :> x, {0, Infinity}];

ChangeFunctionAlphabet::usage = "ChangeFunctionAlphabet[expr, to, from] changes the symmetric-function expressions in expr from alphabet from to alphabet to. The source alphabet defaults to None.";
ChangeFunctionAlphabet[expr_, to_, from_: None] := If[ 
	to === from,
	expr,
	(expr /. (bb:coreBasesList)[mu__, from] :> bb[mu, to])];
	
SymmetricMonomialList::usage = "SymmetricMonomialList[expr] returns the list of symmetric-function monomials occurring in expr.";
SymmetricMonomialList[expr_]:=With[{mm=Union@Cases[expr, (bb:coreBasesList)[mu__, x_],{0,Infinity}]},
		MonomialList[expr,mm]
];


SymmetricFunctionDegree::usage = "SymmetricFunctionDegree[expr] returns the degree of the symmetric function.";

SymmetricFunctionDegree[0, ___] := -Infinity;
SymmetricFunctionDegree[_?NumericQ, ___] := 0;
SymmetricFunctionDegree[expr_, All] := Max@Table[
			Cases[mm, (bb:coreBasesList)[mu_List, x_] :> Tr[mu], {0,Infinity}]
		,{mm,SymmetricMonomialList[expr]}];

SymmetricFunctionDegree[expr_, yy_:None] := If[
	expr===0, -Infinity,
	Max@Table[
			Cases[mm, (bb:coreBasesList)[mu_List, yy] :> Tr[mu],{0,Infinity}]
		,{mm, SymmetricMonomialList[expr]}]
];

PositiveCoefficientsQ::notnumber = "The coefficient `1` is not a number.";
PositiveCoefficientsQ::usage = "PositiveCoefficientsQ[expr, basis, extraSymbols] returns True if all coefficients of basis terms in expr are nonnegative numbers. The basis defaults to MonomialSymbol and extraSymbols defaults to {}.";

PositiveCoefficientsQ[expr_List, basisSymbol_:MonomialSymbol, extraSymbols_:{}]:=And@@(
	PositiveCoefficientsQ[#, basisSymbol, extraSymbols]&/@expr
);

PositiveCoefficientsQ[expr_, basisSymbol_:MonomialSymbol, extraSymbols_:{}] := 
	Module[{exp = Expand@expr, DUMMY},
	
		(* We need the dummy variable, since if the list of variables is empty, 
				it pretends everything are variables. *)
		And @@ 
			Map[
			With[{c=Last[#]},
				If[ NumberQ[c], 
					c>=0,
					Message[PositiveCoefficientsQ::notnumber,c];
					False
				]
			]
			&
			, CoefficientRules[exp, 
				Join[{DUMMY}, Cases[exp, basisSymbol[__,_], {0,Infinity}], extraSymbols] ]
			]
];



CoefficientsSum::usage = "CoefficientsSum[expr] returns the sum of coefficients.";
CoefficientsSum[expr_] := ReplaceAll[Expand@expr, coreBasesList[__,_]:>1];


(**********************************************************)
(**********************************************************)
(**********************************************************)


(* 
Code to generate transition matrices efficiently. The idea is to avoid
any multiplication in the monomial basis, as this is slow.
*)

SymFuncTransMat::usage = "SymFuncTransMat[fromBasis, toBasis, d] returns the transition matrix
in degree d. Here, fromBasis and toBasis are functions which given a partition, returns the monomial expansion.";

SymFuncTransMat[fromBasisFunc_, toBasisFunc_, deg_Integer]:=
	symFuncTransMatInternal[fromBasisFunc,toBasisFunc,deg];
	
	
(* If from and to are the same, we have identity. *)
symFuncTransMatInternal[fromBasisFunc_, fromBasisFunc_, deg_Integer]:=IdentityMatrix[PartitionsP@deg];


(* Change-of-basis involving S and H *)
(* This uses a quick recursion for inverse Kostka coefficients. *)
(* There is only one matrix inversion used among all these computations. *)

symFuncTransMatInternal[SchurSymmetric, CompleteHSymmetric, deg_Integer]:=
symFuncTransMatInternal[SchurSymmetric, CompleteHSymmetric, deg]=Table[
	InverseKostkaCoefficient[lam,mu]
	,{mu, IntegerPartitions[deg]},
{lam, IntegerPartitions[deg]}];

symFuncTransMatInternal[CompleteHSymmetric, SchurSymmetric, deg_Integer]:=
symFuncTransMatInternal[CompleteHSymmetric, SchurSymmetric, deg]=
Inverse@symFuncTransMatInternal[SchurSymmetric, CompleteHSymmetric, deg];

symFuncTransMatInternal[SchurSymmetric,MonomialSymmetric, deg_Integer]:=
Transpose@symFuncTransMatInternal[CompleteHSymmetric, SchurSymmetric, deg];

symFuncTransMatInternal[MonomialSymmetric,SchurSymmetric, deg_Integer]:=
Transpose@symFuncTransMatInternal[SchurSymmetric,CompleteHSymmetric, deg];

symFuncTransMatInternal[CompleteHSymmetric, MonomialSymmetric, deg_]:=
symFuncTransMatInternal[CompleteHSymmetric, MonomialSymmetric, deg]=
symFuncTransMatInternal[CompleteHSymmetric, SchurSymmetric, deg].symFuncTransMatInternal[SchurSymmetric,MonomialSymmetric, deg];


symFuncTransMatInternal[MonomialSymmetric, CompleteHSymmetric, deg_]:=
symFuncTransMatInternal[MonomialSymmetric, CompleteHSymmetric, deg]=
symFuncTransMatInternal[MonomialSymmetric, SchurSymmetric, deg].symFuncTransMatInternal[SchurSymmetric, CompleteHSymmetric, deg];


(* There is a simple relation between StoH and StoE matrices, where index is conjugated. *)
symFuncTransMatInternal[SchurSymmetric, ElementaryESymmetric, deg_]:=
symFuncTransMatInternal[SchurSymmetric, CompleteHSymmetric, deg][[ conjugatePermutation[deg] ]];

symFuncTransMatInternal[ElementaryESymmetric, SchurSymmetric, deg_]:=
Transpose[
	Transpose[symFuncTransMatInternal[CompleteHSymmetric, SchurSymmetric, deg]][[ conjugatePermutation[deg] ]]
];

symFuncTransMatInternal[ElementaryESymmetric, MonomialSymmetric, deg_]:=
symFuncTransMatInternal[ElementaryESymmetric, MonomialSymmetric, deg]=
symFuncTransMatInternal[ElementaryESymmetric, SchurSymmetric, deg].symFuncTransMatInternal[SchurSymmetric,MonomialSymmetric, deg];


symFuncTransMatInternal[MonomialSymmetric, ElementaryESymmetric, deg_]:=
symFuncTransMatInternal[MonomialSymmetric, ElementaryESymmetric, deg]=
symFuncTransMatInternal[MonomialSymmetric, SchurSymmetric, deg].symFuncTransMatInternal[SchurSymmetric,ElementaryESymmetric, deg];

(* This particular transition matrix is used when converting
   anything to H-basis, so make sure its memoized. *)
symFuncTransMatInternal[ElementaryESymmetric, CompleteHSymmetric, deg_]:=
symFuncTransMatInternal[ElementaryESymmetric, CompleteHSymmetric, deg]=
symFuncTransMatInternal[ElementaryESymmetric, SchurSymmetric, deg].symFuncTransMatInternal[SchurSymmetric,CompleteHSymmetric, deg];



(* Use quick recursion for characters. *)

symFuncTransMatInternal[PowerSumSymmetric, SchurSymmetric, deg_Integer]:=
symFuncTransMatInternal[PowerSumSymmetric, SchurSymmetric, deg]=Table[
	SnCharacter[lam,mu]
	,{mu, IntegerPartitions[deg]},
{lam, IntegerPartitions[deg]}];

(* Use relation for computing the inverse *)
symFuncTransMatInternal[SchurSymmetric,PowerSumSymmetric, deg_Integer]:=
symFuncTransMatInternal[SchurSymmetric,PowerSumSymmetric, deg]=Table[
	SnCharacter[mu,lam]/ZCoefficient[lam]
	,{mu, IntegerPartitions[deg]},
{lam, IntegerPartitions[deg]}];


(* Now we have M-to-P basis *)
symFuncTransMatInternal[MonomialSymmetric, PowerSumSymmetric, deg_]:=
symFuncTransMatInternal[MonomialSymmetric, PowerSumSymmetric, deg]=
symFuncTransMatInternal[MonomialSymmetric, SchurSymmetric, deg].symFuncTransMatInternal[SchurSymmetric, PowerSumSymmetric, deg];


(* Now we have P-to-M basis *)
symFuncTransMatInternal[PowerSumSymmetric, MonomialSymmetric, deg_]:=
symFuncTransMatInternal[PowerSumSymmetric, MonomialSymmetric, deg]=
symFuncTransMatInternal[PowerSumSymmetric, SchurSymmetric, deg].symFuncTransMatInternal[SchurSymmetric, MonomialSymmetric, deg];



(* Thm. 5.1 in https://arxiv.org/pdf/1712.08023.pdf *)
(* Util for expressing monomial in power-sum *)
(* This is not used --- computing transition matrices is quicker using characters. *)
mTop[lam_, mu_] := 0 /; (! PartitionDominatesQ[lam, mu]);
mTop[lam_, lam_] := 1/(Times @@ (PartitionPartCount[lam]!));
mTop[lam_, mu_] := With[{n=Tr@lam},
	augMinPHelper[
		PadRight[PartitionPartCount@lam, n],
		PadRight[PartitionPartCount@mu, n], n]]/(Times @@ (PartitionPartCount[lam]!));

(* This is based on Corollary 3, of  10.1186/s40064-015-1506-5, 
	Mircea Merca - Augmented monomials in terms of power sums.
	This is not used.
*)
augMinPHelper[{}, {}, 0] := 1;
augMinPHelper[muPC_List, muPC_List, n_Integer] := 1;
augMinPHelper[lamPC_List, muPC_List, n_Integer] := 
	augMinPHelper[lamPC, muPC, n] = With[
		{j = FirstPosition[lamPC, i_ /; i > 0][[1]]},
		With[{
			lamPCi = Function[{i}, Table[
				lamPC[[r]] + Which[
					r == i || r == j, -1 - KroneckerDelta[i, j],
					r == i + j, 1,
					True, 0]
				, {r, n}]]
			},
			
		If[
			Length[muPC] >= j && muPC[[j]] != 0,
			augMinPHelper[
				MapAt[# - 1 &, lamPC, j][[1 ;; n - j]],
				MapAt[# - 1 &, muPC, j][[1 ;; n - j]], n - j],
			0]
			-
			Sum[
			With[{coeff = (lamPC[[i]] - KroneckerDelta[i, j])},
				If[coeff != 0, coeff *augMinPHelper[lamPCi[i], muPC, n], 0]]
			, {i, n}]
]];



symFuncTransMatInternal[ElementaryESymmetric, PowerSumSymmetric, deg_]:=
symFuncTransMatInternal[ElementaryESymmetric, PowerSumSymmetric, deg]=
symFuncTransMatInternal[ElementaryESymmetric, MonomialSymmetric,deg].symFuncTransMatInternal[MonomialSymmetric,PowerSumSymmetric, deg];


(* We have this simple relation. *)
symFuncTransMatInternal[CompleteHSymmetric,PowerSumSymmetric,deg_]:=
symFuncTransMatInternal[CompleteHSymmetric,PowerSumSymmetric,deg]=
Abs@symFuncTransMatInternal[ElementaryESymmetric, PowerSumSymmetric, deg];



(* Forgotten to monomial. *)
symFuncTransMatInternal[ForgottenSymmetric, MonomialSymmetric, deg_]:=
Transpose@symFuncTransMatInternal[ElementaryESymmetric, CompleteHSymmetric, deg];


(* Remaining transition matrices are calculated as below via matrix inverse and composition *)

(* Simply matrix multiplication *)
symFuncTransMatInternal[fromBasisFunc_, toBasisFunc_, deg_Integer] :=
symFuncTransMatInternal[fromBasisFunc, MonomialSymmetric,deg].symFuncTransMatInternal[MonomialSymmetric,toBasisFunc, deg];


symFuncTransMatInternal[MonomialSymmetric,toBasisFunc_, deg_Integer]:=
	symFuncTransMatInternal[MonomialSymmetric,toBasisFunc,deg]=(
	Inverse@symFuncTransMatInternal[toBasisFunc, MonomialSymmetric, deg]);


(* Generic basis (defined by user) is calculated via its monomial expansion function. *)
(* This is not used for core bases *)
symFuncTransMatInternal[fromBasisFunc_, MonomialSymmetric, deg_Integer]:=
	symFuncTransMatInternal[fromBasisFunc, MonomialSymmetric, deg]=(Table[
		Coefficient[fromBasisFunc[lam], MonomialSymbol[mu] ]
	,{lam, IntegerPartitions[deg]},
	{mu, IntegerPartitions[deg]}
]);


(**********************************************************)
(**********************************************************)
(**********************************************************)


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
			symFuncTransMatInternal[MonomialSymmetric,toBasisFunc,deg].Table[
				toBasisSymb[lam,y]
			,{lam,IntegerPartitions[deg]}]
		}
	,
	1
];

(* 
Given a function for producing the monomial expansion,
	and a symbol for the new basis, produce a replacement rule to do this conversion.
	It maps from alphabet x, to alphabet y.
	
	We prefer to use the elementary basis, as it multiplies easy. 
*)
fromElementaryToCoreBasisRule[{toBasisFunc_,toBasisSymb_}, deg_Integer, {x_, y_}]:=
	MapThread[
		Rule,
		{
			Table[
				ElementaryESymbol[lam,x]
			,{lam,IntegerPartitions[deg]}]
		,
			symFuncTransMatInternal[ElementaryESymmetric,toBasisFunc,deg].Table[
				toBasisSymb[lam,y]
			,{lam,IntegerPartitions[deg]}]
		}
	,
	1
];

basisInElementary[bb_,lam_]:=Module[{n=Tr@lam, transMat, ip, vec},
	transMat = symFuncTransMatInternal[bb,ElementaryESymmetric,n];
	ip = IntegerPartitions[n];
	
	(* Vector such that elem j, is the e-expansion of bb[ lam[[j]] ] *)
	vec = transMat.(ElementaryESymbol[#]&/@ip);
	
	(* Memoize expansion for all partitions of same size. *)
	Do[
		basisInElementary[bb,ip[[j]]] = vec[[ j ]];
	,{j,Length@ip}];
	
	(* Return specific. *)
	basisInElementary[bb,lam]
];

basisInElementary[bb_,lam_List,x_]:=ChangeFunctionAlphabet[basisInElementary[bb,lam],x];


(* This is used to define the core bases via the transition matrices. *)
basisInMonomial[bb_,{}]:=1;
basisInMonomial[bb_,lam_List]:=Module[{n=Tr@lam, transMat, ip, vec, j, lamFixed},
	transMat = symFuncTransMatInternal[bb,MonomialSymmetric,n];
	ip = IntegerPartitions[n];
	
	
	(* Vector such that elem j, is the m-expansion of bb[ lam[[j]] ] *)
	vec = transMat.(MonomialSymbol[#]&/@ip);
	
	(* Memoize expansion for all partitions of same size. *)
	Do[
		basisInMonomial[bb,ip[[j]]] = vec[[ j ]];
	,{j, Length@ip}];
	
	(* Return specific. *)
	basisInMonomial[bb,lam]
];
basisInMonomial[bb_,lam_,x_]:=ChangeFunctionAlphabet[basisInMonomial[bb,lam],x];



(* Support for several alphabets at once *)
toOtherSymmetricBasis::usage = "toOtherSymmetricBasis[{basisFunc, basisSymbol}, poly, x] converts poly to the basis specified by basisFunc and basisSymbol. The alphabet x defaults to None; a list of alphabets is also accepted.";
toOtherSymmetricBasis[{basisFunc_,basisSymbol_}, poly_, alphabet_List] :=Fold[
	toOtherSymmetricBasis[{basisFunc,basisSymbol}, #1, #2]&,
poly, alphabet];

(* Make listable *)
toOtherSymmetricBasis[{basisFunc_,basisSymbol_}, polyList_List, x_: None] :=Table[
	toOtherSymmetricBasis[{basisFunc,basisSymbol}, poly, x]
	, {poly, polyList}];

(* Uses elementary as intermediate basis. *)
toOtherSymmetricBasis[{basisFunc_, basisSymbol_}, poly_, x_: None] := Module[
	{bases,replaceRule,degrees,inEBasis},
	
	bases = {
		{MonomialSymmetric, MonomialSymbol},
		{PowerSumSymmetric, PowerSumSymbol},
		{ElementaryESymmetric,ElementaryESymbol},
		{CompleteHSymmetric,CompleteHSymbol},
		{SchurSymmetric,SchurSymbol},
		{ForgottenSymmetric, ForgottenSymbol}
	}; 
	
	(* 
		Rule for doing conversion to elementary basis in expression.
	*)
	replaceRule = (#2[lam_,x] :> basisInElementary[#1,lam,x])& @@@ bases;
	
	(* Convert everything to elementary basis. Here, do replace first, THEN expand. *)
	inEBasis = Expand[ poly /. replaceRule ];
	
	(* Which degrees appear *)
	degrees = Union[Cases[inEBasis, ElementaryESymbol[lam__, x] :> Tr[lam], {0, Infinity}]];
	
	(* Replace everything to desired basis, in each degree *)
	(* Since we expanded before, there are no product in this, only sum/difference *)
	Expand[
		inEBasis /. (Join@@Table[
			fromElementaryToCoreBasisRule[{basisFunc,basisSymbol},d,{x,x}]
		, {d, degrees}])
	]
];


(* These are all listable. *)
ToSchurBasis::usage = "ToSchurBasis[poly, x] converts poly to the Schur basis. The alphabet x defaults to None.";
ToSchurBasis[poly_, x_: None] := toOtherSymmetricBasis[{SchurSymmetric,SchurSymbol},Expand@poly, x];
ToPowerSumBasis::usage = "ToPowerSumBasis[poly, x] converts poly to the power-sum basis. The alphabet x defaults to None.";
ToPowerSumBasis[poly_, x_: None] := toOtherSymmetricBasis[{PowerSumSymmetric,PowerSumSymbol},Expand@poly, x];
ToElementaryEBasis::usage = "ToElementaryEBasis[poly, x] converts poly to the elementary basis. The alphabet x defaults to None.";
ToElementaryEBasis[poly_, x_: None] := toOtherSymmetricBasis[{ElementaryESymmetric,ElementaryESymbol},Expand@poly, x];
ToCompleteHBasis::usage = "ToCompleteHBasis[poly, x] converts poly to the complete homogeneous basis. The alphabet x defaults to None.";
ToCompleteHBasis[poly_, x_: None] := toOtherSymmetricBasis[{CompleteHSymmetric, CompleteHSymbol},Expand@poly, x];
ToMonomialBasis::usage = "ToMonomialBasis[poly, x] converts poly to the monomial basis. The alphabet x defaults to None.";
ToMonomialBasis[poly_, x_: None] := toOtherSymmetricBasis[{MonomialSymmetric,MonomialSymbol},Expand@poly, x];


(* A normalized version, where we multiply by the z-coefficient. *)
ToPowerSumZBasis::usage = "ToPowerSumZBasis[poly, x] converts poly to the power-sum basis with each power-sum term multiplied by its z-coefficient. The alphabet x defaults to None.";
ToPowerSumZBasis[poly_, x_: None] := 
	(ToPowerSumBasis[poly,x]/.PowerSumSymbol[lam_,x] :> ZCoefficient[lam] PowerSumSymbol[lam,x]);


(*********************************************************************************************)
(*********************************************************************************************)
(*********************************************************************************************)
(*
	Define core symmetric functions via transition matrices.
*)


MonomialSymmetric::usage = "MonomialSymmetric[lam, x] returns the monomial symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.";
MonomialSymmetric[lam_List,x_:None]:=MonomialSymbol[lam,x];

AugmentedMonomialSymmetric::usage = "AugmentedMonomialSymmetric[lam, x] returns the augmented monomial symmetric function indexed by the integer vector lam in alphabet x. The alphabet x defaults to None.";
AugmentedMonomialSymmetric[lam_] :=AugmentedMonomialSymmetric[lam,None];
AugmentedMonomialSymmetric[lam_?VectorQ,x_] := Times @@ (PartitionPartCount[lam]!) MonomialSymbol[sortToPartition@lam,x];

CompleteHSymmetric::usage = "CompleteHSymmetric[lam, x] returns the complete homogeneous symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.";
CompleteHSymmetric[a_]:=CompleteHSymmetric[a,None];
CompleteHSymmetric[d_Integer, x_] := Which[d<0,0,d==0,1,True,CompleteHSymmetric[{d}, x]];
CompleteHSymmetric[{}, x_] := 1;
CompleteHSymmetric[lam_?VectorQ, x_] := 
	If[Min[lam]>=0, basisInMonomial[CompleteHSymmetric, sortToPartition@lam, x],0];

ElementaryESymmetric::usage = "ElementaryESymmetric[lam, x] returns the elementary symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.";
ElementaryESymmetric[a_]:=ElementaryESymmetric[a,None];
ElementaryESymmetric[d_Integer, x_] := Which[d<0,0,d==0,1,True,ElementaryESymmetric[{d}, x]];
ElementaryESymmetric[{}, x_] := 1;
ElementaryESymmetric[lam_?VectorQ, x_] := 
	If[Min[lam]>=0, basisInMonomial[ElementaryESymmetric,sortToPartition@lam, x],0];

PowerSumSymmetric::usage = "PowerSumSymmetric[lam, x] returns the power-sum symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.";
PowerSumSymmetric[a_]:=PowerSumSymmetric[a,None];
PowerSumSymmetric[d_Integer, x_] := Which[d<0,0,d==0,1,True,PowerSumSymmetric[{d}, x]];
PowerSumSymmetric[{}, x_] := 1;
PowerSumSymmetric[lam_?VectorQ, x_] := 
	If[Min[lam]>=0, basisInMonomial[PowerSumSymmetric,sortToPartition@lam,x],0];


ForgottenSymmetric::usage = "ForgottenSymmetric[lam, x] returns the forgotten symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.";
ForgottenSymmetric[mu_?VectorQ] := ForgottenSymmetric[mu, None];
ForgottenSymmetric[mu_?VectorQ, x_] := 
	If[Min[mu]>=0, basisInMonomial[ForgottenSymmetric,sortToPartition@mu, x],0];


SchurSymmetric::usage = "SchurSymmetric[lam, x] returns the Schur symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.";
SchurSymmetric[d_Integer,x_]:=SchurSymmetric[{d},x];
SchurSymmetric[lam_?VectorQ]:=SchurSymmetric[lam, None];
SchurSymmetric[{},x_]:=1;

(* Slinky rule. *)
SchurSymmetric[lam_?VectorQ, x_] := With[{slr=CompositionSlinky[lam]},
	If[slr[[2]]==0 || Min[lam]<0,
	0,
	slr[[2]]*SchurSymmetric[slr[[1]], x]
	]
] /; Not[OrderedQ[Reverse@lam]];

SchurSymmetric[mu_?VectorQ, x_]:=basisInMonomial[SchurSymmetric,sortToPartition@mu,x];



(*********************************************************************************************)
(*********************************************************************************************)
(*********************************************************************************************)


OmegaInvolution::usage = "OmegaInvolution[expr, x] applies the omega involution to expr. The alphabet x defaults to None. \
Caution: It only acts on the common symmetric functions.";
OmegaInvolution[poly_, x_: None] := (
	poly /. {
		MonomialSymbol[mu_, x] :> ForgottenSymbol[mu, x],
		ForgottenSymbol[mu_, x] :> MonomialSymbol[mu, x],
		PowerSumSymbol[mu_,x] :> (-1)^(Tr[mu] - Length[mu]) PowerSumSymbol[mu, x],
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



SymmetricFunctionToPolynomial::usage = "SymmetricFunctionToPolynomial[expr, x, n, y] expresses expr as a polynomial in n variables x[1] through x[n], using alphabet y in expr. The alphabet x and variable count n default to None and the degree of expr, respectively; y defaults to None.";

SymmetricFunctionToPolynomial[MonomialSymbol[mu_List, None], x_, 0] := 0;
SymmetricFunctionToPolynomial[MonomialSymbol[mu_List, None], x_, n_Integer] := 
	If[
		Length[mu]>n, 0,
		Sum[ Times @@ ((x /@ Range[n])^p)
	, {p, Permutations[PadRight[mu, n]]}]
];


SymmetricFunctionToPolynomial[ElementaryESymbol[mu_List, None], x_, 0] := 0;
SymmetricFunctionToPolynomial[ElementaryESymbol[mu_List, None], x_, n_Integer] := Which[
		Max[mu]>n, 0,
		True,
		Product[ 
			Sum[Times @@ (x /@ ss), {ss, Subsets[Range@n, {m}]}]
		,{m,mu}]
];

SymmetricFunctionToPolynomial[expr_, x_:None] := SymmetricFunctionToPolynomial[expr, x, SymmetricFunctionDegree[expr]];

(* TODO - This can be made more efficient if we replace expressions directly for some bases,
 as we did above. *)
SymmetricFunctionToPolynomial[expr_, x_, n_Integer, yy_: None] := 
	ReplaceAll[
		ToElementaryEBasis[expr,yy]
		,
		ElementaryESymbol[mu_List, yy] :> 
			SymmetricFunctionToPolynomial[ElementaryESymbol[mu, None], x, n]
];


PolynomialToSymmetricFunction::usage = "PolynomialToSymmetricFunction[poly, x, basisSymbol, n] converts a symmetric polynomial in x[1], ..., x[n] to the requested basis (MonomialSymbol, SchurSymbol, ElementaryESymbol, CompleteHSymbol, PowerSumSymbol or ForgottenSymbol). The basis defaults to SchurSymbol and n is inferred from the variables present when omitted. Only partitions with at most n parts can occur.";
PolynomialToSymmetricFunction::nonsymmetric = "The polynomial is not symmetric in the variables `1`[1], ..., `1`[`2`].";
PolynomialToSymmetricFunction::basis = "Unknown symmetric-function basis symbol `1`.";

polynomialVariableIndices[poly_, x_] := Union@Cases[Variables[Expand[poly]],
	HoldPattern[x[i_]] :> i, Infinity];

(* Coefficients of poly as a polynomial in x[1], ..., x[n], grouped by the sorted exponent
   vector. poly is symmetric iff each group is a full orbit with equal coefficients. *)
symmetricOrbitGroups[poly_, x_, n_Integer] :=
	GroupBy[CoefficientRules[Expand[poly], x /@ Range[n]], Sort[First[#], Greater] & -> Last];

polynomialSymmetricQ[groups_Association, n_Integer] := And @@ KeyValueMap[
	Function[{lam, cs},
		Length[cs] == Multinomial @@ Tally[lam][[All, 2]] &&
		AllTrue[cs, Expand[# - First[cs]] === 0 &]],
	groups];

PolynomialToSymmetricFunction[poly_, x_, n_Integer] :=
	PolynomialToSymmetricFunction[poly, x, SchurSymbol, n];

PolynomialToSymmetricFunction[poly_, x_, basisSymbol_: SchurSymbol] := Module[
	{n = Max[0, Select[polynomialVariableIndices[poly, x], IntegerQ]]},
	PolynomialToSymmetricFunction[poly, x, basisSymbol, n]
];

PolynomialToSymmetricFunction[poly_, x_, basisSymbol_, n_Integer] := Module[
	{groups, monomial},
	groups = symmetricOrbitGroups[poly, x, n];
	If[! SubsetQ[Range[n], polynomialVariableIndices[poly, x]] ||
			! polynomialSymmetricQ[groups, n],
		Message[PolynomialToSymmetricFunction::nonsymmetric, x, n]; Return[$Failed]];
	monomial = Total[KeyValueMap[
		With[{lam = DeleteCases[#1, 0]},
			If[lam === {}, First[#2], First[#2] MonomialSymbol[lam, None]]] &,
		groups]];
	Switch[basisSymbol,
		MonomialSymbol, monomial,
		SchurSymbol, ToSchurBasis[monomial],
		ElementaryESymbol, ToElementaryEBasis[monomial],
		CompleteHSymbol, ToCompleteHBasis[monomial],
		PowerSumSymbol, ToPowerSumBasis[monomial],
		ForgottenSymbol, toOtherSymmetricBasis[{ForgottenSymmetric, ForgottenSymbol}, monomial],
		_, Message[PolynomialToSymmetricFunction::basis, basisSymbol]; $Failed]
];


(***********************************************************************************)



(* TODO -- Optimize so that known formulas for princ. spec. is used? *)
(*

 PrincipalSpecialization[ElementaryESymbol[n], q, k]  ===     q^Binomial[n, 2] qBinomial[k, n, q] 
 PrincipalSpecialization[CompleteHSymbol[n], q, k] ===  qBinomial[n + k - 1, n, q] 
 PrincipalSpecialization[PowerSumSymbol[n], q, k] === (1 - q^(k n))/(1 - q^n) 
*)

PrincipalSpecialization::usage = "PrincipalSpecialization[poly, q, k, x] gives the principal specialization of poly. The parameter k gives the number of variables and defaults to Infinity; the alphabet x defaults to None.";

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
			Expand[ToMonomialBasis[poly, x]] /. MonomialSymbol[lam_, x] :> psMon[lam],
		
		True,
			ToPowerSumBasis[poly,  x] /. 
			PowerSumSymbol[lam_,x] :> Product[Sum[q^(i (j - 1)), {j, k}], {i, lam}]
		]
];



(* Use power-sum, as this is compatible with q,t-extension. *)
HallInnerProduct::usage="HallInnerProduct[f, g] returns the Hall inner product of symmetric functions f and g.
HallInnerProduct[f, g, {q, t}, x] computes the q,t inner product in alphabet x, which defaults to None.";

HallInnerProduct[f_, g_] := HallInnerProduct[f, g, {0, 0}, None];
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
	
(* Combine all power-sum terms *)
Expand@Sum[
	vF = First@rF;
	vG = First@rG;
	
	(* Special cases depending on constant term. *)
	If[Total[vF] == 0, 
		lam = {}
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


JackInnerProduct::usage="JackInnerProduct[f, g, a] returns the Jack inner product of symmetric functions f and g with parameter a, which defaults to 1.
JackInnerProduct[f, g, a, x] computes the inner product in alphabet x, which defaults to None.";

JackInnerProduct[f_, g_,a_:1] := JackInnerProduct[f, g, a, None];
JackInnerProduct[f_, g_, a_, x_: None] := Module[{
	ff, gg, lam,
	vars, rulesF, rulesG, vF, vG, rF, rG
	},

	(* Convert to powersum *)
	ff = ToPowerSumBasis[f, x];
	gg = ToPowerSumBasis[g, x];

	vars = Cases[Variables[{ff, gg}], PowerSumSymbol[__,x], {0, Infinity}];

	rulesF = CoefficientRules[ff, vars];
	rulesG = CoefficientRules[gg, vars];

(* Combine all power-sum terms *)
Expand@Sum[
	vF = First@rF;
	vG = First@rG;

	(* Special cases depending on constant term. *)
	If[Total[vF] == 0,
		lam = {}
		,
		lam = Pick[vars, vF, 1][[1, 1]]
	];

	If[vF != vG,
		0,
		Last[rF] * Last[rG] * ZCoefficient[lam] * a^Length[lam]
	]
	, {rF, rulesF}, {rG, rulesG}]
];

JackLowerHook::usage = "JackLowerHook[mu, a, {r, c}] returns the Jack lower hook at box {r, c} of partition mu. JackLowerHook[mu, a] returns the product of lower hooks over mu. The parameter a defaults to 1.";
JackLowerHook[mu_List,
   a_ : 1, {r_Integer, c_Integer}] :=
  (a PartitionArm[mu, {r, c}] +
    PartitionLeg[mu, {r, c}] + 1);
JackLowerHook[mu_List, a_ : 1] := Product[
   JackLowerHook[mu, a, box], {box, DiagramBoxes[mu]}];

JackUpperHook::usage = "JackUpperHook[mu, a, {r, c}] returns the Jack upper hook at box {r, c} of partition mu. JackUpperHook[mu, a] returns the product of upper hooks over mu. The parameter a defaults to 1.";
JackUpperHook[mu_List,
   a_ : 1, {r_Integer, c_Integer}] :=
  (a PartitionArm[mu, {r, c}] +
    PartitionLeg[mu, {r, c}] + a);
JackUpperHook[mu_List, a_ : 1] := Product[
   JackUpperHook[mu, a, box], {box, DiagramBoxes[mu]}];


(* Computes f(-x1,-x2,...) *)
(* TODO-UPDATE with rules for all bases *)
NegateAlphabet[f_, x_: None] := ToMonomialBasis[f] /. MonomialSymbol[mu__, x] :> (-1)^(Tr@mu) MonomialSymbol[mu, x];


(* Here, f is in the xx alphabet, and plethysm act on ALL given alphabets in the g-expression. *)
Plethysm::usage = "Plethysm[f, g, xx] computes the plethysm of symmetric functions f and g, acting on all alphabets in g. The alphabet xx of f defaults to None.";
Plethysm[f_, g_, xx_: None] := Module[
	{PkPlethysmWithG, fInP, gInP,
		gVars, auxVars, alphabets
	},
	
	(* HERE, we decide which alphabet to replace into. *)
	fInP = ToPowerSumBasis[f, xx];
	
	(* Here, we might have several alphabets *)
	alphabets = FunctionAlphabets[g];
	gInP = Fold[ToPowerSumBasis[#1, #2] &, g, alphabets];
	
	gVars = Cases[Variables[gInP], PowerSumSymbol[__,_], {0, Infinity}];
	
	auxVars = Complement[Variables[{fInP, gInP}],
		Cases[Variables[fInP], PowerSumSymbol[__,_], {0, Infinity}], gVars];
	
	PkPlethysmWithG[k_] := (gInP /. Table[v -> v^k, {v, auxVars}]) /. {PowerSumSymbol[mu__,x_] :> PowerSumSymbol[k*mu, x]};
	
	(* Here, we substitute into ALL alphabets in the f-expression. *)
	Expand[fInP /. PowerSumSymbol[mu_List, x_] :>
		Times @@ (PkPlethysmWithG /@ mu)]
];


KroneckerCoefficient::usage = "KroneckerCoefficient[lam,mu,nu] returns the Kronecker coefficient.";
KroneckerCoefficient[lam_List, mu_List, nu_List] := Module[{pleth, n = Tr@lam, x, y,
	lamP = sortToPartition[lam], muP = sortToPartition[mu], nuP = sortToPartition[nu]},
   If[Not[Tr[lam] == Tr[mu] == Tr[nu]], 0,
    cached[{KroneckerCoefficient, lamP, muP, nuP},
     pleth =
      ToSchurBasis[
       Plethysm[SchurSymbol[lamP, x],
        PowerSumSymbol[1, x] PowerSumSymbol[1, y], {x}], {x, y}];
     (* Store the coefficients for all pairs of partitions of n. *)
     Do[
      sfCache[{KroneckerCoefficient, lamP, a, b}] = Coefficient[
         pleth, SchurSymbol[a, x] SchurSymbol[b, y]];
      , {a, IntegerPartitions@n}, {b, IntegerPartitions@n}];
     sfCache[{KroneckerCoefficient, lamP, muP, nuP}]
     ]
    ]
];

InternalProduct::usage = "InternalProduct[f, g, x] computes the internal (Kronecker) product of symmetric functions f and g in alphabet x. The alphabet x defaults to None.";
InternalProduct[f_, g_, x_ : None] :=
  Module[{ff, gg, vars, rulesF, rulesG, vF, vG, rF,
    rG},(*Convert to Schur basis*)ff = ToPowerSumBasis[f, x];
   gg = ToPowerSumBasis[g, x];
   vars =
    Cases[Variables[{ff, gg}], PowerSumSymbol[__, x], {0, Infinity}];
   rulesF = CoefficientRules[ff, vars];
   rulesG = CoefficientRules[gg, vars];
   Sum[
    vF = First@rF;
    vG = First@rG;
    Which[vF =!= vG, 0,(*Special cases depending on constant term.*)

         Tr[vF] == Tr[vG] == 0,
     Last[rF]*Last[rG], True,
     With[{lam = Pick[vars, vF, 1][[1, 1]]},
      Last[rF]*Last[rG] PowerSumSymbol[lam, x] ZCoefficient[lam]]
     ]
    , {rF, rulesF}, {rG, rulesG}]
   ];

(*
InternalProduct[f_, g_, x_ : None] := Module[{ff, gg, lam, mu, nu, vars, rulesF, rulesG, vF, vG, rF, rG},
   (*Convert to Schur basis *)
   ff = ToSchurBasis[f, x];
   gg = ToSchurBasis[g, x];
   vars =
    Cases[Variables[{ff, gg}], SchurSymbol[__, x], {0, Infinity}];
   rulesF = CoefficientRules[ff, vars];
   rulesG = CoefficientRules[gg, vars];

   (* Combine all Schur terms *)
   Sum[
    vF = First@rF;
    vG = First@rG;
    (*Special cases depending on constant term.*)

    If[Tr@vF == 0, lam = {}, lam = Pick[vars, vF, 1][[1, 1]]];
    If[Tr@vG == 0, mu = {}, mu = Pick[vars, vG, 1][[1, 1]]];

    Which[
     Tr[lam] != Tr[mu], 0,
     Tr[vF] == Tr[vG] == 0, Last[rF]*Last[rG],
     True,
     Last[rF]*
      Last[rG] Sum[
       KroneckerCoefficient[lam, mu, nu] SchurSymbol[nu, x], {nu,
        IntegerPartitions@Tr@mu}]
     ]
    , {rF, rulesF}, {rG, rulesG}]
   ];
*)

(*********************************************************************************************)
(*********************************************************************************************)
(*********************************************************************************************)
(*
		Definitions of other bases below. These are defined in the natural core basis.
*)
(*********************************************************************************************)
(*********************************************************************************************)
(*********************************************************************************************)


SkewSchurSymmetric::usage = "SkewSchurSymmetric[lam, x] returns the Schur symmetric function of partition lam in alphabet x. The alphabet x defaults to None.
SkewSchurSymmetric[{lam, mu}, x] returns the skew Schur symmetric function of shape lam/mu in alphabet x. The alphabet x defaults to None.";
SkewSchurSymmetric[lam_List]:=SkewSchurSymmetric[{lam, {}}, None];
SkewSchurSymmetric[lam_List, x_]:=SkewSchurSymmetric[{lam, {}}, x];
SkewSchurSymmetric[{lam_List, mu_List}]:=SkewSchurSymmetric[{lam, mu}, None];

SkewSchurSymmetric[{lam_List, mu_List},None] := cached[{SkewSchurSymmetric, lam, mu}, Module[
	{bb, mup = PadRight[mu, Length@lam]},
	
Expand@Which[
	!PartitionLessEqualQ[mu, lam],
		0
	,
	Length[lam]==0, 
		1
	,
	(* Use Jacob-Trudi *)
	True,
		With[{d = lam[[1]]},
			jacobiTrudiDet[
				{PadRight[ConjugatePartition[lam], d],
				PadRight[ConjugatePartition[mu], d]}
				, bb, d]
		] /. bb[v_] :> ElementaryESymbol[v]
	]
]];

SkewSchurSymmetric[{lam_List, mu_List}, x_] := ChangeFunctionAlphabet[ SkewSchurSymmetric[{lam, mu}, None],  x];



(* This uses https://doi.org/10.37236/1539  *)
JackPSymmetric::usage = "JackPSymmetric[lam, a, x] returns the Jack P symmetric function indexed by partition lam with parameter a. The alphabet x defaults to None.";
JackPSymmetric[lam_List,a_, x_: None] := cached[{JackPSymmetric, lam, a, x}, Sum[
	KostkaCoefficient[lam,mu,a] MonomialSymbol[mu,x]
,{mu,IntegerPartitions[Tr@lam]}]];

JackJSymmetric::usage = "JackJSymmetric[lam, a, x] returns the Jack J symmetric function indexed by partition lam with parameter a. The alphabet x defaults to None.";
JackJSymmetric[lam_List,a_, x_: None] := Together[JackPSymmetric[lam,a,x] Product[
	a*PartitionArm[lam,s] + PartitionLeg[lam,s]+1
,{s,DiagramBoxes[lam]}]];



(* This is not the modified Hall-Littlewood! *)
(*
http://igm.univ-mlv.fr/~fpsac/FPSAC02/ARTICLES/Tudose.pdf
*)
HallLittlewoodTSymmetric::usage = "HallLittlewoodTSymmetric[lam, q, x] returns the transformed Hall-Littlewood symmetric function indexed by partition lam with parameter q. The alphabet x defaults to None.";
HallLittlewoodTSymmetric[lam_List, q_, x_: None] := 
cached[{HallLittlewoodTSymmetric, lam, q, x}, Module[{Rij,
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
	
	operators = Reverse[Join @@
		Table[applyIJ[i, j], {j, n, 1, -1}, {i, j - 1, 1, -1}]];
	
	res = Composition[Sequence @@ operators][ hh[PadRight[lam, n]] ];
	(* Replace with complete homogeneous sym funcs.*)
	
	Expand[res /. {hh[a_] :> CompleteHSymbol[a, x], qq -> q}]
]];


kSchurSymmetric::usage = "kSchurSymmetric[mu, k, t, x] returns the k-Schur function indexed by partition mu, with mu1 <= k. The parameter t defaults to 1 and the alphabet x defaults to None.";
kSchurSymmetric[mu_List, kk_Integer, t_ : 1, x_ : None] := cached[{kSchurSymmetric, mu, kk, t, x}, Module[
    {Rij, res, operators, ll = Length@mu, n = Tr@mu, applyIJ, ss, qq},
     Rij[vec_List, i_Integer, j_Integer, k_Integer] :=
     If[i != j,
      ReplacePart[vec, {i -> vec[[i]] + k, j -> vec[[j]] - k}], vec];

    (*Applies 1/(1-qR_ij) to the polynomial*)

    applyIJ[i_, j_][poly_] := Module[{max, out},
      max = (n - j) + Max@Cases[poly, ss[a_] :> a[[j]], {0, Infinity}];
      out =
       poly + Sum[
         poly /. ss[a_List] :> qq^k ss[Rij[a, i, j, k]], {k, max}];
      Expand[out /. {ss[a_] /; Min[a] < 0 :> 0}]
      ];
    (*Operators must appear in this order,in order to optimize*)
    operators =
     Reverse[Join @@
       Table[applyIJ[i, j], {i, ll, 1, -1}, {j,
         kk - PadRight[mu, n][[i]] + i + 1, ll}]];
    operators = SortBy[operators, {Last[#] &, First[#] &}];

    res = Composition[Sequence @@ operators][ss[PadRight[mu, n]]];
    (*Replace symbols with actual Schurs*)

    Expand[res /. {ss[a_] :> SchurSymbol[a, x], qq -> t}]
    ]];

(* Compute Hall-Littlewood P via inverse Kostka-Foulkes matrix.*)
(* This matrix is computed via HallLittlewoodTSymmetric *)


kostkaFoulkesMat[n_Integer]:=kostkaFoulkesMat[n] = Module[{lam,mu},
	Table[
		With[{
			poly = ToSchurBasis[HallLittlewoodTSymmetric[lam, SPECIALT ]]
			},
				Table[Coefficient[poly, SchurSymbol[mu]], {mu, IntegerPartitions@n}]
		]
	, {lam, IntegerPartitions@n}]
];



HallLittlewoodPSymmetricHelper[lam_List] := HallLittlewoodPSymmetricHelper[lam] = Module[{hlPolys,ip,j,n=Tr@lam},

	hlPolys = (SchurSymbol/@IntegerPartitions[n]).(Inverse@kostkaFoulkesMat[n]);
	
	ip = IntegerPartitions[n];
	(* Memoize expansion for all partitions of same size. *)
	Do[
		HallLittlewoodPSymmetricHelper[ip[[j]]] = hlPolys[[j]];
	,{j, Length@ip}];
	
	HallLittlewoodPSymmetricHelper[lam]
];

HallLittlewoodPSymmetric::usage = "HallLittlewoodPSymmetric[lam, t, x] returns the Hall-Littlewood P symmetric function indexed by partition lam with parameter t. The alphabet x defaults to None.";
HallLittlewoodPSymmetric[lam_List, t_, x_: None] := ChangeFunctionAlphabet[
	HallLittlewoodPSymmetricHelper[lam], x] /. {SPECIALT -> t};


ToHallLittlewoodPBasis::usage = "ToHallLittlewoodPBasis[poly, t, x, mh] converts poly to the Hall-Littlewood P basis with parameter t. The alphabet x defaults to None and the basis symbol mh defaults to HallLittlewoodPSymbol.";
ToHallLittlewoodPBasis[poly_, t_, x_: None, mh_:HallLittlewoodPSymbol] := 
Expand@Together@toOtherSymmetricBasis[{ToMonomialBasis[HallLittlewoodPSymmetric[#,t]]&, mh}, poly, x];


HallLittlewoodPSymbol::usage = "HallLittlewoodPSymbol[lam, x] represents a Hall-Littlewood P basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.";
CreateBasis[HallLittlewoodPSymbol, "P", 
	MultiplicationFunction -> None,
	PowerFunction->None
];


SchursQSymmetric::usage = "SchursQSymmetric[lam]";
SchursQSymmetric[lam_List, x_: None]:=cached[{SchursQSymmetric, lam, x}, ToMonomialBasis@Module[{q},
	Plethysm[
	HallLittlewoodTSymmetric[lam, q,x]
	, (1 - q) PowerSumSymmetric[{1},x]
] /. q -> -1]];

SchursPSymmetric::usage = "SchursPSymmetric[lam, x] returns the Schur P symmetric function indexed by strict partition lam. The alphabet x defaults to None.";
SchursPSymmetric[lam_List, x_: None]:=Together[SchursQSymmetric[lam,x]/2^Length[lam]];


(* This is the modified Hall-Littlewood polynomial. *)

HallLittlewoodMSymmetric::usage = "HallLittlewoodMSymmetric[lam, q, x] returns the Hall-Littlewood M symmetric function indexed by partition lam with parameter q. The alphabet x defaults to None.";
HallLittlewoodMSymmetric[lam_List, q_, x_: None] := 
	Together[q^PartitionN[lam] HallLittlewoodTSymmetric[lam, 1/q, x]];

MacdonaldPSymmetric::usage = "MacdonaldPSymmetric[lam, q, t, x] returns the Macdonald P symmetric function indexed by partition lam with parameters q and t. The alphabet x defaults to None.";
MacdonaldPSymmetric[lam_List, q_, t_, x_: None] := ChangeFunctionAlphabet[
	MacdonaldPSymmetricHelper[lam, SPECIALQ, SPECIALT]
	, x] /. {SPECIALQ -> q, SPECIALT -> t};


MacdonaldJSymmetric::usage = "MacdonaldJSymmetric[lam, q, t, x] returns the integral-form Macdonald J symmetric function indexed by partition lam with parameters q and t. The alphabet x defaults to None.";
MacdonaldJSymmetric[lam_List,q_,t_,x_:None]:=(
	Together[
		MacdonaldPSymmetric[lam,q,t,x] * 
		Product[1-q^PartitionArm[lam,s]*t^(1+PartitionLeg[lam,s])
			, {s,DiagramBoxes[lam]}]]);
	
(* 7.13 p. 346 in Macdonald`s book. *)
MacdonaldPSymmetricHelper[mu_List, q_, t_]:=MacdonaldPSymmetricHelper[mu,q,t]=
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
   , {ssyt, SemiStandardYoungTableaux[{mu, {}}, Tr@mu]
 }];


ShiftedJackPSymmetric::usage = "ShiftedJackPSymmetric[lam, a, x] returns the shifted Jack P symmetric function indexed by partition lam with parameter a. The alphabet x defaults to None.";
ShiftedJackPSymmetric[lam_List, a_, x_: None] := ChangeFunctionAlphabet[
	ShiftedJackPSymmetricHelper[lam, SPECIALA], x] /. {SPECIALA -> a};

ShiftedJackPSymmetricHelper[mu_List, a_]:=ShiftedJackPSymmetricHelper[mu,a]=Module[{asPoly,n=Tr@mu,z,r},

(* This is now non-homogeneous in z. *)
	asPoly = Sum[
			(* Product over all boxes in the ssyt *)
			Product[
				( z[ n+1-Extract[ssyt[[1]],s] ] - (s[[2]]-1) + (s[[1]]-1)/a )
			, {s, DiagramBoxes[mu]}]
			*
			With[{ribbons = Table[YoungTableauShape[ssyt, i], {i, Max[ssyt]}]},
				Product[
					JackPsi[rib, a]
					, {rib, Partition[ Reverse@ribbons , 2, 1]}]
			]
		, {w, WeakIntegerCompositions[n,n]}
		, {ssyt, SemiStandardYoungTableaux[{mu, {}}, w]}
	];
	
	(* We do the following shift, and the result should be a symmetric function. *)
	asPoly = asPoly/. z[i_]:>(z[i] + i)/a;
	
(* Extract the coefficients. This is symmetric so only take the partitions *)
Sum[
	With[{nu=r[[1]],c=r[[2]]},
		Boole[OrderedQ[nu]] MonomialSymmetric[nu] c
	]
	,{r,CoefficientRules[asPoly, z/@Range[n]]}]
 
];
 

(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)




MacdonaldHSymmetric::usage = "MacdonaldHSymmetric[lam, q, t, x] returns the modified Macdonald symmetric function H~_lam(x; q, t) in Haglund's convention, so that H~_{2} = s_2 + q s_11, H~_{1,1} = s_2 + t s_11, and H~_mu = t^n(mu) J_mu[X/(1 - 1/t); q, 1/t]. The alphabet x defaults to None.\nMacdonaldHSymmetric[{lam, mu}, q, t, x] gives the skew version.";
MacdonaldHSymmetric[lam_List, q_, t_,x_: None] := MacdonaldHSymmetric[{lam, {}}, q, t,x];

(* The cached internal form below has q and t interchanged relative to Haglund's
   convention (it gives s_2 + t s_11 for mu = {2}); swap them here so that, as in the
   literature, H_mu = t^n(mu) J_mu[X/(1 - 1/t); q, 1/t] and H_{2} = s_2 + q s_11. *)
MacdonaldHSymmetric[{lam_List, mu_List}, q_, t_,x_:None] :=
ChangeFunctionAlphabet[MacdonaldHSymmetric[{lam, mu}, SPECIALQ, SPECIALT], x] /. {SPECIALQ -> t, SPECIALT -> q};

(* This is computed via the F-expansion, using the slinky rule *)
MacdonaldHSymmetric[{lam_List, mu_List}, SPECIALQ, SPECIALT] := cached[{MacdonaldHSymmetric, lam, mu}, Module[
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
]];


ToMacdonaldHBasis::usage = "ToMacdonaldHBasis[poly, q, t, x, mh] converts poly to the modified Macdonald H basis with parameters q and t. The alphabet x defaults to None and the basis symbol mh defaults to MacdonaldHSymbol.";
ToMacdonaldHBasis[poly_, q_, t_, x_: None, mh_:MacdonaldHSymbol] := 
Expand@Together@toOtherSymmetricBasis[{ToMonomialBasis[MacdonaldHSymmetric[#,q,t]]&, mh}, poly, x];

MacdonaldHSymbol::usage = "MacdonaldHSymbol[lam, x] represents a modified Macdonald H basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.";
CreateBasis[MacdonaldHSymbol, "H", 
	MultiplicationFunction -> None,
	PowerFunction->None
];



(* Used in SkewMacdonaldESymmetric *)
symmetricFunctionsPermutationCharge[p_List] := MajorIndex[Reverse@Ordering@p];

PostfixedCharge[mu_List, w_List] := Module[{postFix, decomp},
   postFix = 
    Reverse[Join @@ 
      MapIndexed[ConstantArray[#2[[1]], #1] &, ConjugatePartition@mu]];
   decomp = ChargeWordDecompose[Join[w, postFix]];
   Total[symmetricFunctionsPermutationCharge /@ decomp]
];


SkewMacdonaldESymmetric::usage = "SkewMacdonaldESymmetric[{lam, mu}, q, x] returns the skew Macdonald E symmetric function of shape lam/mu with parameter q in alphabet x.";

SkewMacdonaldESymmetric[lam_List, q_,x_] := SkewMacdonaldESymmetric[{lam,{}}, q, x];

SkewMacdonaldESymmetric[{lam_List, mu_List}, q_,x_]:=
ChangeFunctionAlphabet[SkewMacdonaldESymmetric[{lam, mu}, q,None],x];

SkewMacdonaldESymmetric[{lam_List, mu_List}, q_,None] := 
SkewMacdonaldESymmetric[{lam, mu}, SPECIALQ] /. {SPECIALQ -> q};

SkewMacdonaldESymmetric[{lam_List, mu_List}, SPECIALQ] := cached[{SkewMacdonaldESymmetric, lam, mu},
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
]];


(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)



LLTSymmetric::usage = "LLTSymmetric[nu, q, x] returns the LLT symmetric function associated with the tuple nu of shapes with parameter q. The alphabet x defaults to None.";

LLTSymmetric[nu_List, q_,x_:None]:=ChangeFunctionAlphabet[LLTSymmetric[nu,q,None],x];
LLTSymmetric[nu_List, q_,None] := cached[{LLTSymmetric, nu, q}, Module[
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
			Which[ 
				nui =={}, 0, (* Empty partition *)
				
				IntegerQ[nui[[1]]], Tr[nui],
				True, Tr[nui[[1]]] - Tr[nui[[2]]]
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
]];



(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)

SymplecticSchurSymmetric::usage = "SymplecticSchurSymmetric[lam, x] returns the symplectic Schur symmetric function indexed by partition lam in alphabet x.";
SymplecticSchurSymmetric[lam_,x_]:=ChangeFunctionAlphabet[SymplecticSchurSymmetric[lam],x];

SymplecticSchurSymmetric[{}] := 1;
SymplecticSchurSymmetric[lam_List] := Expand[(1/2) Det@Table[
      CompleteHSymbol[lam[[i]] - i + j] + 
       CompleteHSymbol[lam[[i]] - i - j + 2], 
	{i, Length@lam}, 
	{j, Length@lam}]];


OrthogonalSchurSymmetric::usage = "OrthogonalSchurSymmetric[lam, x] returns the orthogonal Schur symmetric function indexed by partition lam in alphabet x.";
OrthogonalSchurSymmetric[lam_,x_]:=ChangeFunctionAlphabet[OrthogonalSchurSymmetric[lam],x];
OrthogonalSchurSymmetric[{}] := 1;
OrthogonalSchurSymmetric[lam_List] := Det@Table[
    CompleteHSymbol[lam[[i]] - i + j] - 
     CompleteHSymbol[lam[[i]] - i - j], 
	{i, Length@lam}, 
	{j, Length@lam}];

	
bsHelper[n_] := bsHelper[n]=Sum[2^Length[lam] MonomialSymbol[lam, None], {lam,IntegerPartitions[n]}];

BigSchurSymmetric::usage = "BigSchurSymmetric[lam, x] returns the big Schur symmetric function indexed by partition lam in alphabet x.";
BigSchurSymmetric[lam_,x_]:=ChangeFunctionAlphabet[BigSchurSymmetric[lam],x];
BigSchurSymmetric[{}]:=1;
BigSchurSymmetric[lam_List] := MExpand[
Det@Table[
	bsHelper[ lam[[i]] - i + j ], 
	{i, Length@lam}, 
	{j, Length@lam}]];
	

PetrieSymmetric::usage = "PetrieSymmetric[k, m, x] returns the degree-m part of the kth Petrie symmetric function in alphabet x. The alphabet x defaults to None.";
PetrieSymmetric[k_Integer,0,x_:None]:= 1;
PetrieSymmetric[k_Integer,m_Integer,x_:None]:= Sum[
		MonomialSymbol[lam,x]
,{lam, Select[IntegerPartitions[m], #[[1]]<k &] }];


(* Use F-expansion formula instead, if possible. *)
CylindricSchurSymmetric::usage = "CylindricSchurSymmetric[{lam, mu}, d, x] returns the cylindric Schur symmetric function of shape lam/mu and shift d in alphabet x. The shift d and alphabet x default to 0 and None.";
CylindricSchurSymmetric[{lam_List, mu_List}, d_Integer: 0,x_:None]:= Sum[
	MonomialSymbol[YoungTableauWeight@ssyt,x]
,
{ssyt, CylindricTableaux[{lam, mu}, d]}];


(* Based on 7.10c in https://arxiv.org/pdf/1907.02645.pdf *)
LahSymmetricFunction::usage = "LahSymmetricFunction[n, k, x] returns the Lah symmetric function with parameters n and k in alphabet x. The alphabet x defaults to None.";
LahSymmetricFunction[n_Integer, k_Integer,x_:None] := cached[{LahSymmetricFunction, n, k, x}, Expand[
	((n - 1)!/(k - 1)!) Sum[
			With[{ll = Table[Count[alpha, j], {j, n - k}]},
				(-1)^Tr[ll - 1] (Multinomial @@ Append[ll, n - 1])
				Product[(CompleteHSymbol[j,x]/(j + 1))^ll[[j]], {j, n - k}]
				]
			, {alpha, IntegerPartitions[n - k]}]
]];

LahSymmetricFunctionNegative::usage = "LahSymmetricFunctionNegative[n, k, x] returns the negative Lah symmetric function with parameters n and k in alphabet x. The alphabet x defaults to None.";
LahSymmetricFunctionNegative[n_Integer, k_Integer,x_:None] := cached[{LahSymmetricFunctionNegative, n, k, x}, Expand[
	((n - 1)!/(k - 1)!) Sum[
			With[{ll = Table[Count[alpha, j], {j, n - k}]},
				(-1)^Tr[ll - 1] (Multinomial @@ Append[ll, n - 1])
				Product[(ElementaryESymbol[j,x]/(j + 1))^ll[[j]], {j, n - k}]
				]
			, {alpha, IntegerPartitions[n - k]}]
]];



LyndonSymmetric::usage="LyndonSymmetric[lam, x] returns the Lyndon symmetric function indexed by lam. The alphabet x defaults to None.";
LyndonSymmetric[0, x_: None] := 1;
LyndonSymmetric[n_Integer, x_: None] := 
  1/n Sum[MoebiusMu[d] PowerSumSymbol[d, x]^(n/d), {d, Divisors@n}];
LyndonSymmetric[lam_List, x_: None] := Module[{n = Tr@lam, nn},
   Which[
    lam == {}, 1,
    Length[lam] == 1, LyndonSymmetric[lam[[1]],x],
    True,
    Product[
      With[{k = Count[lam, nn]},
       Plethysm[CompleteHSymbol[k], LyndonSymmetric[nn, x]]
       ]
      , {nn, Union[lam]}] // Expand
    ]
];




CompleteHSuperSymmetric[n_Integer, x_, y_] := 
  Sum[CompleteHSymbol[j, x] ElementaryESymbol[n - j, y], {j, 0, n}];
ElementaryESuperSymmetric[n_Integer, x_, y_] := 
  Sum[ElementaryESymbol[j, x] CompleteHSymbol[n - j, y], {j, 0, n}];
SchurSuperSymmetric[lam_List, x_, y_] := Det@Table[
	CompleteHSuperSymmetric[lam[[i]] - i + j, x, y], 
	{i, Length@lam},
	{j, Length@lam}];


(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)



DeltaOperator::usage = "DeltaOperator[f, g, q, t] applies the Garsia Delta operator Delta_f to g: it multiplies each modified Macdonald H~_mu by f[B_mu], where B_mu = sum over cells (r, c) of mu of q^(c-1) t^(r-1) (Haglund's convention).";

DeltaOperator[f_, g_, q_, t_] := cached[{DeltaOperator, f, g, q, t}, Module[{x, val, inH, monoms},
		
		(* Monomials defined by shape. *)
		monoms[mu_List] := (q^(#2 - 1) t^(#1 - 1) & @@@ DiagramBoxes[mu]);
		
		(* This correspond to f[B(mu)] -- it is quicker than plethysm *)
		
		val[mu_List] := SymmetricFunctionToPolynomial[f, x, Tr[mu]] /. x[i_] :> monoms[mu][[i]];
		
		inH = Expand[ToMacdonaldHBasis[g, q, t]];
		Together[inH /. MacdonaldHSymbol[lam_List, None] :> val[lam] MacdonaldHSymmetric[lam, q, t]]
]];

NablaOperator::usage = "NablaOperator[g, q, t] applies the nabla operator to symmetric function g with parameters q and t.";
NablaOperator[g_, q_, t_] := DeltaOperator[ElementaryESymmetric[SymmetricFunctionDegree[g]], g, q, t];

DeltaPrimOperator::usage = "DeltaPrimOperator[f, g, q, t] applies the primed Delta operator to symmetric functions f and g with parameters q and t.";
DeltaPrimOperator[f_, g_, q_, t_] := cached[{DeltaPrimOperator, f, g, q, t}, Module[{x, val, inH, monoms},

		(* Monomials defined by shape. *)
		monoms[mu_List] := Rest[q^(#2 - 1) t^(#1 - 1) & @@@ DiagramBoxes[mu]];
		
		(* This correspond to f[B(mu)-1] -- it is quicker than plethysm *)
		
		val[mu_List] := SymmetricFunctionToPolynomial[f, x, Tr[mu] - 1] /. x[i_] :> monoms[mu][[i]];
		
		inH = Expand[ToMacdonaldHBasis[g, q, t]];
		Together[
		inH /. MacdonaldHSymbol[lam_List, None] :> val[lam] MacdonaldHSymmetric[lam, q, t]]
]];


PrecomputeBasisMatrices::usage = "PrecomputeBasisMatrices[Dimensions->n] computes the transition matrices for
all the most common elementary symmetric functions indexed by partitions of size at most n.
These matrices are stored as a local object (in your home folder).
Use LoadBasisMatrices[] to load these matrices.
Note that time and space used grows rapidly with n!";

Options[PrecomputeBasisMatrices] = {Dimensions -> 20};
PrecomputeBasisMatrices[opts : OptionsPattern[]] :=
  Module[{bases, file},
   file = LocalObject["SymmetricFunctionsMatrices"];
   bases = {
     MonomialSymmetric,
     CompleteHSymmetric,
     ElementaryESymmetric,
     PowerSumSymmetric,
     SchurSymmetric};
   Do[
    Do[
      If[b1 =!= b2,
       SymmetricFunctions`Private`symFuncTransMatInternal[b1, b2, d]
       ]
      , {b1, bases}, {b2, bases}];
    , {d, OptionValue[Dimensions]}];
   (*
   file=FileNameJoin[OptionValue[Directory],OptionValue[
   FileName]];
   *)
   DumpSave[file,
    SymmetricFunctions`Private`symFuncTransMatInternal]
];


LoadBasisMatrices::usage = "LoadBasisMatrices[] loads previously stored transition matrices.
To store transition matrices, use PrecomputeBasisMatrices[]";
Options[LoadBasisMatrices] = {
   Directory -> HomeDirectory[]
   };
LoadBasisMatrices[opts : OptionsPattern[]] := Module[{file},
   file = LocalObject["SymmetricFunctionsMatrices"];
   Check[
    Get[file];
    ,
    Print[
      "No precomputed matrices found.\nUse PrecomputeBasisMatrices to \
compute and store matrices."];
    ,
    LocalObject::nso]
];




SnModuleCharacters::usage = "SnModuleCharacters[polynomialBasis, x] takes a list of polynomials in x[1]...x[n] and returns the Sn-character under the action where Sn acts on the variable indices";
SnModuleCharacters[polysBasisIn_List, xx_]:=SnModuleCharacters[polysBasisIn,{xx}];

SnModuleCharacters[{}, varList_List]:=0;
SnModuleCharacters[{1}, varList_List]:=1; (* Really should be Schur[{n}] *)

SnModuleCharacters[polysBasisIn_List, varList_List] := Module[
	{LinIndependentRows,vars,polysBasis, monomialSupport, polynomialToBasisVector, basisMatrix, rank,
	n, subst,vv,lSolveFunc, imgVec, character, monomialList, indx},
	
	polysBasis = polysBasisIn;
	
	LinIndependentRows[A_List] := Module[{ranks},
		ranks = MatrixRank[A[[1 ;; #]]] & /@ Range[Length@A];
		 First[FirstPosition[ranks, #]] & /@ Range[MatrixRank@A] 
	];
	
	
	(* All variables appearing in total in the bases. *)
	vars = Cases[Variables[polysBasis], a_[_Integer]];
	vars = Select[vars, MemberQ[varList, Head[#]] &];
	
	(* Compute largest index appearing. This is Sn-group size *)
	n = Max[Last/@vars];
	
	(* All monomials (as lists) appearing among all bases. *)
	monomialSupport = 
		Union[Join @@ 
		Table[First /@ CoefficientRules[bb, vars], {bb, polysBasis}]];
	
	(* Writes any vector as combination of monomials appearing in the basis *)
	polynomialToBasisVector[pol_] := 
		Table[Coefficient[pol, Times @@ (vars^m)], {m, monomialSupport}];
	
	(* Each ROW here is a basis vector. *)
	basisMatrix = polynomialToBasisVector /@ polysBasis;
	
	(* If columns are not linearly independent, need to select a subset to be a basis. *)
	rank = MatrixRank[basisMatrix];
	
	Print["The rank is ", rank, "   number of rows in is : ", Length@basisMatrix];
	
	If[rank < Length[basisMatrix],
		indx = LinIndependentRows[basisMatrix];
		basisMatrix = basisMatrix[[indx]];
		polysBasis =  polysBasis[[indx]];
	];
	
	Print[" rank again: ",MatrixRank[basisMatrix]];
	
	(* Each COLUMN is now a basis vector. *)
	basisMatrix = Transpose@basisMatrix;
	
	Quiet[
		lSolveFunc = LinearSolve[basisMatrix];
	];
	
	(* Compute the characters. *)
	Sum[
		character = Tr@Table[
			pi = Permute[Range[n], Cycles[PartitionList[Range@n, mu]]];
			
			(* Rule for the diagonal action on variables. *)
			subst = Join@@Table[ vv[i] -> vv[pi[[i]]] , {i,n}, {vv, varList}];
			
			imgVec = polynomialToBasisVector[bb /. subst];
			
			(* HERE! NEED TO REDUCE WRT THE GB AGAIN, BEFORE !!! *)
			
			lSolveFunc[imgVec]
			
		, {bb, polysBasis}];
		character*PowerSumSymbol[mu]/ZCoefficient[mu]
		, {mu, IntegerPartitions@n}]
];



HookFilterMap::usage = "HookFilterMap[f, t, [x]] applies the hook filter map to a symmetric \
function f, returning a polynomial in t. The map sends SchurSymbol[lam] to t*(t-1)^(lam[[1]]-1) \
if lam is a hook shape (first part arbitrary, all remaining parts equal to 1), and to 0 otherwise.";

HookFilterMap[f_, t_, x_: None] := Module[{inSchur, vars, rules, lam, rF},
	inSchur = ToSchurBasis[f, x];
	vars = Cases[Variables[inSchur], SchurSymbol[__, x], {0, Infinity}];
	rules = CoefficientRules[inSchur, vars];
	Expand @ Sum[
		If[Tr@First[rF] == 0,
			lam = {},
			lam = Pick[vars, First[rF], 1][[1, 1]]
		];
		If[Length[lam] >= 1 && (Length[lam] == 1 || Max[lam[[2 ;;]]] == 1),
			Last[rF] * t * (t - 1)^(lam[[1]] - 1),
			0
		]
	, {rF, rules}]
];

UnitTest[HookFilterMap] := And[
	HookFilterMap[SchurSymmetric[{3, 1, 1}], t] === t (t - 1)^2,
	HookFilterMap[SchurSymmetric[{2, 2}], t] === 0,
	HookFilterMap[SchurSymmetric[{4}], t] === t (t - 1)^3,
	HookFilterMap[SchurSymmetric[{1, 1, 1}], t] === t
];



End[(* End private *)];





Protect @@ Select[
  Names["SymmetricFunctions`*"],
  !StringMatchQ[#, ___ ~~ "$" ~~ ___] &
];

EndPackage[];
