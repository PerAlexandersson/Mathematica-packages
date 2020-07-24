

Clear["QuasiSymmetricFunctions`*"];
BeginPackage["QuasiSymmetricFunctions`"];
Needs["CombinatoricsUtil`"];


MonomialQSymmetric;
FundamentalQSymmetric;
PowerSumQSymmetric;
PowerSumAltQSymmetric;

ToFundamentalBasis;
ToQSymPowerSumBasis;


Begin["Private`"];



(*This takes a list of lists!*)
(* This is used for quasisymmetric power-sums. *)
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
		MonomialQSymmetric[
		Table[
			p[[i]].{ alphaPad[[ 1 + ap[[i, 1]]]], betaPad[[ 1 + ap[[i, 2]]]]}
			, {i, Length@p}]
		, x]
		, {p, specPaths[m, n]}]
];

MonomialQSymmetric[{}, x_: None] := 1;

MonomialQSymmetric[alpha_List] := MonomialQSymmetric[alpha, None];

MonomialQSymmetric /: Times[MonomialQSymmetric[a_List, x_], MonomialQSymmetric[b_List, x_]] := 
	QMonomialProduct[a, b, x];
	
MonomialQSymmetric /: Power[MonomialQSymmetric[a_List, x_], 0] := 1;

MonomialQSymmetric /: Power[MonomialQSymmetric[a_List, x_], 1] := 
  MonomialQSymmetric[a, x];
	
MonomialQSymmetric /: Power[MonomialQSymmetric[a_List, x_], n_Integer] := 
	Expand[Power[MonomialQSymmetric[a, x], n - 2] QMonomialProduct[a, a, x]];

(* TODO Make formatting compatible with TEX *)

MonomialQSymmetric /: Format[MonomialQSymmetric[a_List, x_]] := With[
	{r = Row[a /. i_Integer :> If[i > 9, OverBar@i, i]]},
	If[
	x === None,
	Subscript["M", r],
	Row[{Subscript["M", r], "(", ToString@x, ")"}]
]];

FundamentalQSymmetric[alpha_List, x_: None] := 
FundamentalQSymmetric[alpha, x] = Sum[
	MonomialQSymmetric[beta, x]
	, {beta, CompositionRefinements[alpha]}];

	
(* The fundamental Qsym fulfills the shuffle product rule. *)

UnitTest[FundamentalQSymmetric] := And[
	Expand[FundamentalQSymmetric[{1, 2}] FundamentalQSymmetric[{1}]] ==
					FundamentalQSymmetric[{1, 3}] + 
			FundamentalQSymmetric[{1, 2, 1}] +
			FundamentalQSymmetric[{2, 2}] + FundamentalQSymmetric[{1, 1, 2}]
];




PowerSumQSymmetric[alpha_List, x_: None] :=
  
  PowerSumQSymmetric[alpha, x] = Module[{pi},
    pi[comp_List] := Times @@ Accumulate[comp];
    Expand[
     ZCoefficient[alpha]
      Sum[
       1/(Times @@ (pi /@ beta)) MonomialQSymmetric[Total /@ beta, x]
       , {beta, PartitionedCompositionCoarsenings[List /@ alpha]}]
     ]
    ];


PowerSumAltQSymmetric[alpha_List, x_: None] :=
  
  PowerSumAltQSymmetric[alpha, x] = Module[{spi},
    spi[comp_List] := Length[comp!] (Times @@ comp);
    Expand[
     ZCoefficient[alpha]
      Sum[
       1/(Times @@ (spi /@ beta)) MonomialQSymmetric[Total /@ beta, 
         x]
       , {beta, PartitionedCompositionCoarsenings[List /@ alpha]}]
     ]
    ];


CompositionIndexedBasisRule[size_Integer, basisSymb_, toBasis_, 
   monom_: MonomialQSymmetric, x_: None] := 
  CompositionIndexedBasisRule[size, basisSymb, toBasis, monom, x] =
   Module[{parts, mat, imat},
    parts = IntegerCompositions[size];
    
    (* Matrix with the toBasis expanded in monomials. *)
    (* 
    Here we use the "None" alphabet *)
    mat = Table[
      Table[
       Coefficient[toBasis[p], monom[q] ], {q, parts}]
      , {p, parts}];
    imat = Inverse[mat];
    Table[
     monom[parts[[p]], x] -> Sum[
       basisSymb[parts[[q]]]*imat[[p, q]]
       , {q, Length@parts}]
     , {p, Length@parts}]
    ];


ToOtherQSymmetricBasis[basis_, pol_, newSymb_, x_: None, mm_: MonomialQSymmetric] := Module[
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



ToFundamentalBasis[poly_, ff_, x_: None] := 
  ToOtherQSymmetricBasis[FundamentalQSymmetric, poly, ff, x];

ToQSymPowerSumBasis[poly_, pp_, x_: None] := 
  ToOtherQSymmetricBasis[PowerSumQSymmetric, poly, pp, x];


End[(* End private *)];
EndPackage[];
