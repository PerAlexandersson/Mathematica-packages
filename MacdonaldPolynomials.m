(* ::Package:: *)


(* MathKernel -script file.m *)

Clear["MacdonaldPolynomials`*"];

BeginPackage["MacdonaldPolynomials`"];

Needs["OldYoungTableaux`"];
Needs["CombinatoricsUtil`"]; (*For reduced words *)


KnuthRepresentative::usage="KnuthRepresentative[w] returns a permutation w which is Knuth-equivalent to w, and is the reading word of an SYT.";

(*
WordDecompose::usage="WordDecompose[word] takes a word with partition shape, and returns a list of permutations.";
ChargeWordDecompose::usage="ChargeWordDecompose[word] takes a word with partition shape, and returns a list of permutations.";
WordCharge::usage="Given a word (does not need to have partition weight), return the charge of the word.";
*)

IsInversionTripletTypeAQ::usage = "";
IsInversionTripletTypeBQ::usage = "";

KeyCompositionToPermutation::usage="Given composition, returns the {permutation, partition} used to match with operator definition.";
AtomCompositionToPermutation::usage="Given composition, returns the {permutation, partition} used to match with operator definition.";

OperatorKeyPolynomial::usage="OperatorKeyPolynomial[alpha,x] or OperatorKeyPolynomial[pi,lam,x]";
OperatorAtomPolynomial::usage="OperatorAtomPolynomial[alpha,x] or OperatorAtomPolynomial[pi,lam,x]";

DividedDifference::usage="DividedDifference[poly, x, i] applies the divided difference operator.";
PiOperator::usage="PiOperator[poly, x, i] applies the key pi_i operator to the polynomial. ";
ThetaOperator::usage="ThetaOperator[poly,x,i].";

TPiOperator::usage="TPiOperator[poly, x, t,i] applies the key pi_i operator to the polynomial. ";
TThetaOperator::usage="TThetaOperator[poly,x,t,i].";

(* TODO - Rename *)
OperatorKeyTPolynomial::usage="OperatorKeyPolynomial[alpha,x,t] or OperatorKeyPolynomial[pi,lam,x,t]";

SSAFillings::usage="SSAFillings[shape, basement ]";

SSAFMajorIndex::usage="SSAFMajorIndex[ssaf] returns the major index.";
SSAFCoInversions::usage="SSAFCoInversions[ssaf] returns the number of coinversions.";
SSAFInversions::usage="SSAFInversions[ssaf] returns the number of inversions.";
SSAFDn::usage="SSAFDn[ssaf] returns the number different horizontally adjacent cells in the filling.";

SSAFWeight::usage = "SSAFWeight[f] returns the weight vector of f.";
SSAFMonomial::usage = "SSAFMonomial[ssaf, x] returns the monomial associated with a filling. ";
MacdonaldMonomial::usage = "MacdonaldMonomial[ssaf_, x, q, t] returns the weight associated to this filling.";

TAtomFillings::usage = "AtomFillings[shape]";
AtomFillings::usage = "AtomFillings[shape, [weight] ]";
KeyFillings::usage = "KeyFillings[shape, [weight] ]";
LockFillings::usage = "LockFillings[shape]";
KeyAsAtomFillings::usage = "KeyAsAtomFillings[shape, [weight] ]";
KeyAsPBFs::usage = "KeyAsPBFs[shape, [weight] ]";
MacdonaldEFillings::usage = "MacdonaldEFillings[shape, basement]";
MacdonaldHFillings::usage = "MacdonaldHFillings[shape, basement]";

GeneralTableauFillings::usage="";

AtomPolynomial::usage = "AtomPolynomial[shape, [basement], x]";
KeyPolynomial::usage = "KeyPolynomial[shape,x]";
LockPolynomial::usage = "LockPolynomial[shape,x]";
AtomTPolynomial::usage="AtomPolynomial[shape, [basement], x, t]";
MacdonaldEPolynomial::usage = "MacdonaldEPolynomial[shape, [basement],x,q,t]";
MacdonaldHPolynomial::usage = "MacdonaldHPolynomial[shape, [basement],x,q,t]";
MacdonaldJPolynomial::usage = "MacdonaldJPolynomial[shape ,x,q,t]";
MacdonaldJFillings::usage="MacdonaldJFillings[lam]";
MacdonaldJMonomial::usage="MacdonaldJMonomial[ssaf,x,q,t]";

IntegralFormNonSymmetricJack::usage = "IntegralFormNonSymmetricJack[alpha,x,a] gives the non-sym integral form Jack.";

ElementaryPolynomial::usage = "ElementaryPolynomial[alpha_, x_, qq_: 1] is a q-deformation of the elementary sym. func. e_(alpha'). ";

PowerSumPolynomial::usage = "PowerSumPolynomial[lam,x] is the powersum symmetric polynomial.";

IntegralFormFactor::usage="IntegralFormFactor[alpha,q,t] returns the factor to multiply the non-sym Macdonald polynomial with, to get the integral form.";


SkewMacdonaldE::usage = "SkewMacdonaldE[lam,mu,nvars,x,q]";


CompositionIndexedBasisRule::usage = "CompositionIndexedBasisRule[size, len, x, aa, [basement] ] returns substitution rule to go from monomial basis to basis indexed by compositions. ";
PartitionIndexedBasisRule::usage = "PartitionIndexedBasisRule[size, len, x, aa, [basement] ] returns substitution rule to go from monomial basis to basis indexed by partitions. ";

ToMacdonaldEBasis::usage="ToMacdonaldEBasis[polIn_, basement, x, q, t, ee] rewrites poly in the MacdonaldE basis with given basement.";
ToAtomBasis::usage="ToAtomBasis[polIn_, basement, x, aa]  rewrites poly in the MacdonaldE basis with given basement.";
ToKeyBasis::usage = "ToKeyBasis[pol,x,kk]";

ToElementaryBasis::usage = "ToElementaryBasis[pol, x, ee] expresses the polynomial in the elementary symmetric functions. Note, the polynomial must be symmetric.";
ToCompleteHomogeneousBasis::usage = "ToCompleteHomogeneousBasis[pol, x, ee] expresses the polynomial in the complete homogeneous symmetric functions. Note, the polynomial must be symmetric.";
ToPowerSumBasis::usage = "ToPowerSumBasis[pol,x,pp] expresses the polynomial in the powersum basis. Note, the polynomial must be symmetric.";
ToLockBasis::usage = "ToLockBasis[pol,x,pp] writes the polynomial in lock basis.";


SSYTToAtom::usage="Given an SSYT, it produces an atom filling with the same weight, using the insertion algorithm given by Mason.";

RPPToAtom::usage=="Given an RPP, it produces an atom filling with the same weight, preserving column sets.";


SSAFCrystalWord::usage = "SSAFCrystalWord[ssaf, i] extracts the i-word.";
SSAFRaising::usage = "SSAFRaising[ssaf,i] returns the crystal raising operator on the SSAF, or {} if this is impossible.";
SSAFLowering::usage = "SSAFLowering[ssaf,i] returns the crystal raising operator on the SSAF, or {} if this is impossible.";
SSAFCrystalString::usage = "SSAFCrystalString[ssaf,i] returns the i-string containing the ssaf.";

LascouxSchutzenberger::usage = "LascouxSchutzenberger[ssaf, i] performs the L--S s_i involution on the SSAF.";

ModifiedLascouxSchutzenberger::usage = "ModifiedLascouxSchutzenberger[ssaf, i] performs the L--S s_i involution on the SSAF.";

LascouxSchutzenbergerNormalizeWord::usage="Convert word using LS involutions to get a word with partition weight.";

SSYTLascouxSchutzenberger::usage="LascouxSchutzenberger[ssyt, i] performs the L--S s_i involution on the SSYT.";
SSYTLascouxSchutzenbergerInvolution::usage="";

ChargeToMajMap::usage="ChargeToMajMap[filling] applies Ryan and Jennifers map on the filling. This maps charge to maj, if the tableau has partition shape and weight. It also sends weight to shape, and shape to weight.";

SSAFKnownCharge::usage="Return the charge of an SSAF, whenever it has partition shape.";

SSAFColumnSets::usage="SSAFColumnSets[fil,[startCol]] returns a list of the column sets of the filling, starting from specified column. Default start is 2.";

SSYTWeightNormalize::usage="Applies LS involutions until the result has partition weight.";
SSAFWeightNormalize::usage="";


SchubertPolynomial::usage="SchubertPolynomial[pi, x] returns the Schubert polynomial.";

QSymMonomial::usade = "QSymMonomial[alpha_List, n_, [nvars], x_] ";

GesselFundamental::usade = "GesselFundamental[des_List, n_, [nvars], x_] ";
FundamentalSlide::usade = "FundamentalSlide[alpha, x] returns the fundamental slide polynomial.";

ToFundamentalSlideBasis::usage = "ToFundamentalSlideBasis[pol,x,fs] returns the polynomial written in the fs basis.";
ToGesselSubsetBasis::usage = "ToGesselSubsetBasis[poly, x, ff] returns the Gessel expansion of poly indexed by the descent set. Polynomial should be homogeneous.";

QSymSchur::usage="QSymSchur[alpha,n,x]";

(* See https : // arxiv.org/pdf/1710.11613.pdf for definition *)
QuasiSymmetricPowerSum::usage="QuasiSymmetricPowerSum[alpha,n,x] gives qsym power-sum.";
QuasiSymmetricPowerSum2::usage="QuasiSymmetricPowerSum2[alpha,n,x] gives qsym power-sum, version 2.";

PartitionedCompositionCoarsenings::usage = "";

QuasiSymmetricCompleteHomogeneous::usage="QuasiSymmetricCompleteHomogeneous[alpha,n,x]"; 

DualGrothendieckPolynomial::usage = "DualGrothendieckPolynomial[alpha,x]";
DualGrothendieckFillings::usage = "DualGrothendieckFillings[shape]";
RPPColumnWeight::usage = "RPPColumnWeight[fil]";

(***************** Switch to private context *********************************)
Begin["Private`"];



ClockwiseQ[x_, y_, z_] := Or[x < y < z, y < z < x, z < x < y];
IsInversionTripletTypeAQ[aa_Integer, bb_Integer, cc_Integer] := ClockwiseQ[aa + 0.1, cc + 0.3, bb + 0.2];
IsInversionTripletTypeBQ[aa_Integer, bb_Integer, cc_Integer] := ClockwiseQ[aa + 0.2, cc + 0.1, bb + 0.3];





(*************************** OPERATOR STUFF FOR KEYS AND ATOMS *************************)

(* THIS IS INEFFICIENT--OPTIMIZE as for atom*)
KeyCompositionToPermutation[alpha_List] := Module[{n = Length@alpha, lam = Sort[alpha, Greater], pp},
	pp = Select[Permutations[Range@n], Reverse[alpha][[#]] == lam &];
	{First[SortBy[pp, Length[ReducedWord[#]] &]], lam}
];

AtomCompositionToPermutation[alpha_List] := Module[{m = Length@alpha},
   Transpose@SortBy[Transpose[{Range@m, alpha}], -#[[2]] &]
];


(* Perform the DividedDifference operator on a polynomial in x[1]...x[n] *)
(*
DividedDifference[pol_, x_, j_Integer] := With[
   {sipol = pol /. {x[j] -> x[j + 1], x[j + 1] -> x[j]}},
   Together[(pol - sipol)/(x[j] - x[j + 1])]
];
*)


DividedDifference[poly_, x_, i_Integer] := Module[{ml = MonomialList[poly], doSubst},
	doSubst[mon_, ii_Integer] := Module[{a, b, dd},
		a = Exponent[mon, x[ii]];
		b = Exponent[mon, x[ii + 1]];
		dd = Abs[b - a];
		Which[a > b, Sum[Together[mon*x[ii]^(-j-1)*x[ii + 1]^(j)], {j, 0, dd-1}],
			  a < b, -Sum[Together[mon*x[ii]^j * x[ii + 1]^(-j-1)], {j, 0, dd-1}],
			  True ,  0
		]
	];
	Sum[doSubst[monom, i], {monom, ml}]
];

(* Old version, not as quick as the new. *)
(*
PiOperator[poly_, x_, i_Integer] := DividedDifference[ x[i]*poly, x, i];
*)

(* This version is a lot quicker than the divided difference definition. *)
PiOperator[poly_, x_, i_Integer] := Module[{ml, xvars, doSubst},
	
	xvars = Cases[Variables[poly], x[_]];
	ml = MonomialList[poly, xvars];
	
	doSubst[mon_, ii_] := Module[{a, b, dd},
		a = Exponent[mon, x[ii]];
		b = Exponent[mon, x[ii + 1]];
		dd = Abs[b - a];
		If[a >= b,
			Sum[Together[mon*x[ii]^(-j)*x[ii + 1]^j], {j, 0, dd}]
		,
			-Sum[Together[mon*x[ii]^j*x[ii + 1]^(-j)], {j, 1, dd - 1}]
		]
	];
	Sum[doSubst[monom, i], {monom, ml}]
];


ThetaOperator[poly_, x_, i_Integer] := PiOperator[poly, x, i] - poly;


(* Key-Grothendieck polynomials. *)
(*
PiTOperator[poly_, x_, i_Integer] := DividedDifference[ (x[i]+t*x[i]*x[i+1])*poly, x, i];
PiTOperator[poly_, x_, {}] := poly;
PiTOperator[poly_, x_, redWord_List] := PiTOperator[poly, x, redWord] =
   PiTOperator[PiTOperator[poly, x, redWord[[1]] ], x, Rest@redWord];
*)

(* This applies the transpositions from LEFT to RIGHT in the list *)
PiOperator[poly_, x_, {}] := poly;
PiOperator[poly_, x_, redWord_List] := PiOperator[poly, x, redWord] =
   PiOperator[PiOperator[poly, x, redWord[[1]] ], x, Rest@redWord];

(* This applies the transpositions from LEFT to RIGHT in the list *)
ThetaOperator[poly_, x_, rw_List] := Fold[ThetaOperator[#1, x, #2] &, poly, rw];
(*
ThetaOperator[poly_, x_, {}] := poly;
ThetaOperator[poly_, x_, redWord_List] := ThetaOperator[poly, x, redWord] =
   ThetaOperator[ThetaOperator[poly, x, redWord[[1]] ], x, Rest@redWord
];
*)


TThetaOperator[poly_, x_, t_, j_Integer] := (1 - t) (ThetaOperator[poly, x, j]) + (t*poly /. {x[j] -> x[j + 1], x[j + 1] -> x[j]});
TThetaOperator[poly_, x_, t_, {}] := poly;
TThetaOperator[poly_, x_, t_, redWord_List] := 
  TThetaOperator[poly, x, t, redWord] = TThetaOperator[TThetaOperator[poly, x, t, redWord[[1]]], x, t, Rest@redWord];


TPiOperator[poly_, x_, t_, j_Integer] := (1 - t) (PiOperator[poly, x, j]) + (t*poly /. {x[j] -> x[j + 1], x[j + 1] -> x[j]});
TPiOperator[poly_, x_, t_, {}] := poly;
TPiOperator[poly_, x_, t_, redWord_List] := 
  TPiOperator[poly, x, t, redWord] = TPiOperator[TPiOperator[poly, x, t, redWord[[1]]], x, t, Rest@redWord];




OperatorKeyPolynomial[pi_List, lam_List, x_] := 
  Module[{init, n, sa, w},
   n = Length@lam;
   init = Product[x[i]^(lam[[i]]), {i, n}];
   w = ReducedWord[pi];
   PiOperator[init, x, w]
];

OperatorKeyPolynomial[alpha_List, x_] := Module[{pi, lam},
   {pi, lam} = KeyCompositionToPermutation[alpha];
   OperatorKeyPolynomial[pi, lam, x]
];

OperatorAtomPolynomial[pi_List, lam_List, x_] := 
  Module[{init, n, sa, w},
   n = Length@lam;
   init = Product[x[i]^(lam[[i]]), {i, n}];
   w = ReducedWord[pi];
   ThetaOperator[init, x, w]
];

OperatorAtomPolynomial[alpha_List, x_] := Module[{pi, lam},
   {pi, lam} = AtomCompositionToPermutation[alpha];
   OperatorAtomPolynomial[pi, lam, x]
];


OperatorKeyTPolynomial[pi_List, lam_List, x_,t_] := 
  Module[{init, n, sa, w},
   n = Length@lam;
   init = Product[x[i]^(lam[[i]]), {i, n}];
   w = ReducedWord[pi];
   TPiOperator[init, x, t, w]
];

OperatorKeyTPolynomial[alpha_List, x_,t_] := Module[{pi, lam},
   {pi, lam} = KeyCompositionToPermutation[alpha];
   OperatorKeyTPolynomial[pi, lam, x,t]
];





(*************************** FILLINGS AND GENERAL ATOMS        *************************)



(** Used for t-atoms and Macdonald polynomials **)
CheckNonAttackingTab[tab_List, r_Integer, c_Integer, goalShape_List] := Module[{b, i},
	
	b = tab[[r, c]]; (* The new box value. *)
	Catch[
	
		(*** Non-attacking condition ***)
		(* Note, 
		we only need to check the same column, or column to the left of b, 
		and only above, since there is no box below b yet. *)
		Do[
			If[(Length[tab[[i]]] >= c && 
			tab[[i, c]] == b) || (Length[tab[[i]]] >= c - 1 && 
			tab[[i, c - 1]] == b)
			,
			Throw[False]
		]
	, {i, r - 1}];
	True
	]
];


(* 
Check that the new box which was added to row r do not introduce coinversions.
 *)
(* This assumes that there is no box below the latest added box yet. *)
CheckCoinversions[tab_List, r_Integer, c_Integer, goalShape_List] := Module[{b, i},
	
	b = tab[[r, c]]; (* The new box value. *)
		
	Catch[
	
		(*** Non-attacking condition ***)
		If[ !CheckNonAttackingTab[tab, r, c, goalShape], Throw[False] ];
	
	(** Type A and B triplets **)
	Do[
		(* Check type A triples. Note, the newest added box needs to be the [b] box *)
		
		If[ goalShape[[i]] >= goalShape[[r]] && 
			Length[tab[[i]]] >= c && !IsInversionTripletTypeAQ[ tab[[i, c]], tab[[r, -1]], tab[[i, c - 1]] ],
			Throw[False]
		];
		
		(* Check type B triples. Note, the newest added box needs to be the [c] box *)
		
		If[ goalShape[[i]] < goalShape[[r]] && 
			Length[tab[[i]]] >= c - 1 && !IsInversionTripletTypeBQ[ tab[[i, c - 1]], tab[[r, -2]], tab[[r, -1]] ],
			Throw[False]
		];
	, {i, r - 1}]; (* Over rows above the box we just added. *)
	
	(* Return true if all is ok. *)
	True
	]
];


CheckStrictlyDecreasingRows[tab_List, r_Integer, c_Integer, goalShape_List] := (tab[[r,c-1]] > tab[[r, c]]);


(* Check that the new box which was added to row r do not violate any rules. *)
(* This assumes that there is no box below the latest added box yet. *)

(* TODO -- REWRITE! *)
CheckPBFTab[tab_List, r_Integer, c_Integer, goalShape_List] := 
  Module[{CheckTypeATriple, CheckTypeBTriple, b, i},
   
   CheckTypeATriple[aa_, bb_, cc_] := ! (cc >= bb >= aa);
   (* Extra PBF-condition. *)
   
   CheckTypeBTriple[aa_, bb_, cc_] := ! (cc <= aa <= bb) && cc < aa;
   
   b = tab[[r, c]];(* The new box value. *)
   Catch[
    (*** Non-attacking condition ***)
    (* Note, 
    we only need to check the same column, or column to the left of b, 
    and only above, since there is no box below b yet. *)
    Do[
     If[
      (Length[tab[[i]]] >= c && tab[[i, c]] == b) ||
       
       Length[tab[[i]]] >= c - 1 && tab[[i, c - 1]] == b
      ,
      Throw[False]
      ]
     , {i, r - 1}];
    
    (** Type A and B triplets **)
    Do[
     (* Check type A triples. Note, 
     the newest added box needs to be the [b] box *)
     
     If[ goalShape[[i]] >= goalShape[[r]] && 
       Length[tab[[i]]] >= c && ! 
        CheckTypeATriple[ tab[[i, c]], tab[[r, -1]], tab[[i, c - 1]]],
      Throw[False]
      ];
     (* Check type B triples. Note, 
     the newest added box needs to be the [c] box *)
     
     If[ goalShape[[i]] < goalShape[[r]] && 
       Length[tab[[i]]] >= c - 1 && ! 
        CheckTypeBTriple[ tab[[i, c - 1]], tab[[r, -2]], tab[[r, -1]]],
      Throw[False]
      ];
     , {i, r - 1}]; (* Over rows above the box we just added. *)
    (* Return true if all is ok. *)
    True
    ]
];


(* Columns stricty decreasing, rows weakly decreasing. Used for Dual Grothendieck. *)

CheckRPPTab[tab_List, r_Integer, c_Integer, goalShape_List] := 
  Module[{b},
   b = tab[[r, c]];
   Catch[(* Rows are automatically weakly decr. 
    by general algorithm. *)
    Do[
     (* Something above, which is less than newest added box, is bad. *)
     If[Length[tab[[i]]] >= c && tab[[i, c]] < b,
      Throw[False]
      ]
     , {i, r - 1}];
    True
    ]
];


(* Columns have unique elements, rows weakly decreasing. *)
CheckSimpleTab[tab_List, r_Integer, c_Integer, goalShape_List] := 
  Module[{b, i},
   b = tab[[r, c]];
   Catch[(* Rows are automatically weakly decr. 
    by general algorithm. *)
    Do[
     (* Something above, which is equal, to newest added box, 
     is bad. *)
     If[Length[tab[[i]]] >= c && tab[[i, c]] == b,
      Throw[False]
      ]
     , {i, r - 1}];
    True
    ]
];


(* Aux functions used to compute polynomials. *)

SSAFWeight[ssaf_List] := Tally[Join @@ (Rest /@ ssaf)];
SSAFMonomial[ssaf_List, x_] := Times @@ (x[#1]^#2 & @@@ SSAFWeight[ssaf]);
SSAFWeightVector[ssaf_List] := Normal@SparseArray[#1 -> #2 & @@@ SSAFWeight[ssaf], Length@ssaf];

RPPColumnWeight[tab_List] := Module[{tt},
   tt = SSAFColumnSets[tab];
   (* Delete duplicates in columns. *)
   Tally[Join @@ (Union /@ tt)]
];
RPPMonomial[tab_List, x_] := Times @@ (x[#1]^#2 & @@@ RPPColumnWeight[tab]);


SSAFCoInversions[ssaf_List]:=Module[{rows, coinv, shape,c,r},
	
	rows = Length[ssaf];
	shape = Length /@ ssaf;
	
	(* Number of non-inversion triples. *)
	coinv = Sum[
	Boole@Or[
		(shape[[i]] >= shape[[r]] && 
			shape[[i]] >= c && !IsInversionTripletTypeAQ[ ssaf[[i, c]], ssaf[[r, c]], ssaf[[i, c - 1]] ])
		,
		(shape[[i]] < shape[[r]] && 
			shape[[i]] >= c - 1 && !IsInversionTripletTypeBQ[ ssaf[[i, c - 1]], ssaf[[r, c - 1]], ssaf[[r, c]]])
	]
	, {r, rows}, {c, 2, Length[ssaf[[r]]]}, {i, r - 1}];
	
	coinv
];

SSAFInversions[ssaf_List]:=Module[{rows, inv, shape,c,r},
	
	rows = Length[ssaf];
	shape = Length /@ ssaf;
	
	(* Number of non-inversion triples. *)
	inv = Sum[
	Boole@Or[
		(shape[[i]] >= shape[[r]] && 
			shape[[i]] >= c && IsInversionTripletTypeAQ[ ssaf[[i, c]], ssaf[[r, c]], ssaf[[i, c - 1]] ])
		,
		(shape[[i]] < shape[[r]] && 
			shape[[i]] >= c - 1 && IsInversionTripletTypeBQ[ ssaf[[i, c - 1]], ssaf[[r, c - 1]], ssaf[[r, c]]])
	]
	, {r, rows}, {c, 2, Length[ssaf[[r]]]}, {i, r - 1}];
	
	inv
];


SSAFDn[ssaf_List]:=Module[{rows, shape, dn, c, r},
	rows = Length[ssaf];
	shape = Length /@ ssaf;
	
	dn = Sum[
		Boole[shape[[r]] >= c && (ssaf[[r, c]] != ssaf[[r, c - 1]])]
	, {r, rows}, 
	{c, 2, Length[ssaf[[r]]]}];
	
	 dn
];


SSAFMajorIndex[ssaf_List]:=Module[{rows, maj, shape,c,r, leg},
	
	rows = Length[ssaf];
	shape = Length /@ ssaf;
	
	(* Sum over all leg values of the decents *)
	maj = Sum[
		leg = shape[[r]] - c;
		Boole[ ssaf[[r, c]] > ssaf[[r, c-1]] ]* (1 + leg )
	, {r, rows}, {c, 2, Length[ssaf[[r]]]}];
	
	maj
];


SSAFMacdonaldFactor[ssaf_List, q_,t_]:=Module[{rows, shape,c,r, i,arm, leg,fac},
	rows = Length[ssaf];
	shape = Length /@ ssaf;
	
	fac = Product[
		
		If[ ssaf[[r, c]] != ssaf[[r, c-1]] ,
			
			arm = Sum[ Boole[ c <= shape[[i]] <= shape[[r]]  ], {i,r+1,rows}] + Sum[ Boole[ c-1 <= shape[[i]] < shape[[r]]  ], {i,1,r-1}];
			leg = shape[[r]] - c;
			
			(1-t)/(1- q^(leg+1) t^(arm+1))
			,
			1
		]
		
	, {r, rows}, {c, 2, shape[[r]] }];
	
	fac
];



IntegralFormFactor[shape_List, q_,t_]:=Module[{rows, c, r, i, arm, leg, fac},
	
	rows = Length[shape];
	
	fac = Product[
		
		arm = Sum[ Boole[ c <= shape[[i]] <= shape[[r]]  ], {i,r+1,rows}] + Sum[ Boole[ c-1 <= shape[[i]] < shape[[r]]  ], {i,1,r-1}];
		leg = shape[[r]] - c;
		
		
		
		(1- q^(leg+1) t^(arm+1))
		
		
	, {r, rows}, {c, 1, shape[[r]] }];
	
	fac
];


(* Monomial obtained from a weakly decreasing non-attacking filling. *)
HLMonomial[ssaf_List, x_, t_] := Module[{coinv, dn},
	
	coinv = SSAFCoInversions[ssaf];
	dn = SSAFDn[ssaf];
	
	SSAFMonomial[ssaf, x]*(t^coinv)*(1 - t)^dn
];

MacdonaldMonomial[ssaf_List, x_, q_,t_] := Module[{ coinv, maj, fac},
	
	coinv = SSAFCoInversions[ssaf];
	maj = SSAFMajorIndex[ssaf];
	fac = SSAFMacdonaldFactor[ssaf, q, t];
	
	SSAFMonomial[ssaf, x]*(t^coinv)*(q^maj)* fac
];



MacdonaldHMonomial[ssaf_List, x_, q_,t_] := Module[{ inv, maj },
	inv = SSAFInversions[ssaf];
	maj = SSAFMajorIndex[ssaf];
	
	SSAFMonomial[ssaf, x]*(t^inv)*(q^maj)
];

MacdonaldJMonomial[ssaf_List, x_, q_, t_]:=Module[{rows, shape,c,r, i,arm, leg,fac, coinv,maj},
	rows = Length[ssaf];
	shape = Length /@ ssaf;
	
	fac = Product[
		
		If[ ssaf[[r, c]] == ssaf[[r, c-1]] ,
			
			arm = Sum[ Boole[ c <= shape[[i]] <= shape[[r]]  ], {i,r+1,rows}] + Sum[ Boole[ c-1 <= shape[[i]] < shape[[r]]  ], {i,1,r-1}];
			leg = shape[[r]] - c;
			
			(1- q^(leg+1) t^(arm+1))
			,
			(1-t)
		]
		
	, {r, rows}, {c, 2, shape[[r]] }];
	
	coinv = SSAFCoInversions[ssaf];
	maj = SSAFMajorIndex[ssaf];
	
	fac*SSAFMonomial[ssaf, x]*(t^coinv)*(q^maj)
];


(* Main function for generating fillings. *)
GeneralTableauFillings[alpha_List, basement_List, tabFunc_: List, joinFunc_: Join, checkfunc_: CheckCoinversions, decreasing_:True] := Module[
	{recurseAddBox, i, rowSequence, initTab},
	
	(* (3,0,1) => (1,1,1,3) *)
	
	rowSequence = Join @@ Table[ConstantArray[i, alpha[[i]]], {i, Length@alpha}];
	
	(* Algorithm assumes that there are no boxes below or to the right of the newest added box. *)
	recurseAddBox[tab_List, {}] := tabFunc[tab];
	
	recurseAddBox[tab_List, rowSeq_List] := Module[{r, newTab, maxNew},
		
		r = First@rowSeq;
		
		If[ decreasing == True,
			 (* Add box with value 1,2,..., less than or equal to box on the left. *)
			maxNew = tab[[r, -1]];
			,
			maxNew = Length[alpha];
		];
		
		joinFunc @@ Table[
			 (* Insert tableau *)
			newTab = Insert[tab, b, {r, -1}];
			
			If[checkfunc[newTab, r, Length[newTab[[r]]], alpha],
				recurseAddBox[newTab, Rest@rowSeq],
				Sequence @@ {}
			]
		, {b, maxNew }]
		
	];
	
	
	initTab = Transpose[{basement}];
	
	(* Do recursion. *)
	recurseAddBox[initTab, rowSequence]
];




(* General function for handling SSAFs. *) 
SSAFillings[alpha_List, basement_List, tabFunc_: List, joinFunc_: Join] := 
  GeneralTableauFillings[alpha, basement, tabFunc, joinFunc, CheckCoinversions];


  
  
AtomPolynomial[alpha_List, basement_List, x_] := AtomPolynomial[alpha, basement, x] = SSAFillings[alpha, basement, SSAFMonomial[#, x] &, Plus];

AtomPolynomial[alpha_List, x_] := AtomPolynomial[alpha, Range[Length@alpha], x];

AtomFillings[alpha_List] := AtomFillings[alpha] = SSAFillings[alpha, Range[Length@alpha]];
AtomFillings[alpha_List, gam_List] := AtomFillings[alpha, gam] = Select[AtomFillings[alpha], SSAFWeightVector[#] == gam &];
AtomFillings[alpha_List, gam_List, bas_List]:=Select[SSAFillings[alpha, bas], SSAFWeightVector[#] == gam &];

KeyFillings[alpha_List] := KeyFillings[alpha] = SSAFillings[alpha, Range[Length@alpha, 1, -1]];

KeyFillings[alpha_List, gam_List] := KeyFillings[alpha, gam] = Select[KeyFillings[alpha], SSAFWeightVector[#] == gam &];


KeyAsAtomFillings[alpha_List] := KeyAsAtomFillings[alpha] = Module[{lam, pi, gams},
    lam = Reverse@Sort@alpha;
    pi = AtomCompositionToPermutation[alpha][[1]];
	
    (* Stuff that are more sorted. *)
    gams = Select[Permutations[alpha], 
      StrongBruhatGreaterQ[KeyCompositionToPermutation[#][[1]], pi] &];
    Sort[Join @@ (AtomFillings /@ gams)]
];

KeyAsAtomFillings[alpha_List, gam_List] := 
  KeyAsAtomFillings[alpha, gam] = 
   Select[KeyAsAtomFillings[alpha], SSAFWeightVector[#] == gam &];

KeyAsPBFs[alpha_List] := KeyAsPBFs[alpha] = Module[{lam, pi, gams},
    lam = Reverse@Sort@alpha;
    pi = AtomCompositionToPermutation[alpha][[1]];
    SSAFillings[lam, Reverse[Range[Length@alpha]][[pi]] ]
];
KeyAsPBFs[alpha_List, gam_List] := KeyAsPBFs[alpha, gam] = Select[KeyAsPBFs[alpha], SSAFWeightVector[#] == gam &];


(* Compute key polynomial. *)
KeyPolynomial[alpha_List, x_] := AtomPolynomial[alpha, Range[Length@alpha, 1, -1], x];


MacdonaldEFillings[alpha_List, basement_List] := GeneralTableauFillings[alpha, basement, List, Join, CheckNonAttackingTab, False];


MacdonaldHFillings[alpha_List, basement_List] := GeneralTableauFillings[alpha, basement, List, Join, (#;True)&, False];


(* MacdonaldE-polynomial. *)
(* Non-symmetric, non-integral, as in Haglund's paper (26), except that Haglund's requires a reversal of the shape. *)

MacdonaldESymbolic[alpha_List, basement_List, x_,q_,t_] := MacdonaldESymbolic[alpha, basement, x, q, t] = 
GeneralTableauFillings[alpha, basement, MacdonaldMonomial[#, x,q,t] &, Plus, CheckNonAttackingTab, False];
MacdonaldEPolynomial[alpha_List, basement_List, x_,q_,t_] := (MacdonaldESymbolic[alpha, basement, xx, qq, tt]/.{xx->x, qq->q,tt->t});

MacdonaldEPolynomial[alpha_List, x_, q_, t_] := MacdonaldEPolynomial[alpha, Range[Length@alpha, 1, -1] , x, q,t];


(* NOT USED *)
(*
MacdonaldESymbolic[alpha_List, basement_List, x_,0,t_] := MacdonaldESymbolic[alpha, basement, x, 0, t] = 
GeneralTableauFillings[alpha, basement, MacdonaldMonomial[#, x,0,t] &, Plus, CheckNonAttackingTab, False];
MacdonaldEPolynomial[alpha_List, basement_List, x_,0,t_] := (MacdonaldESymbolic[alpha, basement, xx, 0, tt]/.{xx->x,tt->t});
MacdonaldEPolynomial[alpha_List, x_, 0, t_] := MacdonaldEPolynomial[alpha, Range[Length@alpha, 1, -1] , x, 0,t];
*)


MacdonaldHSymbolic[alpha_List, basement_List, x_,q_,t_] := MacdonaldHSymbolic[alpha, basement, x, q, t] = 
GeneralTableauFillings[alpha, basement, MacdonaldHMonomial[#, x,q,t] &, Plus, (#;True)&, False];

MacdonaldHPolynomial[alpha_List, basement_List, x_,q_,t_] := (MacdonaldHSymbolic[alpha, basement, xx, qq, tt]/.{xx->x, qq->q,tt->t});
MacdonaldHPolynomial[alpha_List, x_, q_, t_] := MacdonaldHPolynomial[alpha, Length[alpha]+Range[Length@alpha, 1, -1] , x, q,t];


MacdonaldJPolynomial[lambda_List, x_, q_, t_] := MacdonaldJPolynomial[lambda, x, q, t]=
GeneralTableauFillings[lambda, Range[2*Length@lambda, Length@lambda+1, -1], MacdonaldJMonomial[#, x, q, t] &, Plus, CheckNonAttackingTab, False];

MacdonaldJFillings[lambda_List]:=GeneralTableauFillings[lambda, Range[2*Length@lambda, Length@lambda+1, -1], List, Join, CheckNonAttackingTab, False];


IntegralFormNonSymmetricJack[alpha_List, x_, a_] := IntegralFormNonSymmetricJack[alpha, x, a] =
	Module[{n = Length@alpha, intMac, blah},
	intMac = Together[ IntegralFormFactor[alpha, q, t] MacdonaldEPolynomial[alpha,Reverse@Range@n, x, q, t]];
	blah = Together[(intMac /. {q -> t^a})/((1 - t)^Tr[alpha])];
	Limit[blah, t -> 1]
];


(* Note - this agrees with e_lambda' when alpha = lambda = partition *)
ElementaryPolynomial[alpha_List, x_, qq_: 1] := ElementaryPolynomial[alpha, x, qq] = 
	Module[{q, t}, 
		GeneralTableauFillings[alpha, Range[Length@alpha, 1, -1], MacdonaldMonomial[#, x, q, t]& , Plus, CheckCoinversions, False] /. {t -> 0, q -> qq}
];

PowerSumPolynomial[lam_List,x_]:=PowerSumPolynomial[lam,x]=Product[PowerSymmetricPolynomial[a, x/@Range[Length@lam]], {a, DeleteCases[lam,0]}];


(* TODO: Make the thing below better! This is not efficient, there should be a sorting algorithm. *)

SSAFKeySortColumns[keyFil_List] := KeyFromShapeAndColumnSet[(Length /@ keyFil) - 1,  SSAFColumnSets[keyFil]];

(* Returns the unique key filling with this shape and column set, or {} if there is none. *)
KeyFromShapeAndColumnSet[alpha_List, cSets_List] := KeyFromShapeAndColumnSet[alpha, cSets] = Module[{keyFils,kf},
	
(*
	Print["Key from shape and column: ", alpha, cSets];
*)
	keyFils = KeyFillings[alpha];
	
	(* Define default, {} means no such filling. *)
	KeyFromShapeAndColumnSet[alpha, any_] := {};
	
	Do[
		KeyFromShapeAndColumnSet[alpha, SSAFColumnSets[kf]] = kf;
	, {kf, keyFils}];
	
	KeyFromShapeAndColumnSet[alpha, cSets]
];

(*
NonSymmetricHallLittlewoodPolynomial[alpha_List, x_, t_] := 
  NonSymmetricHallLittlewoodPolynomial[alpha, x, t] = 
   GeneralTableauFillings[alpha, Range[Length@alpha], 
    HLMonomial[#, x, t] &, Plus, CheckNonAttackingTab];
*)

(*
KeyHallLittlewoodPolynomial[alpha_List, x_, t_] := 
  KeyHallLittlewoodPolynomial[alpha, x, t] = 
   GeneralTableauFillings[alpha, Range[Length@alpha, 1, -1], 
    HLMonomial[#, x, t] &, Plus, CheckNonAttackingTab];
*)


(* Sum over all permutations of alpha, and obtain the ordinary HL-poly. *)
(* t=0 give the General Demazure atom, and t=1 gives a kind of monomial. *)

AtomTPolynomial[alpha_List, beta_List, x_, t_] := AtomTPolynomial[alpha, beta, x, t] = 
   GeneralTableauFillings[alpha, beta, HLMonomial[#, x, t]& , Plus, CheckNonAttackingTab];


AtomTPolynomial[alpha_List, x_, t_]:= AtomTPolynomial[ alpha, Range[Length@alpha], x, t];

TAtomFillings[alpha_] :=   TAtomFillings[alpha] =  GeneralTableauFillings[alpha, Range[Length@alpha], List, Join, CheckNonAttackingTab];

TAtomFillings[alpha_, beta_] :=   TAtomFillings[alpha,beta] =  GeneralTableauFillings[alpha, beta, List, Join, CheckNonAttackingTab];

TAtomFillings[alpha_,beta_,gam_]:= TAtomFillings[alpha,beta,gam] = Select[TAtomFillings[alpha,beta], SSAFWeightVector[#] == gam &];

PBFFillings[alpha_List, basement_List, tabFunc_: List, 
   joinFunc_: Join] := GeneralTableauFillings[alpha, basement, tabFunc, joinFunc, CheckPBFTab];

PBFFillings[alpha_List] :=  PBFFillings[alpha, Range[Length@alpha, 1, -1]];

(* Compute pbf polynomial. *)
(* This is similar to the key polynomials, but with extra conditions. *)
PBFPolynomial[alpha_List, x_] := PBFFillings[alpha, Range[Length@alpha, 1, -1], SSAFMonomial[#, x] &, Plus];
PBFPolynomial[alpha_List, basement_List, x_] :=  PBFFillings[alpha, basement, SSAFMonomial[#, x] &, Plus];


DualGrothendieckFillings[alpha_List] := 
  GeneralTableauFillings[alpha, 
   ConstantArray[Length[alpha], Length[alpha]], List, Join, 
   CheckRPPTab];
DualGrothendieckPolynomial[alpha_List, x_] := 
  DualGrothendieckPolynomial[alpha, x] = 
   GeneralTableauFillings[alpha, 
    ConstantArray[Length[alpha], Length[alpha]], RPPMonomial[#, x] &, 
    Plus, CheckRPPTab];

(* See 
http://de.arxiv.org/pdf/1711.09498.pdf
*)
CheckLockTab[tab_List, r_Integer, c_Integer, goalShape_List] := 
  Module[{b, i},
   b = tab[[r, c]];
   Catch[
	
	If[b>tab[[r,1]], Throw[False]]; (* Entries in row i <= i *)
	If[c>2 && b<tab[[r,c-1]], Throw[False] ]; (* Entries weakly increasing in rows (ignoring basement) *)
    Do[
     (* Something above, which is smaller-equal to newest added box, is bad. *)
     If[Length[tab[[i]]] >= c && tab[[i, c]] <= b,
      Throw[False]
      ]
     , {i, r - 1}];
    True
    ]
];

LockFillings[alpha_List] := 
 LockFillings[alpha] = 
   GeneralTableauFillings[alpha, 
    Range[Length@alpha, 1, -1], List, 
    Join, CheckLockTab, False];

LockPolynomial[alpha_List, x_] := 
 LockPolynomial[alpha, x] = 
   GeneralTableauFillings[alpha, 
    Range[Length@alpha, 1, -1], SSAFMonomial[#, x] &, 
    Plus, CheckLockTab, False];



KostkaFoulkes[alpha_List, gamma_List, t_] := (Print["Gen-Kostka: Size mismatch:", alpha, " ", 
     gamma]; Abort[]) /; (Tr@alpha != Tr@gamma);

KostkaFoulkes[alpha_List, gamma_List,t_] := KostkaFoulkes[alpha, gamma, Range@Length@alpha, t];

KostkaFoulkes[alpha_List, gamma_List, basement_List, t_] := (
   DefineKostkaFoulkes[alpha, basement,t];
   KostkaFoulkes[alpha, gamma, basement, t]
);


(* This is using subst. rule. *)
DefineKostkaFoulkes[alpha_List, basement_List,t_] := Module[{intcomps, rule, len, size, asTAtoms, aaa, xx , v},
	
	(* Private persistent variable name. *)
	aaa = DefineKostkaFoulkes`aaa;
	xx = DefineKostkaFoulkes`x;
	
	len = Length[alpha];
	size = Tr@alpha;
	
	
	(* The substitution rule. *)
	With[{size=size, len=len},
		rule = CompositionIndexedBasisRule[size, len, DefineKostkaFoulkes`x, aaa, (AtomTPolynomial[#1, basement, DefineKostkaFoulkes`x, t])&];
	];
	
	
	(* Change of basis. *)
	asTAtoms = Expand[Expand[OperatorKeyPolynomial[alpha, xx]] /. rule];
	
	intcomps = Join @@ (Permutations /@ IntegerPartitions[size, {len}, Range[0, size]]);
	
	Do[
		v = Coefficient[asTAtoms, aaa[gam] ];
		KostkaFoulkes[alpha, gam, basement, t] = v;
	,{gam, intcomps}];
];



GeneralLittlewoodRichardsonCoefficient[alpha_, gamma_, nu_, 
   basement_] := (
   If[Tr@alpha + Tr@gamma != Tr@nu, 0,
    DefineGenLRCoeff[alpha, gamma, basement];
    GeneralLittlewoodRichardsonCoefficient[alpha, gamma, nu, 
     basement]
    ]
   );

DefineGenLRCoeff[alpha_, gamma_, basement_] := 
  Module[{size, len, intComps, c, x, poly, tbzp, tbz, vars, sol},
   size = Tr@alpha + Tr@gamma;
   len = Length@alpha;
   intComps = 
    Join @@ (Permutations /@ 
       IntegerPartitions[size, {len}, Range[0, size]]);
   poly = 
    KeyPolynomial[alpha, x] AtomTPolynomial[gamma, basement, x, t];

   tbzp = 
    poly - Sum[
      c[alpha, ic] *
       AtomTPolynomial[ic, basement, x, t], {ic, 
       intComps}];
   tbz = Union[
     Last /@ CoefficientRules[tbzp, x /@ Range[Length[alpha]]]];
   vars = Complement[Variables[tbz], {t}];
   sol = First@Solve[Thread[tbz == 0], vars];
   
   Do[
    GeneralLittlewoodRichardsonCoefficient[alpha, gamma, s[[1, 2]], 
       basement] = s[[2]];
    , {s, sol}];
];



(* This assumes both columns have the same size. 
Produces a list {newCol,DES}, where newCol has been sorted in the \
canonical manner,
and DES is a set describing in which rows there are descents.
*)
SkewMacdonaldDescents[s1_List, s2_List] := 
  Module[{md, des = 0, newCol},
   md[{}, {}] := {{}, {}};
   md[ss1_List, ss2_List] := Module[{opt, rCol, rdes},
     opt = Select[Range@Length@ss2, ss1[[1]] >= ss2[[#]] &, 1];
     
     If[opt == {},
      {rCol, rdes} = md[Rest@ss1, Rest@ss2];
      {Prepend[rCol, First@ss2], Append[1 + rdes, 1]}
      ,
      {rCol, rdes} = md[Rest@ss1, Drop[ss2, opt]];
      {Prepend[rCol, ss2[[opt[[1]]]]], 1 + rdes}
      ]
     ];
   md[s1, Sort[s2, Greater]]
   ];


SkewMacdonaldE[lam_List, mu_List, nvars_Integer, x_, q_] := 
  Module[{lamc, muc, cols, combinations, p},
   
   lamc = ConjugatePartition[lam];
   cols = Length@lamc;
   muc = PadRight[ConjugatePartition[mu], cols];
   
   combinations = Flatten@Outer[p,
      Sequence @@ Table[
        Subsets[Range[nvars], {lamc[[c]] - muc[[c]]}]
        , {c, cols}], 1];
   (* Combinations now contains all fillings --- 
   all combinations of different subsets. *)
   
   combinations = combinations /. p :> List;
   
   Module[{prevCol, maj, col2, col1, desSet, allDes, diff},
    Sum[
     prevCol = 
      Join[ConstantArray[Infinity, muc[[1]]], 
       Sort[comb[[1]], Greater]];
     allDes = {};
     maj =
      Sum[
       
       col2 = 
        Join[ConstantArray[Infinity, muc[[c + 1]]], comb[[c + 1]]];
       col1 = prevCol[[1 ;; Length[col2]]];
       
       {prevCol, desSet} = SkewMacdonaldDescents[col1, col2];
       allDes = Append[allDes, desSet];
       Sum[lam[[r]] - c, {r, desSet}]
       , {c, cols - 1}];
     q^maj
      (Times @@ (x /@ (Join @@ comb)))
     , {comb, combinations}]
    ]
   ];


(*************************** OPERATOR STUFF FOR KEYS AND ATOMS *************************)



(* Returns the substitution rule to convert monomials to general atom basis. 
Example: keyPoly[{3,1,2},x] /. CompositionIndexedBasisRule[6,3,x,aa] returns aa[{2, 1, 3}] + aa[{2, 3, 1}] + aa[{3, 1, 2}] + aa[{3, 2, 1}]
*)


(* Rewrite s.t. memoization is symbol independent! *)
CompositionIndexedBasisRule[size_Integer, len_Integer, x_, a_, genFunc_:OperatorAtomPolynomial] := CompositionIndexedBasisRule[size,len,x,a,genFunc]=Module[{intcomps, vars, mat, imat, bas},
	intcomps = Join @@ (Permutations /@ IntegerPartitions[size, {len}, Range[0, size]]);
	
	vars = x /@ Range[len];
	mat = Table[
		With[{poly = genFunc[p, x]},
		Table[
			Coefficient[poly, Times @@ (vars^q)]
		, {q, intcomps}]
	], {p, intcomps}];
		
	imat = Inverse[mat];
	
	Table[
		Times @@ (vars^intcomps[[p]])->Sum[a[intcomps[[q]]]*imat[[p, q]], {q, Length@intcomps}]
	, {p, Length@intcomps}]
];




(* TODO: This needs to be rewritten! *)
(* Rewrite s.t. memoization is symbol independent! *)
PartitionIndexedBasisRule[size_Integer, len_Integer, x_, a_, genFunc_:OperatorKeyPolynomial] := PartitionIndexedBasisRule[size,len,x,a,genFunc]=Module[{parts,intcomps, vars, mat, imat, bas},
	parts = IntegerPartitions[size, {len}, Range[0, size]];
	intcomps = Join @@ (Permutations /@ parts);
	
	vars = x /@ Range[len];
	mat = Table[
		With[{poly = genFunc[p, x]},
		Table[
			Coefficient[poly, Times @@ (vars^q)]
		, {q, intcomps}]
	], {p, intcomps}];
		
	imat = Inverse[mat];
	
	Table[
		Times @@ (vars^intcomps[[p]])->Sum[a[intcomps[[q]]]*imat[[p, q]], {q, Length@intcomps}]
	, {p, Length@intcomps}]
];



MacdonaldERule[size_, bas_, x_, q_, t_, aa_] := MacdonaldERule[size, bas, x, aa] = 
	CompositionIndexedBasisRule[size, Length@bas, x, aa, MacdonaldEPolynomial[#, bas, x, q, t] &];

ToMacdonaldEBasis[polIn_, bas_List, x_, q_, t_, aa_] := 
	Module[{vars, z, deg, monomList},
		If[polIn === 0,
			0
			,
		    vars = Cases[Variables[polIn], x[_]];
			monomList = MonomialList[polIn, vars];
			
			Sum[ 
				deg = Exponent[mon /. x[i_] :> z, z];
				If[deg <= 0,
					mon
					,
					mon /. MacdonaldERule[deg, bas, x, q, t, aa]
				]
			, {mon, monomList}]
		]
];

AtomRule[size_, bas_, x_, aa_] := AtomRule[size, bas, x, aa] = 
	CompositionIndexedBasisRule[size, Length@bas, x, aa, AtomPolynomial[#, bas, x] &];

ToAtomBasis[polIn_, bas_List, x_, aa_] :=
	Module[{vars, z, deg, monomList},
		If[polIn === 0,
			0
			,
			vars = Cases[Variables[polIn], x[_]];
			monomList = MonomialList[polIn, vars];
			Sum[
				deg = Exponent[mon /. x[i_] :> z, z];
				If[deg <= 0,
					mon
					,
					mon /. AtomRule[deg, bas, x, aa]
				]
			, {mon, monomList}]
		]
];
ToKeyBasis[pol_,x_,kk_]:= Module[{deg,maxVar},
	If[pol === 0,
		0
		,
		maxVar = Max[Cases[Variables[pol], x[_]]/. x[i_]:>i,0];
		If[maxVar == 0, 
			pol
		, 
			ToAtomBasis[pol,Range[maxVar,1,-1],x,kk]
		]
	]
];
		

LockRule[size_Integer, nvars_Integer, x_, aa_] := LockRule[size, nvars, x, aa] = 
	CompositionIndexedBasisRule[size, nvars, x, aa, LockPolynomial[#, x] &];

ToLockBasis[polIn_, x_, aa_] := 	Module[{vars,nn, z, deg, monomList},
		
		If[polIn === 0,
			0
			,
			vars = Cases[Variables[polIn], x[_]];
			monomList = MonomialList[polIn, vars];
			nn = Max[First /@ Cases[Variables[polIn], x[_]], 0];
			Sum[
				deg = Exponent[mon /. x[i_] :> z, z];
				If[deg <= 0,
					mon
					,
					mon /. LockRule[deg, nn, x, aa]
				]
			, {mon, monomList}]
		]
];


ToElementaryBasis[pol_, x_, ee_] := Module[{toPartition, evs, ep, nn, inE},
	nn = Max[First /@ Cases[Variables[pol], x[_]], 0];
	
	toPartition[mSet_] := Sort[Flatten@Table[ConstantArray[i, mSet[[i]]], {i, Length@mSet}], Greater];
	
	If[nn == 0,
		pol
	,
	    evs = ep /@ Range[nn];
	    inE = First[SymmetricReduction[pol, x /@ Range[nn], evs]];
	    Total[#[[2]] ee[ PadRight[toPartition[#[[1]] ], nn] ] & /@ CoefficientRules[inE, evs]]
	]
];
ToCompleteHomogeneousBasis[0, x_, hh_]:=0;
ToCompleteHomogeneousBasis[pol_, x_, hh_] :=  Module[{CompleteHomogeneousPolynomial,
    t, h, ips, nn, dd, c, xvars, tbz},
	
	nn = Max[First /@ Cases[Variables[pol], x[_]], 0];
	
	If[nn==0,
		pol
		,
		
		dd = Exponent[pol /. x[_] :> t, t];
		
		xvars = x /@ Range[nn];
		CompleteHomogeneousPolynomial[d_Integer] := OperatorKeyPolynomial[PadRight[{d}, nn], x];
		
		h[lam_List] := h[lam] = Product[CompleteHomogeneousPolynomial[a], {a, DeleteCases[lam, 0]}];
		ips = IntegerPartitions[dd, {nn}, Range[0, dd]];
		tbz = Union[Last /@ CoefficientRules[pol - Sum[c[mu] h[mu], {mu, ips}],  xvars]];
		Sum[c[mu] hh[mu], {mu, ips}] /. Solve[Thread[tbz == 0], c /@ ips][[1]]
    ]
];

ToPowerSumBasis[pol_, x_, pp_] := Module[{t, p, ips, nn, dd, c, xvars, tbz},
	nn = Max[First /@ Cases[Variables[pol], x[_]], 0];
	dd = Exponent[pol /. x[_] :> t, t];
	
	xvars = x /@ Range[nn];
	p[lam_] := p[lam] = Product[PowerSymmetricPolynomial[a, xvars], {a, DeleteCases[lam,0]}];
	ips = IntegerPartitions[dd, {nn}, Range[0, dd]];
	
	tbz = Union[Last /@ CoefficientRules[pol - Sum[ c[mu] p[mu], {mu, ips}], xvars]];
	
	Sum[c[mu] pp[mu], {mu, ips}] /. Solve[ Thread[tbz == 0], c /@ ips][[1]]
];



(* Takes a permutation, and returns the unique permutation that is RSK-equivalent to w, and is a reading word. *)
KnuthRepresentative[w_List] := KnuthRepresentative[w] = Join @@ Reverse[BiwordRSK[Transpose@{Range[Length@w], w}][[1]]];


(* This assumes the RPP has a basement! *)
RPPToAtom[rpp_List] := 
  Module[{n = Max[rpp], atom, cols = 1, col, rWord, insertElement},
   (* Start with empty basement. *)
   atom = List /@ Range[n];

   (* Inserts v in atmFil, starting from {r,c} *)
   
   insertElement[atmFil_List, v_Integer, {r_Integer, c_Integer}] := 
    Module[{nn = Length@atmFil, nr, nc},
     (*
     Print["Insering ",v," in ",YoungTableauForm@atmFil, 
     " ",{r,c}];
     *)
     nr = r + 1;
     nc = c;
     If[nr == n + 1, nr = 1; nc = c - 1];
	 
     Which[
      c == 1, Print["Cant insert ", v, " in atom ", atmFil]; Abort[],
      (* Right of an element, empty spot. *)
      
      Length[atmFil[[r]]] + 1 == c && atmFil[[r, c - 1]] >= v, 
      Insert[atmFil, v, {r, c}],
      (* Spot not empty, and box there is geq v, 
      continue in reading order. *)
      
      Length[atmFil[[r]]] + 1 > c && atmFil[[r, c - 1]] >= v && 
       atmFil[[r, c]] >= v,
      insertElement[atmFil, v, {nr, nc}],
      (* Spot not empty, and it's smaller, we do replace. *)
      
      Length[atmFil[[r]]] + 1 > c && atmFil[[r, c - 1]] >= v && 
       atmFil[[r, c]] < v,
      insertElement[ReplacePart[atmFil, {r, c} -> v], 
       atmFil[[r, c]], {nr, nc}],
      True, insertElement[atmFil, v, {nr, nc}]
      ]
     ];

   Do[
	    atom = insertElement[atom, v, {1, 1+Max[Length /@ atom]}];
    , {col, Sort/@SSAFColumnSets[rpp]}, {v, col}];

   atom
];

(*
(* This tests the above code *)
Do[
 ips = PadRight[#, n] & /@ IntegerPartitions[m, n];
 intcomps = Join @@ (Permutations /@ ips);
 Do[
  rpp = KeyFromShapeAndColumnSet[Sort[Length /@ atom - 1, Greater], 
    SSAFColumnSets@atom];
  If[
   RPPToAtom[rpp] =!= atom,
   Print[YoungTableauForm@atom, " ", YoungTableauForm@rpp, " ", 
    RPPToAtom[rpp]];
   Abort[]
   ];
  , {alpha, intcomps}, {atom, AtomFillings[alpha]}];
 , {m, 1, 6}, {n, 1, 4}]
*)



SSYTToAtom[ssyt_List] := 
  Module[{n = Max[ssyt], atom, cols = 1, rWord, insertElement},
   (* Start with empty basement. *)
   atom = List /@ Range[n];
   
   (* The reading word, which we insert into the atom filling. *)
   
   rWord = Join @@ Reverse[ssyt];
   
   (* Inserts v in atmFil, starting from {r,c} *)
   
   insertElement[atmFil_List, v_Integer, {r_Integer, c_Integer}] := 
    Module[{nn = Length@atmFil, nr, nc},
     (*
     Print["Insering ",v," in ",YoungTableauForm@atmFil, 
     " ",{r,c}];
     *)
     nr = r + 1;
     nc = c;
     If[nr == n + 1, nr = 1; nc = c - 1];
     
     Which[
      c == 1, Print["Cant insert ", v, " in atom ", atmFil]; Abort[],
      (* Right of an element, empty spot. *)
      
      Length[atmFil[[r]]] + 1 == c && atmFil[[r, c - 1]] >= v, 
      Insert[atmFil, v, {r, c}],
      (* Spot not empty, and box there is geq v, 
      continue in reading order. *)
      
      Length[atmFil[[r]]] + 1 > c && atmFil[[r, c - 1]] >= v && 
       atmFil[[r, c]] >= v,
      insertElement[atmFil, v, {nr, nc}],
      (* Spot not empty, and it's smaller, we do replace. *)
      
      Length[atmFil[[r]]] + 1 > c && atmFil[[r, c - 1]] >= v && 
       atmFil[[r, c]] < v,
      insertElement[ReplacePart[atmFil, {r, c} -> v], 
       atmFil[[r, c]], {nr, nc}],
      True, insertElement[atmFil, v, {nr, nc}]
      ]
     ];
   
   Do[
    atom = insertElement[atom, v, {1, Max[Length /@ atom] + 1}];
    , {v, Reverse@rWord}];
   atom
   ];


(*************************** WORD STUFF AND LASCOUX-SCHUTZENBERRGER   ******************)


WordDecompose[{}] = {};
WordDecompose[word_List] := Module[{findPos, n = Length@word, currPos = 0, nPos, subWordIdx = {}},
	
   (* Find the first position of e, to the cyclically right of position p.  Returns -1 if not found. *)
   
	findPos[e_, p_] := Catch[Do[
		If[word[[j]] == e, Throw[j]], 
		{j, Ordering@RotateRight[Range[n], p]}]; -1];
   
	Do[
	(* If we find next element, put it as past of subword, and update position.
	Otherwise, just keep looking.  *)
	nPos = findPos[e, currPos];
		
	If[nPos > 0,
		AppendTo[subWordIdx, nPos];
		currPos = nPos;
	];
		
	, {e, Max[word]}];
   
	subWordIdx = Sort[subWordIdx];
	Join[{word[[subWordIdx]]},
		WordDecompose[ word[[ Complement[Range[n], subWordIdx]  ]] ]
	]
];



ChargeWordDecompose[{}] = {};
ChargeWordDecompose[{},{}] = {};
ChargeWordDecompose[word_List, subwordSizes_List:{}]:= Module[
	{findPos, n = Length@word, currPos = Length@word, nPos, subWordIdx = {}, swSize = Max@word},
	
	If[Length[subwordSizes]>0,
		swSize = First@subwordSizes;
	];
	
	
	(* Find the first position of e, to the cyclically left of position p.  Returns -1 if not found. *)
	findPos[e_, p_] := Catch[Do[
		If[word[[j]] == e, Throw[j]], 
		{j, Ordering@RotateRight[Reverse@Range[n], p]}]; -1];
	
	Do[
	(* If we find next element, put it as past of subword, and update position.
	Otherwise, just keep looking.  *)
	nPos = findPos[e, currPos];
		
		If[nPos > 0,
			AppendTo[subWordIdx, nPos];
			currPos = nPos;
		];
		
	, {e, swSize}];
	
	
	subWordIdx = Sort[subWordIdx];
	
	Join[{word[[subWordIdx]]},
		ChargeWordDecompose[ 
			word[[ Complement[Range[n], subWordIdx]  ]]
			, 
			If[Length@subwordSizes == 0,{},Rest[subwordSizes]] 
			]
	]
];



(* Extract the boxes that form a left-to-right word of the form (i+1)^a i^b.
The word is a list of the form {r,c}->entry, where the columns appear in strictly increasing order. *)
SSAFCrystalWord[ssaf_List, i_Integer] := Module[
	{RemovePair, SameColDelete,len, subWord, theSpecWord, a,c,r, replace, replaceRule, colStart=2},
	
	len = Max[Length /@ ssaf];
	
	(* The subword consisting of elements in {i, i+1} *)
	subWord = Join @@ Table[
		If[Length[ssaf[[r]]] >= c && i <= ssaf[[r, c]] <= i + 1,
			{r, c} -> ssaf[[r, c]],
			Sequence @@ {}
		]
		,{c, colStart, len}, {r, Length[ssaf], 1,-1}];
	
	(* Delete all pairs of letters {i,i+1} that appear in the same column. *)
	SameColDelete[wrd_]:=Complement[wrd, Join@@Table[
		With[{sel = Select[wrd, #[[1,2]]==c &] },
			If[ Length[sel]==2, sel, Sequence@@{} ]
		]
		, {c, colStart, len}]];
	
	
	(* Remove the first subpair {i , i+1} *)
	RemovePair[wrd_] := Catch[
		Do[
			If[wrd[[j, 2]] + 1 == wrd[[j + 1, 2]],
				Throw[Join[wrd[[;; j - 1]], wrd[[j + 2 ;;]]]]
			]
			, {j, Length[wrd] - 1}];
		wrd
	];
	
	(* Mathematica is retarded and do not preserve item order in Complement. *)
	(* Thus, need to sort agan. *)
	subWord = SortBy[SameColDelete@subWord, #[[1,2]] & ];
	
	(* This is now of the form (i+1)^a, i^b *)
	theSpecWord = FixedPoint[RemovePair, subWord];
	
	theSpecWord
];


SSAFRaising[ssaf_List, i_Integer]:=Module[{word,k,pos},
	
	(* Obtain crystal word, (i+1)^a i^b *)
	word = SSAFCrystalWord[ssaf,i];
	
	If[ Length[word]==0 || word[[-1,2]] == (i+1),
		
		(* If there is no such filling, return empty *)
		{}
		
		,
		
		pos = Position[word, {_, _} -> i, 1, 1][[1, 1]];
		word = ReplacePart[word, pos-> (word[[pos,1]]->(i+1))];
		
		(* Return the raised SSAF, or {} if impossible to sort columns. *)
		SSAFKeySortColumns@ReplacePart[ssaf, word]
	]
];

SSAFLowering[ssaf_List, i_Integer]:=Module[{word,k,pos},
	
	(* Obtain crystal word, (i+1)^a i^b *)
	word = SSAFCrystalWord[ssaf,i];
	
	If[ Length[word]==0 || word[[1,2]] == i,
		
		(* If there is no such filling, return empty *)
		{}
		
		,
		
		pos = Last[Position[word, {_, _} -> (i+1), 1]][[1]];
		word = ReplacePart[word, pos-> (word[[pos,1]]->i)];
		
		(* Return the raised SSAF, or {} if impossible to sort columns. *)
		SSAFKeySortColumns@ReplacePart[ssaf, word]
	]
];

SSAFCrystalString[ssaf_List, i_Integer]:=Module[{ raisingPart },
	
	raisingPart = Most@Rest@NestWhileList[ SSAFRaising[#,i]&, ssaf, #=!={} & ];
	loweringPart = Reverse[Most@Rest@NestWhileList[ SSAFLowering[#,i]&, ssaf, #=!={} & ]];
	
	Join[loweringPart,{ssaf},raisingPart]
];



(* Performs the Lascoux--Schutzeberger involution on the ssaf. *)
(* This works good on all SSAFs, but one needs to sort the columns afterwards. *)
LascouxSchutzenberger[ssaf_List, i_Integer] := Module[
	{theSpecWord, replace, replaceRule},
	
	(* Obtain crystal word. *)
	theSpecWord = SSAFCrystalWord[ssaf,i];
	
	(* This constructs the replacing rule. *)
	replace = Reverse[theSpecWord[[All, 2]] /. {i + 1 -> i, i -> i + 1}];
	replaceRule = MapThread[#1 -> #2 &, {theSpecWord[[All, 1]], replace}];
	
	ReplacePart[ssaf, replaceRule]
];

(* Interchanges a and b via a series of involutions *)
LascouxSchutzenberger[ssaf_List, {a_Integer,b_Integer}] := Module[{n, word},
	n = Length[ssaf];
	
	word = ReducedWord@ReplacePart[Range[n], {a -> b, b -> a}];
	Fold[LascouxSchutzenberger[#1,#2]&, ssaf, word]
];

(* We need to generalize to the case when a column contains duplicates *)
ModifiedLascouxSchutzenberger[ssaf_, i_Integer] := Module[
	{RemovePair, SameColDelete,len, subWord, theSpecWord, a, r, c, replace, replaceRule, colStart=2},
	
	len = Max[Length /@ ssaf];
	
	(* The subword consisting of elements in {i, i+1} *)
	subWord = Join @@ Table[
		If[Length[ssaf[[r]]] >= c && i <= ssaf[[r, c]] <= i + 1,
			{r, c} -> ssaf[[r, c]],
			Sequence @@ {}
		]
		,{c, colStart, len}, {r, Length@ssaf,1,-1}];
	

	(* Delete all pairs of letters {i,i+1} that appear in the same column. *)
	SameColDelete[wrd_] := Complement[wrd,
		Join@@Table[
			With[{sel = Select[wrd, #[[1,2]]==c &] },
				
				If[ Length[ Union[Last/@sel] ] == 2, (* Exactly two differen entries. *)
					sel,
					Sequence@@{} 
				]
			]
		, {c, colStart, len}]
	];
	
	(* Remove the first subpair {i , i+1} *)
	RemovePair[wrd_] := Catch[
		Do[
			If[wrd[[j, 2]] + 1 == wrd[[j + 1, 2]],
				Throw[Join[wrd[[;; j - 1]], wrd[[j + 2 ;;]]]]
			]
			, {j, Length[wrd] - 1}];
		wrd
	];
	
	
	
	(* Mathematica is retarded and do not preserve item order in Complement. *)
	(* Thus, need to sort agan. *)
	subWord = SortBy[SameColDelete@subWord, #[[1,2]] & ];
	
	(* This is now of the form (i+1)^a, i^b *)
	theSpecWord = FixedPoint[RemovePair, subWord];
	
	(* This constructs the replacing rule. *)
	replace = Reverse[theSpecWord[[All, 2]] /. {i + 1 -> i, i -> i + 1}];
	replaceRule = MapThread[#1 -> #2 &, {theSpecWord[[All, 1]], replace}];
	
	ReplacePart[ssaf, replaceRule]
];

ModifiedLascouxSchutzenberger[ssaf_, {a_Integer,b_Integer}] := Module[{n, word},
	n = Length[ssaf];
	
	word = ReducedWord@ReplacePart[Range[n], {a -> b, b -> a}];
	
	Fold[ModifiedLascouxSchutzenberger[#1,#2]&, ssaf, word]
];


SSAFWeightNormalize[ssaf_]:=Module[{word, gam, pi, rWord},
	word = Join@@Reverse[Rest/@ssaf];
	gam = Normal@SparseArray[#1 -> #2 & @@@ (Tally@word), Max@word];
	
	pi = AtomCompositionToPermutation[gam][[1]];
	rWord = Reverse@ReducedWord[pi];
	
	(* Apply the reduced word, such that the resulting SSAF has partition weight. *)
	SSAFKeySortColumns@Fold[LascouxSchutzenberger, ssaf , rWord]
];

SSYTLascouxSchutzenberger[ssyt_, i_Integer] := Module[
	{RemovePair, SameColDelete,len, subWord, theSpecWord, a, replace, replaceRule},
	len = Max[Length /@ ssyt];
	
	(* The subword consisting of elements in {i, i+1} *)
	subWord = Join @@ Table[
		If[Length[ssyt[[r]]] >= c && i <= ssyt[[r, c]] <= i + 1,
			{r, c} -> ssyt[[r, c]],
			Sequence @@ {}
		]
		,{c, len}, {r, Length@ssyt,1,-1}];
	
	
	(* Delete all pairs of letters {i,i+1} that appear in the same column. *)
	SameColDelete[wrd_]:=Complement[wrd, Join@@Table[
		With[{sel = Select[wrd, #[[1,2]]==c &] },
			If[ Length[sel]==2, sel, Sequence@@{} ]
		]
		, {c, len}]];
	
	
	(* Remove the first subpair {i+1 , i} *)
	RemovePair[wrd_] := Catch[
		Do[
			If[ wrd[[j, 2]] == wrd[[j + 1, 2]] + 1,
				Throw[ Join[wrd[[;; j - 1]], wrd[[j + 2 ;;]]] ]
			]
		, {j, Length[wrd] - 1}];
		wrd
	];
	
	
	(* Mathematica is retarded and do not preserve item order in Complement. *)
	(* Thus, need to sort agan. *)
	subWord = SortBy[ SameColDelete@subWord,  #[[1,2]] &];
	
	(* This is now of the form (i)^a, (i+1)^b *)
	theSpecWord = FixedPoint[RemovePair, subWord];
	
	
	(* This constructs the replacing rule. *)
	replace = Reverse[theSpecWord[[All, 2]] /. {i + 1 -> i, i -> i + 1}];
	replaceRule = MapThread[#1 -> #2 &, {theSpecWord[[All, 1]], replace}];
	
	ReplacePart[ssyt, replaceRule]
];

SSYTLascouxSchutzenbergerInvolution[ssyt_]:=Module[{w0},
	
	w0 = ReducedWord@Reverse@Range[Max[ssyt]];
	
	Fold[SSYTLascouxSchutzenberger, ssyt, w0]
];


SSYTWeightNormalize[ssyt_]:=Module[{word,gam, pi, rWord, newWord},
	word = Join@@Reverse[ssyt];
	gam = Normal@SparseArray[#1 -> #2 & @@@ (Tally@word), Max@word];
	
	pi = AtomCompositionToPermutation[gam][[1]];
	rWord = Reverse@ReducedWord[pi];
	
	(* Apply the reduced word, such that the resulting SSYT has partition weight. *)
	Fold[SSYTLascouxSchutzenberger, ssyt , rWord]
];


LascouxSchutzenbergerNormalizeWord[word_List]:=Module[{gam, pi, rWord, newWord},
	gam = Normal@SparseArray[#1 -> #2 & @@@ (Tally@word), Max@word];
	pi = AtomCompositionToPermutation[gam][[1]];
	rWord = Reverse@ReducedWord[pi];
	
	(* This word now has partition weight. *)

	newWord = Rest@Fold[LascouxSchutzenberger, {Prepend[word, 0]}, rWord][[1]]
];

(* Decompose into permutations, and compute sum of charges. *)
WordCharge[word_List] := WordCharge[word]=Tr[PermutationCharge /@ ChargeWordDecompose[LascouxSchutzenbergerNormalizeWord[word]]];


(* This is probably not the optimal reading word... *)
SSAFReadingWord[tab_List] := Join @@ Reverse[(tab)];

(* This only works if shape is a partition! *)
SSAFKnownCharge[tab_] := 
  Tr[PermutationCharge[#] & /@ 
    WordDecompose[
     LascouxSchutzenbergerNormalizeWord@SSAFReadingWord[tab]]];

(* Returns the column sets. Note, sets are sorted, and basement is not included by default! *)
SSAFColumnSets[tab_List, startingColumn_:2] := Module[{len},
   len = Max[Length /@ tab];
   Sort /@ Table[
     If[Length[tab[[r]]] >= c, tab[[r, c]], Sequence @@ {}]
     , {c, startingColumn, len}, {r, Length@tab}]
];


(*
Ryan and Jennifer's map, that sends charge to maj.
*)
ChargeToMajMap[fil_List] :=ChargeToMajMap[fil]= Module[{IndexWordDecompose, word, standardWordIndices, standardWords, rcfl},

	(* Return list of INDICES of the standard subwords. *)
	IndexWordDecompose[{}] = {};
	IndexWordDecompose[w_] := {} /; Max[w] == 0;
	IndexWordDecompose[word_List] := Module[{findPos, n = Length@word, currPos = 0, nPos, subWordIdx = {}, cmpWord},
	
	(* Find the first position of e, to the cyclically RIGHT of position p. Returns -1 if not found. *)
	findPos[e_, p_] := 
		Catch[
			Do[
				If[ word[[j]] == e, Throw[j]], 
					{j,	Ordering@RotateRight[Range[n], p]}]; -1];
		
		Do[
			(*If we find next element,put it as past of subword,
			and update position. Otherwise, just keep looking.
			*)
			
			nPos = findPos[e, currPos];
			
			If[nPos > 0,
				AppendTo[subWordIdx, nPos];
				currPos = nPos;
			];
		,{e, Max[word]}];
		
		subWordIdx = Sort[subWordIdx];
	
		cmpWord = ReplacePart[word, # -> 0 & /@ subWordIdx];
		Prepend[IndexWordDecompose[cmpWord], subWordIdx]
	];
	
	
	(* Construct list with (row, col, entry) *)
	
	word = Join @@ Table[
		{r, c, fil[[r, c]]},
		{r, Length[fil], 1, -1}, {c, Length[fil[[r]]],1,-1}];
	
	
	(*
	 Print["word: ", word]; 
	*)
	
	standardWordIndices = IndexWordDecompose[Last /@ word];
	
	standardWords = word[[#]] & /@ standardWordIndices;
	
(* 	Print["stw: ",standardWords]; *)
	
	rcfl = Join @@ Table[
		Append[#, k] & /@ standardWords[[k]]
		, {k, Length@standardWords}];
	
	
(* 	Print["rcfl: ",rcfl]; *)
		
	DeleteCases[
		Normal@SparseArray[{#3, #4} -> #1 & @@@ rcfl, {Max[fil], 
		Max[Last /@ rcfl]}], 0, {2}]
];


SchubertPolynomialFromReducedWord[{}, x_, n_] := SchubertPolynomialFromReducedWord[{}, x, n] = Product[x[i]^(n - i), {i, n}];

SchubertPolynomialFromReducedWord[redWord_List, x_, n_] := 
  SchubertPolynomialFromReducedWord[redWord, x, n] =
   DividedDifference[ SchubertPolynomialFromReducedWord[ Most@redWord, x, n], x, redWord[[-1]] ];

SchubertPolynomial[perm_List, x_] :=  SchubertPolynomial[perm, x] = Module[{w0, w, n, rw},
	n = Max[perm];
	w0 = Range[n, 1, -1];
	w = PermutationProduct[w0, Ordering[perm]];
	SchubertPolynomialFromReducedWord[ ReducedWord[w], x, n]
];


QSymMonomial[alpha_List, n_Integer, x_] := QSymMonomial[alpha, n, x] = 
With[{k = Length@alpha},
	Sum[
		Product[ x[id[[i]]]^alpha[[i]], {i, k}]
	, {id, Subsets[Range[n], {k}]}]
];


GesselFundamental[des_List, n_Integer, x_] := GesselFundamental[des, n, n, x];
GesselFundamental[des_List, n_Integer, nvars_Integer, x_] := GesselFundamental[des, n, nvars, x] = Module[{IsOk, ips},
	
	IsOk[p_] := And @@ (p[[#]] < p[[# + 1]] & /@ des);
	ips = Join @@ Table[
		Reverse /@ IntegerPartitions[k, {n}, Range[nvars]], {k, n, n*nvars}];
	
	ips = Select[ips, IsOk];
	
	Tr[(Times @@ (x /@ #)) & /@ ips]
];

(* The fundamental slide polynomials, as defined by S. Assaf *)
FundamentalSlide[alpha_List, x_] := FundamentalSlide[alpha, x] = Module[
	{beta, n = Length[alpha], comps, exp, flatA},
	
	flatA = DeleteCases[alpha, 0];
	comps = Join @@ (Permutations /@ IntegerPartitions[Tr@alpha, {n}, Range[0, Max[alpha]]]);
	exp = Select[comps,
		(And@@Thread[ Accumulate[alpha] <= Accumulate[#] ])  && 
		MemberQ[CompositionRefinements[flatA], DeleteCases[#, 0]] &];
	
	Total[Times @@ ((x /@ Range[n])^#) & /@ exp]
];


FundamentalSlideRule[size_, n_, x_, aa_] := FundamentalSlideRule[size, n, x, aa] = 
	CompositionIndexedBasisRule[size, n, x, aa, FundamentalSlide[#, x] &];

ToFundamentalSlideBasis[polIn_, varsIn_:0, x_, aa_] := Module[{vars, z, deg, monomList,nn},
		
		If[polIn === 0,
			0
			,
		    vars = x/@ Range[ If[varsIn==0, Max[First/@Cases[Variables[polIn], x[_]]], varsIn ] ];
		    
		    nn = Max[First /@ vars, 0];
			monomList = MonomialList[polIn, vars];
			
			Sum[ 
				deg = Exponent[mon /. x[i_] :> z, z];
				If[deg <= 0,
					mon
					,
					mon /. FundamentalSlideRule[deg, nn, x, aa]
				]
			, {mon, monomList}]
		]
];


ToGesselSubsetBasis[poly_, x_, ff_] := ToFundamentalSlideBasis[poly, x, ff] /. {ff[a_] :> ff[Most@Accumulate@DeleteCases[a, 0]]};


(* Alpha is supposed to have all parts >= 0 *)
QSymSchur[alpha_List, n_Integer, x_] := Module[{ss, args},
	ss = Subsets[Range[n], {Length[alpha]}];
	args = Table[Normal@SparseArray[MapThread[Rule, {s, alpha}], n], {s, ss}];
	Sum[OperatorAtomPolynomial[a, x], {a, args}]
];



(* This takes a list of lists! *)
PartitionedCompositionCoarsenings[{{a_Integer}}] := {{{a}}};
PartitionedCompositionCoarsenings[alpha_List] := 
  Module[{rest, first},
   rest = Rest[alpha];
   first = alpha[[1]];
   Join[
    (* Either first block is separate, 
    or joined with the thing on the right. *)
    
    Prepend[#, first] & /@ PartitionedCompositionCoarsenings[rest],
    {Join[first, #1], ##2} & @@@ 
     PartitionedCompositionCoarsenings[rest]
    ]
];

QuasiSymmetricPowerSum[alpha_List, n_Integer, x_] :=
	QuasiSymmetricPowerSum[alpha, n, x] = Module[{pi},
	pi[comp_List] := Times @@ Accumulate[comp];
	ZCoefficient[alpha] Sum[
	1/(Times @@ (pi /@ beta)) QSymMonomial[Total /@ beta, n, x]
	, {beta, PartitionedCompositionCoarsenings[List /@ alpha]}]
];


QuasiSymmetricPowerSum2[alpha_List, n_Integer, x_] :=
	QuasiSymmetricPowerSum2[alpha, n, x] = Module[{spi},
	spi[comp_List] := Length[comp!] (Times @@ comp);
	ZCoefficient[alpha] Sum[
	1/(Times @@ (spi /@ beta)) QSymMonomial[Total /@ beta, n, x]
	, {beta, PartitionedCompositionCoarsenings[List /@ alpha]}]
];



(****************************************************************************************************)

(*
Given a list of subsets {{ss1}->{vv1}, {ss2}->{vv2}...} we refine \
these relations.
*)
(* NOTE: We shift the numbers in sparse array, since 0->5 is illegal. *)
MultiSetToSparseArray[ms_List, dim_Integer] := SparseArray[ (#1+1) -> #2 & @@@ Tally[ms], {dim}];
SparseArrayToMultiSet[spA_SparseArray] := Join @@ Table[ ConstantArray[r[[1, 1]] - 1 , r[[2]]], {r, Most[ArrayRules[spA]]}];

RefineSubsetsRelations[subsetRelations_List] := Module[{i, j, ii, s1, s2, m1, m2, int, com, fail = False,
    newRel, foundNew = True, relations = subsetRelations, sa, dim},
   
   (* Maximal possible statistic. *)
   dim = 1 + Max[Last /@ relations];
   
   (* Sort input subsets. *)
   
   relations = {Sort@#1, MultiSetToSparseArray[#2, dim]} & @@@ relations;
   
   While[ foundNew == True && fail == False,
    
    foundNew = False;
    (* Sort by size. *)
    
    relations = Select[relations, Length[#1] > 0 &];
    relations = SortBy[Union@relations, Length[#[[1]]] &];
    
    Do[
     
     If[relations[[i]] === relations[[j]] ,
      Continue[]
      ];
     
     {s1, m1} = relations[[i]];
     {s2, m2} = relations[[j]];
     int = Intersection[s1, s2];
     
     If[ Length[s1] == 0 || Length[s2] == 0,
      Continue[]
      ];
     
     If[ Length[int] == Length[s1],
      
      If[Min[m2 - m1] < 0,
       Print["Contradicion in RefineSubsetsRelations", Complement[s2, s1], ArrayRules[m2 - m1]];
	   Print["Params: ", s1 , " ", s2 ];
	   fail = True;
       Continue[];
       ];
      (* Replace the more general rule with a specific one. *)
      
      relations[[j]] = {Complement[s2, s1], m2 - m1};
      (*
      Print[ "New rule:",YoungTableauForm/@Complement[s2,s1],
      SparseArrayToMultiSet[m2-m1]];
      *)
      foundNew = True;
      Continue[]
      ];
     
     (* Only one common value, this means that the intersection only takes this value. *)
     
     com = Intersection[SparseArrayToMultiSet[m1], 
       SparseArrayToMultiSet[m2]];
     If[ Length[com] == 1 && Length[int] > 0,
      com = First[com];
      sa = SparseArray[com -> Length[int], dim];
      newRel = {int, sa};
      relations[[i]] = {Complement[s1, int], m1 - sa};
      relations[[j]] = {Complement[s2, int], m2 - sa};

      Do[
       AppendTo[relations, { {ii}, SparseArray[com -> 1, dim]}];
       , {ii, int}];
      (* Print[ "New rule2:",YoungTableauForm/@int," ",
      com]; *)
      
      foundNew = True;
      Continue[]
      ];
     
     , {i, Length@relations}, {j, i + 1, Length@relations}];
    ];
   
	If[fail == False,
	   {#1, SparseArrayToMultiSet[#2]} & @@@ relations
	   ,
	   {}
	]
];




End[(* End private *)];
EndPackage[];


