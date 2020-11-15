(* ::Package:: *)

Clear["CombinatoricsUtil`*"];

BeginPackage["CombinatoricsUtil`"];

(*

Use https://reference.wolfram.com/language/ref/SyntaxInformation.html

Unit-testing: https://www.wolfram.com/mathematica/new-in-10/integrated-unit-testing/run-one-off-tests.html

*)

UnimodalQ;
Nicify;
DescentSet;
Descents;
MajorIndex;
CoMajorIndex;
Inversions;
CoInversions;
Runs;
RightToLeftMinima;
WeakStandardize;
StandardizeList;
PackedWords;
PartitionList;
ChargeWordDecompose;
WordCharge;
WordCocharge;

MultiSubsets;

PathExceedanceDecreaseMap;

IntegerPartitionQ;
SetPartitions;
IntegerCompositions;
WeakIntegerCompositions;
CompositionRefinements;
CompositionToDescentSet;
DescentSetToComposition;
CompositionToRibbon;
CompositionWord;
WordComposition;
CompositionSlinky;
PartitionPartCount;
PartitionCores;
HookLengths;
ConjugatePartition;
PartitionJoin;
PartitionLessEqualQ;
PartitionDominatesQ;
PartitionStrictDominatesQ;
PartitionN;
ZCoefficient;
PartitionAddBox;
PartitionRemoveBox;
PartitionRemoveHorizontalStrip;
PartitionRemoveVerticalStrip;

PartitionArm;
PartitionLeg;
ShapeBoxes;
MacdonaldPsi;

PartitionCore;
PartitionQuotient;
PartitionAbacus;
AbacusForm;

KostantPartitionFunction;

SubsetTuples;
TupleDescents;
TupleMajorIndex;
TupleInversions;

qInteger;
qFactorial;
qBinomial;
qMultinomial;
qHookFormula;
qCatalan;
qCarlitzCatalan;
qtCatalan;
qNarayana;
qKreweras;
qAlternatingSignMatrices;
qIntegerFactorize;

Is123AvoidingQ;
Is132AvoidingQ;
Is213AvoidingQ;
Is231AvoidingQ;
Is312AvoidingQ;
Is321AvoidingQ;

PermutationAllCycles;
PermutationType;
PermutationCycleMap;
ReducedWord;
PermutationFromWord;
PermutationCode;
FoataMap;
CarlitzMap;
StrongBruhatGreaterQ;
WeakBruhatGreaterQ;
PermutationCharge;
PermutationCocharge;

HStarPolynomial;


SnCharacter;
KostkaCoefficient;
InverseKostkaCoefficient;

Begin["Private`"];

Nicify::usage = "Rewrites polynomial expression in a nice form";
Nicify[expr_] := Module[{polVars, collected},
	polVars = Select[Variables[expr], Length[#] > 0 && Head[#[[1]]] === List &];
	Collect[Expand@expr, polVars] /. {aa_[m_List] :> Subscript[ToString[aa], Row[m]], x_[i_Integer] :> Subscript[x, i]}
];



SimionSchmidtMap::usage = "SimionSchmidtMap[perm] ,see https://core.ac.uk/download/pdf/82232421.pdf";
SimionSchmidtMap[pi_List] := 
  Module[{x = pi[[1]], n = Length@pi, out},
   out = {x};
   Do[
    If[pi[[i]] < x,
     (AppendTo[out, pi[[i]]]; x = pi[[i]];)
     ,
     AppendTo[out,
      Min[Select[Range[x + 1, n], ! MemberQ[out, #] &]]
      ]
     ]
    , {i, 2, n}];
   out
];




(**Lists and words **)

UnimodalQ[list_List] := With[{dd = Reverse@DeleteCases[Sign@Differences[list], 0]}, dd === Sort[dd]];


DescentSet[p_List]:=Table[If[p[[i]]>p[[i+1]],i,Sequence@@{}],{i,Length[p]-1}];

Descents[p_List] := Length@DescentSet[p];

MajorIndex[p_List] := Tr@DescentSet[p];

CoMajorIndex[w_List] := With[{n = Length@w}, Sum[Boole[w[[i]] > w[[i + 1]]] (n - i), {i, n - 1}]];

Inversions[p_List] := With[{n = Length@p}, 
	Sum[Boole[p[[i]] > p[[j]]], {i, n}, {j,i+1,n}]
];

CoInversions[p_List]:=Binomial[Length@p,2]-Inversions[p];

RightToLeftMinima::usage = "RightToLeftMinima returns the elements in p which are right-to-left minima.";

RightToLeftMinima[p_List] := p[[Table[If[p[[i]] == Min[p[[i ;;]]], i, Nothing], {i, Length@p}]]];

Runs[{}] := {};
Runs[lst_List] := With[
   {ds = Join[{0}, DescentSet@lst, {Length@lst}]},
   lst[[#1 + 1 ;; #2]] & @@@ Partition[ds, 2, 1]
];


WeakStandardize[list_List]:=With[{rule = Thread[Union[list]->Ordering@Union[list]]}, list /. rule];

StandardizeList::usage = "StandardizeList[list] standardizes the list. For equal entries, order from the left.";
StandardizeList[list_List]:=Ordering[ Range[Length[list]][[Ordering@list]] ];
UnitTest[StandardizeList]:=And[
	StandardizeList[{9,1,5,7}]==={4,1,2,3},
	StandardizeList[{1,1,2,2}]==={1,2,3,4}
];

PackedWords::usage = "PackedWords[n] returns all packed words of length n.";
PackedWords[n_Integer] := PackedWords[n] = Union[WeakStandardize /@ Tuples[Range[n], n]];

PartitionList::usage = "PartitionList[list,mu] partitions the list into non-overlapping pieces of sizes given by mu.";

PartitionList::mismatch = "The list must have same length as sum of part sizes.";
PartitionList[lst_List, mu_List]/;(Length[lst]!=Tr[mu]):= Message[PartitionList::mismatch];

PartitionList[lst_List, mu_List] := With[
{acc = Accumulate@Prepend[mu, 0]},
 Table[lst[[1 + acc[[i - 1]] ;; acc[[i]]]], {i, 2, Length@acc}]
];
UnitTest[PartitionList]:=And[
  PartitionList[Range[5],{3,2}]==={{1,2,3},{4,5}},
  PartitionList[Range[5],{3,0,2}]==={{1,2,3},{},{4,5}},
  PartitionList[{1,1,1},{3}]==={{1,1,1}},
  PartitionList[{1,1,1},{1,2}]==={{1},{1,1}},
  PartitionList[Range[5],{1,1,1,1,1}]==={{1},{2},{3},{4},{5}}
];



ChargeWordDecompose::usage = "ChargeWordDecompose[word] decomposes a word with partition type into a list of standard subwords.";
ChargeWordDecompose[{}] = {};
ChargeWordDecompose[{},{}] = {};
ChargeWordDecompose[word_List, subwordSizes_List:{}]:= Module[
	{findPos, n = Length@word, 
	currPos = Length@word, nPos, subWordIdx = {}, swSize},
	
	swSize = If[
	Length[subwordSizes]>0,
		First@subwordSizes,
		Max@word];
	
	(* Find the first position of e, to the cyclically 
	left of position p.  Returns -1 if not found. *)
	
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

WordCharge::usage = "WordCharge[w] returns the charge of a word with partition weight.";
WordCharge[w_List]:=Tr[PermutationCharge/@ChargeWordDecompose[w]];

WordCocharge::usage = "WordCocharge[w] returns the cocharge of a word with partition weight.";
WordCocharge[w_List]:=Tr[PermutationCocharge/@ChargeWordDecompose[w]];





MultiSubsets[a_Integer, b_Integer]:=MultiSubsets[Range@a,b];

MultiSubsets[elems_List, 0] := {{}};
MultiSubsets[elems_List, 1] := List /@ elems;
MultiSubsets[elems_List, b_Integer] :=
  Union[
   Join @@ Outer[Sort[Join[#1, {#2}]] &,
     MultiSubsets[elems, b - 1], elems, 1]
   ];
   
UnitTest[MultiSubsets]:=And@@(
Join@@Table[
 Length[MultiSubsets[Range[n], k]] - Binomial[n + k - 1, k]==0
 , {k, 1, 5}, {b, 1, 4}]);




(*
Given a path from (0,0) to (n,n), decrease its exceedance by 1.
Catalan Paths are fixed points.
https://en.wikipedia.org/wiki/Catalan_number#Third_proof
*)
PathExceedanceDecreaseMap::usage="PathExceedanceDecreaseMap[bw] applies the path exceedance map on a path from (0,0) to (n,n).";
PathExceedanceDecreaseMap[w_List] := Module[
   {size = Length@w, a, x, aw, above},
   aw = Accumulate[w];
   above = Select[Range[size], 2 aw[[#]] > # &, 1];
   If[Length@above > 0,
    a = First[above];
    x = Select[Range[a + 1, size], 2 aw[[#]] == # &, 1][[1]];
    Join[w[[x + 1 ;;]], {0}, w[[;; x - 1]]]
    ,
    w]
];
UnitTest[PathExceedanceDecreaseMap]:=(
PathExceedanceDecreaseMap[{0,1,0,0,1,1,1,1,0,0,1,0,0,0,1,1}]=={1,0,0,0,1,1,0,0,1,0,0,1,1,1,1,0}
);


(**PARTITIONS and compositions**)


IntegerPartitionQ::usage = "IntegerPartitionQ[lam] returns 
true only if lam is a weakly decreasing list of positive integers.";

IntegerPartitionQ[lam_List] := And[
	VectorQ[lam, IntegerQ[#] && # > 0 &],
	Max[Differences[lam]] <= 0
];


SetPartitions::usage = "SetPartitions[n] returns all set partitions of {1,2,...,n}. The cardinalities are given by the Bell numbers, A000110.";

SetPartitions[1] := {{{1}}};
SetPartitions[n_Integer] := SetPartitions[n] = Module[{sp},
    sp = SetPartitions[n - 1];
    Join @@ Table[
      Append[
       Table[MapAt[Append[#, n] &, p, k], {k, Length[p]}]
       ,
       Append[p, {n}]
       ]
      , {p, sp}]
];


IntegerCompositions::usage = "IntegerCompositions[n] returns all interger compositions of n. IntegerCompositions[n,k] gives all compositions with length k.";
IntegerCompositions[n_Integer] := Differences[Join[{0}, #, {n}]] & /@ Subsets[Range[n - 1]];
IntegerCompositions[n_Integer,k_Integer]:=Select[IntegerCompositions[n], Length[#]==k&];

WeakIntegerCompositions::usage = "WeakIntegerCompositions[n,k] gives a list of all weak compositions of n with k parts.";
WeakIntegerCompositions[n_Integer, nparts_Integer] := 
  Join @@ (Permutations /@ IntegerPartitions[n, {nparts}, Range[0, n]]);


CompositionRefinements::usage="CompositionRefinements[comp] returns all refinements of the composition.";
CompositionRefinements[comp_List/;(VectorQ[comp,IntegerQ] && Max[comp]==1)]:= {comp};
CompositionRefinements[comp_List/;VectorQ[comp,IntegerQ]] := CompositionRefinements[comp] = 
Module[{n = Length@comp, refs},
	refs = Join @@ Table[
		ReplacePart[comp, i -> Sequence[k, comp[[i]] - k]]
	, {i, n}, {k, comp[[i]]-1}];
	
	Union@Join[{comp}, refs, Join @@ (CompositionRefinements /@ refs)]
];


CompositionToDescentSet::usage = "CompositionToDescentSet[alpha] returns the set presentation of the composition.";
CompositionToDescentSet[alpha_List] := Most@Accumulate[alpha];

DescentSetToComposition::usage = "DescentSetToComposition[des,n] returns the composition associated with a descent set D subset [n-1].";
DescentSetToComposition[des_List, n_Integer] := Differences[Join[{0},des,{n}]];


CompositionToRibbon::usage = "CompositionToRibbon[alpha] returns a ribbon skew shape, where row j from the bottom has a_j boxes.";
CompositionToRibbon[alpha_List] := 
  Module[{lam, mu, n = Length@alpha},
   lam = Reverse[Accumulate[alpha] - Range[0, n - 1]];
   mu = Rest[lam] - 1;
   {lam, mu}
];

CompositionWord::usage = "CompositionWord[alpha] returns a binary word representing the composition.";
CompositionWord[comp_List] := Table[Boole@MemberQ[Accumulate@comp, j], {j, Tr[comp] - 1}];
WordComposition::usage = "WordComposition[bw] is the inverse of CompositionWord, and returns a composition.";
WordComposition[{}] := {}; 
WordComposition[bw_List] := Differences@Join[{0}, Pick[Range[Length[bw]], bw, 1], {1 + Length@bw}]


CompositionSlinky::usage = "CompositionSlinky[comp] applies the slinky rule to the composition.
The return value is {lam,s} where lam is the resulting partition, and s is the sign, i.e, parity of number of 'slinks'
The value of s=0 is a partition could not be reached.";
CompositionSlinky[comp_List] := Module[{slink, doSlink, s = 0, final},
	doSlink[a_, b_] := (s++; {a, b});
	slink[v_] := 
		SequenceReplace[
		v, {a_Integer, b_Integer} /; a + 1 < b :> 
		Sequence @@ doSlink[b - 1, a + 1]];
	final = FixedPoint[slink, comp];
	{final, (-1)^s Boole[OrderedQ[Reverse@final]]}
];


PartitionJoin::usage = "PartitionJoin[a,b] returns a new partition whose parts are the union of a and b.";
PartitionJoin[a_List, b_List] := Sort[Join[a, b], Greater];


PartitionPartCount::usage="PartitionPartCount[lam] returns (m1,m2,...) so that mi is then number of parts of size i.";
PartitionPartCount[{}]:={};
PartitionPartCount[lam_List] := Normal[SparseArray[#1 -> #2 & @@@ Tally[ DeleteCases[lam,0]  ]]];


PartitionCores::usage = "PartitionCores[n,p] returns all partitions of n with no hook-length divisible by p.";
PartitionCores[n_Integer,p_Integer]:=
	Select[IntegerPartitions[n],Min[Mod[(Join@@HookLengths[#]),p]]>0&];

(* See https://arxiv.org/pdf/math/0505519.pdf for k-involution and some interesting maps on cores. *)
(* Some more examples in https://arxiv.org/pdf/1004.4886.pdf *)


HookLengths::usage="HookLengths[lam] returns a table with hook values as entries.";
HookLengths[lambda_List]:=Module[{cLambda,r,c},
	cLambda=ConjugatePartition@lambda;
	Table[
		1-c+lambda[[r]]-r+cLambda[[c]]
	,{r,Length@lambda},{c,lambda[[r]]}]
];
UnitTest[HookLengths]:=And[
	HookLengths[{2,1,1}]==={{4,1},{2},{1}}
];


ConjugatePartition::usage = "ConjugatePartition[lam] returns the conjugate of partition.";
ConjugatePartition[{}]={};
ConjugatePartition[p_List]:=Table[Count[p, j_ /; j >= m], {m, First[p]}];
UnitTest[ConjugatePartition]:=And[
	ConjugatePartition[{}]==={},
	ConjugatePartition[{2,1,1}]==={3,1},
	ConjugatePartition[{4}]==={1,1,1,1},
	ConjugatePartition[{1}]==={1}
];


PartitionLessEqualQ::usage = "PartitionLessEqualQ[lam,mu] returns true if lam is entrywise less-equal to mu.";
PartitionLessEqualQ[p1_List,p2_List]:=With[{n=Max[Length/@{p1,p2}]},
	And@@Thread[PadRight[p1,n]<=PadRight[p2,n]]
];
UnitTest[PartitionLessEqualQ]:=And[
	PartitionLessEqual[{1,1},{2,1,1}],
	PartitionLessEqual[{3,2,1},{3,2,1}],
	Not@PartitionLessEqual[{4},{3,2,1}]
];


PartitionDominatesQ::usage = "PartitionDominatesQ[lam, mu] returns true if mu dominates lam.";
PartitionDominatesQ[p1_List, p2_List] := With[{n=Max[Length/@{p1,p2}]},
	 And@@Thread[Accumulate[PadRight[p1,n]]<=Accumulate[PadRight[p2,n]]] 
];
PartitionStrictDominatesQ[p1_List,p2_List]:=And[p1!=p2,PartitionDominatesQ[p1,p2]];


PartitionN[p_List] := Sum[(i-1)*p[[i]], {i, Length@p}];


ZCoefficient::usage = "ZCoefficient[lam] returns the Z-coefficient, as p. 299, Enumerative Combinatorics II, Stanley";
ZCoefficient[{}]:=1;
ZCoefficient[lam_List] := Times@@MapIndexed[ (#1!) * First[#2]^#1 &, PartitionPartCount@lam];


PartitionAddBox::usage = "PartitionAddBox[lam] returns all partitions with one box more than lambda.";
PartitionAddBox[lam_List] := With[{lamP = DeleteCases[lam, 0]},
   Append[
    Table[
     Which[r == 1, MapAt[# + 1 &, lamP, 1],
      lamP[[r - 1]] > lamP[[r]], MapAt[# + 1 &, lamP, r],
      True, Nothing]
     , {r, Length[lamP]}]
    ,
    Append[lamP, 1](* Can create new row with one box.*)
    ]
];
UnitTest[PartitionAddBox] := SameQ[
   PartitionAddBox[{5, 4, 4, 3, 3, 1, 1, 0}],
   {
    {6, 4, 4, 3, 3, 1, 1},
    {5, 5, 4, 3, 3, 1, 1},
    {5, 4, 4, 4, 3, 1, 1},
    {5, 4, 4, 3, 3, 2, 1},
    {5, 4, 4, 3, 3, 1, 1, 1}
}];


PartitionRemoveBox::usage = "PartitionRemoveBox[lam] lists all partitions obtainable from lambda with one box removed.";
PartitionRemoveBox[{}] := {};
PartitionRemoveBox[{lam__, 0}] := PartitionRemoveBox[{lam}];
PartitionRemoveBox[lam_List] := With[{ll = Length[lam]},
   Table[
    Which[
     r == ll && lam[[ll]] == 1, Most[lam],
     r == ll, MapAt[# - 1 &, lam, ll],
     lam[[r + 1]] < lam[[r]],
     MapAt[# - 1 &, lam, r],
     True, Nothing]
    ,
{r, ll}]];
UnitTest[PartitionRemoveBox] := SameQ[
   PartitionRemoveBox[{5, 4, 4, 3, 3, 1, 1, 0}],
   {
    {4, 4, 4, 3, 3, 1, 1},
    {5, 4, 3, 3, 3, 1, 1},
    {5, 4, 4, 3, 2, 1, 1},
    {5, 4, 4, 3, 3, 1}
    }];


PartitionRemoveHorizontalStrip::usage = "PartitionRemoveHorizontalStrip[lam, k] returns all partitions obtainable from lam, by removing a horizontal strip of size k.";

PartitionRemoveHorizontalStrip[la_List, k_Integer] := PartitionRemoveHorizontalStrip[la, k] = PartitionRemoveHorizontalStrip[la, k, 1];

PartitionRemoveHorizontalStrip[la_List, k_Integer, r_Integer] := 
  With[{ll = Length@la},
   Which[
    k == 0, {DeleteCases[la, 0]},
    r > ll, {},
    True,
    (* Max possible boxes to remove from row r. 
    Also, cannot remove more than k boxes.
    *)
    With[{bb = Min[la[[r]] - If[r < ll, la[[r + 1]], 0], k]},
     Join @@ Table[
       PartitionRemoveHorizontalStrip[MapAt[# - b &, la, r], k - b, r + 1]
       , {b, 0, bb}]
]]];
		
PartitionRemoveVerticalStrip[la_List, k_Integer] := Map[ 
	ConjugatePartition, PartitionRemoveHorizontalStrip[ConjugatePartition@la, k], 1];

		
PartitionArm[mu_List,{r_Integer,c_Integer}]:= If[r> Length@mu, 0, mu[[r]] - c];
PartitionLeg[mu_List,{r_Integer,c_Integer}]:= PartitionArm[ ConjugatePartition@mu, {c,r} ];


ShapeBoxes::usage = "ShapeBoxes[{lam,mu}] returns a list of (r,c)-coords of boxes in the shape.";
ShapeBoxes[lam_List] := ShapeBoxes[{lam, {}}];
ShapeBoxes[{lam_List, mu_List}] :=
   Join @@ Table[
     Which[
      Length[mu] < r,
      Table[{r, j}, {j, lam[[r]]}],
      True,
      Table[{r, j}, {j, mu[[r]] + 1, lam[[r]]}]
      ]
     , {r, Length@lam}];


(*p. 340, Macdonald *)
MacdonaldPsi[{lam_List, mu_List}, q_, t_] := MacdonaldPsi[{lam, mu}, q,t] = Module[{s,stripBoxes,muBoxes,rowOk,colOk,bb},
	
	stripBoxes = ShapeBoxes[{lam,mu}];
	muBoxes    = ShapeBoxes[{mu,{}}];
	
	(* Product over all skew boxes in lam/mu, but only some of them count;
	namely those boxes that DO have a proper box (somewhere) to the right, 
	but no proper box somewhere below. *)
	
	rowOk[r_Integer]:=rowOk[r] = Count[stripBoxes, {r,_Integer}] > 0;
	colOk[c_Integer]:=colOk[r] = Count[stripBoxes, {_Integer, c}] == 0;
	
	(*p. 340, Macdonald *)
	bb[nu_, s_List]:=If[
		MemberQ[ShapeBoxes[nu],s],
			Divide[
				1-q^(PartitionArm[nu,s])*t^(1+PartitionLeg[nu,s]) 
				,
				1-q^(1+PartitionArm[nu,s])*t^(PartitionLeg[nu,s])
				]
			,
		1];
	
	Product[
		If[rowOk[First@s] && colOk[Last@s]
			,
			bb[mu,s]/bb[lam,s]
			,
			1
		]
	, {s, muBoxes}]
];

(* p.341, Macdonald. *)
MacdonaldPsiPrime[{lam_,mu_},q_,t_]:=MacdonaldPsi[{ConjugatePartition@lam,ConjugatePartition@mu},t,q];


    
(* This follows Macdonald's book. *)
PartitionQuotient::usage = "PartitionQuotient[lam,d] returns the partition quotient by d. Also works on skew shapes.";
PartitionQuotient[lam_List, d_Integer] := 
  Module[{m = Length@lam, xi, xis, mr},
   xi = PadRight[lam, m] + Range[m - 1, 0, -1];
   Table[
    xis = Select[xi, Mod[# - r, d] == 0 &];
    mr = Length[xis];
    MapIndexed[
     #1 - mr + #2[[1]] &,
     Sort[(xis - r)/d, Greater]
     ], {r, 0, d - 1}]
   ];

(* Skew shapes works by skewing the quotients. *)
PartitionQuotient[{lam_List, mu_List}, d_Integer] :=
  MapThread[Join, {
    List /@ PartitionQuotient[lam, d],
    List /@ PartitionQuotient[mu, d]}, 1];

PartitionCore::usage = "PartitionCore[lam,d] returns the d-core of lambda.";
PartitionCore[lam_List, d_Integer] := 
  Module[{m = Length@lam, xi, xis, mr, xiTildes},
   xi = PadRight[lam, m] + Range[m - 1, 0, -1];
   xiTildes = Sort[
     Join @@ Table[
       xis = Select[xi, Mod[# - r, d] == 0 &];
       mr = Length[xis];
       Table[d s + r, {s, 0, mr - 1}]
       , {r, 0, d - 1}], Greater];
   DeleteCases[MapIndexed[#1 - m + #2[[1]] &, xiTildes], 0]
   ];
Unittest[PartitionCore] := And[
   (* https://arxiv.org/abs/q-alg/9512031 *)
   
   PartitionCore[{8, 7, 7, 4, 1, 1, 1, 1, 1}, 3] == {2, 1, 1},
   PartitionQuotient[{8, 7, 7, 4, 1, 1, 1, 1, 1}, 
     3] == {{2, 1}, {2, 2, 0, 0}, {2, 0, 0}},
   (* Haglund. *)
   
   PartitionQuotient[{5, 5, 5, 5, 5, 5}, 
     3] == {{2, 2}, {2, 2}, {1, 1}},
   (* Pak *)
   
   PartitionQuotient[{9, 8, 7, 7, 7, 4}, 3] == {{3, 3}, {2, 1}, {3, 2}}
];
   
PartitionAbacus::usage = "PartitionAbacus[lam,d] returns the abacus representation of lambda.";
PartitionAbacus[lam_List, d_Integer] := Module[{diffs, word, n},
   diffs = Differences@Prepend[Reverse[lam], 0];
   word = Flatten[{ConstantArray[1, #], 0} & /@ diffs];
   word = PadRight[word, d Ceiling[Length[word]/d], 1];
   n = Length[word];
   Partition[MapThread[List, {Range[n], word}], d]
   ];
   
AbacusForm::usage = "AbacusForm[abacus] returns a graphical representation of the abacus.";
AbacusForm[abacus_List] := Module[{},
   TableForm[
    Map[If[#1[[2]] == 0,
       Framed@#1[[1]], #1[[1]]
       ] &
     , abacus, {2}]
    ]
   ];
    
    
    

(*
See https://arxiv.org/pdf/1508.07934.pdf
*)
KostantPartitionFunction::usage = "KostantPartitionFunction[w] lists all ways to 
express w as a non-negative combination of type A roots.";
KostantPartitionFunction[w_List] /; Tr[w] != 0 = {};
KostantPartitionFunction[w_List] := Module[
	{a, n = Length@w, roots, idx, vars, sol},
	idx = Subsets[Range[n], {2}];
	roots = Table[SparseArray[ss -> {1, -1}, {n}], {ss, idx}];
	vars = a /@ idx;
	sol = Solve[
		And @@ Join[Thread[vars >= 0],
		Thread[w == vars.roots]]
		, vars, Integers];
	If[Length[sol] == 0, {}, vars /. sol]
];






SubsetTuples::usage = "SubsetTuples[weight,k] returns a list of sets, each have size k, 
 such that the multiset has the given weight. Thus, we require that the total of weight is a multiple of k.";
SubsetTuples[weight_List, size_Integer] := SubsetTuples[weight, size] =
   Module[{rek, idx, ss, ll = Length@weight, nLam, s},
    
    idx = Select[Range@ll, weight[[#]] > 0 &];
    
    Which[
     Tr@weight == 0, {{}},
     
     Length[idx] < size, {},
     
     True,
     ss = Subsets[idx, {size}];
     Join @@ Table[
       nLam = 
        weight - SparseArray[s -> ConstantArray[1, size], {ll}];
       rek = SubsetTuples[nLam, size];
       Prepend[#, s] & /@ rek
       , {s, ss}]
     ]
];

TupleDescents::usage = "TupleDescents[s1,s2] returns the minimal number of descents between the sets if they are put in adjacent columns.";
TupleDescents[s1_List, s2_List] := 
  TupleDescents[s1, s2] = Module[{md, s1s, s2s},
    s1s = Sort[s1];
    s2s = Sort[s2];
    
    md[{}, {}] := 0;
    md[ss1_List, ss2_List] :=
     If[ss1[[1]] >= ss2[[1]],
      md[Rest@ss1, Rest@ss2]
      ,
      1 + md[Rest@ss1, Most@ss2]
      ];
    md[s1s, s2s]
];

TupleMajorIndex::usage = "TupleMajorIndex[{s1,s2,...sk}] 
returns the tuple-maj associated with this tuple.";
TupleMajorIndex[sets_List]:=Module[{i},
 Sum[ i*TupleDescents[ sets[[i]], sets[[i + 1]] ] , {i, Length[sets] - 1}]
];

TupleInversions::usage = "TupleInversions[{s1,s2,...sk}] returns 
the tuple-inv associated with the tuple.";
TupleInversions[tupl_List] := 
	Module[{val, rows, cols, ru, rd, rr, c, aa, bb, cc},
	val[r_, 0] := Infinity;
	val[r_, c_] := tupl[[r, c]];
	rows = Length[tupl];
	cols = Length[tupl[[1]]];
	Sum[
	{ru, rd} = rr;
	{aa, bb, cc} = {val[ru, c - 1], val[ru, c], val[rd, c]};
	Boole[
	(aa >= bb > cc || bb > cc > aa || cc > aa >= bb)
	]
	, {rr, Subsets[Range[rows], {2}]}, {c, cols}]
   ];



(**PERMUTATIONS**)

Is213AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; c > a > b];
Is231AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; b > a > c];
Is123AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; c > b > a];
Is132AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; b > c > a];
Is312AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; a > c > b];
Is321AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; a > b > c];

PermutationCharge[p_List] := PermutationCharge[p] = MajorIndex[Reverse@Ordering@p];

PermutationCocharge[p_List] := PermutationCocharge[p] = Binomial[Max@p,2]-MajorIndex[Reverse@Ordering@p];


(* Produces a reduced word from a permutation in one-line notation *)
(* The list {1,2,3,1} corresponds to the word "s1,s2,s3,s1" *)

ReducedWord::usage = "ReducedWord[perm] takes a permutation in one-line notation and produces a reduced word. ";
ReducedWord[pi_List] := {} /; (Sort[pi] == pi);
ReducedWord[pi_List] := Module[{d, piSort},
	d = First@DescentSet[pi];
	piSort = ReplacePart[pi, {d -> pi[[d + 1]], d + 1 -> pi[[d]]}];
	Join[{d},ReducedWord[piSort]]
];

PermutationAllCycles::usage="PermutationAllCycles[pi] returns all cycles of the permutation, including fixed-points.";
PermutationAllCycles[perm_List]:=With[{cycles = PermutationCycles[perm][[1]]},
	SortBy[Join[ cycles, List/@Complement[ Range[Length@perm], Join@@cycles] ],First]
];

PermutationType::usage = "PermutationType[pi] returns the partition of cycle lengths";
PermutationType[pi_List]:=Sort[Length/@PermutationAllCycles[pi],Greater];


PermutationCycleMap::usage = "PermutationCycleMap[p] is the map defined on p.23 Stanley's EC1, where one writes the permutation in cycle form, and removes parenthesises.";
PermutationCycleMap[p_List] := (
   Join @@ (
     RotateRight@RotateLeft[#, Position[#, Max@#][[1, 1]]] &
      /@ SortBy[PermutationAllCycles[p], Max])
   );


(* The genus of a permutation *)
PermutationGenus[sigma_List] := PermutationGenus[sigma, RotateLeft[Range@Max@sigma]];
PermutationGenus[sigma_List, alpha_List] := 
With[{n = Max[sigma], z = (Length[PermutationAllCycles[#]] &)},
	1 + (n - z[alpha] - z[sigma] - z[ Ordering[alpha][[sigma]]  ])/2
];


ReducedWord::usage = "ReducedWord[pi] returns a reduced word for pi.";
ReducedWord[{}] := {};
ReducedWord[pi_List] := With[{onePos = Ordering[pi, 1][[1]]},
	If[onePos == 1,
		1 + ReducedWord[(Rest@pi) - 1]
	,
		Prepend[
			ReducedWord[
			Join[pi[[1 ;; onePos - 2]], {1, pi[[onePos - 1]]}, 
			pi[[onePos + 1 ;;]]]]
		, onePos - 1]
	]
];

PermutationFromWord::usage = "PermutationFromWord[perm,n] returns a permutation of [n] from a word of simple transpositions. ";

PermutationFromWord[{}, n_Integer] := Range[n];
PermutationFromWord[word_List, n_Integer] := PermutationFromWord[word] = Module[{perms, prod},
	perms = Cycles[{{#, # + 1}}] & /@ Reverse[word];
	prod = PermutationProduct @@ perms;
	If[n > 0, 
		Permute[Range[n], prod]
		, 
		{}
	]
];

UnitTest[PermutationFromWord]:=And@@Table[
 PermutationFromWord[ReducedWord[pi], 5] == pi
 , {pi, Permutations@Range@5}];



PermutationCode::usage = "PermutationCode[p] returns the code of the permutation.";
PermutationCode[p_List] := Table[Count[p[[i + 1 ;;]], a_ /; a < p[[i]]], {i, Length@p}];


CarlitzMap::usage = "CarlitzMap[pi] sends inv to maj.";
CarlitzMap[p_List] := Module[{code, new = {}, n = Length@p, n2},
   code = Reverse@PermutationCode[p];
   Do[
    new = Catch[
       Do[
        n2 = Insert[new, n + 1 - i, j];
        If[MajorIndex[n2] - MajorIndex[new] == code[[i]],
         Throw[n2]]
        , {j, n + 1}]
       ];
    , {i, n}];
   new
];

FoataMap::usage = "FoataMap[pi] sends maj to inv.";
FoataMap[{}] = {};
FoataMap[{1}] = {1};
FoataMap[p_List] := FoataMap[p] = Module[{n = Length@p, foataAdd},
	foataAdd[{a_}, s_Integer] := {a, s};
	foataAdd[q_List, s_Integer] :=
	
	DeleteCases[Flatten[RotateRight /@ SplitBy[
		{Sequence @@ Which[
			Last[q] < s,
			ReplaceAll[q, i_ /; (i < s) :> Sequence[i, 0]],
			Last[q] > s,
			ReplaceAll[q, i_ /; (i > s) :> Sequence[i, 0]]
			], s}, # != 0 &]], 0];
		Fold[foataAdd, {First@p}, Rest[p]]
];

UnitTest[FoataMap]:=(FoataMap[{4,1,3,7,5,6,2}]=={7,1,4,3,5,6,2});



(* Weak Bruhat order. TODO -- Check that this is ok *)
(* This corresponds to using ADJACENT transpositions. *)
(* This should really be called STRONG Bruhat order. *)

WeakBruhatGreaterQ::usage = "WeakBruhatGreaterQ[p1,p2] returns true iff p1 is greater 
than p2 in weak Bruhat order. The identity is smaller than all other, and w0 is largest.";
WeakBruhatGreaterQ[p1_List, p2_List] := WeakBruhatGreaterQ[p1, p2] = Module[{diff},
	Message["WeakBruhatGreaterQ is experimental!"];
	diff = Ordering[p1][[p2]];
	Inversions[p1] == Inversions[diff] + Inversions[p2]
];


StrongBruhatGreaterQ::usage="StrongBruhatGreaterQ[p1,p2] returns true iff p1 
is greater-equal than p2 in strong Bruhat order. The identity is smaller than all other, and w0 is largest.";

StrongBruhatGreaterQ[p1_List,p2_List]:=StrongBruhatGreaterQ[p1,p2]=MemberQ[
StrongBruhatDownSet[p1],p2];

StrongBruhatDownSet[p_List] := ({p} /; p == Range[Max@p]);
StrongBruhatDownSet[p_List] := StrongBruhatDownSet[p] = Module[{ss, n = Max@p},
	Union[
	Append[
		Join @@ Table[
			If[Greater @@ p[[ss]]
			,
			With[{a = ss[[1]], b = ss[[2]]},
				With[{pSort = ReplacePart[p, {a -> p[[b]], b -> p[[a]]}]},
				Append[StrongBruhatDownSet[pSort], pSort]
				]
				],
			Nothing
			]
			, {ss, Subsets[Range[n], {2}]}]
		,
		p
		]]
];





(**Q-ANALOGS**)
qBinomial[n_Integer,k_Integer,q_:1]:=0 /; Not[ 0<= k <=n ];
qBinomial[n_Integer,k_Integer,q_:1]:=qBinomial[n,k,q]=FunctionExpand[QBinomial[n,k,q]];
qFactorial[n_Integer,q_:1]:=qFactorial[n,q]=FunctionExpand[QFactorial[n,q]];
qInteger[n_Integer,q_:1]:=qBinomial[n,1,q];

qMultinomial[lam_List,q_:1]:=Together[
qFactorial[Tr[lam],q]/Product[ qFactorial[p,q],{p,lam}]];

(* Equidistributed with maj, comaj and cocharge on SYT *)
qHookFormula[lam_List,q_:1]:=Together[q^PartitionN[lam]
qFactorial[Tr@lam,q]/(Times@@(qInteger[#,q]&/@(Join@@HookLengths[lam])))];

qCatalan[n_Integer,q_:1]:=1/qInteger[n+1,q]qBinomial[2n,n,q];
qNarayana[n_Integer,k_Integer,q_:1]:=Together[q^(k(k-1))/qInteger[n,q]qBinomial[n,k,q]qBinomial[n,k-1,q]];
qKreweras[lam_List,q_:1]:=Together@With[
{n=Tr@lam,k=Length@lam,lc=ConjugatePartition@lam},
	q^(k(n-1)-Sum[lc[[i]]lc[[i-1]],{i,2,Length@lc}])/qInteger[n+1,q]
	qMultinomial[{Sequence@@PartitionPartCount[lam],n-k+1},q]
];

qCarlitzCatalan::usage = "qCarlitzCatalan[n,q] is the area-generating q-Catalan number";
qCarlitzCatalan[0, q_] := 1;
qCarlitzCatalan[n_Integer, q_] := qCarlitzCatalan[n,q]=
  Expand@Sum[qCarlitzCatalan[k, q] qCarlitzCatalan[n - 1 - k, q] q^(k), {k, 0, n - 1}];
(*
Sum[q^Tr[a], {a, DyckAreaLists[4]}]
qCarlitzCatalan[4, q] // Expand
*)

(* This recursion is basically thm 3.4 in Jims book *)
qtH[n_, n_, q_, t_] := q^Binomial[n, 2];
qtH[n_, k_, q_, t_] := 
  qtH[n, k, q, t] = t^(n - k) q^Binomial[k, 2] Sum[
     qBinomial[r + k - 1, r, q] qtH[n - k, r, q, t], {r, n - k}];
qtCatalan::usage = "qtCatalan[n,q,t] is the qt-Catalan polynomial.";
qtCatalan[n_Integer, q_: 1, t_: 1] := Together[qtH[n + 1, 1, q, t]/t^n];




qAlternatingSignMatrices[n_Integer, q_] := qAlternatingSignMatrices[n, q] = 
Module[{qq,k, poly },
	poly = Together[
		Product[qFactorial[3 k + 1, qq]/qFactorial[n + k, qq], {k, 0, n - 1}]];
	
	poly /. qq -> q
];


(*Improve *)
qIntegerFactorize[expr_Integer, q_] := {expr};
qIntegerFactorize[expr_, q_] := Module[{pow, quot, rem, val},
   pow = Exponent[expr, q];
   val = Catch[
     Do[
      {quot, rem} = 
       PolynomialQuotientRemainder[expr, Expand@qInteger[k, q], q];
      If[rem === 0,
       Throw[{k, quot}];
       ];
      , {k, pow + 1, 2, -1}];
     None];
   If[val === None,
    {expr},
    Append[
     qIntegerFactorize[val[[2]], q], {val[[1]]}
     ]
    ]
];


(* TODO: Make into a substitution instead? *)
HStarPolynomial::usage="HStarPolynomial[pol,x,t] returns the h*-polynomial (in t).";
HStarPolynomial[poly_,x_,t_] := Module[{bb, c, d,y},
	bb[xx_, i_, s_] := 1/i! Product[(xx - k + s), {k, 0, i - 1}];
	
	d = Exponent[poly, x];
	If[d<0, 0,
		tbz = CoefficientList[poly - Sum[ c[i] bb[x, d, i], {i, 0, d}], x];
		vars = Variables[tbz];
		Reverse[vars] /. Solve[Thread[tbz == 0], vars][[1]]
	].(t^Range[0,d])
];




(* Special case for hook shapes. *)
SnCharacter[la_List, {k_Integer}] := Which[
	la == {k}, 1,
	la[[2]] > 1, 0,
	True, (-1)^(k - la[[1]])
];

(* Special case when all parts of mu=1, has closed form. *)
SnCharacter[la_List, mu_List] := qHookFormula[la]/;(Max[mu]==1);

(* Branching thm, James-Kerber *)
SnCharacter[la_List, {muS__, 1}] := SnCharacter[la, {muS, 1}] =  With[{mu = {muS}},
	Sum[
		If[i == Length[la] || la[[i]] > la[[i + 1]],
			SnCharacter[
				If[la[[i]]==1, 
					Drop[la,{i}]
					,
					MapAt[(# - 1)&, la, i]
				]
			, mu]
		, 0], {i, Length@la}]
];

(* https://arxiv.org/pdf/1712.08023.pdf *)
SnCharacter[la_List, {muS___, m_Integer}] := SnCharacter[la, {muS, m}] =
With[{mu = {muS}},
	1/(m - 1) (
		Sum[
			If[i == Length[la] || la[[i]] > la[[i + 1]],
				Times[
					(la[[i]] - i),
					SnCharacter[
						If[la[[i]]==1, 
							Drop[la,{i}]
							,
							MapAt[(# - 1)&, la, i]
						]
						,
						Append[mu, m - 1]
					]
				]
			, 0], {i, Length@la}]
		-
		Sum[
				If[ j == 1 || mu[[j - 1]] > mu[[j]],
					Times[
						Count[Append[mu,m-1],  mu[[j]]]
						,
						mu[[j]]
						,
							SnCharacter[la,Append[MapAt[# + 1 &, mu, j], m - 1]]
						]
				, 0]
			,{j, Length@mu}])
];


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


(* Recursion via removing horizontal strips.*)
(* If we remove the larger parts first, once we hit only 1s, we can use hook-formula. *)
(* This is not as fast as the KostkaCoefficient code. *) 
naiveKostka[la_List, la_List] := 1;
naiveKostka[la_List, mu_List] := 0 /; ! PartitionDominatesQ[mu, la];
naiveKostka[la_List, mu_List] := qHookFormula[la] /; Max[mu] == 1;
naiveKostka[la_List, mu_List] := 
  Sum[naiveKostka[nu, Rest@mu], {nu, 
    PartitionRemoveHorizontalStrip[la, First@mu]}];


(* This code is comparably very fast. *)
InverseKostkaCoefficient::usage = "InverseKostkaCoefficient[lam,mu] returns the inverse Kostka coefficient.";

InverseKostkaCoefficient[mu_List, mu_List]  :=1;
InverseKostkaCoefficient[lam_List, mu_List] := (0 /; PartitionDominatesQ[lam, mu]);
InverseKostkaCoefficient[lam_List, mu_List] := inverseKostkaHelper[
Reverse@lam, Reverse@mu];



(* The convention in https://doi.org/10.1080/03081089008817966
uses partitions with parts ordered increasingly, which makes notation easier.
*)

inverseKostkaHelper[mu_List, mu_List] := 1;
inverseKostkaHelper[lam_List, {i_Integer}] := Boole[lam==={i}];
inverseKostkaHelper[lam_List, mu_List] := inverseKostkaHelper[lam, mu] = If[
	
	Max[mu]==1, 
			(* Special case if mu=11...1 *)
			With[{n=Tr@lam}, (-1)^(n - Length[lam]) Multinomial @@ (PartitionPartCount@lam)]
			,
	Sum[
		With[{pos = Position[lam, mu[[j]] + j - 1, 1, 1]},
			If[pos == {}, 0
				, (-1)^(j-1)
				inverseKostkaHelper[
					(* Remove a part equal to p *)
					ReplacePart[lam, pos -> Nothing]
					,
						(* Subtract 1 from the first j-1 entries, and drop jth entry *)
						MapAt[ If[#==1,Nothing,#-1]&, Drop[mu, {j}], List/@Range[j-1] ]
				]
			]
		]
, {j, Length@mu}]
];



(* Uses partcount, much slower!? *)
(*
inverseKostkaHelper[mu_List, mu_List] := 1;

inverseKostkaHelper[{}, {}]:=1;
inverseKostkaHelper[lamPPC_List, {i_Integer}] := Boole[lamPPC[[i]] == 1];

inverseKostkaHelper[lamPPC_List, mu_List] := inverseKostkaHelper[lamPPC, mu] = If[
	
	Max[mu]==1, 
			(* Special case if mu=11...1 *)
			With[{n=Tr@mu}, (-1)^(n - Tr[lamPPC]) Multinomial @@ (lamPPC)]
	,
	Sum[
		With[{a =  mu[[j]] + j - 1},
			If[lamPPC[[a]] == 0, 0
				, (-1)^(j-1)
				inverseKostkaHelper[
				
					(* Remove a part equal to p *)
					MapAt[#-1&, lamPPC, a][[1;;Tr[mu]-a]]
					,
						(* Subtract 1 from the first j-1 entries, and drop jth entry *)
						MapAt[ If[#==1,Nothing,#-1]&, Drop[mu, {j}], List/@Range[j-1] ]
				]
			]
		]
, {j, Length@mu}]
];
*)



(* This is marginally slower than Remmel's alg, but can perhaps be improved? *)
inverseKostkaHuan[mu_List, mu_List] := 1;
inverseKostkaHuan[lam_List, mu_List] := (0 /; PartitionDominatesQ[lam, mu]);
inverseKostkaHuan[mu_List, {i_Integer}] := Boole[mu === {i}];
inverseKostkaHuan[{}, mu_List] := Boole[mu === {}];
inverseKostkaHuan[la_List, mu_List] := 
  inverseKostkaHuan[la, mu] = Module[
    {mu1 = First@mu, mm = Rest@mu},
    Sum[
     (-1)^(rj - mu1)
      Sum[
       inverseKostkaHuan[DeleteCases[la, rj, 1, 1], omega]
       , {omega, PartitionRemoveVerticalStrip[mm, rj - mu1]}]
     , {rj, Union@Select[la, # >= mu1 &]}]
];



(**TESTING**)
UnitTestPackage::failed="Unittest failed: `1`";
UnitTestPackage::passed="Unittests passed";
UnitTestPackage[]:= Module[{args, failed},
	args = Cases[DownValues[UnitTest], UnitTest[a_] :> a, 3];
	failed = Select[args, UnitTest[#] == False &];
	If[Length[failed]>0, 
		Message[UnitTestPackage::failed, failed]
		,
		Message[UnitTestPackage::passed, True]
	]
];

(*
UnitTestPackage[]
*)


End[(* End private *)];

EndPackage[];


(*

(* TODO - incorporate and IMPROVE! *)
QIntegerFactorize[expr_Integer, q_] := {expr};
QIntegerFactorize[expr_, q_] := Module[{pow, quot, rem, val},
   pow = Exponent[expr, q];
   val = Catch[
     Do[
      {quot, rem} = 
       PolynomialQuotientRemainder[expr, Expand@qInteger[k, q], q];
      If[rem === 0,
       Throw[{k, quot}];
       ];
      , {k, pow + 1, 2, -1}];
     None];
   If[val === None,
    {expr},
    Append[
     QIntegerFactorize[val[[2]], q], {val[[1]]}
     ]
    ]
   ];

   *)




(* This is from example 3 in https://arxiv.org/pdf/1611.04973.pdf *)
(*

PermutationReflectPosition[pi_List, i_Integer] := With[{n = Length@pi},
   Which[
    2 i == n, pi,
    i > n/2, 
    Join[ pi[[;; n - i]], RotateRight[ pi[[n - i + 1 ;; i]] ] , 
     pi[[i + 1 ;;]] ],
    True, Join[
      pi[[;; i - 1]], RotateLeft[ pi[[i ;; n - i + 1]] ] , 
     pi[[n - i + 2 ;;]]
      ]]
   ];
(* This preserves the number of inversions. *)

BasementLiftMap[pi_List] := 
  Fold[PermutationReflectPosition[#1, Ordering[#1][[#2]] ] &, pi, 
   Range[Length@pi]];
   
*)






(*

PartitionStripDecomposition::usage =   "PartitionStripDecomposition[{lam,mu},d] returns a list {quotient,rem}, \
 where quotient is a list of skew shapes, and rem is a \
single skew shape. Each shape in the quotient has d boxes, and they \
form a border-strip tableau.";
 
PartitionStripDecomposition[lam_List, d_Integer] :=  PartitionStripDecomposition[{lam, {}}, d];

PartitionStripDecomposition[{{}, {}}, d_Integer] := {{}, {{}, {}}};

PartitionStripDecomposition[{lam_List, mu_List}, d_Integer] := 
  Module[{bst, newShape, strip, quoth, rem},
   bst = BorderStrips[{lam, mu}, d];
   If[Length@bst == 0,
    {{}, {lam, mu}} (*Strips and remainder. *)
    ,
    {newShape, strip} = First@bst;(* 
    We only take the first strip. *)
    {quoth, rem} = 
     PartitionStripDecomposition[newShape, d];
    {Prepend[quoth, {lam, mu}], rem}
    ]
   ];
   
*)
