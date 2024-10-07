

(* ::Package:: *)
BeginPackage["PermutationTools`",{"CombinatoricTools`","CatalanObjects`"}];

Unprotect["`*"]
ClearAll["`*"]

Si;



Is123AvoidingQ;
Is132AvoidingQ;
Is213AvoidingQ;
Is231AvoidingQ;
Is312AvoidingQ;
Is321AvoidingQ;
IsPermutationAvoidingQ;


PermutationAllCycles;
PermutationType;
PermutationCycleMap;
PermutationGenus;
ReducedWord;
PermutationFromWord;
PermutationCode;
FoataMap;
CarlitzMap;
SimionSchmidtMap;


ToSubExcedance;
FromSubExcedance;

StrongBruhatGreaterQ;
StrongBruhatDownSet;
WeakOrderGreaterQ;
PermutationCharge;
PermutationCocharge;

WeakLowerOrderIdeal;
BruhatLowerOrderIdeal; (* Same as StrongOrderDownSet, but more efficient. *)


(* Subsets of permutations *)
GrassmannPermutations;
SimsunPermutations;

WachsPermutations;
AlternatingPermutations;


(* Parity-alternating permutations *)
PairToPAP;
PAPToPair;
IsPAPQ;
GeneratePAPS;

PAPS123;
PAPS132;
PAPS213;
PAPS231;
PAPS312;
PAPS321;

TupleToRAP;
GenerateRAPS;

TypeBPermutations;


PermutationMatrixPlot;

Begin["`Private`"];

Si[pi_List, i_Integer] := ReplacePart[pi, {i -> pi[[i + 1]], i + 1 -> pi[[i]]}];
Si[i_Integer][pi_List] := ReplacePart[pi, {i -> pi[[i + 1]], i + 1 -> pi[[i]]}];




Is213AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; c > a > b];
Is231AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; b > a > c];
Is123AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; c > b > a];
Is132AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; b > c > a];
Is312AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; a > c > b];
Is321AvoidingQ[p_List] := !MatchQ[p, {___, a_, ___, b_, ___, c_, ___} /; a > b > c];


IsPermutationAvoidingQ::usage = "IsPermutationAvoidingQ[sigma,pi] returns true if pi avoids the pattern sigma.";
IsPermutationAvoidingQ[pi_?PermutationListQ, p_List]:= !MemberQ[Ordering /@ Subsets[Ordering@p, {Length@pi}], pi];

(*
Map from section 4,
 http://repository.gunadarma.ac.id/1424/1/Three%20Length%20Pattern%20Avoiding%20Permutation%20and%20the%20Catalan%20Numbers_UG.pdf
*)



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
PermutationGenus::usage = "PermutationGenus[pi] returns the genus of the permutation."
PermutationGenus[sigma_List] := PermutationGenus[sigma, RotateLeft[Range@Max@sigma]];
PermutationGenus[sigma_List, alpha_List] := 
With[{n = Max[sigma], z = (Length[PermutationAllCycles[#]] &)},
	1 + (n - z[alpha] - z[sigma] - z[ Ordering[alpha][[sigma]]  ])/2
];
(* From https://arxiv.org/pdf/2306.16237.pdf
*)
(*
PermutationGenus2[pi_List] := (1/2) With[{n = Max@pi
     },
    n + 1 - Length[PermutationAllCycles[pi]] -
     Length[PermutationAllCycles[
       Ordering[RotateRight@pi]
       ]
      ]
    ];
*)

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


(* Same as Lehmer code .*)
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



(* Prop 19 in https://core.ac.uk/download/pdf/82232421.pdf *)
(* Sends 123-avoiding to 132-avoiding *)
SyntaxInformation[SimionSchmidtMap] = {"ArgumentsPattern" -> {{_...}}};
SimionSchmidtMap::usage = "SimionSchmidtMap[perm] ,see https://core.ac.uk/download/pdf/82232421.pdf";
SimionSchmidtMap[pi:iList] := 
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



(* ToSubExcedance preserves the RTLMin set. *)

ToSubExcedance[{1}] := {1};
ToSubExcedance[pi:iList] := ToSubExcedance[pi] = Append[(ToSubExcedance[Most[pi] /. {Max[pi] -> pi[[-1]]}]), pi[[-1]]];


(* With this definition, we can use FromSubexcedance on any word 
where entries <= length.*)

FromSubExcedance[f:iList] := FromSubExcedance[f, Range[Length@f]];
FromSubExcedance[{}, pi:iList] := pi;
FromSubExcedance[f:iList, pi:iList] := With[{n = Length@f, fn = Last@f},
   (* Apply transposition *)
   ReplaceAll[FromSubExcedance[Most@f, pi], {fn -> n, n -> fn}]
];



(* Weak Bruhat order. TODO -- Check that this is ok *)
(* This corresponds to using ADJACENT/simple transpositions. *)
WeakOrderGreaterQ::usage = "WeakOrderGreaterQ[p1,p2] returns true iff p1 is greater 
than p2 in weak order. The identity is smaller than all other, and w0 is largest.";
WeakOrderGreaterQ[p1_List, p2_List] := WeakOrderGreaterQ[p1, p2] = Module[{diff},
	(*
		Message["WeakOrderGreaterQ is experimental!"];
	*)
	diff = Ordering[p1][[p2]];
	Inversions[p1] == Inversions[diff] + Inversions[p2]
];


(* This uses all transpositions *)
StrongBruhatGreaterQ::usage="StrongBruhatGreaterQ[p1,p2] returns true iff p1 
is greater-equal than p2 in strong Bruhat order. The identity is smaller than all other, and w0 is largest.";

StrongBruhatGreaterQ[p1_List,p2_List]:=StrongBruhatGreaterQ[p1,p2]=MemberQ[StrongBruhatDownSet[p1],p2];

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



WeakLowerOrderIdeal::usage = "WeakLowerOrderIdeal[pi] returns all permutations below pi in the weak order";
WeakLowerOrderIdeal[pi_List] := Module[{n = Length@pi, interval, toCheck, tau},
   interval = CreateDataStructure["HashSet"];
   toCheck = CreateDataStructure["HashSet"];
   toCheck["Insert", pi];
   While[toCheck["Length"] > 0, tau = toCheck["Pop"];
    interval["Insert", tau];
    Do[(*Do all possible swaps.*)
     If[tau[[i]] > tau[[i + 1]],
       With[{nt =
          ReplacePart[tau, {i -> tau[[i + 1]], i + 1 -> tau[[i]]}]},
        If[! interval["MemberQ", nt], toCheck["Insert", nt]]
        ]
       ];
     , {i, Range[n - 1]}]];
   Normal[interval]
];

BruhatLowerOrderIdeal[pi_List] := Module[{n = Length@pi,
    interval, toCheck, tau},
   interval = CreateDataStructure["HashSet"];
   toCheck = CreateDataStructure["HashSet"];
   toCheck["Insert", pi];

   While[toCheck["Length"] > 0,
    tau = toCheck["Pop"];
    interval["Insert", tau];
    Do[
     (* Do all possible swaps. *)

     With[{i = First@ij, j = Last@ij},
       If[tau[[i]] > tau[[j]],
        With[{nt = ReplacePart[tau, {i -> tau[[j]], j -> tau[[i]]}]},
         If[! interval["MemberQ", nt],
          toCheck["Insert", nt]
          ]]]];
     , {ij, Subsets[Range@n, {2}]}];
    ];
   Normal[interval]
];


GrassmannPermutations::usage = "GrassmannPermutations[n] returns all permutations with at most one descent. See A000325.";
(* Created by selecting subset to the left of the descent.
   A decent is always created unless it is the first choice, 
   which produces the identity permutation.
   Hence, ignore this choice, and add the identity separately instead.
*)
GrassmannPermutations[n_Integer] := GrassmannPermutations[n] =
Prepend[Join @@ Table[
		Table[Join[ss, Complement[Range@n, ss]],{ss, Rest@Subsets[Range@n, {k}]}]
,{k, n}], Range@n]


SimsunPermutations::usage = "SimsunPermutations[n] returns all Simsun permutations.";
SimsunPermutations[n_Integer] := SimsunPermutations[n] = Select[
    Permutations@n,
    And @@ Table[
       ! (SequenceCount[
           Sign[Differences[DeleteCases[#, i_Integer /; i > k]]]
           , {-1, -1}] > 0)
       , {k, 3, n}]
     &];


WachsPermutations::usage = "WachsPermutations[n] returns a list of all Wachs permutations in S_n. See arxiv:2212.04932.";
WachsPermutations[n_Integer] := 
  WachsPermutations[n] = Module[{iStar},
    iStar[i_] := Which[EvenQ@i, i - 1, i + 1 <= n, i + 1, True, n];
    (* https://arxiv.org/pdf/2212.04932.pdf *)
    Select[
     Permutations@n,
     (And @@ 
        Table[Abs[Ordering[#][[i]] - Ordering[#][[iStar@i]]] <= 1, {i,
           n - 1}]) &]
];

AlternatingPermutations::usage = "AlternatingPermutations[n] returns a list of alternating permutations (up-down permutations), see A000111.";

AlternatingPermutations[0] := {{}};
AlternatingPermutations[1] := {{1}};
AlternatingPermutations[n_Integer] := 
AlternatingPermutations[n] = Module[{altLeft, altRight, cc, ss},
	Reap[
		Do[
			altLeft = AlternatingPermutations[k];
			altRight = AlternatingPermutations[n - 1 - k];
			Do[
			cc = Complement[Range[n - 1], ss];
			
			Sow[Join[ss[[al]], {n}, cc[[ar]]]];
			
			, {ss, Subsets[Range[n - 1], {k}]}
			, {al, altLeft}
			, {ar, altRight}]
			
			, {k, 1, n - 1, 2}]
		][[2, 1]]
	];

	
	
(************************************************)


PairToPAP[{p1_List, p2_List}] := Riffle[2 p1 - 1, 2 p2];
PAPToPair[{i_Integer}] := {{i}, {}};
PAPToPair[pap_List] := {(pap[[1 ;; ;; 2]] + 1)/2, pap[[2 ;; ;; 2]]/2};

IsPAPQ[pi_List] := And @@ Join[
    OddQ /@ pi[[1 ;; ;; 2]],
    EvenQ /@ pi[[2 ;; ;; 2]]
    ];


GeneratePAPS[1] := {{1}};
GeneratePAPS[n_Integer] := GeneratePAPS[n] = Module[{a, b},
    {a, b} = {Ceiling[n/2], Floor[n/2]};
    Join @@ Outer[
      PairToPAP[{#1, #2}] &,
      Permutations[a],
      Permutations[b], 1]
];


GeneratePAPS[n_Integer, Is321AvoidingQ] := 
  GeneratePAPS[n, Is321AvoidingQ] = Module[{a, b},
    {a, b} = {Ceiling[n/2], Floor[n/2]};
    Select[
     Join @@ Table[
       Riffle[2 p1 - 1, 2 p2],
       {p1, Av321[a]},
       {p2, Av321[b]}]
     , Is321AvoidingQ]
];

(*
GeneratePAPS[n_Integer, Is123AvoidingQ] := 
  GeneratePAPS[n, Is123AvoidingQ] = Module[{a, b},
    {a, b} = {Ceiling[n/2], Floor[n/2]};
    Select[
     Join @@ Table[
       Riffle[2 p1 - 1, 2 p2],
       {p1, Av123[a]},
       {p2, Av123[b]}]
     , Is123AvoidingQ]
];
*)

GeneratePAPS[n_Integer, Is231AvoidingQ] := 
  GeneratePAPS[n, Is123AvoidingQ] = Module[{a, b},
    {a, b} = {Ceiling[n/2], Floor[n/2]};
    Select[
     Join @@ Table[
       Riffle[2 p1 - 1, 2 p2],
       {p1, Av231[a]},
       {p2, Av231[b]}]
     , Is231AvoidingQ]
];

GeneratePAPS[n_Integer, cond_: True] := 
  GeneratePAPS[n, cond] = Module[{a, b},
    {a, b} = {Ceiling[n/2], Floor[n/2]};
    Select[
     Join @@ Table[
       Riffle[2 p1 - 1, 2 p2],
       {p1, Select[Permutations[a], cond]},
       {p2, Select[Permutations[b], cond]}]
     , cond]
];


PAPS123[n_] := PAPS123[n] = GeneratePAPS[n, Is123AvoidingQ];
PAPS132[n_] := PAPS132[n] = GeneratePAPS[n, Is132AvoidingQ];
PAPS213[n_] := PAPS213[n] = GeneratePAPS[n, Is213AvoidingQ];
PAPS231[n_] := PAPS231[n] = GeneratePAPS[n, Is231AvoidingQ];
PAPS312[n_] := PAPS312[n] = GeneratePAPS[n, Is312AvoidingQ];


(*
PAPS321[n_] := PAPS321[n] = GeneratePAPS[n, Is321AvoidingQ];
*)


PAPS321[n_] := PAPS321[n] = Select[
Join @@ Table[
      PairToPAP[{a, b}]
      , {a, Av321[Ceiling[n/2]]}, {b, Av321[Floor[n/2]]}]
, Is321AvoidingQ];

PAPS123[n_] := PAPS123[n] = Select[
Join @@ Table[
      PairToPAP[{a, b}]
      , {a, Av123[Ceiling[n/2]]}, {b, Av123[Floor[n/2]]}]
, Is123AvoidingQ];



(* Paps, avoiding patterns *)

GeneratePAPS[n_Integer, f1_, f2_, fTot_] := 
  GeneratePAPS[n, f1, f2, fTot] = Module[{a, b},
    {a, b} = {Ceiling[n/2], Floor[n/2]};
    Select[
     Join @@ Table[
       Riffle[2 p1 - 1, 2 p2],
       {p1, Select[Permutations[Range@a], f1[#] && fTot[#] &]},
       {p2, Select[Permutations[Range@b], f2[#] && fTot[#] &]}]
     , fTot]
    ];
	
	


TupleToRAP[lists_List, r_: 1] := Module[
   {k = Length[lists],
    p = Length[lists[[1]]], z},
   Join @@ (
      Transpose[
       Table[
        PadRight[
         k (lists[[i]] - 1) + (1 + Mod[i + r - 2, k])
         , p, z],
        {i, k}]]) /. {z -> Nothing}
   ];

Clear[GenerateRAPS];
GenerateRAPS[1, k_Integer] := {{1}};
GenerateRAPS[n_Integer, k_Integer, r_: 1] := 
  GenerateRAPS[n, k, r] = Module[{lens, c, z},
    
    If[Mod[n, k] != 0 && Mod[r, k] != 1, {},
     
     If[Mod[n, k] == 0,
      c = 0,
      c = (n - k Floor[n/k])/(Ceiling[n/k] - Floor[n/k])
      ];
     lens = 
      Join[ConstantArray[Ceiling[n/k], c], 
       ConstantArray[Floor[n/k], k - c]];
     Flatten@
       Outer[z[TupleToRAP[{##}, r]] &, 
        Sequence @@ Table[Permutations[lens[[i]]], {i, k}], 1] /. 
      z[m_] :> m
     ]
    ];

(* Old special case below *)
(*
	
TupleToRAP[lists_List] := Module[
   {k = Length[lists], p = Length[lists[[1]]], z},
   Join @@ (Transpose[Table[
        PadRight[k (lists[[i]] - 1) + i, p, z]
        , {i, k}]]) /. {z -> Nothing}
];

Clear[GenerateRAPS];
GenerateRAPS[1, k_Integer] := {{1}};
GenerateRAPS[n_Integer, k_Integer] := 
  GenerateRAPS[n, k] = Module[{lens, c, z},
    If[Mod[n, k] == 0,
     c = 0,
     c = (n - k Floor[n/k])/(Ceiling[n/k] - Floor[n/k])
     ];
    lens = Join[
      ConstantArray[Ceiling[n/k], c],
      ConstantArray[Floor[n/k], k - c]
      ];
    Flatten@Outer[
       z[TupleToRAP[{##}]] &,
       Sequence @@ Table[Permutations[lens[[i]]], {i, k}],
       1] /. z[m_] :> m
    ];
	
*)

TypeBPermutations::usage = "TypeBPermutations[n] returns all permutations of type B.";
TypeBPermutations[n_Integer] := TypeBPermutations[n] =
  Join @@ Outer[#1*#2 &, Permutations@n, Tuples[{-1, 1}, n], 1];

  

PermutationMatrixPlot::usage = "PermutationMatrixPlot[pi] returns a graphical representation of the permutation.";
PermutationMatrixPlot[p_List] := With[{n = Max@p},
ArrayPlot[
    Normal@SparseArray[Transpose[{Range[n], p}] -> 1, {n, n}]
    , Mesh -> True, Frame -> False
    ,PixelConstrained -> 6]
];

  
End[(* End private *)];
EndPackage[];

