

(* ::Package:: *)
BeginPackage["PermutationTools`",{"CombinatoricTools`","CatalanObjects`"}];

Unprotect["`*"]
ClearAll["`*"]



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


Begin["`Private`"];


GrassmannPermutations::usage = "GrassmannPermutations[n] returns all permutations with at most one descent. See A000325.";
GrassmannPermutations[n_Integer] := GrassmannPermutations[n] =
Union@Table[
	Join[ss, Complement[Range@n, ss]]
, {ss, Subsets[Range@n]}];


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



End[(* End private *)];
EndPackage[];

