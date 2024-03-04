

(* ::Package:: *)
BeginPackage["MatroidTools`",{"CombinatoricTools`"}];

Unprotect["`*"]
ClearAll["`*"]


IsMatroidQ;
IsSubsetClosedQ;

MatroidDual;
MatroidLoops;
MatroidColoops
MatroidTuttePolynomial;

TransversalBases;
RookBases;
PathBases;
VamosBases;
UniformBases;

SetSymmetries;
SetIsomorphisms;
SetsGeneratingPolynomial;

Begin["`Private`"];

IsMatroidQ::usage = "Given a list of sets, see if they satisfy the basis exchange axioms.";

IsMatroidQ[bases_List] := Module[{AA, BB, aSet, bSet, a, b},
   And @@ Join[
     Table[
      AA = ss[[1]];
      BB = ss[[2]];
      aSet = Complement[AA, BB];
      bSet = Complement[BB, AA];
      (* Checking basis exchange axiom. *)
      And @@ Table[
        Or @@ Table[
          MemberQ[bases,
           Sort[Append[DeleteCases[AA, a], b]]
           ]
          , {b, bSet}]
        , {a, aSet}]
      , {ss, Subsets[bases, {2}]}]]
];

IsSubsetClosedQ::usage = "IsSubsetClosedQ[sets] returns true if every subset of every set in list of sets is also in the list of sets.";
IsSubsetClosedQ[sets_List] := Module[{i, set},
   Catch[
    Do[
     Do[
       If[! MemberQ[sets, Drop[set, {i}]],
         Throw@False];
       , {i, Length@set}];
     , {set, sets}];
    True
    ]
];

MatroidDual::usage = "MatroidDual[e,bases] returns the list of bases for the dual of the matroid.";
MatroidDual[ee_List, bb_List]:=(Complement[ee,#]&/@bb);


MatroidLoops[ee_List, bb_List] := Complement[ee, Union @@ bb];
MatroidColoops[ee_List, bb_List] := 
  If[Length@bb == 0, {}, Intersection @@ bb];
MatroidDeletion[bb_List, e_] := Select[bb, ! MemberQ[#, e] &];
MatroidContraction[bb_List, e_] := 
  DeleteCases[#, e, 1, 1] & /@ Select[bb, MemberQ[#, e] &];

MatroidTuttePolynomial[{ee_List, bb_List}, {x_, y_}] := Module[
   {loops, coloops},
   
   Which[
    Length[ee] == 0, 1,
    Length[bb] == 0, y^Length[ee],
    
    loops = MatroidLoops[ee, bb]; (Length@loops > 0),
    y^Length[loops]*
     MatroidTuttePolynomial[{Complement[ee, loops], bb}, {x, y}],
    
    coloops = MatroidColoops[ee, bb]; (Length@coloops > 0),
    x^Length[coloops]*
     MatroidTuttePolynomial[{Complement[ee, coloops],
       Complement[#, coloops] & /@ bb}, {x, y}],
    
    True,
    With[{e = First@ee},
     MatroidTuttePolynomial[{Rest@ee, MatroidDeletion[bb, e]}, {x, 
        y}] +
      MatroidTuttePolynomial[{Rest@ee, MatroidContraction[bb, e]}, {x,
         y}]
     ]
    ]
];





TransversalBases::usage = "TransversalBases[sets] returns all complete transversals of the list of sets.";
TransversalBases[sets_List] := Module[{bases},
   Which[
    Length[sets] == 0, {},
    Length[sets] == 1,
    List /@ First[sets]
    ,
    True,
    bases = Join @@ Table[
       With[{tb =
          TransversalBases[
           Select[DeleteCases[#, j] & /@ Rest[sets], 
            Length[#] > 0 &]]
         },
        Prepend[#, j] & /@ tb
        ]
       , {j, First@sets}
       ];
    Union[Sort /@ bases]
    ]
];



RookBases::usage="RookBases[lam,mu] returns the rook matroid bases associated with the skew shape.";
RookBases[lam_List, mu_List : {}] := Module[{
    boxes, tSets,
    rows = Length@lam, cols = lam[[1]]},
   boxes = DiagramBoxes[{lam, mu}];
   tSets = Table[
     Append[
      First /@ Select[boxes, Last[#] == k &], rows + k]
     , {k, cols}];
  TransversalBases[tSets]
];

PathBases::usage="PathBases[lam,mu] returns the path matroid bases associated with the skew shape.";
PathBases[lam_List, mu_List : {}] := 
  Module[{lp, mp, tSets, rows = Length@lam, cols},
   lp = ConjugatePartition[lam];
   cols=Length@lp;
   mp = PadRight[ConjugatePartition[mu], cols];
   tSets = Table[
     Range[rows - lp[[k]], rows - mp[[k]]] + k
     , {k, cols}];
   TransversalBases[tSets]
];

VamosBases::usage = "VamosBases[] returns the list of bases of the non-realizable Vamos matroid.";
VamosBases[]:=Complement[
Subsets[Range@8,{4}],
{
  {1, 2, 3, 4},
  {1, 2, 5, 6},
  {1, 2, 7, 8},
  {3, 4, 5, 6},
  {5, 6, 7, 8}
}];

UniformBases[r_Integer,n_Integer]:=Subsets[Range@n,{r}];





SetIsomorphisms::usage = "SetIsomorphisms[setsA,setsB] returns all isomorphisms between the two sets.";
SetIsomorphisms[setsA_List, setsB_List] := Module[{from, to, sub},
   from = Union @@ setsA;
   to = Union @@ setsB;
   Which[Length[from] != Length[to], {},
    Length[setsA] != Length[setsB], {},
    True,
    Table[
     sub = Thread[from -> to[[pi]]];
     If[Complement[Sort /@ setsB, Sort /@ (setsA /. sub)] == {},
      sub, Nothing]
     , {pi, Permutations@to}]
    ]
];


SetSymmetries::usage = "SetSymmetries[sets] returns a list of lists, 
  each list describes a permutation group which is in the automorphism group the collection of sets.";
SetSymmetries[sets_List] := Module[{ground = Union @@ sets, edges},
   edges = Table[
     With[{i = ss[[1]], j = ss[[2]]},
      If[
       Length[Complement[sets, Sort /@ (sets /. {i -> j, j -> i})]] ==
         0, UndirectedEdge[i, j], Nothing]
      ]
     , {ss, Subsets[ground, {2}]}];
   Sort[Sort /@ ConnectedComponents[Graph[edges]]]
];

SetsGeneratingPolynomial::usage = "SetsGeneratingPolynomial[sets,x] returns the multivariate set generating polynomial. Each set {a1,a2,...,ak} contributes with a monomial x[a1]x[a2]...x[ak].";
SetsGeneratingPolynomial[sets_List,x_]:=Sum[Times @@ (x /@ s), {s, sets}];

End[(* End private *)];
EndPackage[];

