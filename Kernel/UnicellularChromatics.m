(* ::Package:: *)


(* MathKernel -script file.m *)


BeginPackage["UnicellularChromatics`",{"SymmetricFunctions`","CombinatoricTools`","CatalanObjects`","GraphTools`"}];

Unprotect["`*"]
ClearAll["`*"]


AreaLists;
GraphAreaLists;
Circular;
Width;
UnitIntervalEdges;
GraphColoringAscents;
UnicellularLLTSymmetric;
UnicellularLLTSymmetricSchur;
ChromaticSymmetric;


(*****************************)

(*
GraphOrientations;
GraphAcyclicOrientations;
OrientationSinks;
*)


(*****************************)

SchroederWordToArea;
SchroederWordStrictEdges;
SchroederLLTSymmetric;
SchroederPlot;
SchroederOrientations;
SchroederAcyclicOrientations
SchroederColorings;
SchroederColoringAscents;

(*
RecursionVertices; (* This is temporary - should not be in the library after paper is done. *)
*)

BounceList;
BounceEndpoint;
EdgesHRVRule;


GraphColoringAscents;
GraphColoringMonochromaticEdges;

(* Compatibility and graph/area utilities ported from ChromaticFunctions. *)
AreaConjugate;
ValleyEdges;
DiagramRookPlacements;
OrientationPlot;
PArrayPlot;
UnitIntervalPlot;
PathShapes;
BounceLengths;
AttackingPoset;
IncomparabilityGraph;
ColorOrientation;
AcyclicAscents;
AttackingVertices;
ChromaticSymmetricColorings;
PArray;
GasharovPTableauQ;
GasharovOpPTableauQ;
GasharovPTableaux;
GasharovOpPTableaux;
AlternatingChains;
ChromaticSymmetricPolynomial;
SingleCelledLLTPolynomial;
UnitIntervalData;
GraphAttackingEdges;
GraphChromaticSymmetricColorings;
GraphColoringInversions;
GraphColoringOrientation;
GraphOrientationIntersection;
GraphOrientationAscents;
GraphOrientationInversions;
AreaRowPermutation;
DinvFromAreaSeq;
MajFromAreaSeq;
AyclicAreaListQ;
NoAscendingCycleOrientations;
GraphOrientationSinks;
GraphOrientationSources;
GraphOrientationHalfSinks;
GraphOrientationHalfSources;
LLTOrientationVertexPartition;
LLTOrientationLowestReachableVertex;
LLTOrientationShape;
LLTOrientationForest;
InnerCorners;
OuterCorners;
AreaToTopBounceShape;
AreaToDyckWord;
AreaDinv;
GraphChromaticSymmetricPolynomial;
HomogeneousGraphChromaticSymmetricPolynomial;
GraphChromaticLLTPolynomial;
HomogeneousGraphLLTPolynomial;
GraphChromaticLLTPolynomialAttacking;
StripSizesToEdges;
VerticalStripLLTColorings;
VerticalStripLLTPolynomial;
PartitionRookPlacements;
SouthWestDiagram;
SouthEastDiagram;
AthanasiadisUnimodalSets;
AthanasiadisS;

Begin["`Private`"];


AreaLists::usage = "AreaLists[size, All->False, Circular->True, Width->-1] returns all area lists of size n.";
Circular::usage  = "Option for AreaLists";
Width::usage = "Option for AreaLists";


Options[AreaLists] = {All -> False, Circular -> True, Width->-1};
AreaLists[n_Integer, opts:OptionsPattern[]] := AreaLists[n, opts] = 
	Module[{rec, isOkQ, gData = {}, isMinimal, w = -1 },
		
		(* Max-entry *)
		w = OptionValue[Width];
		If[w == -1, w=n];
		
		rec[gList_List, n] := AppendTo[gData, gList];
		rec[gList_List, i_Integer] :=
		Do[
			rec[ Append[gList, k], i + 1]
		, {k, Max[Last[gList] - 1, 0], w - 1}];
	
	(* Init recursion *)
	Do[rec[{k}, 1], {k, 0, w - 1}];
	
	(* The number of entries in dGata seems to be given by A274969 *)
	
	(* It has to be compatible when wrapped around as well! *)
	(* This extra condition reduces it to A194460 *)
	
	isOkQ[lst_List] := (lst[[1]] >= lst[[-1]] - 1) && (OptionValue[Circular]===True || Last[lst]==0);
	
	(* The cardinalities is not in OEIS. *)
	isMinimal[lst_List] := (lst === Last@Sort[Table[RotateLeft[lst, k], {k, 0, n - 1}]]);
	
	Reverse/@If[ Not@OptionValue[All],
		Select[gData, isOkQ[#] && isMinimal[#] &]
	,
		Select[gData, isOkQ ]
	]
];

GraphAreaLists::usage = "GraphAreaLists[n, opts] returns area lists of unit interval graphs on n vertices, starting with 0 as in CatalanObjects (UnitIntervalEdges accepts them). Options: Circular -> True (default) also includes circular (cylindric) area lists; Width -> w bounds the entries by w - 1 (default n); All -> False (default) keeps one representative per rotation class, All -> True returns all.";
Options[GraphAreaLists] = {All -> False, Circular -> True, Width -> -1};
GraphAreaLists[n_Integer, opts : OptionsPattern[]] := GraphAreaLists[n, opts] =
	Module[{rec, isOkQ, gData = {}, isMinimal, w = -1},
		w = OptionValue[Width];
		If[w == -1, w = n];
		rec[gList_List, n] := AppendTo[gData, gList];
		rec[gList_List, i_Integer] :=
		Do[rec[Append[gList, k], i + 1],
			{k, Max[Last[gList] - 1, 0], w - 1}];
		Do[rec[{k}, 1], {k, 0, w - 1}];
		isOkQ[lst_List] :=
			(lst[[1]] >= lst[[-1]] - 1) &&
			(OptionValue[Circular] === True || Last[lst] == 0);
		isMinimal[lst_List] :=
			(lst === Last@Sort[Table[RotateLeft[lst, k], {k, 0, n - 1}]]);
		(* The construction uses lists ending with 0; reverse them to the shared
		   0-first convention (CONVENTIONS.md). *)
		Reverse /@ If[Not@OptionValue[All],
			Select[gData, isOkQ[#] && isMinimal[#] &],
			Select[gData, isOkQ]
		]
	];



UnitIntervalEdges::usage = "UnitIntervalEdges[area] returns the edges of the unit interval graph.";
UnitIntervalEdges[area_List] :=With[
	{n = Length@area},
	Join @@ Table[
		{Mod[k - i - 1, n] + 1, k}
		,{k, n}, {i, area[[k]]}]
];


GraphColoringAscents::usage="GraphColoringAscents[edges, coloring] returns the number of edges whose color increases along the ordered edge.";
GraphColoringAscents[edges_List, col_List] := Sum[Boole[col[[e[[1]]]] < col[[e[[2]]]] ], {e, edges}];

GraphColoringMonochromaticEdges::usage="GraphColoringMonochromaticEdges[edges, coloring] returns the number of edges whose endpoints have equal colors.";
GraphColoringMonochromaticEdges[edges_List, col_List] := Sum[Boole[col[[e[[1]]]] == col[[e[[2]]]] ], {e, edges}];



Options[UnicellularLLTSymmetric] = {StrictEdges -> {}};
UnicellularLLTSymmetric[attacking : {{_Integer, _Integer} ...}, n_Integer, q_: 1, opts:OptionsPattern[]] := 
UnicellularLLTSymmetric[attacking, n, q, opts] = Module[{c,lam,perms,strict},
	
	(* We have a list of strict edges *)
	strict = OptionValue[StrictEdges];
	perms = Select[
		Permutations@Range@n
		,
		And@@Table[
			#[[e[[1]]]] < #[[e[[2]]]],{e,strict}] &
		];
	
	(* Here we use the F-expansion formula *)
	Sum[
		With[{lam = DescentSetToComposition[DescentSet@Reverse@Ordering@c,n]},
			q^GraphColoringAscents[attacking, c] SchurSymmetric[lam]
		]
	,{c, perms}]
];


UnicellularLLTSymmetric::usage = "UnicellularLLTSymmetric[area,q] returns the 
unicellular LLT polynomial associated with given area sequence.";
UnicellularLLTSymmetric[area:{_Integer ..}, q_: 1, opts:OptionsPattern[]] :=
UnicellularLLTSymmetric[UnitIntervalEdges@area, Length@area,q,opts];


UnicellularLLTSymmetricSchur::usage="UnicellularLLTSymmetricSchur[area, q, ss] returns the LLT polynomial for an area sequence in the basis supplied by the function ss; q defaults to 1.";
UnicellularLLTSymmetricSchur[area_List, q_: 1, ss_] := Module[{c,colorings,lam,n,attacking},
	attacking = UnitIntervalEdges@area;
	n = Length@area;
	Sum[
		With[{lam = DescentSetToComposition[DescentSet@Reverse@Ordering@c,n]},
			q^GraphColoringAscents[attacking, c] ss[lam]
		]
	,{c, Permutations@Range@n}]/. ss[c_] :> ( If[#2!=0, ss[#1] #2, 0]& @@ (CompositionSlinky@c) )
];



ChromaticSymmetric::usage = "ChromaticSymmetric[area,q] returns the chromatic symmetric polynomial associated with given area sequence. One can also pass a graph object as argument.";
ChromaticSymmetric[area:{_Integer ..}, q_: 1, opts:OptionsPattern[]] :=
ChromaticSymmetric[UnitIntervalEdges@area, Length@area,q,opts];

(* Convention *)
ChromaticSymmetric[{},q_:1] := 1;

ChromaticSymmetric[attacking_List, q_:1, opts:OptionsPattern[]] := ChromaticSymmetric[attacking, Max@attacking,q,opts];

ChromaticSymmetric[attacking_List, n_Integer, q_, opts:OptionsPattern[]] := 
	ChromaticSymmetric[attacking, n, q, opts] = Module[{c,colorings,lam},
	(* TODO: This is inefficient! Use F-expansion instead! *)
	Sum[
		(* All colorings with lam as weight *)
		colorings = Permutations@(Join @@ MapIndexed[ ConstantArray[#2[[1]], #1 ] &, lam]);
		colorings = Select[colorings,
			GraphColoringMonochromaticEdges[attacking,#]==0&];
		MonomialSymmetric[lam]*
		If[q === 1,
			Length[colorings]
			,
			Sum[q^GraphColoringAscents[attacking, c], {c, colorings}]
		]
		
	,{lam, IntegerPartitions[n] }]
];


ChromaticSymmetric[gg_Graph, opts : OptionsPattern[]] := 
  ChromaticSymmetric[gg, opts] = Module[{
     vv = VertexList@gg,
     ee = EdgeList@gg,
     properQ,
     colorings},
    properQ[col_List] := With[
      {sub = Association[Thread[vv -> col]]},
      And @@ Table[Lookup[sub, First[e]] != Lookup[sub, Last[e]], {e, ee}]
      ];
    Sum[
     (*All colorings with lam as weight, and proper *)
     colorings = 
      Permutations@(Join @@ 
         MapIndexed[ConstantArray[#2[[1]], #1] &, lam]);
     colorings = Select[colorings, properQ[#] &];
     Length[colorings]*MonomialSymmetric[lam]
     , {lam, IntegerPartitions[Length@vv]}]
];




(***************************************************)

(* Function which takes a string, --000++,
and convert to (area, strictEdges) pair.
The diagonal steps do not contribute to area.
*)

SchroederWordToArea::usage="SchroederWordToArea[word] converts a Schroeder word, given as a string or list using -/n, +/e, and 0/d, to {area, strictEdges}.";
SchroederWordToArea[""] := {{}, {}};
SchroederWordToArea[word_String] := 
  SchroederWordToArea[Characters[word]];
SchroederWordToArea[word_List] := 
  SchroederWordToArea[word] = Module[{aRest, strictRest, numSeq,
     coArea, area, strictRows, strictEdges},
    
    (* Minus is up. *)
    numSeq = Replace[word, {
       ("-" | "n") -> {1, 0},
       ("+" | "e") -> {0, 1},
       ("0" | "d") :> Sequence @@ {{0, 1}, {1, 0}}}, 1];
    
    strictRows = Join @@ Position[
       Replace[
        word, {("-" | "n") -> "0", ("+" | "e") -> 
          Nothing, ("0" | "d") :> 1}, 1]
       , 1];
    
    coArea = Last /@ (Last /@ GatherBy[
         Accumulate[Prepend[numSeq, {0, 0}]]
         , First]);
    (* This is now the area. *)
    
    area = Most[Range[Length[coArea]] - 1 - coArea];
    
    strictEdges = Table[
      {r - area[[r]] - 1, r}
      , {r, strictRows}];
    
    {area, strictEdges}
];

SchroederWordStrictEdges::usage="SchroederWordStrictEdges[word] returns the strict edge list extracted from a Schroeder word.";
SchroederWordStrictEdges[w_]:=SchroederWordToArea[w][[2]];

SchroederLLTSymmetric::usage="SchroederLLTSymmetric[word, q] returns the LLT symmetric polynomial associated with a Schroeder word and parameter q.";
SchroederLLTSymmetric[word_, q_] := 
  SchroederLLTSymmetric[word, q] = Module[{area, strict},
    {area, strict} = SchroederWordToArea[word];
    UnicellularLLTSymmetric[area, q, StrictEdges -> strict]
    ];

SchroederPlot::usage="SchroederPlot[word] returns a plot of the area and strict edges encoded by a Schroeder word.";
SchroederPlot[word_] := Module[{aa, strict},
   {aa, strict} = SchroederWordToArea[word];
   AreaListPlot[aa, Circular -> False, Labels -> Join[
      # -> "\[Rule]" & /@ strict,
      {#, #} -> # & /@ Range[Length@aa]
      ]
    ]];

SchroederOrientations::usage="SchroederOrientations[word] returns all orientations of the unit interval graph encoded by a Schroeder word, respecting its strict edges.";
SchroederOrientations[word_] := 
  With[{data = SchroederWordToArea[word]},
   GraphOrientations[
	UnitIntervalEdges@data[[1]], StrictEdges -> data[[2]]]
];

SchroederAcyclicOrientations::usage="SchroederAcyclicOrientations[word] returns all acyclic orientations of the unit interval graph encoded by a Schroeder word, respecting its strict edges.";
SchroederAcyclicOrientations[word_] := 
  With[{data = SchroederWordToArea[word]},
   GraphAcyclicOrientations[
	UnitIntervalEdges@data[[1]], StrictEdges -> data[[2]]]
];


Options[SchroederColorings] = {Partition->True};
SchroederColorings::usage="SchroederColorings[word, ncols, Partition -> True] returns colorings of the graph encoded by a Schroeder word satisfying its strict edges. ncols defaults to 0; with Partition -> False, colors range from 1 to ncols.";
SchroederColorings[word_, ncols_: 0,opts:OptionsPattern[SchroederColorings]] := Module[
	{area, strict, n, cols,colorings},
   {area, strict} = SchroederWordToArea[word];
   n = Length@area;
   cols = If[ncols == 0, n, ncols];

   If[OptionValue[Partition]===True,
		Join@@Table[
		colorings = Permutations@(Join @@ MapIndexed[ ConstantArray[#2[[1]], #1 ] &, lam]);
		Select[colorings
		,
		And @@ Table[
		#[[ s[[1]] ]] < #[[ s[[2]] ]]
		, {s, strict}]
		&]
		,{lam, IntegerPartitions[n] }]
	
	,
		
		Select[Tuples[Range@cols, n]
	    ,
		And @@ Table[
			#[[ s[[1]] ]] < #[[ s[[2]] ]]
		, {s, strict}]
		&]
	]
];


SchroederPermutationColorings[word_, ncols_: 0] := 
  Module[{area, strict, n, cols},
   {area, strict} = SchroederWordToArea[word];
   n = Length@area;
   cols = If[ncols == 0, n, ncols];

   Select[Permutations@Range@n
    ,
    And @@ Table[
       #[[ s[[1]] ]] < #[[ s[[2]] ]]
       , {s, strict}]
     &]
   ];

SchroederColoringAscents::usage="SchroederColoringAscents[word, coloring] returns the number of ascents of a coloring on the graph encoded by a Schroeder word.";
SchroederColoringAscents[word_, col_] := 
  With[{data = SchroederWordToArea[word]},
   GraphColoringAscents[UnitIntervalEdges[data[[1]]], col]
];




(***************************************************)


(* Returns y', where we have started a bounce path from row indexed by y  *)
BounceEndpoint::usage="BounceEndpoint[word, y] returns the endpoint row reached by the bounce path starting at row y for a Schroeder word.";
BounceEndpoint[word_, y_Integer] := Module[{aa, strict, strictRows, x, xlist},
   {aa, strict} = SchroederWordToArea[word];
   strictRows = Last /@ strict;
   x = y - 1;
   Which[
    y == 1, y,
    aa[[y]] != aa[[x]], y,
    ! MemberQ[strictRows, y] || ! MemberQ[strictRows, x], y,
    True,
    BounceEndpoint[word, y - aa[[y]] - 1]
    ]
];

(* The x in the (x,y)-pairs of the bounce path. *)
BounceList::usage="BounceList[word, y] returns the list of x-coordinates visited by the bounce path starting at row y for a Schroeder word.";
BounceList[word_, y_Integer] := Module[{aa, strict, strictRows, x},
   {aa, strict} = SchroederWordToArea[word];
   strictRows = Last /@ strict;
   x = y - 1;
   Which[
    y == 1, {x},
 
    aa[[y]] != aa[[x]], {x},
 
    !MemberQ[strictRows, y] || ! MemberQ[strictRows, x], {x},
 
    True,
    Prepend[ BounceList[word, y - aa[[y]] - 1], x ]
    ]
];

(*
RecursionVertices[word_, z_Integer, type_: 2] := Module[
	{aa, strict, strictQ, n, x, y, w, validQ, bounceList},
	
	{aa, strict} = SchroederWordToArea[word];
	n = Length@aa;
	
	(* Return true if row is a diagonal step *)
	
	strictQ[row_] := MemberQ[Last /@ strict, row];

	(* Do the bounce *)
	bounceList = BounceList[word, z - aa[[z]]];
	x = Last[bounceList];
	y = x + 1;
	
	Which[
		type == 2, w = y - aa[[y]];
		type == 3, w = y - aa[[y]] - 1;
		type == 4, w = y - aa[[y]] - 1;
		];

	validQ = And[
		(* Row z must be strict in all 3 cases.*)
		strictQ[z],
		(* Make sure there is at least one east-step going from row z to z+1 *)
		(z == n || 
		aa[[z + 1]] < aa[[z]] || 
		And[aa[[z + 1]] == aa[[z]], ! strictQ[z] ])
		,
		(y > x > w >= 1)
		,
		Or[
		(type == 2 && 
			aa[[y]] == 1 + aa[[x]] && ! strictQ[x] && ! strictQ[y]) 
		,
		(type == 3 && aa[[y]] == aa[[x]] && ! strictQ[x] && 
			strictQ[y]) 
		,
		(type == 4 && aa[[y]] == 1 + aa[[x]] && 
			strictQ[x] && ! strictQ[y])
		]
		];
	
	(*
	{validQ, {w, x, y, z}}
	*)
	
	(* (x,y) is now first diagonal touch pair *)
	(*
	{validQ, {w, bounceList[[1]], bounceList[[1]]+1, z}}
	*)
	
	{validQ, {z, Sequence@@bounceList, w}}
	
];
*)

EdgesHRVRule::usage="EdgesHRVRule[edges] returns replacement rules assigning each vertex the list consisting of that vertex and all vertices reachable from it by ascending edge paths.";
EdgesHRVRule[edges_List] := EdgesHRVRule[edges] = Module[
    {lrvSteps, stepLength, verts, ascEdges},
    verts = Union[Join @@ edges];
    ascEdges = Select[edges, Less @@ # &];
    
    lrvSteps[k_Integer] := lrvSteps[k] = Module[{out, nxt},
       out = Last /@ Select[ascEdges, First[#] == k &];
       (*Recursive definition.*)
       
       If[Length[out] == 0, {}, 
        Union@Join[out, Join @@ (lrvSteps /@ out)]]
       ];
    Join[# -> Prepend[lrvSteps[#], #] & /@ verts, {i_Integer :> {i}}]
];


(*
   The following routines are the non-plotting part of the old
   ChromaticFunctions interface.  Area sequences in this package are read in
   the CatalanObjects convention: the first entry is zero.  The old package
   used the reverse convention, so the few shape routines below which are
   defined recursively in the old orientation use an explicit reversal.
*)

PathShapes::usage = "PathShapes[n] gives all partitions that fit inside the size n triangle.";
PathShapes[nn_Integer] := With[{maxP = Reverse[Range[nn] - 1]},
	Join @@ Table[
		Select[IntegerPartitions[k, {nn}, Range[0, nn - 1]],
			And @@ Thread[# <= maxP] &],
		{k, 0, Tr@maxP}]
];

BounceLengths::usage = "BounceLengths[lambda] gives the lengths of bounce triangles, from top to bottom.";
BounceLengths[{}] := {};
BounceLengths[lam_List] := With[{step = Count[lam, 0]},
	With[{nlam = Max[0, #] & /@ (lam[[;; -step - 1]] - step)},
		Append[BounceLengths[nlam], step]
	]
];

AttackingPoset::usage = "AttackingPoset[lambda] returns the attacking poset edges of a triangular shape.";
AttackingPoset[lambda_List] := AttackingPoset[lambda] = Module[{n = Length@lambda},
	If[! And @@ Table[lambda[[i]] <= n - i, {i, n}],
		Print["AttackingPoset:Shape does not fit in triangle"]; Abort[]];
	Join @@ Table[{r, n - c + 1}, {r, n}, {c, lambda[[r]]}]
];

IncomparabilityGraph::usage = "IncomparabilityGraph[lambda] returns the incomparability graph edges of a triangular shape.";
IncomparabilityGraph[lambda_List] := IncomparabilityGraph[lambda] =
	With[{n = Length@lambda},
		Complement[AttackingPoset[Range[n - 1, 0, -1]], AttackingPoset[lambda]]
	];

ColorOrientation::usage = "ColorOrientation[lambda, coloring] gives the orientation induced by a coloring.";
ColorOrientation[lam_List, col_List] := ColorOrientation[lam, col] =
	Table[If[col[[e[[1]]]] < col[[e[[2]]]], e, Reverse@e],
		{e, IncomparabilityGraph[lam]}];

AcyclicAscents::usage = "AcyclicAscents[lambda, coloring] or AcyclicAscents[orientation] counts ascending edges.";
AcyclicAscents[lam_List, col_List] := AcyclicAscents[ColorOrientation[lam, col]];
AcyclicAscents[acyclic_List] := Count[acyclic, {a_Integer, b_Integer} /; a < b];

AttackingVertices::usage = "AttackingVertices[lambda, coloring] returns monochromatic incomparability edges.";
AttackingVertices[lam_List, col_List] :=
	AttackingVertices[lam, col] =
		Select[IncomparabilityGraph[lam], col[[#[[1]]]] == col[[#[[2]]]] &];

ChromaticSymmetricColorings::usage = "ChromaticSymmetricColorings[lambda, allowAttacking, maxColor] returns colorings of a shape.";
ChromaticSymmetricColorings[lam_List, allowAttacking : (_?BooleanQ) : False] :=
	ChromaticSymmetricColorings[lam, allowAttacking, Length@lam];
ChromaticSymmetricColorings[lam_List, maxCol_Integer] :=
	ChromaticSymmetricColorings[lam, False, maxCol];
ChromaticSymmetricColorings[lam_List, allowAttacking : (_?BooleanQ) : False,
	maxCol_Integer] := ChromaticSymmetricColorings[lam, allowAttacking, maxCol] =
	Select[Tuples[Range@maxCol, Length@lam],
		allowAttacking || Length[AttackingVertices[lam, #]] == 0 &];

PArray::usage = "PArray[coloring] returns the P-array associated with a coloring.";
PArray[col_List] := With[{nn = Length@col},
	Table[Sort@Select[Range[nn], col[[#]] == i &], {i, Max[col, nn]}]
];

GasharovPTableauQ::usage = "GasharovPTableauQ[lambda, coloring] tests the P-tableau condition.";
GasharovPTableauQ[lam_List, col_List] := Module[{arr, shape, poset = AttackingPoset[lam]},
	arr = PArray[col]; shape = Length /@ arr;
	Catch[
		If[Sort[shape, Greater] =!= shape, Throw[False]];
		Do[If[MemberQ[poset, {arr[[r + 1, c]], arr[[r, c]]}], Throw[False]],
			{r, Length@arr - 1}, {c, shape[[r + 1]]}];
		True
	]
];

GasharovOpPTableauQ::usage = "GasharovOpPTableauQ[lambda, coloring] tests the opposite P-tableau condition.";
GasharovOpPTableauQ[lam_List, col_List] := Module[{arr, shape, poset = AttackingPoset[lam]},
	arr = Reverse /@ PArray[col]; shape = Length /@ arr;
	Catch[
		If[Sort[shape, Greater] =!= shape, Throw[False]];
		Do[If[MemberQ[poset, {arr[[r, c]], arr[[r + 1, c]]}], Throw[False]],
			{r, Length@arr - 1}, {c, shape[[r + 1]]}];
		True
	]
];

GasharovPTableaux::usage = "GasharovPTableaux[lambda] returns all P-tableau colorings.";
GasharovPTableaux[lam_List] := GasharovPTableaux[lam] =
	Select[ChromaticSymmetricColorings[lam], GasharovPTableauQ[lam, #] &];
GasharovOpPTableaux::usage = "GasharovOpPTableaux[lambda] returns all opposite P-tableau colorings.";
GasharovOpPTableaux[lam_List] := GasharovOpPTableaux[lam] =
	Select[ChromaticSymmetricColorings[lam], GasharovOpPTableauQ[lam, #] &];

AlternatingChains::usage = "AlternatingChains[lambda, coloring, i] returns the alternating chains using colors i and i+1.";
AlternatingChains[lam_List, col_List, i_Integer] := AlternatingChains[lam, col, i] =
	With[{ap = IncomparabilityGraph[lam], colPos = Flatten@Position[col, (i | i + 1)]},
		Split[colPos, MemberQ[ap, {#1, #2}, {1}] &]
	];

ChromaticSymmetricPolynomial::usage = "ChromaticSymmetricPolynomial[lambda, x, q, n] returns the chromatic symmetric polynomial in variables x.";
ChromaticSymmetricPolynomial[lam_List, x_, q_: 1] :=
	ChromaticSymmetricPolynomial[lam, x, q, Length@lam];
ChromaticSymmetricPolynomial[lam_List, x_, q_: 1, n_Integer] :=
	ChromaticSymmetricPolynomial[lam, x, q, n] =
		Sum[q^AcyclicAscents[lam, c] (Times @@ (x /@ c)),
			{c, ChromaticSymmetricColorings[lam, False, n]}];

SingleCelledLLTPolynomial::usage = "SingleCelledLLTPolynomial[lambda, x, q, n] returns the single-celled LLT polynomial.";
SingleCelledLLTPolynomial[lam_List, x_, q_: 1] :=
	SingleCelledLLTPolynomial[lam, x, q, Length@lam];
SingleCelledLLTPolynomial[lam_List, x_, q_: 1, n_Integer] :=
	SingleCelledLLTPolynomial[lam, x, q, n] =
		Sum[q^AcyclicAscents[lam, c] (Times @@ (x /@ c)),
			{c, ChromaticSymmetricColorings[lam, True, n]}];

(* The old routine takes an area list ending in zero. *)
UnitIntervalData::usage = "UnitIntervalData[area, coloring] returns the unit-interval representation of a coloring.";
UnitIntervalData[area_List, col_List] := Module[{lam = Reverse@area, ap, bl, n = Length@area,
		bouncePieces, pairs, comparer, changed = True, seen = {}},
	bl = BounceLengths[lam];
	ap = AttackingPoset[lam];
	bouncePieces = Range[n][[#[[1]] + 1 ;; #[[2]]]] & /@
		Partition[Prepend[Accumulate[bl], 0], 2, 1];
	pairs = Join @@ Table[{i, b}, {i, Length@bouncePieces},
		{b, bouncePieces[[i]]}];
	comparer[{b1_Integer, v1_Integer}, {b2_Integer, v2_Integer}] := Which[
		b1 == b2, v2 < v1,
		b1 + 1 == b2 && ! MemberQ[ap, {v1, v2}], True,
		b1 + 1 == b2 && MemberQ[ap, {v1, v2}], False,
		b1 == b2 + 1 && MemberQ[ap, {v2, v1}], True,
		b1 == b2 + 1 && ! MemberQ[ap, {v2, v1}], False,
		True, 0];
	While[changed,
		If[! FreeQ[seen, pairs], Break[]];
		AppendTo[seen, pairs];
		changed = False;
		Do[If[comparer[pairs[[i]], pairs[[j]]] === False,
			pairs = ReplacePart[pairs, {i -> pairs[[j]], j -> pairs[[i]]}];
			changed = True], {i, n}, {j, i + 1, n}]
	];
	{#1, col[[#2]]} & @@@ pairs
];

PArrayPlot::usage = "PArrayPlot[lambda, coloring] returns a Graphics of a coloring as a P-array.";
PArrayPlot[lam_List, col_List, test_ : (False &)] :=
	Module[{arr, shape, color, poset, arrows, levelEdges, baseGraphics},
		poset = AttackingPoset[lam];
		arr = Select[PArray[col], Tr[#] > 0 &];
		shape = Length /@ arr;
		color = If[test[lam, col], LightBlue, LightGray];
		baseGraphics = Table[
			{{color, EdgeForm[Black],
				Rectangle[{c, -r}, {c + 1, -(r + 1)}]},
				Inset[arr[[r, c]], {c + 1/2, -(r + 1/2)}]},
			{r, Length@shape}, {c, shape[[r]]}];
		levelEdges[r_] := Join @@ Table[
			Which[
				MemberQ[poset, {arr[[r, c1]], arr[[r + 1, c2]]}],
				{Blue, Arrow[{{c1 + 1/2, -(r + 1/2)},
					{c2 + 1/2, -(r + 3/2)}}]},
				MemberQ[poset, {arr[[r + 1, c2]], arr[[r, c1]]}],
				{Red, Arrow[{{c2 + 1/2, -(r + 3/2)},
					{c1 + 1/2, -(r + 1/2)}}]},
				True, Sequence @@ {}
			],
			{c1, shape[[r]]}, {c2, shape[[r + 1]]}];
		arrows = If[Length[shape] < 2, {},
			Join @@ Table[levelEdges[r], {r, Length[shape] - 1}]];
		Graphics[{baseGraphics, arrows},
			ImageSize -> 40 {Max[1, Max[shape, 0]], Max[1, Length@shape]}]
	];

OrientationPlot::usage = "OrientationPlot[area, orientation] returns a Graphics of an oriented unit interval graph.";
OrientationPlot[area_List, ao_List] :=
	Module[{n = Length@area, edges, ascEdges, points, edgeGraphics, vertexGraphics},
		edges = UnitIntervalEdges[area];
		ascEdges = GraphOrientationIntersection[edges, ao];
		points = Table[{Cos[2 Pi (i - 1)/Max[1, n]],
			Sin[2 Pi (i - 1)/Max[1, n]]}, {i, n}];
		edgeGraphics = Map[
		Function[e,
			{If[MemberQ[ascEdges, e], Blue, Red],
				Arrow[{points[[e[[1]]]], points[[e[[2]]]]}]}], edges];
		vertexGraphics = Table[
			{White, EdgeForm[Black], Disk[points[[i]], .08],
				Black, Text[i, points[[i]]]}, {i, n}];
		Graphics[{edgeGraphics, vertexGraphics}, PlotRange -> 1.2]
	];

UnitIntervalPlot::usage = "UnitIntervalPlot[area, coloring] returns a Graphics of a coloring in unit interval order.";
UnitIntervalPlot[area_List, col_List] :=
	Module[{pairs, bplen, array, r, c, i},
		pairs = UnitIntervalData[area, col];
		bplen = If[pairs === {}, 0, Max[First /@ pairs]];
		array = Table[
			{r, c} = {-i, -pairs[[i, 1]]};
			{{LightBlue, EdgeForm[Black],
				Rectangle[{c, -r}, {c + 1, -(r + 1)}]},
				Inset[pairs[[i, 2]], {c + 1/2, -(r + 1/2)}]},
			{i, Length@pairs}];
		Graphics[array, ImageSize -> 20 {Max[1, bplen], Max[1, Length@area]}]
	];

GraphAttackingEdges::usage = "GraphAttackingEdges[edges, coloring] returns monochromatic edges.";
GraphAttackingEdges[edges_List, col_List] := GraphAttackingEdges[edges, col] =
	Select[edges, col[[#[[1]]]] == col[[#[[2]]]] &];
GraphChromaticSymmetricColorings::usage = "GraphChromaticSymmetricColorings[edges, n, allowAttacking, maxColor] returns graph colorings.";
GraphChromaticSymmetricColorings[edges_List, n_Integer, allowAttacking_: False] :=
	GraphChromaticSymmetricColorings[edges, n, allowAttacking, n];
GraphChromaticSymmetricColorings[edges_List, n_Integer, allowAttacking_: False,
	maxCol_Integer] := GraphChromaticSymmetricColorings[edges, n, allowAttacking, maxCol] =
	Select[Tuples[Range@maxCol, n],
		allowAttacking || Length[GraphAttackingEdges[edges, #]] == 0 &];

GraphColoringInversions::usage = "GraphColoringInversions[edges, coloring] counts descending edges.";
GraphColoringInversions[edges_List, col_List] :=
	Sum[Boole[col[[e[[1]]]] > col[[e[[2]]]]], {e, edges}];
GraphColoringOrientation::usage = "GraphColoringOrientation[edges, coloring] returns the induced orientation.";
GraphColoringOrientation[edges_List, col_List] :=
	Table[If[col[[e[[1]]]] < col[[e[[2]]]], e, Reverse@e], {e, edges}];
GraphOrientationIntersection::usage = "GraphOrientationIntersection[edges, orientation] returns equally oriented edges.";
GraphOrientationIntersection[edges_List, orient_List] :=
	With[{m = Length@edges},
		Table[If[edges[[i]] === orient[[i]], orient[[i]], Sequence @@ {}], {i, m}]
	];
GraphOrientationAscents::usage = "GraphOrientationAscents[edges, orientation] counts correctly oriented edges.";
GraphOrientationAscents[edges_List, orient_List] :=
	Length[GraphOrientationIntersection[edges, orient]];
GraphOrientationInversions::usage = "GraphOrientationInversions[edges, orientation] counts oppositely oriented edges.";
GraphOrientationInversions[edges_List, orient_List] :=
	GraphOrientationAscents[edges, Reverse /@ orient];

AreaRowPermutation::usage = "AreaRowPermutation[area] returns the row-to-column permutation for a 0-first area list.";
areaRowPermutationLegacy[area_List] := With[{lbl = Range@Length@area},
	areaRowPermutationLegacy[area, lbl, lbl]];
areaRowPermutationLegacy[area_List, {}, {}] := {};
areaRowPermutationLegacy[area_List, rl_List, cl_List] := Module[{n = Length@area, pair},
	pair = cl[[n - area[[1]]]];
	Prepend[areaRowPermutationLegacy[Rest@area, Rest@rl,
		Drop[cl, {n - area[[1]]}]], pair]
];
AreaRowPermutation[area_List] := areaRowPermutationLegacy[Reverse@area];

DinvFromAreaSeq::usage = "DinvFromAreaSeq[area] returns dinv for a 0-first Catalan area sequence.";
DinvFromAreaSeq[area_List] := Module[{n = Length@area},
	Sum[Boole[area[[i]] == area[[j]]] +
		Boole[area[[i]] == area[[j]] + 1], {i, n}, {j, i + 1, n}]
];

MajFromAreaSeq::usage = "MajFromAreaSeq[area] returns the major index of a 0-first area sequence.";
majFromAreaSeqLegacy[aseq_List] := With[{n = Length@aseq},
	Sum[If[aseq[[k + 1]] >= aseq[[k]], 2 k + aseq[[k]], 0], {k, n - 1}] +
		(2 n + aseq[[n]]) Boole[aseq[[1]] >= aseq[[n]]]
];
MajFromAreaSeq[area_List] := majFromAreaSeqLegacy[Reverse@area];

AyclicAreaListQ::usage = "AyclicAreaListQ[area] tests whether an area list has minimum zero.";
AyclicAreaListQ[area_List] := Min[area] == 0;

NoAscendingCycleOrientations::usage = "NoAscendingCycleOrientations[edges] returns orientations with no ascending directed cycle.";
NoAscendingCycleOrientations[edges_List, opts : OptionsPattern[GraphOrientations]] :=
	Select[GraphOrientations[edges, opts],
		AcyclicGraphQ[Graph[GraphOrientationIntersection[edges, #] /.
			{List[a_Integer, b_Integer] :> DirectedEdge[a, b]}]] &];

GraphOrientationSinks::usage = "GraphOrientationSinks[orientation, n] returns the sinks.";
GraphOrientationSinks[ao_List, n_Integer] :=
	Table[If[Count[ao, {k, _}] == 0, k, Sequence @@ {}], {k, n}];
GraphOrientationSources::usage = "GraphOrientationSources[orientation, n] returns the sources.";
GraphOrientationSources[ao_List, n_Integer] :=
	Table[If[Count[ao, {_, k}] == 0, k, Sequence @@ {}], {k, n}];
GraphOrientationHalfSinks::usage = "GraphOrientationHalfSinks[edges, orientation, n] returns half-sinks.";
GraphOrientationHalfSinks[edges_List, ao_List, n_Integer] := Module[{ascEdges},
	ascEdges = GraphOrientationIntersection[ao, edges];
	Table[If[Count[ascEdges, {k, _}] == 0, k, Sequence @@ {}], {k, n}]
];
GraphOrientationHalfSources::usage = "GraphOrientationHalfSources[edges, orientation, n] returns half-sources.";
GraphOrientationHalfSources[edges_List, ao_List, n_Integer] := Module[{ascEdges},
	ascEdges = GraphOrientationIntersection[ao, edges];
	Table[If[Count[Reverse /@ ascEdges, {k, _}] == 0, k, Sequence @@ {}], {k, n}]
];

Options[LLTOrientationLowestReachableVertex] = {StrictEdges -> {}};
LLTOrientationLowestReachableVertex::usage = "LLTOrientationLowestReachableVertex[area, orientation] returns the lowest reachable vertex of each vertex.";
LLTOrientationLowestReachableVertex[area_List, or_List, opts : OptionsPattern[]] := Module[
	{lrvSteps, stepLength, n = Length@area, ascEdges, strict = OptionValue[StrictEdges]},
	ascEdges = Join[GraphOrientationIntersection[UnitIntervalEdges@area, or], strict];
	stepLength[{a_Integer, b_Integer}] := If[a < b, b - a, n - a + b];
	lrvSteps[k_Integer] := lrvSteps[k] = Module[{out, nxt},
		out = Last /@ Select[ascEdges, First[#] == k &];
		If[out === {}, {k, 0},
			nxt = Last@SortBy[out, stepLength[{k, #}] + lrvSteps[#][[2]] &];
			{lrvSteps[nxt][[1]], stepLength[{k, nxt}] + lrvSteps[nxt][[2]]}]
	];
	lrvSteps[#][[1]] & /@ Range[n]
];

Options[LLTOrientationVertexPartition] = {StrictEdges -> {}};
LLTOrientationVertexPartition::usage = "LLTOrientationVertexPartition[area, orientation] returns the induced vertex partition.";
LLTOrientationVertexPartition[area_List, or_List, opts : OptionsPattern[]] :=
	GatherBy[Range[Length@area], (LLTOrientationLowestReachableVertex[area, or, opts][[#]] &)];
Options[LLTOrientationShape] = {StrictEdges -> {}};
LLTOrientationShape::usage = "LLTOrientationShape[area, orientation] returns the orientation shape.";
LLTOrientationShape[area_List, or_List, opts : OptionsPattern[]] :=
	Sort[Length /@ LLTOrientationVertexPartition[area, or, opts], Greater];
LLTOrientationForest::usage = "LLTOrientationForest[area, orientation] returns the orientation forest map.";
LLTOrientationForest[area_List, or_List] := Module[{lrvSteps, stepLength, n = Length@area, ascEdges},
	ascEdges = GraphOrientationIntersection[UnitIntervalEdges@area, or];
	stepLength[{a_Integer, b_Integer}] := If[a < b, b - a, n - a + b];
	lrvSteps[k_Integer] := lrvSteps[k] = Module[{out, nxt},
		out = Last /@ Select[ascEdges, First[#] == k &];
		If[out === {}, {k, 0, k},
			nxt = Last@SortBy[out, {stepLength[{k, #}] + lrvSteps[#][[2]] &, -stepLength[{k, #}] &}];
			{lrvSteps[nxt][[1]], stepLength[{k, nxt}] + lrvSteps[nxt][[2]], nxt}]
	];
	lrvSteps[#][[3]] & /@ Range[n]
];

InnerCorners::usage = "InnerCorners[area] returns the inner corners of a 0-first area list.";
InnerCorners[area_List] := Module[{n = Length@area, edgeList = UnitIntervalEdges@area},
	Select[edgeList,
		! MemberQ[edgeList, {#[[1]], Mod[#[[2]], n] + 1}] &&
			! MemberQ[edgeList, {Mod[#[[1]] - 2, n] + 1, #[[2]]}] &]
];
ValleyEdges::usage = "ValleyEdges[edges, n] returns the edges that are valleys in the diagram.";
ValleyEdges[edgeList_List, n_Integer] := Select[
	edgeList,
		! MemberQ[edgeList, {#[[1]], Mod[#[[2]], n] + 1}] &&
			! MemberQ[edgeList, {Mod[#[[1]] - 2, n] + 1, #[[2]]}] &
];
OuterCorners::usage = "OuterCorners[area] returns the outer corners of a 0-first area list.";
OuterCorners[area_List] := Module[{n = Length@area, edgeList, nonEdges},
	edgeList = Join[UnitIntervalEdges@area, Table[{k, k}, {k, n}]];
	nonEdges = Complement[UnitIntervalEdges[ConstantArray[n - 1, n]], edgeList];
	Select[nonEdges,
		MemberQ[edgeList, {#[[1]], Mod[#[[2]] - 2, n] + 1}] &&
			MemberQ[edgeList, {Mod[#[[1]], n] + 1, #[[2]]}] &]
];

AreaToTopBounceShape::usage = "AreaToTopBounceShape[area] returns the top-starting bounce shape for a 0-first area list.";
areaToTopBounceShapeLegacy[{}] := {};
areaToTopBounceShapeLegacy[area_List] := Module[{k = area[[1]]},
	Join[Range[k, 0, -1], areaToTopBounceShapeLegacy[area[[k + 2 ;;]]]]
];
AreaToTopBounceShape[area_List] := areaToTopBounceShapeLegacy[Reverse@area];

AreaConjugate::usage = "AreaConjugate[area] returns the conjugate of a non-circular 0-first area list.";
areaConjugateLegacy[area_List] := Module[{n = Length@area, cg},
	cg = Reverse[Range[n] - 1];
	cg - Table[Count[cg - area, i_ /; i >= k], {k, n}]
];
AreaConjugate[area_List] := Reverse@areaConjugateLegacy[Reverse@area];

AreaToDyckWord::usage = "AreaToDyckWord[area, corners] converts a 0-first area list to its Dyck word.";
areaToDyckWordLegacy[area_List, innerCorners_List : {}] := Module[{cr = First /@ innerCorners,
		n = Length@area},
		Flatten@Table[{If[k == 1,
			ConstantArray[0, Boole[! MemberQ[cr, 1]] + area[[1]] - area[[n]]],
			ConstantArray[0, Boole[! MemberQ[cr, k]] + area[[k]] - area[[k - 1]]]],
			If[MemberQ[cr, k], {2}, {1}]}, {k, n}]
];
AreaToDyckWord[area_List, innerCorners_List : {}] :=
	areaToDyckWordLegacy[Reverse@area, innerCorners];

AreaDinv::usage = "AreaDinv[area] returns dinv for a 0-first area list.";
AreaDinv[area_List] := Module[{n = Length@area},
	Sum[Boole[area[[i]] == area[[j]]] +
		Boole[area[[i]] == area[[j]] + 1], {i, n}, {j, i + 1, n}]
];

Options[GraphChromaticSymmetricPolynomial] = {Weights -> {}};
GraphChromaticSymmetricPolynomial::usage = "GraphChromaticSymmetricPolynomial[edges, n, x, q] returns the graph chromatic symmetric polynomial.";
GraphChromaticSymmetricPolynomial[area : {_Integer ..}, x_, q_: 1, opts : OptionsPattern[]] :=
	GraphChromaticSymmetricPolynomial[UnitIntervalEdges@area, Length@area, x, q, opts];
GraphChromaticSymmetricPolynomial[edges_List, n_Integer, x_, q_: 1,
	opts : OptionsPattern[]] := Module[{w = OptionValue[Weights], nn},
	nn = If[w === {}, n, Tr@w];
	Sum[If[w === {}, q^GraphColoringAscents[edges, c] (Times @@ (x /@ c)),
		q^GraphColoringAscents[edges, c] (Times @@ ((x /@ c)^w))],
		{c, GraphChromaticSymmetricColorings[edges, n, False, nn]}]
];

HomogeneousGraphChromaticSymmetricPolynomial::usage = "HomogeneousGraphChromaticSymmetricPolynomial[edges, n, x, q, t] returns the homogeneous graph chromatic polynomial.";
HomogeneousGraphChromaticSymmetricPolynomial[edges_List, n_Integer, x_, q_: 1, t_: 1] :=
	Sum[With[{asc = GraphColoringAscents[edges, c]},
		q^asc t^(Length[edges] - asc) (Times @@ (x /@ c))],
		{c, GraphChromaticSymmetricColorings[edges, n, False]}];

Options[GraphChromaticLLTPolynomial] = {StrictEdges -> {}, WeakEdges -> {}};
GraphChromaticLLTPolynomial::usage = "GraphChromaticLLTPolynomial[edges, n, x, q] returns the graph LLT polynomial.";
GraphChromaticLLTPolynomial[attacking_List, n_Integer, x_, q_: 1,
	opts : OptionsPattern[]] := GraphChromaticLLTPolynomial[attacking, n, x, q, opts] =
	Module[{colorings, strict = OptionValue[StrictEdges], weak = OptionValue[WeakEdges], qEdges},
		colorings = GraphChromaticSymmetricColorings[Range[n - 1, 0, -1], n, True, n];
		If[strict =!= {} || weak =!= {},
			colorings = Select[colorings,
				GraphColoringAscents[strict, #] == Length[strict] &&
					GraphColoringAscents[weak, #] == 0 &]];
		qEdges = Complement[attacking, strict];
		Sum[(Times @@ (x /@ c)) q^GraphColoringAscents[qEdges, c], {c, colorings}]
	];
GraphChromaticLLTPolynomial[area : {_Integer ..}, x_, q_: 1,
	opts : OptionsPattern[]] :=
	GraphChromaticLLTPolynomial[UnitIntervalEdges@area, Length@area, x, q, opts];

Options[HomogeneousGraphLLTPolynomial] = {StrictEdges -> {}, WeakEdges -> {}};
HomogeneousGraphLLTPolynomial::usage = "HomogeneousGraphLLTPolynomial[edges, n, x, q, t] returns the homogeneous graph LLT polynomial.";
HomogeneousGraphLLTPolynomial[edges_List, n_Integer, x_, q_: 1, t_: 1,
	opts : OptionsPattern[]] := HomogeneousGraphLLTPolynomial[edges, n, x, q, t, opts] =
	Module[{colorings, strict = OptionValue[StrictEdges], weak = OptionValue[WeakEdges], qEdges, tEdges, asc},
		colorings = GraphChromaticSymmetricColorings[Range[n - 1, 0, -1], n, True, n];
		If[strict =!= {} || weak =!= {},
			colorings = Select[colorings,
				GraphColoringAscents[strict, #] == Length[strict] &&
					GraphColoringAscents[weak, #] == 0 &]];
		qEdges = Complement[edges, strict];
		tEdges = Complement[edges, weak, strict];
		Sum[asc = GraphColoringAscents[qEdges, c];
			(Times @@ (x /@ c)) q^asc t^(Length[tEdges] - asc), {c, colorings}]
	];
HomogeneousGraphLLTPolynomial[area : {_Integer ..}, x_, q_: 1, t_: 1,
	opts : OptionsPattern[]] :=
	HomogeneousGraphLLTPolynomial[UnitIntervalEdges@area, Length@area, x, q, t, opts];

GraphChromaticLLTPolynomialAttacking::usage = "GraphChromaticLLTPolynomialAttacking[equalEdges, statisticEdges, n, x, q] returns an LLT polynomial with forced attacking edges.";
GraphChromaticLLTPolynomialAttacking[edgesEq_List, edges2_List, n_Integer, x_, q_: 1] :=
	GraphChromaticLLTPolynomialAttacking[edgesEq, edges2, n, x, q] = Module[
		{attackingEdges = Complement[edgesEq, edges2], colorings, isIncreasingQ},
		isIncreasingQ[edges_, col_] := And @@ Table[col[[e[[1]]]] < col[[e[[2]]]], {e, edges}];
		colorings = GraphChromaticSymmetricColorings[attackingEdges, n, True, n];
		colorings = Select[colorings, isIncreasingQ[attackingEdges, #] &];
		Sum[q^GraphColoringAscents[edges2, c] (Times @@ (x /@ c)), {c, colorings}]
	];

StripSizesToEdges::usage = "StripSizesToEdges[sizes] returns {area, strictEdges}, using a 0-first area list.";
stripSizesToEdgesLegacy[sizes : {{_Integer, _Integer} ..}] := Module[{n, m = Length@sizes,
		tabCoords, cellData, inOrder, area, attacking, strict, r, c, k, att},
	tabCoords = Join @@ Table[If[sizes[[r, 1]] >= c > sizes[[r, 2]], {r, c}, Sequence @@ {}],
		{r, m}, {c, Max[sizes]}];
	inOrder = SortBy[tabCoords, {#[[2]] &, -#[[1]] &}];
	cellData = MapIndexed[Join[#1, #2] &, inOrder]; n = Length@cellData;
	attacking = Sort /@ (Join @@ Table[{r, c, k} = cellData[[i]];
		att = Select[cellData, #[[1]] > r && c <= #[[2]] <= c + 1 &];
		Table[{i, a}, {a, Last /@ att}], {i, n}]);
	strict = Sort /@ (Join @@ Table[{r, c, k} = cellData[[i]];
		att = Select[cellData, #[[1]] == r && #[[2]] == c + 1 &];
		Table[{i, a}, {a, Last /@ att}], {i, n}]);
	area = Table[Max[Last /@ Select[Join[attacking, strict], First[#] == k &] - k, 0],
		{k, Tr[#1 - #2 & @@@ sizes]}];
	{area, strict}
];
StripSizesToEdges[sizes : {__Integer}] := StripSizesToEdges[Table[{s, 0}, {s, sizes}]];
StripSizesToEdges[sizes : {{_Integer, _Integer} ..}] :=
	With[{data = stripSizesToEdgesLegacy[sizes],
		n = Total[First /@ sizes - Last /@ sizes]},
		{Reverse@data[[1]], (n + 1 - #) & /@ data[[2]]}];

VerticalStripLLTColorings::usage = "VerticalStripLLTColorings[sizes] returns colorings satisfying the strip strictness conditions.";
VerticalStripLLTColorings[sizes_List] := Module[{n = Tr@sizes, strict},
	strict = Last@StripSizesToEdges[sizes];
	Select[Tuples[Range@n, n], GraphColoringAscents[strict, #] == Length[strict] &]
];
VerticalStripLLTPolynomial::usage = "VerticalStripLLTPolynomial[sizes, x, q] returns a vertical-strip LLT polynomial.";
VerticalStripLLTPolynomial[sizes_List, x_, q_: 1] := VerticalStripLLTPolynomial[sizes, x, q] =
	Module[{colorings = VerticalStripLLTColorings[sizes], data, attacking, n},
		data = stripSizesToEdgesLegacy[Table[{s, 0}, {s, sizes}]];
		n = Total[sizes];
		attacking = (n + 1 - #) & /@ Complement[lltLegacyAreaEdges[data[[1]]], data[[2]]];
		Sum[(Times @@ (x /@ c)) q^GraphColoringAscents[attacking, c], {c, colorings}]
	];

PartitionRookPlacements::usage = "PartitionRookPlacements[lambda] returns permutations fitting in a partition diagram.";
PartitionRookPlacements[lam_List] := With[{n = Length@lam},
	Select[Permutations[Range[n]], And @@ Table[lam[[i]] >= #[[i]], {i, n}] &]
];

DiagramRookPlacements::usage = "DiagramRookPlacements[diagram, n] returns all non-attacking placements of n rooks in a diagram.";
DiagramRookPlacements[diagram_List, 0] := {{}};
DiagramRookPlacements[{}, n_Integer] := If[n == 0, {{}}, {}];
DiagramRookPlacements[diagram_List, n_Integer] :=
	Module[{minR, maxR, rookPlaceComplement},
		minR = Min[First /@ diagram];
		maxR = Max[First /@ diagram];
		rookPlaceComplement[diag_, {r_, c_}] :=
			Select[diag, #[[1]] != r && #[[2]] != c &];
		Which[
			minR == maxR && n > 1, {},
			minR == maxR && n == 1, List /@ diagram,
			True,
			Join[
				Join @@ Table[
					Append[#, sq] & /@
						DiagramRookPlacements[rookPlaceComplement[diagram, sq], n - 1],
					{sq, Select[diagram, #[[1]] == minR &]}],
				DiagramRookPlacements[
					rookPlaceComplement[diagram, {minR, Infinity}], n]
			]
		]
	];

SouthWestDiagram::usage = "SouthWestDiagram[permutation] returns the south-west diagram.";
SouthWestDiagram[perm_List] := With[{n = Length@perm},
	Union[Join @@ Table[Join[Table[{r, k}, {k, perm[[r]]}],
		Table[{k, perm[[r]]}, {k, r + 1, n}]], {r, n}]]
];
SouthEastDiagram::usage = "SouthEastDiagram[permutation] returns the south-east diagram.";
SouthEastDiagram[perm_List] := With[{n = Length@perm},
	Union[Join @@ Table[Join[Table[{r, k}, {k, perm[[r]], n}],
		Table[{k, perm[[r]]}, {k, r + 1, n}]], {r, n}]]
];

AthanasiadisUnimodalSets::usage = "AthanasiadisUnimodalSets[lambda] returns lambda-unimodal subsets of [n-1].";
AthanasiadisUnimodalSets[{mu__, 0}] := AthanasiadisUnimodalSets[{mu}];
AthanasiadisUnimodalSets[lambda_List] := Module[{svec, rangeSet, int, n = Tr@lambda},
	svec[0] = 0; svec[k_] := Tr@lambda[[;; k]];
	Select[Subsets[Range[n - 1]],
		And @@ Table[rangeSet = Range[svec[i] + 1, svec[i + 1] - 1];
			int = Intersection[#, rangeSet]; int == rangeSet[[;; Length@int]],
			{i, 0, Length[lambda] - 1}] &]
];
AthanasiadisS::usage = "AthanasiadisS[lambda] returns the Athanasiadis partial sums.";
AthanasiadisS[lambda_List] := Accumulate[Most@lambda];

(* The cyclic edge direction is reversed together with the area convention.
   Convert orientation statistics to the old labels, apply the same recursion,
   and convert the resulting vertex-indexed data back. *)
lltLegacyEdge[e_List, n_Integer] := n + 1 - Reverse[e];
lltLegacyAreaEdges[area_List] := With[{n = Length@area},
	Join @@ Table[Table[{k, Mod[k + i - 1, n] + 1},
		{i, area[[k]]}], {k, n}]
];
lltLegacyLrv[area_List, or_List, strict_List] := Module[
	{lrvSteps, stepLength, n = Length@area, ascEdges},
	ascEdges = Join[GraphOrientationIntersection[lltLegacyAreaEdges[area], or], strict];
	stepLength[{a_Integer, b_Integer}] := If[a < b, b - a, n - a + b];
	lrvSteps[k_Integer] := lrvSteps[k] = Module[{out, nxt},
		out = Last /@ Select[ascEdges, First[#] == k &];
		If[out === {}, {k, 0},
			nxt = Last@SortBy[out, stepLength[{k, #}] + lrvSteps[#][[2]] &];
			{lrvSteps[nxt][[1]], stepLength[{k, nxt}] + lrvSteps[nxt][[2]]}]
	];
	lrvSteps[#][[1]] & /@ Range[n]
];
lltLegacyForest[area_List, or_List] := Module[
	{lrvSteps, stepLength, n = Length@area, ascEdges},
	ascEdges = GraphOrientationIntersection[lltLegacyAreaEdges[area], or];
	stepLength[{a_Integer, b_Integer}] := If[a < b, b - a, n - a + b];
	lrvSteps[k_Integer] := lrvSteps[k] = Module[{out, nxt},
		out = Last /@ Select[ascEdges, First[#] == k &];
		If[out === {}, {k, 0, k},
			nxt = Last@SortBy[out, {stepLength[{k, #}] + lrvSteps[#][[2]] &, -stepLength[{k, #}] &}];
			{lrvSteps[nxt][[1]], stepLength[{k, nxt}] + lrvSteps[nxt][[2]], nxt}]
	];
	lrvSteps[#][[3]] & /@ Range[n]
];
LLTOrientationLowestReachableVertex[area_List, or_List, opts : OptionsPattern[]] :=
	With[{n = Length@area,
		oldOr = SortBy[lltLegacyEdge[#, Length@area] & /@ or,
			FirstPosition[Sort /@ lltLegacyAreaEdges[Reverse@area], Sort[#]] &],
		strict = lltLegacyEdge[#, Length@area] & /@ OptionValue[StrictEdges]},
		With[{old = lltLegacyLrv[Reverse@area, oldOr, strict]},
			Table[n + 1 - old[[n + 1 - i]], {i, n}]]
	];
LLTOrientationVertexPartition[area_List, or_List, opts : OptionsPattern[]] :=
	GatherBy[Range[Length@area],
		LLTOrientationLowestReachableVertex[area, or, opts][[#]] &];
LLTOrientationShape[area_List, or_List, opts : OptionsPattern[]] :=
	Sort[Length /@ LLTOrientationVertexPartition[area, or, opts], Greater];
LLTOrientationForest[area_List, or_List] :=
	With[{n = Length@area,
		oldOr = SortBy[lltLegacyEdge[#, Length@area] & /@ or,
			FirstPosition[Sort /@ lltLegacyAreaEdges[Reverse@area], Sort[#]] &]},
		With[{old = lltLegacyForest[Reverse@area, oldOr]},
			Table[n + 1 - old[[n + 1 - i]], {i, n}]]
	];

End[(* End private *)];
EndPackage[];
