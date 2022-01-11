(* ::Package:: *)


(* MathKernel -script file.m *)

Clear["ChromaticFunctions`*"];
BeginPackage["ChromaticFunctions`"];


PathShapes::usage = "PathShapes[n] gives all partitions that fit inside the size n triangle.";

BounceLengths::usage = "BounceLengths[lambda] gives the lengths of bounce triangles, from top to bottom.";

AttackingPoset::usage = "AttackingPoset[lam] returns the poset associated with a shape -- number of edges is equal to size of lam. All edges start with smallest vertex label.";

IncomparabilityGraph::usage = "IncomparabilityGraph[lam] returns the incomparability graph. Edges here should have vertices with different colors to be non-attacking.";

ColorOrientation::usage = "ColorOrientation[lam,col] gives the acyclic orientation on the incomparability graph induced by the coloring. Edges are {v1,v2} such that v1 has smaller color than v2.";

AcyclicAscents::usage = "AcyclicAscents[lam, col] or AcyclicAscents[ao] returns number of ascents in the acyclic orientation, that is, edges with increasing vertex labels.";

AttackingVertices::usage = "AttackingVertices[lam, col] returns monochromatic edges in the incomparability graph. This should be empty in order to be a proper coloring.";


ChromaticSymmetricColorings::usage = "ChromaticSymmetricColorings[lam,[False],[n]] return all colorings that are either allowed to be attacking or not. The parameter n is the largest color allowed.";


PArray::usage = "PArray[col] returns the P-Array associated with the color";

GasharovPTableauQ::usage = "GasharovPTableauQ[lam, col] returns true if the coloring is a proper P-tableau.";

GasharovOpPTableauQ::usage = "GasharovOpPTableauQ[lam, col] returns true if the coloring is the opposite of a P-tableau.";


GasharovPTableaux::usage = "GasharovPTableaux[lam] returns all colorings of the IncomparabilityGraph[lam] that are P-tableaux";

GasharovOpPTableaux::usage = "GasharovOpPTableaux[lam] returns all colorings of the IncomparabilityGraph[lam] that are OpP-tableaux";


AlternatingChains::usage = "AlternatingChains[lam,col,i] return all alternating chains with color i, i+1 in the coloring.";


ChromaticSymmetricPolynomial::usage = "ChromaticSymmetricPolynomial[lam,x,q=1,[n]] returns the chromatic symmetric polynomial.";

SingleCelledLLTPolynomial::usage = "SingleCelledLLTPolynomial[lam,x,q=1,[n]] returns the single celled LLT polynomial.";


PArrayPlot::usage="PArrayPlot[lam,col] displays coloring as P-array.";


Labels::usage="Option for AreaListPlot";
Circular::usage="Option for AreaListPlot";
AreaListPlot::usage="AreaListPlot[areaList,boxLabels:{}] displays the circular Dyck path plot, with possible labels.";
OrientationPlot::usage="OrientationPlot[areaList,ao] shows the acyclic orientation.";

(* These only work for non-circular paths *)
AreaToTopBounceShape::usage = "The bounce path starting at the top.";
AreaToBounceShape::usage = "";
AreaBounce::usage = "";
AreaDinv::usage = "";

AreaToDyckWord::usage = "AreaToDyckWord[area,[corners]] returns the 01[2] sequence associated with path.";

(*
ColorPlot::usage="ColorPlot[lam,col,ao:{}] displays coloring in triangle with induced acyclic orientation.";

DyckDiagramPlot::usage="DyckDiagramPlot[edges, strictEdges] plots the Dyck path model associated with the edges.";
*)

UnitIntervalData::usage="UnitIntervalData[lam,col] returns the unit interval representation of the coloring.";

UnitIntervalPlot::usage="UnitIntervalPlot[lam,col] displays coloring as coloring of unit interval graph.";





(***************** General graph functions   *********************************)


GraphAttackingEdges::usage = "GraphAttackingEdges[edges, col] return the edges that are attacking.";

GraphChromaticSymmetricColorings::usage="GraphChromaticSymmetricColorings[edges, nvertices, [allowAttacking], [maxCol]]";

GraphColoringAscents::usage="GraphColoringAscents[edges,col]";
GraphColoringInversions::usage="GraphColoringInversions[edges,col]"

GraphColoringOrientation::usage="GraphColoringOrientation[edges,col] returns (acyclic) orientation induced by the coloring.";

GraphOrientationIntersection::usage="GraphOrientationIntersection[edges1, edges2] return commonly oriented edges. Order of edges matter!";

GraphOrientationAscents::usage="GraphOrientationAscents[edges, orient] returns number of ascents, that is, correctly oriented edges.";
GraphOrientationInversions::usage="GraphOrientationInversions[edges, orient] returns number of inversions, that is, opposite oriented edges.";

AreaToEdges::usage="AreaToEdges[list] returns edges associated with an area list.";

AreaConjugate::usage="AreaConjugate[area] returns the conjugate area list. Only works for non-circular shapes.";
AreaRowPermutation::usage="AreaRowPermutation[area] returns the permutation that maps rows to columns, such that a row of length k is mapped to a column of length k.  Furthermore, the permutaton is 132-avoiding."

GraphAcyclicOrientations::usage="GraphAcyclicOrientations[edges] returns all acyclic orientations of the edges.";

GraphAreaLists::usage = "GraphAreaLists[size, [onlyMinimal=True]] returns all area lists of size n. onlyMinimal reduces this set mod cyclic permutations.";
Circular::usage  = "Option for GraphAreaLists";
Width::usage = "Option for GraphAreaLists";


DinvFromAreaSeq::usage = "DinvFromAreaSeq[area] return dinv";
MajFromAreaSeq::usage = "MajFromAreaSeq[area] return major index, which is defined in terms of valleys";

AyclicAreaListQ::usage = "Returns true if the area list describes a classical unit interval graph.";

GraphOrientations::usage="GraphOrientations[edges] returns all possible orientations of edges.";

NoAscendingCycleOrientations::usage="NoAscendingCycleOrientations[edges] returns all orientations without ascending cycles.";

GraphOrientationSinks::usage="GraphOrientationSinks[orientation, nverts] returns all sinks.";
GraphOrientationSources::usage="GraphOrientationSources[orientation, nverts] returns all sources.";

GraphOrientationHalfSinks::usage="GraphOrientationHalfSinks[edges,ao, nverts] returns number of half-sinks.";
GraphOrientationHalfSources::usage="GraphOrientationHalfSources[edges,ao, nverts] returns number of half-sources.";

LLTOrientationVertexPartition::usage="LLTOrientationVertexPartition[area,or] gives the set partition defined by the vertiece.";
LLTOrientationLowestReachableVertex::usage="LLTOrientationLowestReachableVertex[area,or] ";
LLTOrientationComposition::usage="LLTOrientationComposition[area,or] returns the composition associated to orientation in order to get the e-exp.";
LLTOrientationShape::usage="LLTOrientationShape[area,or] returns the shape associated to orientation in order to get the e-exp.";
LLTOrientationForest::usage="LLTOrientationForest[area,or] ";

ValleyEdges::usage="ValleyEdges[edges, n] returns all edges that are valleys in the diagram.";
InnerCorners::usage = "InnerCorners[area]";
OuterCorners::usage = "OuterCorners[area]";

HomogeneousGraphChromaticSymmetricPolynomial::usage="HomogeneousGraphChromaticSymmetricPolynomial[edges,n,x,q,t]";
GraphChromaticSymmetricPolynomial::usage="GraphChromaticSymmetricPolynomial[edges,n,x,q]";
Weights::usage="Option for GraphChromaticSymmetricPolynomial";
GraphChromaticLLTPolynomial::usage="GraphChromaticLLTPolynomial[edges,n,x,q,StrictEdges->{},WeakEdges->{}]";
HomogeneousGraphLLTPolynomial::usage="HomogeneousGraphLLTPolynomial[edges,n,x,q,t]";


WeakEdges::usage = "Option for GraphChromaticLLTPolynomial.";
StrictEdges::usage = "Option for GraphChromaticLLTPolynomial.";

StripSizesToEdges::usage = "StripSizesToEdges[list] returns two lists, {attacking, strict}, of edges that encode a vertical-strip LLT polynomial";

VerticalStripLLTColorings::usage = "VerticalStripLLTColorings[stripSizes] returns the colorings.";

VerticalStripLLTPolynomial::usage = "VerticalStripLLTPolynomial[sizes, x, q] gives the vertical strips LLT polynomial.";


(***************** Rook placement functions  *********************************)

PartitionRookPlacements::usage = "PartitionRookPlacements[lam,n]";
DiagramRookPlacements::usage = "DiagramRookPlacements[diagram,n]";

SouthWestDiagram::usage = "SouthWestDiagram[perm]";
SouthEastDiagram::usage = "SouthEastDiagram[perm]";

(***************** Things related to Athanasiadis powersum *******************)

AthanasiadisUnimodalSets::usage = "AthanasiadisUnimodalSets[lambda] return all subsets of [n-1] which are lambda-unimodal, where Tr[lambda]=n";
AthanasiadisS::usage = "AthanasiadisUnimodalSets[lambda] = Tr[Most@lambda], shows up as S(lambda) in his paper.";

(***************** Switch to private context *********************************)
Begin["Private`"];


(* All valid lambda of length n *)
PathShapes[nn_Integer]:=With[{maxP=Reverse[Range[nn]-1]},
	Join@@Table[Select[IntegerPartitions[k,{nn},Range[0,nn-1]],And@@Thread[#<=maxP]&],{k,0,Tr@maxP}]
];

BounceLengths[{}]:={};
BounceLengths[lam_List]:=With[{step=Count[lam,0]}, (* Init step. *)
	With[{
	nlam=Max[0,#]&/@(lam[[;;-step-1]]-step)},
	Append[BounceLengths[nlam],step]
	]
];

(* Edges are of the form {small, large} *)
AttackingPoset[lambda_List]:=AttackingPoset[lambda]=Module[{n=Length@lambda},
	
	If[!And@@Table[lambda[[i]]<=n-i,{i,n}],
		Print["AttackingPoset:Shape does not fit in triangle"];Abort[]
	];
	
	Join@@Table[
	{r,n-c+1},
	{r,n},{c,lambda[[r]]}]
];

(* Edges are of the form {small, large} *)
IncomparabilityGraph[lambda_List]:=IncomparabilityGraph[lambda]=Module[{n=Length@lambda,ap,apAll},
	ap = AttackingPoset[lambda];
	apAll = AttackingPoset[Range[n-1,0,-1]];
	Complement[apAll,ap]
];

(* Gives the AO associated with a coloring. {smallColorVertex, largeColorVertex} *)
ColorOrientation[lam_List,col_List]:=ColorOrientation[lam,col]=
Table[
	If[ col[[ e[[1]] ]] < col[[ e[[2]] ]],
		{e[[1]], e[[2]]}
		,
		{e[[2]], e[[1]]}
	],
{e,IncomparabilityGraph[lam]}];

AcyclicAscents[lam_List,col_List]:=AcyclicAscents[ColorOrientation[lam,col]];
AcyclicAscents[acyc_List]:=Count[acyc,{a_Integer,b_Integer}/;a<b];


AttackingVertices[lam_List, col_List] := 
  AttackingVertices[lam, col] = Select[IncomparabilityGraph[lam], col[[ #[[1]] ]] == col[[ #[[2]] ]] &];


(* Returns all valid colorings *)
ChromaticSymmetricColorings[lam_List,allowAttacking_:False]:=ChromaticSymmetricColorings[lam,allowAttacking,Length@lam];
ChromaticSymmetricColorings[lam_List,allowAttacking_:False,maxCol_Integer]:=ChromaticSymmetricColorings[lam,allowAttacking,maxCol]=Select[
	Tuples[Range@maxCol,Length@lam], allowAttacking || (Length[AttackingVertices[lam,#]]==0)&];

(* Returns the P-array associated to the coloring: Row i are all vertices with color i. *)
PArray[col_List]:=With[{nn=Length@col},
	Table[Sort@Select[Range[nn],col[[#]]==i&] ,{i,Max[col,nn]}]
];


(* Determine if a coloring is a p-tableau *)
GasharovPTableauQ[lam_List,col_List]:=Module[{arr,shape,poset},
	
	poset=AttackingPoset[lam];
	arr=PArray[col];
	shape=Length/@arr;
	
	Catch[
		(* Check partition shape *)
		If[Sort[shape,Greater]=!=shape,Throw[False]];
		
		Do[
			If[MemberQ[poset,{arr[[r+1,c]],arr[[r,c]]}],Throw[False]];
		
		,{r,Length@arr-1},{c,shape[[r+1]]}];
		
		True
	]
];

(* Determine if a coloring is an OpP-tableau *)
(* Has same distribution over inv/asc as PTableaux *)
GasharovOpPTableauQ[lam_List,col_List]:=Module[{arr,shape,poset},
	poset=AttackingPoset[lam];
	arr=Reverse/@PArray[col];
	shape=Length/@arr;
	
	Catch[
		If[Sort[shape,Greater]=!=shape,Throw[False]]
		
		Do[
			If[MemberQ[poset,{arr[[r,c]],arr[[r+1,c]]}],Throw[False]]
		,{r,Length@arr-1},{c,shape[[r+1]]}];
		
		True
	]
];

GasharovPTableaux[lam_List]:=GasharovPTableaux[lam]=Select[ChromaticSymmetricColorings[lam],GasharovPTableauQ[lam,#]&];
GasharovOpPTableaux[lam_List]:=GasharovOpPTableaux[lam]=Select[ChromaticSymmetricColorings[lam],GasharovOpPTableauQ[lam,#]&];



(* Shape, coloring, i *)
AlternatingChains[lam_List, col_List, i_Integer]:=AlternatingChains[lam,col,i]=With[
	{ ap = IncomparabilityGraph[lam], colPos = Flatten@Position[col, (i|i+1)] },
	
	(* We split when adjacent indices are non-attacking. *)
	Split[colPos, MemberQ[ap, {#1, #2}, {1}] &]
];

(* The chromatic symmetric polynomials  *)
ChromaticSymmetricPolynomial[lam_List,x_,q_:1]:=ChromaticSymmetricPolynomial[lam,x,q,Length@lam];
ChromaticSymmetricPolynomial[lam_List,x_,q_:1,n_Integer]:=ChromaticSymmetricPolynomial[lam,x,q,n]=
Sum[
	q^AcyclicAscents[lam,c](Times@@(x/@c))
,{c,ChromaticSymmetricColorings[lam,False,n]}];

(* The single-celled LLT polynomial *)
SingleCelledLLTPolynomial[lam_List,x_,q_:1]:=SingleCelledLLTPolynomial[lam,x,q,Length@lam];
SingleCelledLLTPolynomial[lam_List,x_,q_:1,n_Integer]:=SingleCelledLLTPolynomial[lam,x,q,n]=Sum[
	q^AcyclicAscents[lam,c](Times@@(x/@c))
,{c,ChromaticSymmetricColorings[lam,True,n]}];


(*******************************************************************************************)
(*                                 Plot functions below                                    *)
(*******************************************************************************************)


PArrayPlot[lam_List, col_List, test_:(False&)] := Module[{arr, vcr, shape, color, poset, arrows, levelEdges, baseGraphics},
	
	poset = AttackingPoset[lam];
	arr = PArray[col];
	
	(* Color normalize *)
	arr = Select[arr, Tr[#] > 0 &];
	shape = Length /@ arr;
	
	(* TODO: should depend on highlighter. *)
	color = If[test[lam,col], LightBlue, LightGray];
	
	baseGraphics = Table[
		{
		{color, EdgeForm[Black], 
		Rectangle[{c, -r}, {c + 1, -(r + 1)}]},
		Inset[arr[[r, c]], {c + 1/2, -(r + 1/2)}]
		}
	, {r, Length@shape}, {c, shape[[r]]}];
	
	levelEdges[r_] := Join @@ Table[
		Which[
			MemberQ[poset, {arr[[r, c1]], arr[[r + 1, c2]]}], 
			{Blue, Arrow[{{c1 + 1/2, -(r + 1/2)}, {c2 + 1/2, -(r + 3/2)}}, {0.2, 0.2}]},
			
			MemberQ[poset, {arr[[r + 1, c2]], arr[[r, c1]]}], 
			{Red, Arrow[{{c2 + 1/2, -(r + 3/2)}, {c1 + 1/2, -(r + 1/2)}}, {0.2, 0.2}]},
			
			True, Sequence @@ {}
		]
	, {c1, shape[[r]]}, {c2, shape[[r + 1]]}];
	
	arrows = Join @@ Table[levelEdges[r], {r, Length[shape] - 1}];
	
	Labeled[Graphics[{baseGraphics, arrows}, ImageSize -> 40 {Max[shape], Length@shape}], (Row /@ poset)]
];


(* This can handle circular arc digraphs *)
Options[AreaListPlot]={Circular->True, Labels->{}}
AreaListPlot[areaList_List, opts:OptionsPattern[]] := Module[{edgeToGridCoord,
	nn = Length@areaList, colors, values, r, c, bl, framed, boxLabels},
	
	(* Given directed edge a->b, return the grid {r,c} coord. *)
	edgeToGridCoord[a_Integer, b_Integer] := Which[
		a <= b, {a, 2 nn - b},
		True, {a, nn - b}
	];
	
	boxLabels = Table[edgeToGridCoord[bl[[1, 1]], bl[[1, 2]]] -> bl[[2]], {bl, OptionValue[Labels]}];
	
	colors = Join @@ Table[{r, c} -> Which[
		r + c <= nn, White,
		r + c >= 2 nn + 1, White,
		r + c == 2 nn, Yellow,
		r + c < 2 nn - areaList[[r]], LightGray,
		True, LightBlue
		],{r, nn}, {c, 2 nn - 1}];
	
	values = Table[
		{r, c} /. boxLabels /. {{r, c} -> ""}
	,{r, nn}, {c, 2 nn - 1}];
	
	framed = Join@@Table[
		If[nn < r + c < 2 nn + 1,
			{r, c} -> True,
		Sequence@@{}
		]
	, {r, nn}, {c, 2 nn - 1}];
	
	
	If[ OptionValue[Circular]===False,
		values = #[[nn;;]]& /@ values;
		framed = ({#[[1,1]], #[[1,2]]-nn+1}->#[[2]]) & /@ Select[framed, #[[1,2]] >= nn&];
		colors = ({#[[1,1]], #[[1,2]]-nn+1}->#[[2]]) & /@ Select[colors, #[[1,2]] >= nn&];
	];
	
	Grid[values, ItemSize -> {1,1},
		Alignment -> Center,
		Spacings->{1,1},
		Background -> {None, None, colors}, 
		Frame -> {None, None, framed}
	]
];

OrientationPlot[area_List,ao_List]:=Module[{labels,e,edges,ascEdges},
	
	edges = AreaToEdges@area;
	ascEdges = GraphOrientationIntersection[edges, ao];
	
	labels = Table[e -> If[MemberQ[ascEdges,e],"\[DownArrow]","\[RightArrow]"],{e,edges}];
	
	If[First@area == 0,
		AreaListPlot[area,Labels->labels,Circular->False]
	,
		AreaListPlot[area,Labels->labels,Circular->True]
	]
];


(* This only works with non-cyclic graphs. *)
DyckDiagramPlot[attackingEdges_List, strictEdges_List: {}, n_Integer:1] := Module[{
	nn = Max[attackingEdges, strictEdges,n]
	 , colors, values,r,c},
	
	DyckDiagramPlot::deprecated = "The function DyckDiagramPlot is deprecated. Rewrite using AreaListPlot."
	Message[DyckDiagramPlot::deprecated];

	
	colors = Join @@ Table[
		{r, c} -> Which[
		MemberQ[attackingEdges, {r, nn - c + 1}], LightGray,
		MemberQ[strictEdges, {r, nn - c + 1}], Red,
		r == nn - c + 1, LightBlue,
		True, White]
	,{r, nn}, {c, nn}];

	values = Table[Which[r == nn - c + 1, r, True, ""], {r, nn}, {c, nn}];
	
	Grid[
		values,
		ItemSize -> 1,
		Background -> {None, None, colors},
		Frame -> All]
];


ColorPlot[lam_List, col_List, ao_: Automatic, test_:(False&)] := Module[{colors, nn = Length@lam, fs, values, aorient},
	aorient = If[ao === Automatic, ColorOrientation[lam, col], ao];
	
	ColorPlot::deprecated = "The function ColorPlot is deprecated. Rewrite using AreaListPlot."
	Message[ColorPlot::deprecated];
	
	colors = Join @@ Table[
		{r, c} -> Which[
			lam[[r]] >= c, LightGray,
			r > nn - c + 1, White,
			r == nn - c + 1, LightBlue,
			True, White
		]
	, {r, nn}, {c, nn}];
		
	(* Create acyclic orientation in boxes. *)
	values = Table[
		Which[
		lam[[r]] >= c, 
			Which[	col[[r]] < col[[nn - c + 1]], "\[DownArrow]", 
					col[[r]] > col[[nn - c + 1]], "\[RightArrow]", 
					True, "\[Bullet]"],
					
		r > nn - c + 1, "",
		r == nn - c + 1, ToString[col[[r]]],
		True, If[MemberQ[aorient, {r, nn - c + 1}], "\[DownArrow]", "\[RightArrow]"]
		]
	, {r, nn}, {c, nn}];
	
	(* TODO: Clean up *)
	fs = Which[
		test[lam, col], Directive[Thick, LightBlue],
		True, Directive[Black]
	];
	
	Grid[values, ItemSize -> 1, Background -> {None, None, colors}, Frame -> All, FrameStyle -> fs]
];


UnitIntervalData[lam_List, col_List] := Module[{ap, bl, r, c, 
	n = Length@lam,bouncePieces, pairs, comparer, bplen, sort, changed = True},
	
	bl = BounceLengths[lam];
	ap = AttackingPoset[lam];
	
	bouncePieces = Range[n][[#[[1]] + 1 ;; #[[2]]]] & /@ Partition[Prepend[Accumulate[bl], 0], 2, 1];
	
	bplen = Length@bouncePieces;
	
	(* List of vertices, (i,v), where i indicate which bounce piece it belongs to. *)
	pairs = Join @@ Table[{i, b}, {i, bplen}, {b, bouncePieces[[i]]}];
	
	(* Sort stuff in unit interval graph. *)
	comparer[{b1_Integer, v1_Integer}, {b2_Integer, v2_Integer}] := 
		Which[
			b1 == b2, v2 < v1,
			b1 + 1 == b2 && ! MemberQ[ap, {v1, v2}], True,
			b1 + 1 == b2 && MemberQ[ap, {v1, v2}], False,
			b1 == b2 + 1 && MemberQ[ap, {v2, v1}], True,
			b1 == b2 + 1 && ! MemberQ[ap, {v2, v1}], False,
			True, 0
		];
	
	 (* Perform sorting. *)
	While[changed,
		changed = False;
		Do[
			If[comparer[pairs[[i]], pairs[[j]]] === False,
				pairs = ReplacePart[pairs, {i -> pairs[[j]], j -> pairs[[i]]}];
				changed = True
			];
		, {i, n}, {j, i + 1, n}];
	];
	
	{ #1, col[[#2]] } &@@@pairs
];


UnitIntervalPlot[lam_List, col_List] := Module[{array, pairs,r,c,i,bplen,n=Length@lam},
	
	pairs = UnitIntervalData[lam, col];
	bplen = Max[First/@pairs];
	
	array = Table[
			{r, c} = {-i, -pairs[[i, 1]]};
			{
			{LightBlue, EdgeForm[Black], 
			Rectangle[{c, -r}, {c + 1, -(r + 1)}]},
			Inset[pairs[[i, 2]], {c + 1/2, -(r + 1/2)}]
			}
		, {i, Length@pairs}];
	
	Graphics[array, ImageSize -> 20 {bplen, n}]
];


(***************** General graph functions   *********************************)



GraphAttackingEdges[edges_List, col_List] := GraphAttackingEdges[edges, col] = Select[edges, col[[#[[1]]]] == col[[#[[2]]]] &];


(* Returns all valid colorings *)
GraphChromaticSymmetricColorings[edges_List, n_Integer, allowAttacking_: False] := GraphChromaticSymmetricColorings[edges, n, allowAttacking, n];

GraphChromaticSymmetricColorings[edges_List, n_Integer, allowAttacking_: False, maxCol_Integer] := 
	GraphChromaticSymmetricColorings[edges, n, allowAttacking, maxCol] =
	Select[Tuples[Range@maxCol, n], allowAttacking || (Length[GraphAttackingEdges[edges, #]] == 0) &];


GraphColoringAscents[edges_List, col_List] := Sum[Boole[col[[e[[1]]]] < col[[e[[2]]]] ], {e, edges}];

GraphColoringInversions[edges_List, col_List] := Sum[Boole[col[[e[[1]]]] > col[[e[[2]]]] ], {e, edges}];


GraphColoringOrientation[edges_List, col_List] := Table[
If[ col[[e[[1]]]] < col[[e[[2]]]], e, Reverse@e ]
,{e,edges}];


GraphOrientationIntersection[edges_List, orient_List] := 
	With[{m = Length@edges},
		Table[If[edges[[i]] === orient[[i]], orient[[i]], Sequence @@ {}], {i, m}]
];


GraphOrientationAscents[edges_List, orient_List] := Length[GraphOrientationIntersection[edges, orient]];

GraphOrientationInversions[edges_List, orient_List] := GraphOrientationAscents[edges,Reverse/@orient];


AreaToEdges[lst_List] := With[{n = Length@lst},
	Join @@ Table[
		Table[ {k, Mod[k + i - 1, n] + 1}, {i, lst[[k]]}]
		, {k, n}]
];

AreaConjugate[area_List] := Module[{n = Length@area, cg, k},
	If[area[[-1]] > 0, Print["Warning: AreaConjugate only works on non-curcular shapes!"]];
	cg = Reverse[Range[n] - 1];
	cg - Table[Count[cg - area, i_ /; i >= k], {k, n}]
];

AreaRowPermutaton[area_List] := With[{lbl = Range@Length@area}, AreaRowPermutaton[area, lbl, lbl]];
AreaRowPermutaton[area_List, {}, {}] := {};
AreaRowPermutaton[area_List, rl_List, cl_List] := 
  Module[{n = Length@area, pair, cln, rln},
   pair = cl[[ n - area[[1]] ]];
   Prepend[
    AreaRowPermutaton[Rest@area, Rest[rl], Drop[cl, {n - area[[1]]}]]
    , pair]
];

(* This returns True

And@@Table[perm = AreaRowPermutaton[a];
 AreaConjugate[a] == a[[Ordering@perm]] && ! 
   MatchQ[perm, {___, a_, ___, b_, ___, c_, ___} /; b > c > a]
 , {a, GraphAreaLists[5, Circular -> False, All -> True]}]
 
*)
 




(* Only makes sense for catalan area seqs. *)
DinvFromAreaSeq[aseq_List] := Module[{r = Reverse@aseq},
	Sum[Boole[r[[i]] == r[[j]]], {i, n}, {j, i + 1, n}] +
	Sum[Boole[r[[i]] == r[[j]] + 1], {i, n}, {j, i + 1, n}]
];

(* Sum over DPs and u get Expand@Together[FunctionExpand@QBinomial[2n,n,q]/Sum[q^i,{i,0,n}]] *)
(* Makes sense for all area seqs *)
MajFromAreaSeq[aseq_List] := With[{n = Length[aseq]},
	Sum[If[aseq[[k + 1]]>=aseq[[k]], (2 k + aseq[[k]]), 0], {k, n - 1}] + (2 n + aseq[[n]]) Boole[aseq[[1]] >= aseq[[n]]]
];


(* Last entry is always a 1 *)
AreaToDyckWord[area_List, innerCorners_List: {}] := Module[{k, word = {}, n = Length@area, cr},
	(* Rows that contain corners. *)
	cr = First /@ innerCorners;
	Flatten@Table[{
		
		(* Horizontal. *)
		If[
			k == 1,
			ConstantArray[0, Boole[!MemberQ[cr, 1]] + area[[1]] - area[[n]] ]
			,
			ConstantArray[0, Boole[! MemberQ[cr, k]] + area[[k]] - area[[k-1]]]
		]
		,
		(* Vertical/diagonal *)
		If[MemberQ[cr, k], {2}, {1}]
		
		}
	, {k, n}]
];

(*
AreaToDyckWord[area_List, innerCorners_List: {}] := Module[{k, word = {}, n = Length@area, cr},
	(* Rows that contain corners. *)
	cr = First /@ innerCorners;
	Flatten@Table[{
		
		(* Vertical/diagonal *)
		If[MemberQ[cr, k], {2}, {1}]
		
		,
		
		(* Horizontal. *)
		Which[
			k == n,
			ConstantArray[0, Boole[!MemberQ[cr, 1]] + area[[1]] - area[[n]] ],
			
			True, ConstantArray[0, Boole[! MemberQ[cr, k]] + area[[k+1]] - area[[k]]]
		]
		}
	, {k, n}]
];
*)

(* Private, unused *)
GraphCycles[el_] := Module[
	{f, edges = Rule @@@ el // Dispatch},
	
	f[x_, b___, x_] := {{x, b, x}};
	f[___, x_, ___, x_] = {};
	f[c___, v_] := Join @@ (f[c, v, #] & /@ ReplaceList[v, edges]);
	
	Join @@ f /@ Union @@ el
];

Options[GraphAcyclicOrientations] = {StrictEdges -> {}};
GraphAcyclicOrientations[edges_List, opts:OptionsPattern[]] := GraphAcyclicOrientations[edges,opts] = Module[
	{n = Max[edges, 0], orients,strict, strictIndices},
	(* Try all colorings with different colors. Such colorings can only result in acyclic orientations. *)
	
	
	orients = Union@Table[
	Table[
		If[col[[e[[1]]]] < col[[e[[2]]]] , e, Reverse@e]
		, {e, edges}]
	, {col, Permutations@Range@n}];
		
	
	strict = OptionValue[StrictEdges];
	strictIndices = Select[ Range[Length@edges], MemberQ[strict, edges[[#]]] & ];
	
	If[Length@strict>0,
		orients = Select[ orients, And@@Table[ edges[[i]]==#[[i]],{i,strictIndices }] & ];
	];
	
	orients
];


Options[GraphAreaLists] = {All -> False, Circular -> True, Width->-1};
(*
GraphAreaLists[n_Integer, onlyMinimal_: True]:=GraphAreaLists[n, All->Not[onlyMinimal]];
*)
GraphAreaLists[n_Integer, opts:OptionsPattern[]] := GraphAreaLists[n, opts] = 
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
	
	
	If[ Not@OptionValue[All],
		Select[gData, isOkQ[#] && isMinimal[#] &]
	,
		Select[gData, isOkQ ]
	]
];




(* Returns true if the list describes a proper unit interval graph. 
These are enumerated by the catalan numbers as we all know. *)

AyclicAreaListQ[lst_List] := (Min[lst] == 0);


(* All orientations of edges of a graph. *)
Options[GraphOrientations] = {StrictEdges -> {},WeakEdges -> {}};
GraphOrientations[edges_List, opts:OptionsPattern[]] := Module[
	{isConnectedQ, multiEdgedQ, n = Max@edges, orients, nEdges = Length@edges, strict, weak, weakIndicator,strictIndicator},
	
	strict = OptionValue[StrictEdges];
	weak = OptionValue[WeakEdges];
	
	(* There is a 1 at the strict edges. *)
	strictIndicator = Table[ Boole@MemberQ[strict,e], {e,edges}];
	weakIndicator = Table[ Boole@MemberQ[weak,e], {e,edges}];
	
	orients = Tuples[{0, 1}, nEdges];
	If[Length@strict>0,
		orients = Select[ orients, (#.strictIndicator)==Tr[strictIndicator] & ];
	];
	If[Length@weak>0,
		orients = Select[ orients, ((1-#).weakIndicator)==Tr[weakIndicator] & ];
	];
	
	orients = Table[
		
		Table[
			If[ or[[i]] == 1, 
				edges[[i]],
				Reverse@edges[[i]]]
			,{i, nEdges}]
		
	, {or, orients}];
	
	orients
];

Options[GraphOrientations] = {StrictEdges -> {},WeakEdges -> {}};
NoAscendingCycleOrientations[edges_List, opts:OptionsPattern[]] := Select[
	GraphOrientations[edges,opts],
		AcyclicGraphQ[
			Graph[GraphOrientationIntersection[edges, #] /. {List[a_Integer, b_Integer] :> DirectedEdge[a, b]}]] 
&];


GraphOrientationSinks[ao_List, n_Integer] := Table[If[Count[ao, {k, _}] == 0, k, Sequence @@ {}], {k, n}];

GraphOrientationSources[ao_List, n_Integer] := Table[If[Count[ao, {_, k}] == 0, k, Sequence @@ {}], {k, n}];
	
	
(* Half-sinks. *)
GraphOrientationHalfSinks[edges_List, ao_List, n_Integer] := Module[{ascEdges},
	
	(* Select ascending edges. *)
	ascEdges = GraphOrientationIntersection[ao, edges];
	
	(* Vertex should be a sink wrt. ascEdges. *)
	
	Table[If[Count[ascEdges, {k, _}] == 0, k, Sequence @@ {}], {k, n}]
];
	
GraphOrientationHalfSources[edges_List, ao_List, n_Integer] := Module[{ascEdges},
	
	(* Select ascending edges. *)
	ascEdges = GraphOrientationIntersection[ao, edges];
	
	(* Vertex should be a source wrt. reverse ascEdges. *)
	
	Table[If[Count[Reverse/@ascEdges, {k, _}] == 0, k, Sequence @@ {}], {k, n}]
];


Options[LLTOrientationLowestReachableVertex] = {StrictEdges -> {}};
LLTOrientationLowestReachableVertex[area_List, or_List,opts:OptionsPattern[]] := Module[{
	lrvSteps, stepLength, n = Length@area, ascEdges, strict},
	
	strict = OptionValue[StrictEdges];
	ascEdges = GraphOrientationIntersection[AreaToEdges@area, or];
	
	ascEdges = Join[ ascEdges , strict ];
	
	(* How long an edge is. *)
	
	stepLength[{a_Integer, b_Integer}] := If[a < b, b - a, n - a + b];
	
	lrvSteps[k_Integer] := lrvSteps[k] = Module[{out, nxt},
		out = Last /@ Select[ascEdges, First[#] == k &];
		(* Recursive definition. *)
		If[Length[out] == 0, {k, 0},
			nxt = Last@SortBy[out, stepLength[{k, #}] + lrvSteps[#][[2]] &];
		{lrvSteps[nxt][[1]], stepLength[{k, nxt}] + lrvSteps[nxt][[2]]}
		]
	];
	lrvSteps[#][[1]] & /@ Range[n]
];


Options[LLTOrientationVertexPartition] = {StrictEdges -> {}};
LLTOrientationVertexPartition[area_List, or_List,opts:OptionsPattern[]] := With[
{lrv=LLTOrientationLowestReachableVertex[area,or,opts]},
GatherBy[Range[Length@area], lrv[[#]]&]
];

Options[LLTOrientationShape] = {StrictEdges -> {}};
LLTOrientationShape[area_List, or_List,opts:OptionsPattern[]] := Sort[Length/@LLTOrientationVertexPartition[area,or,opts], Greater];


LLTOrientationForest[area_List, or_List] := Module[
	{lrvSteps, stepLength, n = Length@area, ascEdges},
	
	ascEdges = GraphOrientationIntersection[AreaToEdges@area, or];
	
	(* How long an edge is. *)
	stepLength[{a_Integer, b_Integer}] := If[a < b, b - a, n - a + b];
	
	lrvSteps[k_Integer] := lrvSteps[k] = Module[{out, nxt},
		
		out = Last /@ Select[ascEdges, First[#] == k &];
		
		(* Recursive definition. *)
		If[Length[out] == 0, {k, 0, k}, (* lrv, steps to lrv, next-vertex *)
			
			nxt = Last@SortBy[out, { stepLength[{k, #}] + lrvSteps[#][[2]] &,  -stepLength[{k, #}]  &} ];
			
			{lrvSteps[nxt][[1]], stepLength[{k, nxt}] + lrvSteps[nxt][[2]], nxt}
		]
	];
	
	lrvSteps[#][[3]] & /@ Range[n]
];





(* Edges that are valleys in the diagram. *)
(* Same as inner corners *)
(* This should be deprecated *)
ValleyEdges[edgeList_List, n_Integer] := Select[
	edgeList,
	!MemberQ[edgeList, {#[[1]], Mod[#[[2]], n] + 1}] &&
	!MemberQ[edgeList, {Mod[#[[1]] - 2, n] + 1, #[[2]]}] &
];

(* Corners that are part of the area. *)
InnerCorners[area_List] := Module[{n = Length@area, edgeList},
	edgeList = AreaToEdges@area;
	Select[edgeList,
		!MemberQ[edgeList, {#[[1]], Mod[#[[2]], n] + 1}] &&
		!MemberQ[edgeList, {Mod[#[[1]] - 2, n] + 1, #[[2]]}] &]
];

(* Corners that are not part of the area. *)
OuterCorners[area_List] := Module[{n = Length@area,edgeList, nonEdges},
	edgeList = Join[AreaToEdges@area, Table[{k, k}, {k, n}]];
	nonEdges = Complement[AreaToEdges[ConstantArray[n - 1, n]], edgeList];
	Select[nonEdges,
		MemberQ[edgeList, {#[[1]], Mod[#[[2]] - 2, n] + 1}] &&
		MemberQ[edgeList, {Mod[#[[1]], n] + 1, #[[2]]}] &]
];

AreaToTopBounceShape[{}] := {};
AreaToTopBounceShape[area_List] := Module[{k},
	k = area[[1]];
	Join[ Range[k, 0, -1], AreaToTopBounceShape[area[[k+2;; ]]]]
];

AreaToBounceShape[{}] := {};
AreaToBounceShape[area_List] := Module[{ra = Reverse[area], n = Length@area, k},
	k = LengthWhile[ra - Range[n] + 1, # >= 0 &];
	Join[AreaToBounceShape[area[[;; -k - 1]]], Range[k - 1, 0, -1]]
];
   
AreaBounce[bounceLike_List] := Tr@Most[(Join @@ Position[AreaToBounceShape@bounceLike, 0])];

AreaDinv[areaList_List] := 
  Module[{n = Length@areaList, area = Reverse@areaList},
   Sum[
    Boole[area[[i]] == area[[j]]]
     +
     Boole[area[[i]] == area[[j]] + 1]
    , {i, n}, {j, i + 1, n}]
   ];


Options[GraphChromaticSymmetricPolynomial] = {Weights->{}};
GraphChromaticSymmetricPolynomial[area:{_Integer ..}, x_, q_: 1, opts:OptionsPattern[]] :=
GraphChromaticSymmetricPolynomial[AreaToEdges@area, Length@area,x,q,opts];

GraphChromaticSymmetricPolynomial[edges_List, n_Integer, x_, q_: 1,opts:OptionsPattern[]] :=
	GraphChromaticSymmetricPolynomial[edges, n, x, q] = Module[{w,nn},
    w = OptionValue[Weights];
    
    nn = If[w==={}, n, Tr@w];
    
	Sum[
	If[w==={},
        q^GraphColoringAscents[edges, c] (Times @@ (x /@ c))
        ,
        q^GraphColoringAscents[edges, c] (Times @@ ((x /@ c)^w))
    ]
    , {c, GraphChromaticSymmetricColorings[edges, n, False, nn]} ]
];

HomogeneousGraphChromaticSymmetricPolynomial[edges_List, n_Integer, x_, q_: 1,t_:1] :=
   HomogeneousGraphChromaticSymmetricPolynomial[edges, n, x, q,t] = 
   Sum[
   With[{asc=GraphColoringAscents[edges, c]},
	   q^asc t^(Length[edges]-asc) (Times @@ (x /@ c))
   ]
   ,{c, GraphChromaticSymmetricColorings[edges, n, False]}];


Options[GraphChromaticLLTPolynomial] = {StrictEdges -> {}, WeakEdges->{}};
GraphChromaticLLTPolynomial[attacking_List, n_Integer, x_, q_: 1, opts:OptionsPattern[]] := 
	GraphChromaticLLTPolynomial[attacking, n, x, q, opts] = Module[{colorings,strict,weak,qEdges},
	
	strict = OptionValue[StrictEdges];
	weak = OptionValue[WeakEdges];
	
	
	colorings = GraphChromaticSymmetricColorings[Range[n-1,0,-1], n, True, n];
	If[Length[strict]>0 || Length[weak]>0,
		colorings = Select[colorings, 
			GraphColoringAscents[strict, #] == Length[strict] && GraphColoringAscents[weak, #] == 0
		&];
	];
	
	qEdges = Complement[attacking,strict];
	
	Sum[(Times @@ (x /@ c)) (q^GraphColoringAscents[qEdges, c]), {c, colorings}]
];

GraphChromaticLLTPolynomial[area:{_Integer ..}, x_, q_: 1, opts:OptionsPattern[]] :=
GraphChromaticLLTPolynomial[AreaToEdges@area, Length@area,x,q,opts];


Options[HomogeneousGraphLLTPolynomial] = {StrictEdges -> {}, WeakEdges->{}};
HomogeneousGraphLLTPolynomial[edges_List, n_Integer, x_, q_: 1, t_:1, opts:OptionsPattern[]] := 
	HomogeneousGraphLLTPolynomial[edges, n, x, q, t, opts] = Module[{colorings,strict,weak,qEdges,tEdges,asc},
	
	strict = OptionValue[StrictEdges];
	weak = OptionValue[WeakEdges];
	
	colorings = GraphChromaticSymmetricColorings[Range[n-1,0,-1], n, True, n];
	If[Length[strict]>0 || Length[weak]>0,
		colorings = Select[colorings, 
			GraphColoringAscents[strict, #] == Length[strict] && GraphColoringAscents[weak, #] == 0
		&];
	];
	
	qEdges = Complement[edges,strict];
	tEdges = Complement[edges,weak,strict];
	
	Sum[
		asc = GraphColoringAscents[qEdges, c];
		(Times @@ (x /@ c)) (q^asc) (t^(Length[tEdges]-asc)) 
	, {c, colorings}]
];

HomogeneousGraphLLTPolynomial[area:{_Integer ..}, x_, q_: 1, t_:1, opts:OptionsPattern[]] :=
HomogeneousGraphLLTPolynomial[AreaToEdges@area, Length@area,x,q,t,opts];




(*
GraphChromaticLLTPolynomial[edges_List, n_Integer, x_, q_: 1] := 
  GraphChromaticLLTPolynomial[edges, n, x, q] = 
   Sum[q^GraphColoringAscents[edges, c] (Times @@ (x /@ c)), {c, 
     GraphChromaticSymmetricColorings[edges, n, True, n]}];
*)



(* This allows to specify a shape where we force ATTACKING in the shape edgesEq \ edges2. *)
GraphChromaticLLTPolynomialAttacking[edgesEq_List, edges2_List, n_Integer, x_, q_: 1] := GraphChromaticLLTPolynomialAttacking[edgesEq, edges2, n, x, q] = 
	Module[{isIncreasingQ, colorings, attackingEdges},
	
	attackingEdges = Complement[edgesEq, edges2];
	
	isIncreasingQ[edges_, col_] := And @@ Table[col[[e[[1]]]] < col[[e[[2]]]], {e, edges}];
	
	(* All colorings. *)

	colorings = GraphChromaticSymmetricColorings[attackingEdges, n, True, n];
	colorings = Select[colorings, isIncreasingQ[attackingEdges, #] &];
	
	(* Enough to check ascents wrt edges2 *)
	
	Sum[q^GraphColoringAscents[edges2, c] (Times @@ (x /@ c)), {c, colorings}]
];



(* Given horizontal strip sizes, produce the Dyck-path model edges. *)
(* Also works with skew strips *)
(* TODO: Rewrite! *)
StripSizesToEdges[sizes : {__Integer}] := StripSizesToEdges[Table[{s, 0}, {s, sizes}]];
StripSizesToEdges[sizes : {{_Integer, _Integer} ..}] := Module[{n, m = Length@sizes, tabCoords,
	cellData, inOrder, table,area, attacking, strict, r, c, k, att},
	
	tabCoords = Join @@ Table[
		If[sizes[[r, 1]] >= c > sizes[[r, 2]], {r, c}, Sequence @@ {}],
		{r, m}, {c, Max[sizes]}];
	
	inOrder = SortBy[tabCoords, {#[[2]] &, -#[[1]] &}];
	
	cellData = MapIndexed[Join[#1, #2] &, inOrder];
	n = Length@cellData;
	
	attacking = Sort /@ (Join @@ Table[{r, c, k} = cellData[[i]];
		att = Select[cellData, #[[1]] > r && c <= #[[2]] <= c + 1 &];
		Table[{i, a}, {a, Last /@ att}], {i, n}]
	);
	
	strict = Sort /@ (Join @@ Table[{r, c, k} = cellData[[i]];
		att = Select[cellData, #[[1]] == r && #[[2]] == c + 1 &];
		Table[{i, a}, {a, Last /@ att}], {i, n}]
	);
	
	
	(* Find area sequence. Includes the stric edges! *)
	area = Table[
		Max[Last /@ Select[Join[attacking,strict], First[#] == k &] - k, 0]
		, {k, Tr[#1-#2&@@@sizes]}];
	
	{area, strict}
];


VerticalStripLLTColorings[sizes_List] := Module[{n, maxCol, colorings, strict},
	n = Tr[sizes];
	maxCol = n;
	colorings = Tuples[Range@maxCol, n];
	strict = Last@StripSizesToEdges[sizes];
	
	(* Force strict edges to be strict. *)
	Select[colorings, GraphColoringAscents[strict, #] == Length[strict] &]
];

VerticalStripLLTPolynomial[sizes_List, x_, q_:1] := VerticalStripLLTPolynomial[sizes, x, q] = 
	Module[{colorings, attacking},
		colorings = VerticalStripLLTColorings[sizes];
		attacking = First@StripSizesToEdges[sizes];
		Sum[
		(Times @@ (x /@ c)) (q^GraphColoringAscents[attacking, c]), {c, colorings}]
];





(* Private *)
(*
GraphPsiPolynomialInE[edges_List, nn_Integer, ee_, q_: 1] := GraphPsiPolynomialInE[edges, ee, q] = 
	Module[{toPartition, pol, inE, evs, ep, x},
		pol = GraphChromaticSymmetricPolynomial[edges, x, q, nn];
		ToElementaryBasis[pol, x, ee]
];
*)


(* Only works for partitions! *)
PartitionRookPlacements[lam_List] := With[{n = Length@lam},
	Select[Permutations[Range[n]], 
		And @@ Table[lam[[i]] >= #[[i]], {i, n}] &]
];

(* Given diagram, find all permutations that stay inside the diagram. *)
(*
DiagramRookPlacements[diagram_List, n_Integer] := Select[Permutations[Range[n]], And @@ Table[MemberQ[diagram, {i, #[[i]]}], {i, n}]&];
*)


(* Generates all ways to put n rooks on the diagram in a non-attacking fashion *)
(* Generated as list of subsets of the diagram. *)
DiagramRookPlacements[diagram_List, 0] := {{}};
DiagramRookPlacements[{}, n_Integer] := If[n == 0, {{}}, {}];
DiagramRookPlacements[diagram_List, n_Integer] := 
  Module[{minR, maxR, rookPlaceComplement},
   minR = Min[First /@ diagram];
   maxR = Max[First /@ diagram];
   
   rookPlaceComplement[diag_, {r_, c_}] := 
    Select[diagram, #[[1]] != r && #[[2]] != c &];
   
   Which[
   
    (* Only one 'row', but more than one rook, no placements *)
    minR == maxR && n > 1, {},
    
    (* One rook, one row, pick one square. *)
    minR == maxR && n == 1, List /@ diagram,
    True,
    
    Join[
     (* Put a rook in top row *)
     Join @@ Table[
       Append[#, sq] & /@ 
        DiagramRookPlacements[rookPlaceComplement[diagram, sq], 
         n - 1]
       , {sq, Select[diagram, #[[1]] == minR &]}]
     ,
     
     (* Don't put a rook in top row *)
     DiagramRookPlacements[
      rookPlaceComplement[diagram, {minR, Infinity}], n]
     ]
    ]
];




SouthWestDiagram[perm_List] := With[{n = Length@perm},
Union[Join @@ Table[
      Join[
       (* West *)
       Table[{r, k}, {k, perm[[r]]}]
       ,
       (* South *)
       Table[{k, perm[[r]]}, {k, r + 1, n}]
       ]
      , {r, n}]
    ]
];

SouthEastDiagram[perm_List] := With[{n = Length@perm},
Union[Join @@ Table[
      Join[
       (* East *)
       Table[{r, k}, {k, perm[[r]],n}]
       ,
       (* South *)
       Table[{k, perm[[r]]}, {k, r + 1, n}]
       ]
      , {r, n}]
    ]
];



AthanasiadisUnimodalSets[{mu__, 0}] := AthanasiadisUnimodalSets[{mu}];
AthanasiadisUnimodalSets[lambda_List] := Module[
   {IsLambdaUnimodalQ, svec, rangeSet, int, n = Tr@lambda},
   svec[0] := 0;
   svec[k_] := Tr@lambda[[;; k]];
   
   IsLambdaUnimodalQ[ss_List] := And @@ Table[
      rangeSet = Range[svec[i] + 1, svec[i + 1] - 1];
      int = Intersection[ss, rangeSet];
      int == rangeSet[[;; Length@int]]
      , {i, 0, Length[lambda] - 1}];
   
   Select[Subsets[Range[n - 1]], IsLambdaUnimodalQ]
   ];
   
AthanasiadisS[lambda_List] := Accumulate[Most@lambda];


End[(* End private *)];
EndPackage[];
