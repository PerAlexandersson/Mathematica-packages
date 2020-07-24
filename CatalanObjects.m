(* ::Package:: *)

Clear["CatalanObjects`*"];


BeginPackage["CatalanObjects`"];

(* Generic circular combinatorial object. *)
CircularGraph;
CircularGraphPlot;
CircularGraphShortEdgeSet;
CircularGraphEdges;
CircularGraphProperEdges;
CircularGraphLoops;
CircularGraphRotate;
CircularGraphComponents;
CircularGraphCrossings;
CircularGraphReflectLabels;

PerfectMatchings;
PerfectMatchingCrossings;

NonCrossingMatchings;
NonCrossingMatchingsTypeB;
NCMToDyckPath;
NCMRotate;
NCMMajorIndex;
NCMPeaks;
NCMFaces;



(* 
Todo: implement kreweras complement:
http://de.arxiv.org/pdf/1904.05573.pdf
*)

NonCrossingPartitions;
NonCrossingPartitionsTypeB;
NCPRotate;
NCPToPerfectMatching;
NCPBlocks;
NCPToDyckPath;

NCPRestrictedGrowthFunction;
NCPRightBiggerStatistic;
NCPLeftBiggerStatistic;
NCPMajorLikeStat;


StanleyCatalan60;
StanleyCatalan60Action;
StanleyCatalan60Rotate;
StanleyCatalan60Flip;
StanleyCatalan60EdgesAndLoops;
Stanley60TypeB;

NgonTriangulations;

DyckAreaLists;
AreaToDyckPath;
AreaTranspose;
AreaListPlot;
Labels;
AreaToBounceShape;
AreaBounce;
AreaRemoveVertices;


DyckPath;
DyckPaths;
DyckCoordinates;
DyckPlot;
DyckPathHeight;
DyckMajorIndex;
DyckPeaks;
DyckValleys;
DyckPathRemoveVertices;
SchroederPaths;
SchroederPathUpSteps;
SchroederReverse;


IncreasingParkingFunctions;
ParkingFunctions;

NCFComponents;
NCFVertexDegree;
NonCrossingForests;
NCFRotate;


RookPlacements;
RookInversions;
RookInversionList;
AreaToFerrersBoard;
RookPlacementPlot;

Begin["Private`"];


CircularGraph::usage = "Head for object representing a circular object. It is of the form CircularGraph[n,edgeList].";

(* Canonicalize the representation. *)
CircularGraph[n_Integer, edges_List] := 
CircularGraph[n, 
	MapAll[If[ListQ@#, Sort@#, #]&, edges]
] /; !OrderedQ[edges] || Not[And@@(OrderedQ/@edges)];


(* Make formatting *)
CircularGraph/:Format[CircularGraph[n_Integer,edges_List]]:=CircularGraphPlot[CircularGraph[n,edges]];
CircularGraphPlot::usage="CircularGraphPlot[circularGraph] gives a graphical representation of the graph.";


Options[CircularGraphPlot]={VertexLabeling->True,Circle->True};
CircularGraphPlot[CircularGraph[d_Integer,edgeList_List],opts:OptionsPattern[]] := 
Module[{edgeMultColor,coord, i, rad, pts, lines,smallCircs, lbls},
	
	coord[i_Integer, rad_:1] := rad {Cos[2 Pi (i - 1)/d], Sin[2 Pi (i - 1)/d]};
	coord[il_List,rad_:1]:=coord[#,rad]&/@il;
	
	pts = { EdgeForm[Black], White, Table[Disk[coord[i, 1],0.14], {i, Range[d]}]};
	
	edgeMultColor[e_List]:=Switch[
		Count[edgeList,e],
			0, White,
			1, Black,
			2, Blue,
			3, Red,
			_, Pink
	];
	
	graphicObjects = Table[
		Which[
			Length[e]==1 || (Length[e]==2 && e[[1]] == e[[2]]),
				{EdgeForm[edgeMultColor[e]], LightBlue,Disk[ coord[First@e], 0.2]},
			
			Length[e]==2,
				{Thickness[0.015], edgeMultColor[e] , Line@coord@e },
			
			(* This is used for non-crossing partitions *)
			Length[e]>2,
				{EdgeForm[edgeMultColor[e]], LightBlue, Polygon@coord@e},
			True,
				{Print["Unknown edge, ", e];{}}
			
		]
	,{e,edgeList}];
	
	lbls = {{FontSize -> Scaled[0.13], Table[Text[i, coord[i, 1.0]], {i, d}]}};
	
	Graphics[{
			If[OptionValue[Circle], {Black,Circle[{0,0},1]},{}]
			,
			graphicObjects, pts, 
			If[OptionValue[VertexLabeling],lbls,{}]
		},
	ImageSize->120]
];


CircularGraphShortEdgeSet::usage = 
"CircularGraphShortEdgeSet[cg] returns the list of vertices v, such that (v,v+1) is an edge.";
CircularGraphShortEdgeSet[CircularGraph[n_Integer, edges_List]] := 
   Join@@Table[
		Select[e, MemberQ[e,#+1] &]
   , {e,edges}];

(*
CircularGraphMajorIndex::usage = "CircularGraph[gc] returns the the sum first vertex in the short edges.";
CircularGraphMajorIndex[cg_CircularGraph]:=Tr[CircularGraphShortEdgeSet@cg];
*)

CircularGraphEdges[CircularGraph[n_Integer, edges_List]]:=edges;
CircularGraphProperEdges[CircularGraph[n_Integer, edges_List]]:=Union@Select[
edges,#[[1]]!=#[[2]]&];
CircularGraphLoops[CircularGraph[n_Integer, edges_List]]:=Union@Select[
edges,#[[1]]==#[[2]]&];


CircularGraphRotate::usage = "CircularGraphRotate[gc,[steps]] rotates the configuration.";
CircularGraphRotate[CircularGraph[n_Integer, edges_List], steps_: 1] := 
CircularGraph[n, Map[1+Mod[# + steps - 1, n]&, edges, {1}]];

CircularGraphComponents::usage = "CircularGraphComponents[gc] returns lists of connected components. This assumes we have a graph with edges.";
CircularGraphComponents[CircularGraph[n_Integer, edges_List]] := 
	ConnectedComponents@Graph[Range@n, UndirectedEdge @@@ Select[edges,Length[#]==2&]];


CircularGraphCrossings::usage = "CircularGraphCrossings[gc] returns the number of crossings.";
CircularGraphCrossings[CircularGraph[n_Integer, edges_List]] := 
Module[{crossQ},
	crossQ[{a_Integer, b_Integer}, {c_Integer, d_Integer}] := (a < c < b < d) || (c < a < d < b);
	Sum[
		Boole[crossQ@@ss]
	,{ss,Subsets[edges,{2}]}]
];


CircularGraphReflectLabels::usage = "CircularGraphReflectLabels[cg] sends vertex j to n+1-j.";
CircularGraphReflectLabels[CircularGraph[n_Integer, edges_List]] := 
CircularGraph[n,edges/.{j_Integer:>n+1-j}];

(**********************************************************************)

PerfectMatchings::usage = "PerfectMatchings[n] returns the list of perfect matchings with 2n vertices. Can also use area sequence as argument.";

PerfectMatchings[0] := {CircularGraph[0, {}]};
PerfectMatchings[n_Integer] := PerfectMatchings[n] =
   (* We need to add vertices 2n-1 and 2n *)
   Flatten[Table[
     With[{pairs = pm[[2]]},
      If[k == n,
       CircularGraph[2 n, Append[pairs, {2 n - 1, 2 n}]]
       ,
       With[{a = pairs[[k, 1]], b = pairs[[k, 2]]},
        {
         CircularGraph[2 n, 
          Join[Drop[pairs, k], {{a, 2 n - 1}, {b, 2 n}}]],
         CircularGraph[2 n, 
          Join[Drop[pairs, k], {{b, 2 n - 1}, {a, 2 n}}]]
         }]]]
     , {k, n}, {pm, PerfectMatchings[n - 1]}]
];

(* Perhaps better to make these from rook placements? *)
PerfectMatchings[area_List] := PerfectMatchings[area, Length@area];
PerfectMatchings[area_List, k_Integer] := Module[{n = Length@area, bottom, top},
   bottom = 2 Range[n] - 1 - area;
   top = Complement[Range[2 n], bottom];
   
   Join @@ Table[
     Join @@ Table[
       CircularGraph[2 n, Transpose[{#, start}]] & /@
        
        Select[Permutations[tt], And @@ Thread[# > start] &]
       ,
       {tt, Subsets[top, {Length@start}]}]
     , {start, Subsets[bottom, {k}]}]
];


PerfectMatchingCrossings::usage = "PerfectMatchingCrossings[pm] returns the vector with number of crossings.
The sum of entries is CircularGraphCrossings[pm].";

(*{1, 3}, {2, 4}*)
PerfectMatchingCrossings[pm_CircularGraph] := With[{pairs = pm[[2]]},
	Table[
		Length@Select[pairs, #[[1]] < p[[1]] < #[[2]] < p[[2]] &]
	, {p, SortBy[pairs,First]}]
 ];


(**********************************************************************)

(* 
Private function to rotate any structure which consists of a list of lists.
Rotation means adding +s on all entries, and then sorting.
*)
RotationHelper[struct_List,s_Integer:1]:= With[{n = Max@struct},
	Sort[Sort /@ Map[1 + Mod[# + s - 1 , n] &, struct, {2}]]
];


NonCrossingMatchings::usage="NonCrossingMatchings[n] returns a list of 
all non-crossing matchings of size n.
Non-crossing partitions with k peaks is Narayana(n,k).";

NonCrossingMatchings[0] := {CircularGraph[0,{}]};
NonCrossingMatchings[1] := {CircularGraph[2,{{1,2}}]};
NonCrossingMatchings[n_Integer] := NonCrossingMatchings[n] = 
Reap[
	Do[
		Outer[
			Sow[
				CircularGraph[2n,
				Join[
				1 + #1,
					{{1, 2 k + 2}},
				(2 k + 2) + #2
				]]
			]&
		,
		CircularGraphEdges/@NonCrossingMatchings[k], 
		CircularGraphEdges/@NonCrossingMatchings[n - 1 - k], 1]
	, {k, 0, n - 1}]
][[-1,1]];


NonCrossingMatchingsTypeB[n_Integer] := NonCrossingMatchingsTypeB[n] = Select[
	NonCrossingMatchings[2 n], CircularGraphRotate[#, 2 n] == # &];

NCMToDyckPath::usage="NCMToDyckPath[matching] returns a Dyck path.";
NCMToDyckPath[CircularGraph[n_Integer,ncm_List]] := Module[{downSteps},
	downSteps = Last /@ ncm;
	DyckPath@Normal@SparseArray[
		downSteps->ConstantArray["e", n/2],
	{n},"n"]
];

NCMRotate::usage="NCMRotate[matching,s] rotates the matching s steps. 
The default value for s is 2. Promotion correspond to s=1.";

NCMRotate[cg_CircularGraph, steps_: 2] := CircularGraphRotate[cg, steps];

(* Same as MajorIndex[ NCMToDyckPath@match]] *)
NCMMajorIndex[CircularGraph[n_,edges_]] := Sum[
		i*Boole[ MemberQ[edges, {_Integer, i}] && MemberQ[edges,{i+1, _Integer}] ]
	,{i, n - 1}];


NCMPeaks::usage="NCMPeaks[matching] returns 
the number of peaks (in the corresponding Dyck path). Same as number of instances of {i,i+1}.";
NCMPeaks[cg_CircularGraph] := Length@CircularGraphShortEdgeSet[cg];

NCMFaces::usage = "NCMFaces[ncm] returns the list of faces (as vertex sets)";
NCMFaces[CircularGraph[2, {{1,2}}]] = {{1,2},{1,2}}; (* This is a special case, where we have two
regions determined by the same single edge. *)
NCMFaces[CircularGraph[n_, ncm_]] := 
  Module[{crossQ, faces},
   crossQ[e_] := 
    crossQ[e] = 
     CircularGraphCrossings[CircularGraph[n, Append[ncm, e]]] > 0;
   faces = Join @@ Table[
      With[{v = First@e, w = Last@e},
       {
        Select[Range[v, w], ! crossQ[{v, #}] &] ,
        Select[Join[Range[w, n], Range[v]] , ! crossQ[{v, #}] &]
        }]
      , {e, ncm}];
   Union[Sort /@ faces]
   ];


(* Another Catalan family. *)
NonCrossingPartitions::usage="NonCrossingPartitions[n] returns non-crossing matchings. 
There are Cat(n) many. Non-crossing partitions with k components is Narayana(n,k)."


NonCrossingPartitions[n_Integer]:= NonCrossingPartitions[n] = Map[
CircularGraph[n,#]&, NonCrossingPartitionsHelper[n]];


NonCrossingPartitionsTypeB[n_Integer] := NonCrossingPartitionsTypeB[n] = Select[
	NonCrossingPartitions[2 n], CircularGraphRotate[#, n] == # &];


(* Need this helper, since it keeps track of order of edges. *)
NonCrossingPartitionsHelper[0] := {{}};
NonCrossingPartitionsHelper[1] := {{{1}}};
NonCrossingPartitionsHelper[n_Integer] := 
  NonCrossingPartitionsHelper[n] = Module[{k},
    Join @@ Table[
      Join @@ Outer[
        If[#2 == {},
          Join[#1, {{k + 1}}],
          Join[#1, Most@#2, {Prepend[Last@#2, k + 1]}]] &,
        NonCrossingPartitionsHelper[k],
        k + 1 + NonCrossingPartitionsHelper[n - 1 - k], 1]
      , {k, 0, n - 1}]
    ];



NCPRotate[cg_CircularGraph, steps_: 1] := CircularGraphRotate[cg,steps];

NCPBlocks::usage = "NCPBlocks[ncp] returns the number of blocks.";
NCPBlocks[CircularGraph[n_, blocks_List]]:=Length[blocks];

NCPToPerfectMatching::usage = "NCPToPerfectMatching maps a non-crossing partition to a non-crossing perfect matching.";

NCPToPerfectMatching[CircularGraph[n_, blocks_List]] := 
  Module[{e, s},
   s[1] := 2 n;
   e[1] := 1;
   s[i_] := 2 i - 2;
   e[i_] := 2 i - 1;
   CircularGraph[2 n,
    Join @@ Table[
      {e@#1, s@#2} & @@@ Partition[Append[block, First@block], 2, 1]
      , {block, blocks}]
    ]];

(* p.5 https://www.emis.de/journals/EJC/Volume_18/PDF/v18i1p83.pdf *)

NCPToDyckPath::usage = "NCPToDyckPath[ncp] produces a Dyck path, where number of blocks is sent to the number of peaks. This map is NOT the same as sending NCP to NCM, and then using NCM to Dyck.";
NCPToDyckPath[CircularGraph[n_, blocks_]] := Module[{data},
data = Prepend[{Max[#], Length[#]} & /@ SortBy[blocks, Max], {0, 0}];
DyckPath[
	Join @@ Table[
		Join[
		ConstantArray["n", data[[j, 1]] - data[[j - 1, 1]]],
		ConstantArray["e", data[[j, 2]]]
		]
		, {j, 2, Length@data}]
	]
];

	
(* 
https://pdfs.semanticscholar.org/e25a/4edb2a9c332717543331da06a1808595f95a.pdf
*)

NCPRestrictedGrowthFunction[CircularGraph[n_, ncp_List]] := With[
	{blocks = SortBy[ncp, Min]},
	Table[Position[blocks, i, 2, 1][[1, 1]], {i, n}]
];

NCPRightBiggerStatistic::usage = "NCPRightBiggerStatistic[ncp] is the rb-statistic.";
(*
NCPRightBiggerStatistic[ncp_CircularGraph] := With[
	{w = NCPRestrictedGrowthFunction[ncp]},
	Sum[
		Length@Union[Select[w[[i + 1 ;;]], # > w[[i]] &]]
	, {i, Length[w]}]
];
*)
(* Same as above, but shorter. *)
NCPRightBiggerStatistic[CircularGraph[n_, ncp_List]] := Tr[(Min/@ncp)-1];


NCPLeftBiggerStatistic::usage = "NCPLeftBiggerStatistic[ncp] is the lb-statistic. 
Summing over all non-crossing partitions with k parts gives the q-Narayana number.";
(*
NCPLeftBiggerStatistic[ncp_CircularGraph] := With[
	{w = NCPRestrictedGrowthFunction[ncp]},
	Sum[
		Length@Union[Select[w[[1 ;; i]], # > w[[i]] &]]
 , {i, Length[w]}]
];
*)
NCPLeftBiggerStatistic[CircularGraph[n_, ncp_List]] := (Length/@SortBy[ncp,Min]).Range[0,(Length@ncp)-1];


(*
Same as
n (Length[ncp[[2]]] - 1) + NCPLeftBiggerStatistic[ncp] - 
   NCPRightBiggerStatistic[ncp]) 
*)
NCPMajorLikeStat::usage = "NCPMajorLikeStat[ncp] gives a statistic equidistributed with maj on Dyck paths, and summing over fixed number of blocks give the q-Narayana.";
NCPMajorLikeStat[CircularGraph[n_, ncp_List]]:=(
	n*(Length[ncp]-1) + 
	(Length/@SortBy[ncp,Min]).Range[0,(Length@ncp)-1]
	- Tr[(Min/@ncp)-1]
);


StanleyCatalan60::usage = "StanleyCatalan60[n] gives Catalan elements,
which are certain configurations of n-1 vertices on a circle.
Some vertices are connected by edges or self-loops. 
These must be non-intersecting, even at the end-points.

Example: CircularGraph[4,{{1,2},{3,3}}] is a configuraton on 4 vertices,
with an edge, a self-loop.";

StanleyCatalan60[0] = {CircularGraph[-1,{}]};
StanleyCatalan60[1] = {CircularGraph[0,{}]};
StanleyCatalan60[2] = {CircularGraph[1,{{1,1}}],CircularGraph[1,{}]};

StanleyCatalan60[n_Integer] := StanleyCatalan60[n] = Join[
	(* Add one new large vertex. *)
	StanleyCatalan60[n - 1]/.CircularGraph[i_Integer,edges_]:>CircularGraph[n-1,edges]
	,
	Reap[
		Do[
			(* n-1 is connected to k+1. *)
			Outer[
				Sow@CircularGraph[n-1,
					Join[#1, k + 1 + #2, {{k + 1, n - 1}}]]
				&,
				CircularGraphEdges/@ StanleyCatalan60[k + 1], 
				CircularGraphEdges/@ StanleyCatalan60[n - 2 - k], 1]
		, {k, 0, n - 2}]
	][[-1,1]]
];

StanleyCatalan60Rotate := CircularGraphRotate;

(* If 1 is marked, flip it to unmarked *)
StanleyCatalan60Flip[CircularGraph[n_,edges_]] := 
CircularGraph[n,
	Which[
		MemberQ[edges,{1,1}],
			DeleteCases[edges,{1,1}]
		,
		Length@Select[edges, Min[#]==1 &]==0,
			Append[edges,{1,1}]
		,
		True,
			edges
		]
];

StanleyCatalan60FlipAll[CircularGraph[n_,edges_]] := 
CircularGraph[n,
	Join[
		Select[edges, #[[1]]!=#[[2]]&]
		,
		{#,#}& /@ Complement[Range[n], Join@@edges]
		]
];

StanleyCatalan60Action[cg_CircularGraph] := StanleyCatalan60Rotate@StanleyCatalan60Flip@cg;

StanleyCatalan60Action[cg_, k_Integer] := Nest[StanleyCatalan60Action, cg, k];
  
(* This refinement gives the Narayana numbers. *)
StanleyCatalan60EdgesAndLoops[cg_CircularGraph]:=Length@CircularGraphEdges[cg];


(* Gives binom[2n,n] objects *)
Stanley60TypeB::usage = "Stanley60TypeB[n] returns a family where one edge might appear two times.";
Stanley60TypeB[n_Integer] := Stanley60TypeB[n] = Join[
	StanleyCatalan60[n]
	,
	Join @@ Table[
		CircularGraph[n - 1, Append[es, #]] & /@ 
		Select[es, #[[1]] != #[[2]] &]
		, {es, CircularGraphEdges /@ StanleyCatalan60[n]}]
];




NgonTriangulations::usage = "NgonTriangulations[n] returns a list of all triangulations of the (n+2)-gon.";

NgonTriangulations[0] := {CircularGraph[2, {}]};
NgonTriangulations[1] := {CircularGraph[3, {{1, 2, 3}}]};
NgonTriangulations[n_Integer] := NgonTriangulations[n] =
   Reap[
     Do[
      Outer[
       (* Subdivide according to triangle containing edge (1,n) *)
   
           Sow[CircularGraph[n + 2,
          Join[#1, {{1, n + 2, k + 2}}, k + 1 + #2]]
         ] &
       ,
       CircularGraphEdges /@ NgonTriangulations[k],
       CircularGraphEdges /@ NgonTriangulations[n - 1 - k], 1]
      , {k, 0, n - 1}]][[-1, 1]];


(*************************************************************)




DyckAreaLists::usage = "DyckAreaLists[n] returns all area sequences of Dyck paths of length n.";
DyckAreaLists[0] := {{}};
DyckAreaLists[1] := {{0}};
DyckAreaLists[n_Integer] := DyckAreaLists[n] =
   Flatten[Table[
     Outer[Join[#1, {0}, 1 + #2] &,
      DyckAreaLists[n - 1 - k], DyckAreaLists[k], 1]
     , {k, 0, n - 1}], 2];


(*
AreaToDyckPath[area_List] := With[{n=Length@area},
	1 - Normal[SparseArray[2 Range[n] - 1 - area -> ConstantArray[1, n], 2 n]]
];
*)

AreaToDyckPath[area_List] := With[{n=Length@area},
	DyckPath@Normal[
		SparseArray[
			2 Range[n] - 1 - area -> ConstantArray["n", n], 
			2 n,
			"e"]]
];

AreaTo312Avoiding[area_List] := Module[{isOK, areaC, n = Length@area},
	areaC = Reverse@AreaTranspose[area];
	isOK[p_] := And @@ Table[
		areaC[[i]] + i >= p[[i]] (* Below or on diagonal. *)
	, {i, n}];
	
	(* This is 312-avoiding. *)
	First@Select[Permutations[Range[n]], 
		isOK[#] && area[[#]] == areaC &, 1]
];


AreaTranspose::usage = "AreaTranspose[area] returns the transposed area list.";
AreaTranspose[area_List] := Module[{n = Length@area, cg, k},
	cg = Range[n] - 1;
	cg - Table[Count[cg - area, i_ /; i >= k], {k,n,1,-1}]
];


AreaToBounceShape[{}] := {};
AreaToBounceShape[area_List] := Module[{n = Length@area, k},
   k = LengthWhile[area - Range[n] + 1, # >= 0 &];
   Join[Range[0, k - 1], AreaToBounceShape[area[[k + 1 ;;]]]]];

AreaBounce[area_List] := 
  Tr@Most[(Join @@ Position[Reverse@AreaToBounceShape@area, 0])];


  
AreaRemoveVertices::usage = "AreaRemoveVertices[area, vList] returns the smaller area, where vertices have been removed.";
AreaRemoveVertices[area_List, {}] := area;
AreaRemoveVertices[area_List, v_Integer] := Module[{n = Length@area},
   Table[
    Which[
     i < v, area[[i]],
     i == v, Nothing,
     i > v && area[[i]] >= (i - v), area[[i]] - 1,
     True, area[[i]]
     ]
    , {i, n}]
   ];
AreaRemoveVertices[area_List, verts_List] := 
  Fold[AreaRemoveVertices, area, Sort[verts, Greater]];



AreaListPlot::usage = "AreaListPlot[area,opts] plots the area. Options are Circular and Labels";
Options[AreaListPlot]={Circular->False, Labels->{}};

AreaListPlot[areaList_List, opts:OptionsPattern[AreaListPlot]] := Module[{edgeToGridCoord,
	nn = Length@areaList, colors, values, r, c, bl, framed, boxLabels},
	
	(* Given directed edge a->b, return the grid {r,c} coord. *)
	edgeToGridCoord[a_Integer, b_Integer] := Which[
		a <= b, 
		{nn+1-b,nn-1+ a},
		True,Print["AreaListplot-not implemented"]; Abort[];
	];
	
	boxLabels = Table[edgeToGridCoord[bl[[1, 1]], bl[[1, 2]]] -> bl[[2]], {bl, OptionValue[Labels]}];
	
	colors = Join @@ Table[{r, c} -> Which[
		r + c <= nn, White,
		r + c >= 2 nn + 1, White,
		r + c == 2 nn, Yellow,
		r + c < 2 nn - areaList[[nn+1-r]], LightGray,
		True, LightBlue
		],{r, nn}, {c, 2 nn - 1}];
	
	values = Table[
		{r, c} /. boxLabels /. {{r, c} -> ""}
	,{r, nn}, {c, 2 nn - 1}];
	
	framed = Join@@Table[
		If[nn < r + c < 2 nn + 1,
			{r, c} -> True,
		Nothing
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



SchroederPaths::usage = "SchroederPaths[n] returns all Schroeder paths from (0,0) to (n,n) 
using steps in (n,e,d), and there are no diagonal steps on the main diagonal. The number of paths of length n is given by A001003.";

SchroederPaths[n_Integer] := SchroederPaths[n] = SchroederPaths[{n, n}, 0];
SchroederPaths[{h_Integer, v_Integer},d_Integer] := ({} /; (d < 0 || h < 0 || v < 0));
SchroederPaths[{0, 0}, 0] := {{}};
SchroederPaths[{h_Integer, v_Integer}, d_Integer] := 
Join[
	Prepend[#, "n"] & /@ SchroederPaths[{h, v - 1}, d + 1],
	Prepend[#, "e"] & /@ SchroederPaths[{h - 1, v}, d - 1],
	If[d > 0,Prepend[#, "d"] & /@ SchroederPaths[{h - 1, v - 1}, d], {}]
];



SchroederPathUpSteps::usage = "SchroderPathUpSteps[word] returns a list indices where up or diagonal steps appear.";
SchroederPathUpSteps[path_String] := 
  SchroederPathUpSteps@Characters[path];
SchroederPathUpSteps[path_List] := Join @@ Position[path, "n" | "d"];

SchroederReverse::usage = "SchroederReverse[path] returns the reversed path.";
SchroederReverse[path_List]:=Reverse[path]/.{"n"->"e","e"->"n"};


DyckPaths::usage = "DyckPaths[n] returns all Dyck paths of length n. A path is a list of n and e steps.";
DyckPaths[0] := {DyckPath@{}};
DyckPaths[1] := {DyckPath["ne"]};
DyckPaths[n_Integer] := DyckPaths[n] = Flatten[
	Table[
		Outer[
			DyckPath[Join[#1[[1]], {"n"}, #2[[1]], {"e"}]] &,
			DyckPaths[n - 1 - k], DyckPaths[k], 1]
	, {k, 0, n - 1}], 2];


DyckPath[neWord_String]:=DyckPath[Characters@neWord];

DyckPath/:Format[DyckPath[ne_List]]:=DyckPlot[DyckPath[ne]];


DyckCoordinates::usage = "DyckCoordinates[dp] returns the list of coordinates of path vertices.";

DyckCoordinates[DyckPath[dp_List]]:=DyckCoordinates[dp];
DyckCoordinates[dp_List] := Accumulate@Prepend[ReplaceAll[dp,
     {"n" -> {0, 1}, "e" -> {1, 0}, "d" -> {1, 1}}], {0, 0}];


DyckPlot[path_String] := DyckPlot[Characters@path];

DyckPlot[DyckPath[path_List]] := Module[
   {coords = DyckCoordinates[path], n},
   n = Max@coords;
   Graphics[{
     {LightGray, Polygon[Append[coords, {0, n}]]},
     {Gray, PointSize[0.02], 
      Point@(Join @@ Table[{c, r}, {r, 0, n}, {c, 0, r}])},
     {Dashed, Gray, Line[{{0, 0}, {n, n}}]},
     {Thick, Line[coords]},
     {PointSize[0.03], Point[coords]}
     }, ImageSize -> 50]
   ];
   
DyckPathHeight[p_DyckPath] := Max[#2 - #1 & @@@ DyckCoordinates[p]];

DyckMajorIndex::usage = "DyckMajorIndex[dp] where dp is a ne-path, returns sum of indices of valleys. Summing over all these gives the qCatalan number. This is same as major index of the word where n=0, e=1.";

DyckMajorIndex[DyckPath[dp_List]] := Tr[First /@ SequencePosition[dp, {"e", "n"}]];

DyckValleys[DyckPath[dp_List]] := Length[ SequencePosition[dp, {"e", "n"}] ];

DyckPeaks[DyckPath[dp_List]] := Length[ SequencePosition[dp, {"n", "e"}] ];


DyckPathRemoveVertices::usage = "DyckPathRemoveVertices[dyckPath,v] returns the smaller Dyck path, where north step v and east step v have been removed.";
DyckPathRemoveVertices[dp_DyckPath, v_List] := Fold[DyckPathRemoveVertices, dp, Reverse@v];
DyckPathRemoveVertices[DyckPath[dp_List], v_Integer] := 
Module[{ns, es, nni, eei, nn = Length[dp]},
	nni = SchroederPathUpSteps[dp];
	eei = Complement[Range[nn], nni];
	{ns, es} = {nni[[v]], eei[[v]]};
	Drop[Drop[dp, {es}], {ns}]
];

RationalDyckPaths::usage = "RationalDyckPaths[{m,n}] returns all n/e paths from {0,0} to {m,n} staying weakly above the diagonal.";

(* Todo: Make this use DyckPath *)
RationalDyckPaths[{m_Integer, n_Integer}] := RationalDyckPaths[{m, n}, n/m, {0, 0}];
(* Paths from (x,y) to (m,n) lying weakly above the line y=kx *)
RationalDyckPaths[{m_Integer, n_Integer},  k_, {x_Integer, y_Integer}] := 
  RationalDyckPaths[{m, n}, k, {x, y}] =
   Join[
    If[x == m && y == n, {{}}, {}]
    ,
    If[y >= k*(x + 1) && x < m,(* We can go east. *)
     
     Prepend[#, "e"] & /@ RationalDyckPaths[{m, n}, k, {x + 1, y}]
     , {}]
    ,
    If[y < n,
     Prepend[#, "n"] & /@ RationalDyckPaths[{m, n}, k, {x, y + 1}]
     , {}]
    ];

	
(*******************************************************************************)
	
IncreasingParkingFunctions::usage = "IncreasingParkingFunctions[n] returns all parking functions which are weakly increasing. This is a Catalan family.";

(*This is a Catalan family. *)
Clear[IncreasingParkingFunctions];
IncreasingParkingFunctions[1] := {{1}};
IncreasingParkingFunctions[n_Integer] := 
  IncreasingParkingFunctions[n] = Join @@ Table[
     Append[pf, #] & /@ Range[pf[[-1]], n]
     , {pf, IncreasingParkingFunctions[n - 1]}];

	 
ParkingFunctions::usage = "ParkingFunctions[n] returns all parking functions of length n.";
ParkingFunctions[n_Integer] := ParkingFunctions[n] = Join @@ Map[
     Permutations, IncreasingParkingFunctions[n]];
	
(*******************************************************************************)

   
NCFVertexDegree::usage = "NCFVertexDegree[forest] returns a list {d1,...,dn} such that di is the degree at vertex i.";
NCFVertexDegree[CircularGraph[n_,edges_]] := VertexDegree@Graph[Range@n, UndirectedEdge @@@ edges];

NonCrossingForests::usage = "NonCrossingForests[n] returns all non-crossing forests on n vertices.";
NonCrossingForests[1] := { CircularGraph[1, {}] };
NonCrossingForests[n_Integer] := NonCrossingForests[n] = Module[
	{isNonCrossingQ, admissibleEdges, edges, components},
	
	(* Assuming v1<v2, w1<w2 *)
	isNonCrossingQ[{v1_Integer, v2_Integer}, {w1_, w2_}] := Or[
		v1 < w1 < v2 < w2, 
		w1 < v1 < w2 < v2
	];
	
	isNonCrossingQ[f_List, {v1_, v2_}] := And@@Map[ ! isNonCrossingQ[{v1, v2}, #]&, f ];
	
	Reap[
		Do[
			edges = CircularGraphEdges[f];
			components = CircularGraphComponents[f];
			
			(* admissibleEdges contains all integers i, s.t. (i,n) does not cross any edge in f *)
			
			admissibleEdges = Select[Range[n - 1], isNonCrossingQ[ edges, {#, n}] &];
			
			(* Could be done better. *)
			Do[
				
				Which[
					ss == {}, 
						Sow@CircularGraph[n, edges ], (* Vertex n is isolated. *)
				
					(* Make sure the new edges does not make cycles *)
					And @@ Table[ Length[Intersection[cc, ss]] <= 1, {cc, components}],
						Sow@CircularGraph[n,Join[edges, {#, n} & /@ ss]]
				]
				
			, {ss, Subsets[admissibleEdges]}]
			
		, {f, NonCrossingForests[n - 1]}]
		
	][[-1,1]]
];

NCFComponents:=CircularGraphComponents;


NCFRotate::usage = "NCFRotate[forest,[s]] rotates the forest.";
NCFRotate = CircularGraphRotate;






RookPlacements::usage = "RookPlacements[board,r] returns a list of all possible ways to 
place r non-attacking rooks on a board (given as list of coordinates).";

RookPlacements[board_List]:=RookPlacements[board,Max@board];

RookPlacements[{}, 0] := {{}};
RookPlacements[squares_List, r_Integer] := {} /; (Length[squares] < r || r < 0);
RookPlacements[squares_List, r_Integer] := RookPlacements[squares, r] = With[
    {sq = First[squares]},
    (* Recurse, either first square is covered 
    by a rook, or it is not. *)
    Join[
     Prepend[#, sq] & /@ RookPlacements[
       (* Squares not in same row or col as sq *)
       
       Select[squares, Min@Abs@(sq - #) > 0 &]
       , r - 1]
     ,
     RookPlacements[Rest@squares, r]
     ]
];


RookInversions::usage = 
  "RookInversions[board,rooks] returns the squares on board which 
  are inversions with respect to the rooks. This assumes {r,c} 
coordinates.";

	 
RookInversions[board_List, pl_List] := Select[board, 
And @@ Table[
      With[{d = (# - p)*{1, -1}},(*Not in same row/col,or the non-
       zero coord is positive.*)(Min@Abs@d) > 0 || Tr[d] > 0], 
{p,pl}] &];


RookInversionList[board_List, pl_List]:=With[{invSet=RookInversions[board,pl],n=Max@board},
	Table[ Length@Select[invSet, #[[1]]==k &],{k,n}]
];


AreaToFerrersBoard[aa_List]:=With[{n=Length@aa},(Join @@ Table[{r, c}, {r, n}, {c, r - aa[[r]], n}])];


RookPlacementPlot[board_List, rp_List] := Module[{flip, data, n = Max@board},
	flip[{r_,c_}]:={n+1-r,c};
   data = Table[
     If[MemberQ[flip/@rp, {r, c}],
      "\[BlackRook]", ""
      ]
     , {r, n}, {c, n}];
   Grid[data, 
   ItemSize -> {1,1}, Alignment -> Center,
   Frame -> {None, None, flip[#] -> True & /@ board}]
];



End[(* End private *)];

EndPackage[];





