

(* ::Package:: *)
Clear["GraphTools`*"];

BeginPackage["GraphTools`",{"CombinatoricTools`"}];

KnGraph;
CompleteBipartiteGraph;
StirlingGraph;
FerrersBoardGraph;
TilingGraph;




ConnectedSimpleGraphs;
TreeGraphs;

KeepLoops;
KeepMultipleEdges;
GraphContractVertices;
GraphDeleteEdge;

GraphIndependetSets;
GraphIndependencePolynomial;
GraphMatchings;
GraphNonCrossingMatchings;
GraphNonNestingMatchings;
GraphMatchingPolynomial;
GraphPerfectMatchings;
GraphIndependentTriangles;

GraphTriangles;

ClawFreeQ;


GraphAcyclicOrientations;
GraphOrientations;
OrientationSinks;


AcyclicSinkPolynomial;
OrientationsSinkPolynomial;

Begin["`Private`"];

KnGraph::usage = "KnGraph[n] gives the complete graph on n vertices.";
KnGraph[n_Integer] := Join @@ Table[{i, j}, {i, n}, {j, i + 1, n}];

(* Datasets live in Data/ next to this file; the location is fixed when the package loads. *)
graphToolsDataDirectory = FileNameJoin[{DirectoryName[$InputFileName], "Data"}];

ConnectedSimpleGraphs::nodata = "Data file `1` was not found.";
TreeGraphs::nodata = "Data file `1` was not found.";
importGraphData[caller_Symbol, subdirectory_String, name_String] := With[
	{file = FileNameJoin[{graphToolsDataDirectory, subdirectory, name}]},
	If[FileExistsQ[file],
		Import[file, "Graph6"],
		Message[MessageName[caller, "nodata"], file]; $Failed]
];

ConnectedSimpleGraphs::usage = "ConnectedSimpleGraphs[n] returns a list of all non-isomorphic simple connected graphs on n vertices, for 1 <= n <= 9 (OEIS A001349); other n give Missing[\"NotAvailable\", n]. The data are read from Data/graphs.";

ConnectedSimpleGraphs[n_Integer] := Which[
   n == 1,
   {Graph[{1}, {}]},
   2 <= n <= 9,
   With[{result = importGraphData[ConnectedSimpleGraphs, "graphs", "graph" <> ToString[n] <> "c.g6"]},
      (* A file with a single graph imports as a Graph rather than a list. *)
      If[GraphQ[result], {result}, result]]
   ,
   True, Missing["NotAvailable", n]
];

TreeGraphs::usage = "TreeGraphs[n] returns a list of all non-isomorphic unlabeled trees on n vertices, for 1 <= n <= 20 (OEIS A000055); other n give Missing[\"NotAvailable\", n]. The data for n >= 5 are read from Data/trees, taken from https://houseofgraphs.org/meta-directory/trees";

TreeGraphs[1] := {Graph[{1}, {}]};
TreeGraphs[2] := {Graph[{1,2},{UndirectedEdge[1,2]}]};
TreeGraphs[3] := {Graph[{1,2,3},{UndirectedEdge[1,2],UndirectedEdge[2,3]}]};
TreeGraphs[4] := {
  Graph[{1,2,3,4},{UndirectedEdge[1,2],UndirectedEdge[2,3],UndirectedEdge[3,4]}],
  Graph[{1,2,3,4},{UndirectedEdge[1,2],UndirectedEdge[1,3],UndirectedEdge[1,4]}]
};

TreeGraphs[n_Integer] := With[{result = Which[
   5 <= n <= 20,
   importGraphData[TreeGraphs, "trees", "trees" <> ToString[n] <> ".g6"]
   ,
   True, Missing["NotAvailable", n]
   ]},
   If[result === $Failed || Head[result] === Missing,
      result,
      TreeGraphs[n] = result]
];


CompleteBipartiteGraph::usage = "CompleteBipartiteGraph[a,b] or CompleteBipartiteGraph[{a,b,c,..}] returns the complete bipartite or n-partite graph with prescribed sizes, as lists of edges.";
CompleteBipartiteGraph[a_Integer, b_Integer] := CompleteBipartiteGraph[{a, b}];
CompleteBipartiteGraph[sizes_List] := With[
	{
	vertSets = PartitionList[Range@Total[sizes], sizes],
	edgesFunc := Join @@ Outer[List, #1, #2] &
	},
	Join @@ Table[edgesFunc @@ ss, {ss, Subsets[vertSets, {2}]}]
];


FerrersBoardGraph::usage = "FerrersBoardGraph[lam] returns the bipartite graph of the Ferrers board lam.
FerrersBoardGraph[lam,mu] returns the bipartite graph of the skew Ferrers board lam/mu.";
FerrersBoardGraph[lam_List] := Module[{rows, cols},
   rows = Range[Length[lam]];
   cols = Length[lam] + Range[Max[lam]];
   Graph[
    Join[rows, cols],
    Join @@ Table[
      Table[{i, cols[[j]]}, {j, lam[[i]]}]
      , {i, rows}]
    ]
   ];

FerrersBoardGraph[lam_List, muIn_List] := Module[{rows, cols, mu},
   mu = PadRight[muIn, Length@lam];
   rows = Range[Length[lam]];
   cols = Length[lam] + Range[Max[lam]];
   Graph[
    Join[rows, cols],
    Join @@ Table[
      Table[{i, cols[[j]]}, {j, mu[[i]] + 1, lam[[i]]}]
      , {i, rows}]
    ]
];


StirlingGraph::usage = "StirlingGraph[n] returns the edge list of a bipartite graph whose k-edge matchings are counted by S(n,k).";
StirlingGraph[n_Integer] := Join @@ Table[
		If[i < j, {i, n + j}, Nothing], {i, n}, {j, n}];


TilingGraph::usage = "TilingGraph[w,h,tile] returns the graph where valid placements of the tile on the rectangle are vertices,
and edges are overlaps of tiles.";
TilingGraph[w_Integer, h_Integer, tile_List] := 
  Module[{shiftTile, isOnQ,
    modTile, board, cells, nn, edges, t},
   (* Make a tile that contains the origin for sure. *)
   modTile = # - First[tile] & /@ tile;
   board = Join @@ Table[{i, j}, {i, w}, {j, h}];
   
   shiftTile[tt_, s_] := # + s & /@ tt;
   
   isOnQ[t_List] := And[
     And @@ Thread[1 <= (First /@ t) <= w],
     And @@ Thread[1 <= (Last /@ t) <= h]];
   
   (*Select all cells where if one place the origin of tile there,
   the entire tile is on the board. *)
   cells = Select[board, isOnQ[shiftTile[modTile, #]] &];
   nn = Length[cells];
   edges = Select[
     Subsets[Range@nn, {2}],
     IntersectingQ[shiftTile[modTile, cells[[First@#]]], 
       shiftTile[modTile, cells[[Last@#]]]] &];
   Graph[Range@nn, edges]
   ];

		
		
KeepLoops::usage = "KeepLoops is an option for GraphContractVertices; its default is False, and True preserves loops and multiple edges created by contraction.";
GraphContractVertices::usage= "GraphContractVertices[g,v] contracts all vertices in v into a single vertex named by the first entry of v. v can also be a directed edge, undirected edge, or rule. KeepLoops defaults to False.";

Options[GraphContractVertices] := {KeepLoops -> False};
GraphContractVertices[gg_Graph, DirectedEdge[u_,v_],opts:OptionsPattern[]]:=GraphContractVertices[gg,{u,v},opts];
GraphContractVertices[gg_Graph, UndirectedEdge[u_,v_],opts:OptionsPattern[]]:=GraphContractVertices[gg,{u,v},opts];
GraphContractVertices[gg_Graph, Rule[u_,v_],opts:OptionsPattern[]]:=GraphContractVertices[gg,{u,v},opts];
GraphContractVertices[gg_Graph, contr_List,opts:OptionsPattern[]] := With[{
	keep = OptionValue[KeepLoops],
    verts = VertexList[gg],
    ee = EdgeList@gg,
    f = (If[MemberQ[contr, #], First@contr, #] &)
    },
	
	Graph[
		Union[f /@ verts], 
		If[!keep, (* Make sure to remove multiple edges and loops. *)
			DeleteCases[Union[Map[f, ee, {2}]], a_[b_, b_]]
		,
			Map[f, ee, {2}]
		]
	]
];

KeepMultipleEdges::usage = "KeepMultipleEdges is an option for GraphDeleteEdge; its default is False, and True removes only one matching copy of an edge.";
GraphDeleteEdge::usage= "GraphDeleteEdge[g,e] removes e from graph g. With KeepMultipleEdges -> True, only one matching copy is removed; the default is False.";
Options[GraphDeleteEdge] := {KeepMultipleEdges -> False};
GraphDeleteEdge[gg_Graph, e_,opts:OptionsPattern[]] := With[{
    verts = VertexList[gg],
    ee = EdgeList@gg
},
	If[!OptionValue[KeepMultipleEdges],
		(* Remove every copy; an undirected edge may be given in either orientation. *)
		Graph[verts, DeleteCases[ee,
			e | If[Head[e] === UndirectedEdge, UndirectedEdge[e[[2]], e[[1]]], e]] ]
		,
		Graph[verts, DeleteCases[ee,  a_[e[[1]], e[[2]]] |  a_[e[[2]], e[[1]]]  , 1, 1] ]
	]
];


GraphIndependetSets::usage = "GraphIndependetSets[g] returns a list of all independent vertex sets in graph g.";
GraphIndependetSets[gg_Graph] := Module[{gmFnc, nbhd, vertices, edges},
   vertices = VertexList@gg;
   edges = EdgeList[gg] /. {DirectedEdge -> List, UndirectedEdge -> List};
   
   (* All neighbors of v *)
   nbhd[v_] := nbhd[v] = Union[Sequence @@ Select[edges, MemberQ[#, v] &], {v}];
   
   (*Empty graph, one independent set.*)
   gmFnc[{}] := {{}};
   gmFnc[ss_List] := With[{v = First@ss},
     Join @@ {
       gmFnc[Rest@ss], (* v is not in *)
       Append[#, v] & /@ gmFnc[Complement[ss, nbhd[v]]]}
     ];
   If[Length@vertices == 0, {}, gmFnc[vertices]]
];

GraphIndependencePolynomial::usage = "GraphIndependencePolynomial[g,t] returns the univariate independence polynomial of the graph g.";
GraphIndependencePolynomial[gg_Graph, t_] := Sum[t^Length[ss], {ss, GraphIndependetSets[gg]}];


GraphMatchings::usage = "GraphMatchings[edges] returns all matchings (as subsets of edges).";

GraphMatchings[gg_Graph,cond_:(True&)]:=GraphMatchings[
	EdgeList[gg]/.{UndirectedEdge->List,DirectedEdge->List},cond];

GraphMatchings[edges_List,cond_:(True&)] := Module[{gmFnc},
	(* Two cases, either first edge is in the matching, or not *)
	
	gmFnc[{}] := {{}};(* Empty graph, one matching. *)
	
	gmFnc[edLst_List] := With[{e1=First@edLst},
			Join @@ {
				gmFnc[Rest@edLst] (* First edge is not in the matching *)
				,
				Append[#, e1] & /@
					gmFnc[Select[edLst, Intersection[#, e1] == {} && cond[#,e1] &]]
			}];
	gmFnc[edges]
];

GraphMatchingPolynomial::usage = "GraphMatchingPolynomial[g,t] returns the univariate matching polynomial of the graph g.";
GraphMatchingPolynomial[gg_Graph, t_] := Sum[t^Length[ss],{ss, GraphMatchings[gg]}];

GraphTriangles::usage =  "GraphTriangles[edges] returns all 3-vertex subsets which are 3-cliques.";
GraphTriangles[edges_List] := With[{edgesSort = Sort /@ edges},
   With[
    {threeSS = Subsets[Union @@ edgesSort, {3}]},
    Select[threeSS, 
     Length[Intersection[edgesSort, Subsets[#, {2}]]] == 3 &]
]];


GraphNonCrossingMatchings::usage = "GraphNonCrossingMatchings[edges] returns all non-crossing matchings (as subsets of edges).";

GraphNonCrossingMatchings[gg_]:=
Module[{edgesNonCross},
	edgesNonCross[e1_,e2_]:=!Or[
		e1[[1]]<e2[[1]]<e1[[2]]<e2[[2]],
		e2[[1]]<e1[[1]]<e2[[2]]<e1[[2]]
	];
 	GraphMatchings[gg, edgesNonCross]
];

GraphNonNestingMatchings::usage = "GraphNonNestingMatchings[g] returns all matchings of g with no pair of nested edges.";
GraphNonNestingMatchings[gg_]:=
Module[{edgesNonNest},
	edgesNonNest[e1_,e2_]:=!Or[
		e1[[1]]<e2[[1]]<e2[[2]]<e1[[2]],
		e2[[1]]<e1[[1]]<e1[[2]]<e2[[2]]
	];
 	GraphMatchings[gg, edgesNonNest]
];


GraphPerfectMatchings::usage = "GraphPerfectMatchings[g] returns a list of perfect matchings of g.";

GraphPerfectMatchings[gg_Graph] := Module[{gmFnc, edges, verts},
   edges = EdgeList[gg] /. {UndirectedEdge -> List, DirectedEdge -> List};
   verts = VertexList[gg];
   
   (* No edge graph is perfect iff no vertices *)
   gmFnc[vl_List, {}] := If[Length[vl] == 0, {{}}, {}];
   gmFnc[vl_List, ed_List] :=
    If[Length[Complement[vl, Join @@ ed]] > 0,(* 
     Vertices left which cannot be matched. *)
     {}
     ,
     Join @@ {
       (* First edge is not in the matching *)
       gmFnc[vl, Rest@ed]
       ,
       (* First edge IS in the matching. *)
       With[{e = First@ed},
        Append[#, e] & /@ gmFnc[
          Complement[vl, e],
          Select[ed, Intersection[#, e] == {} &]
          ]]}];
   gmFnc[verts, edges]
];



(* Select all subsets of triangles, where no two triangle share a vertes. *)
GraphIndependentTriangles::usage = "GraphIndependentTriangles[edges] returns all collections of pairwise vertex-disjoint triangles in the graph with edge list edges.";
GraphIndependentTriangles[edges_List] := 
  Module[{tri = GraphTriangles@edges, gmFnc},
   (*Two cases, either first triangle is chosen, or not *)

   gmFnc[{}] := {{}};(*Empty graph, 
   gives the empty set of triangles .*)
   gmFnc[ed_] := Join @@ {
      gmFnc[Rest@ed] (*First triangle is not chosen *)
      ,
      Append[#, First@ed] & /@ 
       gmFnc[Select[ed, Intersection[#, First@ed] == {} &]]};
   gmFnc[tri]
];


(* Here, a struct is simply a (potentially ordered) subsets of vertices. *)
(* Return all possible ways to cover the graph with such subsets, where no two share a vertex. *)

(* 
I think this can be modeled as independence polynomial of some more abstract graph:
We construct a new graph, G=(V,E), V = allowed structs, E = structs 
sharing a vertex. 
So, what we get is simply the independence polynomial of G ? 
*)
GraphIndependentStructures[structs_List] := Module[{gmFnc},
   (* Two cases, either first struct is chosen, or not *)
   
   gmFnc[{}] := {{}};(* Empty graph, gives the empty set. *)
   gmFnc[ss_] := 
    Join @@ {gmFnc[Rest@ss] (* First struct is not chosen *), 
      Append[#, First@ss] & /@ 
       gmFnc[Select[ss, Intersection[#, First@ss] == {} &]]};
   gmFnc[structs]
   ];


	 
ClawFreeQ::usage = "ClawFreeQ[Graph[g]] or ClawFreeQ[edgesList] returns true iff the edges form a claw-free graph.";

ClawFreeQ[edgesIn_List] := ClawFreeQ[  Graph[UndirectedEdge @@@ edgesIn] ];
ClawFreeQ[g_Graph] := Catch[
	Do[
		With[{nbh = NeighborhoodGraph[g, a]},
			Do[
			If[EdgeCount@Subgraph[nbh, bcd] == 0, Throw@False]
			, {bcd,(* Take all 3-vertex subsets adjacent to a. 
				Claw-free means each bcd subgraph has at 
				least one edge present. *)
				Subsets[
				DeleteCases[VertexList[nbh], a]
				, {3}]}]
			];
		, {a, VertexList@g}];
	True
];


GraphAcyclicOrientations::usage = "GraphAcyclicOrientations[Graph[g]] returns all acyclic orientations of g, as directed graphs.";
GraphAcyclicOrientations[gg_Graph] := With[
	{n = VertexCount@gg,
	verts = VertexList@gg,
		(* Pretend all edges are directed. *)
	edges = DirectedEdge[#1, #2] & @@@ (EdgeList@gg)
	},
	(*
	Try all colorings with different colors. 
	Such colorings can only result in acyclic orientations.
	*)
	Union@Table[
		With[{rule = Thread[c -> Range@n]},
		Graph[verts,
			(If[
					Less @@ (# /. rule),
					#,
					Reverse@#]
				) & /@ (edges)
			]
		]
		, {c, Permutations@verts}]
];

GraphOrientations::usage = "GraphOrientations[Graph[g]] returns all orientations of g, as directed graphs.";
GraphOrientations[gg_Graph] := With[
	{
	verts = VertexList@gg,(*Pretend all edges are directed.*)
	edges = DirectedEdge[#1, #2] & @@@ (EdgeList@gg)
	},
	Table[
		Graph[verts,
			MapThread[If[#1, #2, Reverse@#2] &, {or, edges}, 1]
		]
	, {or, Tuples[{True, False}, Length@edges]}
	]
];


OrientationSinks::usage = "OrientationSinks[Graph[g]] returns the list of vertices which are sinks";
OrientationSinks[gg_Graph] := With[
	{verts = VertexList@gg,
	outVerts = First /@ EdgeList@gg},
	(*Sinks are vertices with no outgoing edges.*)
	
	Complement[verts, outVerts]
];

AcyclicSinkPolynomial::usage = "AcyclicSinkPolynomial[g,t] returns the polynomial whose coefficient of t^k counts acyclic orientations of g with k sinks.";
AcyclicSinkPolynomial[g_Graph,t_] := AcyclicSinkPolynomial[g,t] = Sum[t^Length[OrientationSinks@ao], {ao, GraphAcyclicOrientations[g]}];
OrientationsSinkPolynomial::usage = "OrientationsSinkPolynomial[g,t] returns the polynomial whose coefficient of t^k counts orientations of g with k sinks.";
OrientationsSinkPolynomial[g_Graph,t_] := OrientationsSinkPolynomial[g,t] = Sum[t^Length[OrientationSinks@ao], {ao, GraphOrientations[g]}];


End[(* End private *)];
EndPackage[];
