

(* ::Package:: *)
Clear["GraphTools`*"];

BeginPackage["GraphTools`",{"CombinatoricTools`"}];

KnGraph;
CompleteBipartiteGraph;
StirlingGraph;
FerrersBoardGraph;

ConnectedSimpleGraphs;

KeepLoops;
KeepMultipleEdges;
GraphContractVertices;
GraphDeleteEdge;

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

Begin["Private`"];

KnGraph::usage = "KnGraph[n] gives the complete graph on n vertices.";
KnGraph[n_Integer] := Join @@ Table[{i, j}, {i, n}, {j, i + 1, n}];

ConnectedSimpleGraphs::usage = "ConnectedSimpleGraphs[n] returns a list of all simple connected graphs on n vertices.";

ConnectedSimpleGraphs[n_Integer] := Which[
   n == 2,
   List@Import[
     "~/Dropbox/mathematica-packages/graph" <>
      ToString[n] <> "c.g6"],
   3 <= n <= 9,
   Import["~/Dropbox/mathematica-packages/graph" <>
     ToString[n] <> "c.g6"]
   ,
   True, Missing
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

FerrersBoardGraph::usage = "FerrersBoardGraph[lam,mu] returns the bipartite graph associated with a Ferrers board.";
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


StirlingGraph::usage = "StirlingGraph[n] returns a bipartite graph, where matchings sizes are S(n,k).";
StirlingGraph[n_Integer] := Join @@ Table[
		If[i < j, {i, n + j}, Nothing], {i, n}, {j, n}];

GraphContractVertices::usage= "GraphContractVertices[Graph[g],v] constracts all vertices in v into a single vertex; the name of this vertex is the first entry in v. V can also be an edge.";

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

GraphDeleteEdge::usage= "GraphDeleteEdge[Graph[g],e] removes the edge e from the graph.";
Options[GraphDeleteEdge] := {KeepMultipleEdges -> False};
GraphDeleteEdge[gg_Graph, e_,opts:OptionsPattern[]] := With[{
    verts = VertexList[gg],
    ee = EdgeList@gg
},
	If[!OptionValue[KeepMultipleEdges],
		Graph[verts, Select[ee, First@#!=e[[1]] || Last@#!=e[[2]] & ] ]
		,
		Graph[verts, DeleteCases[ee,  a_[e[[1]], e[[2]]] |  a_[e[[2]], e[[1]]]  , 1, 1] ]
	]
];


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
   gmFnc[edges]
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


End[(* End private *)];
EndPackage[];
