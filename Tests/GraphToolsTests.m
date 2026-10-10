testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["GraphTools`"],
  Null,
  TestID -> "GraphTools-loads-cleanly"
]

(* GitHub issue #15: independent triangles must recurse over triangles, not edges. *)
VerificationTest[
  Module[{edges, triangles, brute},
    edges = Join[
      Tuples[{1, 2, 3}, 2],
      Tuples[{4, 5, 6}, 2]
    ];
    edges = Select[edges, #[[1]] < #[[2]] &];
    triangles = GraphTriangles[edges];
    brute = Select[
      Subsets[triangles],
      And @@ (Intersection[#[[1]], #[[2]]] === {} & /@ Subsets[#, {2}]) &
    ];
    Sort[Sort /@ GraphIndependentTriangles[edges]] === Sort[Sort /@ brute]
  ],
  True,
  TestID -> "GraphTools-GraphIndependentTriangles-pairwise-disjoint-triangles"
]

(* GitHub issue #15: an undirected edge must be deleted in either orientation. *)
VerificationTest[
  EdgeList[
    GraphDeleteEdge[
      Graph[{1, 2}, {UndirectedEdge[1, 2]}],
      UndirectedEdge[2, 1]
    ]
  ],
  {},
  TestID -> "GraphTools-GraphDeleteEdge-reverse-undirected-edge"
]

(* GitHub issue #15: failed tree imports must not create memoized DownValues.
   The failure is forced by pointing the data directory at a missing location. *)
VerificationTest[
  Block[{GraphTools`Private`graphToolsDataDirectory = "/nonexistent-graphtools-data"},
    Module[{before, first, second, after},
      before = Length[DownValues[TreeGraphs]];
      first = TreeGraphs[7];
      second = TreeGraphs[7];
      after = Length[DownValues[TreeGraphs]];
      {first, second, after === before}]],
  {$Failed, $Failed, True},
  {TreeGraphs::nodata, TreeGraphs::nodata},
  TestID -> "GraphTools-TreeGraphs-failed-import-not-memoized"
]

(* GitHub issue #15: the one-vertex connected graph is a valid case. *)
VerificationTest[
  ConnectedSimpleGraphs[1],
  {Graph[{1}, {}]},
  TestID -> "GraphTools-ConnectedSimpleGraphs-single-vertex"
]

(* GitHub issue #15: unavailable graph sizes return structured Missing data. *)
VerificationTest[
  ConnectedSimpleGraphs[10],
  Missing["NotAvailable", 10],
  TestID -> "GraphTools-ConnectedSimpleGraphs-out-of-range-missing"
]

(* GitHub issue #15: unavailable tree sizes return structured Missing data. *)
VerificationTest[
  TreeGraphs[21],
  Missing["NotAvailable", 21],
  TestID -> "GraphTools-TreeGraphs-out-of-range-missing"
]

(* GitHub issue #15: by default every copy of a multiple edge is removed; with
   KeepMultipleEdges -> True only one copy is removed. *)
VerificationTest[
  With[{g = Graph[{1, 2, 3}, {UndirectedEdge[1, 2], UndirectedEdge[1, 2], UndirectedEdge[2, 3]}]},
    {EdgeList[GraphDeleteEdge[g, UndirectedEdge[2, 1]]],
     Length@EdgeList[GraphDeleteEdge[g, UndirectedEdge[2, 1], KeepMultipleEdges -> True]]}],
  {{UndirectedEdge[2, 3]}, 2},
  TestID -> "GraphTools-GraphDeleteEdge-multiple-edges"
]
