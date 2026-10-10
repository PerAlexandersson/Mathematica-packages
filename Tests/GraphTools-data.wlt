testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

VerificationTest[
  Needs["GraphTools`"],
  Null,
  TestID -> "GraphTools-data-loads-cleanly"
]

(* GitHub issue #8: datasets are found relative to the package, not ~/Dropbox. *)
VerificationTest[
  Length /@ ConnectedSimpleGraphs /@ Range[1, 7],
  {1, 1, 2, 6, 21, 112, 853},
  TestID -> "GraphTools-ConnectedSimpleGraphs-OEIS-A001349"
]

VerificationTest[
  Length /@ TreeGraphs /@ Range[1, 12],
  {1, 1, 1, 2, 3, 6, 11, 23, 47, 106, 235, 551},
  TestID -> "GraphTools-TreeGraphs-OEIS-A000055"
]

VerificationTest[
  And @@ (TreeGraphQ /@ TreeGraphs[9]) && And @@ (ConnectedGraphQ /@ ConnectedSimpleGraphs[5]),
  True,
  TestID -> "GraphTools-data-graphs-have-expected-type"
]

(* GitHub issue #9: rooted trees moved from TreesData. Counts follow OEIS A000081 and
   the trees for n = 7 are pairwise non-isomorphic as rooted (directed) trees. *)
VerificationTest[
  {Length /@ RootedTreeGraphs /@ Range[1, 10],
   And @@ (Function[g, VertexInDegree[g, 1] == 0 && TreeGraphQ[UndirectedGraph[g]] &&
       Count[VertexInDegree[g], 1] == VertexCount[g] - 1] /@ RootedTreeGraphs[7]),
   Length[DeleteDuplicates[RootedTreeGraphs[7], IsomorphicGraphQ]]},
  {{1, 1, 2, 4, 9, 20, 48, 115, 286, 719}, True, 48},
  TestID -> "GraphTools-RootedTreeGraphs-OEIS-A000081"
]
