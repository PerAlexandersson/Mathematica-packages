testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["UnicellularChromatics`"],
  Null,
  TestID -> "UnicellularChromatics-loads-cleanly"
]

(* GitHub issue #17: an area list with integer q must not dispatch as an edge list. *)
VerificationTest[
  UnicellularLLTSymmetric[{0, 1, 1}, 2],
  UnicellularLLTSymmetric[{0, 1, 1}, q] /. q -> 2,
  TestID -> "UnicellularChromatics-UnicellularLLTSymmetric-area-dispatch"
]

(* GitHub issue #17: SchroederOrientations must use the package's list-edge helper. *)
VerificationTest[
  SchroederOrientations["--++"],
  {{{2, 1}}, {{1, 2}}},
  TestID -> "UnicellularChromatics-SchroederOrientations-small-case"
]

(* GitHub issue #17: SchroederAcyclicOrientations must use the package's helper. *)
VerificationTest[
  SchroederAcyclicOrientations["--++"],
  {{{1, 2}}, {{2, 1}}},
  TestID -> "UnicellularChromatics-SchroederAcyclicOrientations-small-case"
]

(* GitHub issue #17: graph vertex labels are association keys, not positions. *)
VerificationTest[
  ChromaticSymmetric[Graph[{2 <-> 5, 5 <-> 7}]],
  ChromaticSymmetric[Graph[{1 <-> 2, 2 <-> 3}]],
  TestID -> "UnicellularChromatics-ChromaticSymmetric-association-lookup"
]

(* GitHub issue #17: strict is local to each UnicellularLLTSymmetric evaluation. *)
VerificationTest[
  Clear[UnicellularChromatics`Private`strict];
  UnicellularLLTSymmetric[{0, 1}, StrictEdges -> {{1, 2}}];
  ValueQ[UnicellularChromatics`Private`strict],
  False,
  TestID -> "UnicellularChromatics-UnicellularLLTSymmetric-local-strict"
]

(* GitHub issue #9: ChromaticFunctions is legacy, but its useful API is now
   available here with the 0-first area-list convention. *)
VerificationTest[
  Quiet[Needs["ChromaticFunctions`"]],
  Null,
  TestID -> "UnicellularChromatics-ChromaticFunctions-legacy-load"
]

VerificationTest[
  Module[{areas, edgeMap, old, new, n},
    areas = Flatten[Table[
        ChromaticFunctions`GraphAreaLists[n, All -> True, Circular -> False],
        {n, 1, 4}], 1];
    And @@ Table[
      n = Length[old];
      edgeMap[e_] := n + 1 - Reverse[e];
      new = Reverse[old];
      Sort[UnicellularChromatics`UnitIntervalEdges[new]] ===
        Sort[edgeMap /@ ChromaticFunctions`AreaToEdges[old]],
      {old, areas}]
  ],
  True,
  TestID -> "UnicellularChromatics-area-convention-edge-map"
]

VerificationTest[
  Module[{areas, old, new},
    areas = Flatten[Table[
        ChromaticFunctions`GraphAreaLists[n, All -> True, Circular -> False],
        {n, 1, 4}], 1];
    And @@ Table[
      new = Reverse[old];
      And[
        ChromaticFunctions`DinvFromAreaSeq[old] ===
          UnicellularChromatics`DinvFromAreaSeq[new],
        ChromaticFunctions`MajFromAreaSeq[old] ===
          UnicellularChromatics`MajFromAreaSeq[new],
        ChromaticFunctions`AreaDinv[old] ===
          UnicellularChromatics`AreaDinv[new],
        ChromaticFunctions`AreaToTopBounceShape[old] ===
          UnicellularChromatics`AreaToTopBounceShape[new],
        ChromaticFunctions`AreaToDyckWord[old] ===
          UnicellularChromatics`AreaToDyckWord[new],
        ChromaticFunctions`AreaRowPermutation[old] ===
          UnicellularChromatics`AreaRowPermutation[new]
      ],
      {old, areas}]
  ],
  True,
  TestID -> "UnicellularChromatics-area-statistics-legacy-equivalence"
]

VerificationTest[
  Module[{areas, old, new, n, edgeMap, oldCorners, newCorners},
    areas = Flatten[Table[
        ChromaticFunctions`GraphAreaLists[n, All -> True, Circular -> False],
        {n, 1, 4}], 1];
    And @@ Table[
      n = Length[old]; new = Reverse[old];
      edgeMap[e_] := n + 1 - Reverse[e];
      oldCorners = edgeMap /@ ChromaticFunctions`InnerCorners[old];
      newCorners = UnicellularChromatics`InnerCorners[new];
      And[
        Sort[oldCorners] === Sort[newCorners],
        Sort[edgeMap /@ ChromaticFunctions`OuterCorners[old]] ===
          Sort[UnicellularChromatics`OuterCorners[new]],
        With[{legacy = TimeConstrained[
            ChromaticFunctions`UnitIntervalData[old, Range[n]], .5,
            $Aborted]},
          legacy === $Aborted ||
            UnicellularChromatics`UnitIntervalData[new, Range[n]] === legacy]
      ],
      {old, areas}]
  ],
  True,
  TestID -> "UnicellularChromatics-area-geometry-legacy-equivalence"
]

VerificationTest[
  Module[{shapes = ChromaticFunctions`PathShapes[4], shape},
    And @@ Table[
      And @@ {
        ChromaticFunctions`AttackingPoset[shape] ===
          UnicellularChromatics`AttackingPoset[shape],
        ChromaticFunctions`IncomparabilityGraph[shape] ===
          UnicellularChromatics`IncomparabilityGraph[shape],
        ChromaticFunctions`ChromaticSymmetricColorings[shape] ===
          UnicellularChromatics`ChromaticSymmetricColorings[shape],
        ChromaticFunctions`PArray[Range[Length[shape]]] ===
          UnicellularChromatics`PArray[Range[Length[shape]]],
        ChromaticFunctions`GasharovPTableauQ[shape, Range[Length[shape]]] ===
          UnicellularChromatics`GasharovPTableauQ[shape, Range[Length[shape]]],
        Expand[ChromaticFunctions`ChromaticSymmetricPolynomial[shape, z, q]] ===
          Expand[UnicellularChromatics`ChromaticSymmetricPolynomial[shape, z, q]],
        Expand[ChromaticFunctions`SingleCelledLLTPolynomial[shape, z, q]] ===
          Expand[UnicellularChromatics`SingleCelledLLTPolynomial[shape, z, q]]
      },
      {shape, shapes}]
  ],
  True,
  TestID -> "UnicellularChromatics-shape-chromatic-and-tableau-legacy-equivalence"
]

VerificationTest[
  Module[{edges = {{1, 2}, {2, 3}, {1, 3}}, col = {2, 1, 3}, orient},
    orient = ChromaticFunctions`GraphColoringOrientation[edges, col];
    And @@ {
      ChromaticFunctions`GraphAttackingEdges[edges, col] ===
        UnicellularChromatics`GraphAttackingEdges[edges, col],
      ChromaticFunctions`GraphColoringInversions[edges, col] ===
        UnicellularChromatics`GraphColoringInversions[edges, col],
      ChromaticFunctions`GraphColoringOrientation[edges, col] ===
        UnicellularChromatics`GraphColoringOrientation[edges, col],
      ChromaticFunctions`GraphOrientationIntersection[edges, orient] ===
        UnicellularChromatics`GraphOrientationIntersection[edges, orient],
      ChromaticFunctions`GraphOrientationAscents[edges, orient] ===
        UnicellularChromatics`GraphOrientationAscents[edges, orient],
      ChromaticFunctions`GraphOrientationInversions[edges, orient] ===
        UnicellularChromatics`GraphOrientationInversions[edges, orient],
      ChromaticFunctions`GraphOrientationSinks[orient, 3] ===
        UnicellularChromatics`GraphOrientationSinks[orient, 3],
      ChromaticFunctions`GraphOrientationSources[orient, 3] ===
        UnicellularChromatics`GraphOrientationSources[orient, 3],
      ChromaticFunctions`GraphOrientationHalfSinks[edges, orient, 3] ===
        UnicellularChromatics`GraphOrientationHalfSinks[edges, orient, 3],
      ChromaticFunctions`GraphOrientationHalfSources[edges, orient, 3] ===
        UnicellularChromatics`GraphOrientationHalfSources[edges, orient, 3],
      ChromaticFunctions`NoAscendingCycleOrientations[edges] ===
        UnicellularChromatics`NoAscendingCycleOrientations[edges]
    }
  ],
  True,
  TestID -> "UnicellularChromatics-graph-coloring-and-orientation-legacy-equivalence"
]

VerificationTest[
  Module[{areas, old, new, n, edgeMap, oldOr, newOr, oldLrv, oldPart,
    oldForest, expectedLrv, expectedPart, expectedForest},
    areas = Flatten[Table[
        ChromaticFunctions`GraphAreaLists[n, All -> True, Circular -> False],
        {n, 2, 4}], 1];
    And @@ Table[
      n = Length[old]; new = Reverse[old];
      edgeMap[e_] := n + 1 - Reverse[e];
      oldOr = ChromaticFunctions`GraphColoringOrientation[
        ChromaticFunctions`AreaToEdges[old], Range[n]];
      newOr = edgeMap /@ oldOr;
      oldLrv = ChromaticFunctions`LLTOrientationLowestReachableVertex[old, oldOr];
      expectedLrv = Table[n + 1 - oldLrv[[n + 1 - i]], {i, n}];
      oldPart = ChromaticFunctions`LLTOrientationVertexPartition[old, oldOr];
      expectedPart = Sort[Sort /@ ((n + 1 - #) & /@ # & /@ oldPart)];
      oldForest = ChromaticFunctions`LLTOrientationForest[old, oldOr];
      expectedForest = Table[n + 1 - oldForest[[n + 1 - i]], {i, n}];
      And[
        UnicellularChromatics`LLTOrientationLowestReachableVertex[new, newOr] ===
          expectedLrv,
        Sort[Sort /@ UnicellularChromatics`LLTOrientationVertexPartition[new, newOr]] ===
          expectedPart,
        UnicellularChromatics`LLTOrientationShape[new, newOr] ===
          ChromaticFunctions`LLTOrientationShape[old, oldOr],
        UnicellularChromatics`LLTOrientationForest[new, newOr] === expectedForest
      ],
      {old, areas}]
  ],
  True,
  TestID -> "UnicellularChromatics-LLT-orientation-statistics-legacy-equivalence"
]

VerificationTest[
  Module[{edges = {{1, 2}, {2, 3}}, area = {0, 1, 1}},
    And @@ {
      Expand[ChromaticFunctions`GraphChromaticSymmetricPolynomial[
          {1, 2, 3}, z, q, ChromaticFunctions`Weights -> {1, 2, 1}]] ===
        Expand[UnicellularChromatics`GraphChromaticSymmetricPolynomial[
          {1, 2, 3}, z, q, Weights -> {1, 2, 1}]],
      Expand[ChromaticFunctions`GraphChromaticLLTPolynomial[
          edges, 3, z, q, ChromaticFunctions`StrictEdges -> {{1, 2}},
          ChromaticFunctions`WeakEdges -> {{2, 3}}]] ===
        Expand[UnicellularChromatics`GraphChromaticLLTPolynomial[
          edges, 3, z, q, UnicellularChromatics`StrictEdges -> {{1, 2}},
          UnicellularChromatics`WeakEdges -> {{2, 3}}]],
      Expand[ChromaticFunctions`HomogeneousGraphLLTPolynomial[
          edges, 3, z, q, t]] ===
        Expand[UnicellularChromatics`HomogeneousGraphLLTPolynomial[
          edges, 3, z, q, t]],
      Expand[ChromaticFunctions`GraphChromaticLLTPolynomialAttacking[
          edges, {{1, 2}}, 3, z, q]] ===
        Expand[UnicellularChromatics`GraphChromaticLLTPolynomialAttacking[
          edges, {{1, 2}}, 3, z, q]],
      Expand[UnicellularChromatics`GraphChromaticLLTPolynomial[edges, 3, z, 1]] ===
        Expand[UnicellularChromatics`HomogeneousGraphLLTPolynomial[edges, 3, z, 1, 1]],
      Expand[ChromaticFunctions`GraphChromaticSymmetricPolynomial[Reverse@area, z, q]] ===
        Expand[UnicellularChromatics`GraphChromaticSymmetricPolynomial[area, z, q]]
    }
  ],
  True,
  TestID -> "UnicellularChromatics-graph-chromatic-LLT-options-and-identities"
]

VerificationTest[
  Module[{sizes = {{2, 1}, {3, 1}, {2, 2}, {3, 2, 1}}, sizesOne,
    data, oldData},
    And @@ Join[
      Table[
        data = UnicellularChromatics`StripSizesToEdges[sizesOne];
        oldData = ChromaticFunctions`StripSizesToEdges[sizesOne];
        And[Reverse[data[[1]]] === oldData[[1]],
          data[[2]] === ((Total[sizesOne] + 1 - #) & /@
            oldData[[2]]),
          Sort[Reverse /@ ChromaticFunctions`VerticalStripLLTColorings[sizesOne]] ===
            Sort[UnicellularChromatics`VerticalStripLLTColorings[sizesOne]],
          Expand[ChromaticFunctions`VerticalStripLLTPolynomial[sizesOne, z, q]] ===
            Expand[UnicellularChromatics`VerticalStripLLTPolynomial[sizesOne, z, q]]],
        {sizesOne, sizes}],
      {
        ChromaticFunctions`SouthWestDiagram[{2, 1, 3}] ===
          UnicellularChromatics`SouthWestDiagram[{2, 1, 3}],
        ChromaticFunctions`SouthEastDiagram[{2, 1, 3}] ===
          UnicellularChromatics`SouthEastDiagram[{2, 1, 3}],
        ChromaticFunctions`PartitionRookPlacements[{2, 1, 1}] ===
          UnicellularChromatics`PartitionRookPlacements[{2, 1, 1}],
        ChromaticFunctions`AthanasiadisS[{2, 1, 1}] ===
          UnicellularChromatics`AthanasiadisS[{2, 1, 1}],
        ChromaticFunctions`AthanasiadisUnimodalSets[{2, 1}] ===
          UnicellularChromatics`AthanasiadisUnimodalSets[{2, 1}],
        UnicellularChromatics`AyclicAreaListQ[{0, 1, 1}],
        ! UnicellularChromatics`AyclicAreaListQ[{1, 2, 1}]
      }
    ]
  ],
  True,
  TestID -> "UnicellularChromatics-vertical-strip-and-combinatorial-utilities"
]

(* GitHub issue #17: the Weights option is System`Weights; loading must not change
   its usage string. *)
VerificationTest[
  {Names["UnicellularChromatics`Weights"],
   StringContainsQ[ToString[System`Weights::usage], "GraphChromatic"]},
  {{}, False},
  TestID -> "UnicellularChromatics-Weights-is-System-symbol"
]
