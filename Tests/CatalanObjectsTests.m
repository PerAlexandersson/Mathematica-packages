testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["CatalanObjects`"],
  Null,
  TestID -> "CatalanObjects-loads-cleanly"
]

(* GitHub issue #16: DyckPath[{}] must be a stable empty path object. *)
VerificationTest[
  Head[TimeConstrained[DyckPath[{}], 1, $Failed]],
  DyckPath,
  TestID -> "CatalanObjects-DyckPath-empty-does-not-loop"
]

(* GitHub issue #16: DyckPaths must enumerate valid, distinct Catalan paths. *)
VerificationTest[
  Module[{validQ},
    validQ[DyckPath[steps_List], n_Integer] :=
      Count[steps, "n"] == n && Count[steps, "e"] == n &&
       Min[Prepend[Accumulate[steps /. {"n" -> 1, "e" -> -1}], 0]] >= 0;
    And @@ Table[
      With[{paths = DyckPaths[n]},
        Length[paths] == CatalanNumber[n] &&
         Length[DeleteDuplicates[paths]] == Length[paths] &&
         And @@ (validQ[#, n] & /@ paths)],
      {n, 0, 6}]
  ],
  True,
  TestID -> "CatalanObjects-DyckPaths-valid-Catalan-family"
]

(* GitHub issue #16: the matching recursion must remove one pair, not k pairs. *)
VerificationTest[
  Module[{validQ},
    validQ[CircularGraph[vertices_Integer, edges_List], n_Integer] :=
      vertices == 2 n && Length[edges] == n &&
       Sort[Join @@ edges] == Range[2 n];
    And @@ Table[
      With[{matchings = PerfectMatchings[n]},
        Length[matchings] == Factorial[2 n]/(2^n Factorial[n]) &&
         And @@ (validQ[#, n] & /@ matchings)],
      {n, 0, 5}]
  ],
  True,
  TestID -> "CatalanObjects-PerfectMatchings-valid-enumeration"
]

(* GitHub issue #16: Fuss-Catalan paths have n north and (k - 1)n east steps. *)
VerificationTest[
  Module[{k = 3, validQ},
    validQ[DyckPath[steps_List], n_Integer] :=
      Count[steps, "n"] == n && Count[steps, "e"] == (k - 1) n &&
       Min[Prepend[Accumulate[steps /. {"n" -> k - 1, "e" -> -1}], 0]] >= 0;
    And @@ Table[
      With[{paths = FussCatalanPaths[n, k]},
        Length[paths] == Binomial[k n, n]/((k - 1) n + 1) &&
         And @@ (validQ[#, n] & /@ paths)],
      {n, 0, 3}]
  ],
  True,
  TestID -> "CatalanObjects-FussCatalanPaths-step-counts"
]

(* GitHub issue #16: removing vertices preserves the DyckPath head. *)
VerificationTest[
  MatchQ[DyckPathRemoveVertices[DyckPath["nene"], {1}], DyckPath[_List]],
  True,
  TestID -> "CatalanObjects-DyckPathRemoveVertices-list-form"
]

(* GitHub issue #16: the tree-to-permutation map must recurse through itself. *)
VerificationTest[
  Module[{trees = OrderedRootedTrees[4], perms, avoidQ},
    perms = ORTTo231Perm /@ trees;
    avoidQ[pi_List] := And @@ Flatten@Table[
      !(i < j < k && pi[[j]] > pi[[i]] > pi[[k]]),
      {i, Length[pi]}, {j, Length[pi]}, {k, Length[pi]}];
    Length[DeleteDuplicates[perms]] == Length[trees] &&
     And @@ ((Sort[#] == Range[Length[#]]) & /@ perms) &&
     And @@ (avoidQ /@ perms)
  ],
  True,
  TestID -> "CatalanObjects-ORTTo231Perm-injective-231-avoiding"
]

(* GitHub issue #16: string input must dispatch to the Graphics implementation. *)
VerificationTest[
  Head[DyckPlot["ne"]],
  Graphics,
  TestID -> "CatalanObjects-DyckPlot-string-graphics"
]

(* GitHub issue #16: empty Catalan families have one empty object. *)
VerificationTest[
  IncreasingParkingFunctions[0],
  {{}},
  TestID -> "CatalanObjects-IncreasingParkingFunctions-empty"
]

(* GitHub issue #16: ParkingFunctions inherits the empty-object convention. *)
VerificationTest[
  ParkingFunctions[0],
  {{}},
  TestID -> "CatalanObjects-ParkingFunctions-empty"
]

(* GitHub issue #16: the empty non-crossing forest is the empty graph. *)
VerificationTest[
  NonCrossingForests[0],
  {CircularGraph[0, {}]},
  TestID -> "CatalanObjects-NonCrossingForests-empty"
]

(* GitHub issue #16: the empty line-graph area list is one empty list. *)
VerificationTest[
  LineGraphAreaLists[0],
  {{}},
  TestID -> "CatalanObjects-LineGraphAreaLists-empty"
]

(* GitHub issue #16: negative Stanley indices are outside the recursive family. *)
VerificationTest[
  StanleyCatalan60[-1],
  {},
  TestID -> "CatalanObjects-StanleyCatalan60-negative-empty"
]

(* GitHub issue #16: CircularGraphPlot helper definitions must be localized. *)
VerificationTest[
  CircularGraphPlot[CircularGraph[-2, {}]];
  DownValues[CatalanObjects`Private`fromTypeB] === {} && DownValues[CatalanObjects`Private`toTypeB] === {},
  True,
  TestID -> "CatalanObjects-CircularGraphPlot-local-helper-definitions"
]

(* GitHub issue #16: negative labels receive a bar over their positive value. *)
VerificationTest[
  With[{form = SetPartitionForm[{{-1, 2}}]},
    FreeQ[form, OverBar[-1], Infinity] && !FreeQ[form, OverBar[1], Infinity]],
  True,
  TestID -> "CatalanObjects-SetPartitionForm-negative-label"
]

(* GitHub issue #16: ordered rooted tree size is the number of vertices. *)
VerificationTest[
  And @@ Table[
    And @@ ((OrderedRootedTreeSize[#] == n + 1) & /@ OrderedRootedTrees[n]),
    {n, 0, 4}],
  True,
  TestID -> "CatalanObjects-OrderedRootedTreeSize-vertex-count"
]

(* GitHub issue #16: RationalDyckPaths is part of the public package API. *)
VerificationTest[
  Names["CatalanObjects`RationalDyckPaths"] === {"RationalDyckPaths"} &&
   ToExpression["CatalanObjects`RationalDyckPaths[{2, 2}]"] ===
    {{"n", "e", "n", "e"}, {"n", "n", "e", "e"}},
  True,
  TestID -> "CatalanObjects-RationalDyckPaths-public-export"
]
