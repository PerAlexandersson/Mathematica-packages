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
