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
