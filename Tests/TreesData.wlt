testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

VerificationTest[
  Needs["TreesData`"],
  Null,
  TestID -> "TreesData-loads-cleanly"
]

(* GitHub issue #18: the unrooted tree list must not be overwritten. *)
VerificationTest[
  Length /@ (GetTrees /@ Range[4, 9]),
  {2, 3, 6, 11, 23, 47},
  TestID -> "TreesData-GetTrees-unrooted-counts"
]

(* GitHub issue #18: include the one-vertex rooted tree. *)
VerificationTest[
  Length /@ (GetRootedTrees /@ Range[1, 10]),
  {1, 1, 2, 4, 9, 20, 48, 115, 286, 719},
  TestID -> "TreesData-GetRootedTrees-rooted-counts"
]
