VerificationTest[
  Needs["RunSortedWords`"],
  Null,
  TestID -> "RunSortedWords-loads-cleanly"
]

(* GitHub issue #18: the empty input has a terminating base case. *)
VerificationTest[
  RunSortedPermutations[0],
  {},
  TestID -> "RunSortedWords-RunSortedPermutations-zero"
]

(* GitHub issue #18: counts agree with Bell[n-1]. *)
VerificationTest[
  Length /@ (RunSortedPermutations /@ Range[1, 6]),
  {1, 1, 2, 5, 15, 52},
  TestID -> "RunSortedWords-RunSortedPermutations-Bell-counts"
]

(* GitHub issue #9: the deprecated package still provides its old names. *)
VerificationTest[
  {SetPartitionToRSP[{{1, 2}, {3}}], Context[RunSortedPermutations]},
  {{1, 3, 2, 4}, "CombinatoricTools`"},
  TestID -> "RunSortedWords-deprecated-shim"
]
