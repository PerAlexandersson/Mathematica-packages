testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

VerificationTest[
  Needs["PermutationTools`"],
  Null,
  TestID -> "PermutationTools-loads-cleanly"
]

(* GitHub issue #16: separable permutations are counted by the large Schröder numbers. *)
VerificationTest[
  Table[Length[Select[Permutations[Range[n]], SeparablePermutationQ]], {n, 1, 6}],
  {1, 2, 6, 22, 90, 394},
  TestID -> "PermutationTools-SeparablePermutationQ-Schröder-counts"
]

(* GitHub issue #16: separability agrees with independent 2413/3142 avoidance. *)
VerificationTest[
  And @@ (Function[pi,
      SeparablePermutationQ[pi] ===
       (IsPermutationAvoidingQ[{2, 4, 1, 3}, pi] &&
         IsPermutationAvoidingQ[{3, 1, 4, 2}, pi])] /@
    Permutations[Range[5]]),
  True,
  TestID -> "PermutationTools-SeparablePermutationQ-pattern-avoidance"
]

(* GitHub issue #16: False must be passed through recursive direct-sum splits. *)
VerificationTest[
  SplitSeparablePermutation[{2, 1, 3}, False],
  {{2, 1}, {3}},
  TestID -> "PermutationTools-SplitSeparablePermutation-recursive-skew-option"
]

(* GitHub issue #16: the 231 and 123 PAPS caches must remain distinct. *)
VerificationTest[
  Module[{p231, p123},
    p231 = GeneratePAPS[4, Is231AvoidingQ];
    p123 = GeneratePAPS[4, Is123AvoidingQ];
    p231 =!= p123 && And @@ (Is231AvoidingQ /@ p231) &&
     And @@ (Is123AvoidingQ /@ p123)],
  True,
  TestID -> "PermutationTools-GeneratePAPS-pattern-specific-cache"
]

(* GitHub issue #16: memoization must include n. *)
VerificationTest[
  PermutationFromWord[{1}, 2] === {2, 1} &&
   PermutationFromWord[{1}, 3] === {2, 1, 3} &&
   Length[DownValues[PermutationFromWord]] == 4,
  True,
  TestID -> "PermutationTools-PermutationFromWord-memoizes-n"
]

(* GitHub issue #3: package code must not depend on the CombinatoricTools
   convenience rule Permutations[n_Integer], which modifies a System symbol. *)
VerificationTest[
  Internal`InheritedBlock[{Permutations},
    Unprotect[Permutations];
    DownValues[Permutations] = {};
    Protect[Permutations];
    Length /@ {GrassmannPermutations[4], SimsunPermutations[4],
      SkewMergedPermutations[4], WachsPermutations[4], TypeBPermutations[3],
      NQueensPermutations[5], GeneratePAPS[4], GeneratePAPS[4, Is231AvoidingQ],
      GenerateRAPS[4, 2]}],
  Length /@ {GrassmannPermutations[4], SimsunPermutations[4],
    SkewMergedPermutations[4], WachsPermutations[4], TypeBPermutations[3],
    NQueensPermutations[5], GeneratePAPS[4], GeneratePAPS[4, Is231AvoidingQ],
    GenerateRAPS[4, 2]},
  TestID -> "PermutationTools-no-reliance-on-Permutations-patch"
]
