testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["OldYoungTableaux`"],
  Null,
  TestID -> "OldYoungTableaux-loads-cleanly"
]

(* GitHub issue #18: a zero Jacobi--Trudi determinant is zero, not one. *)
VerificationTest[
  SchurPolynomial[{1, 1, 1}, {}, 2][x],
  0,
  TestID -> "OldYoungTableaux-SchurPolynomial-zero-determinant"
]

(* GitHub issue #18: use the defined YoungTableauTeX formatter. *)
VerificationTest[
  TeXForm[YoungTableau[{{1, 2}, {3}}]],
  "\\young(12,3)",
  TestID -> "OldYoungTableaux-YoungTableau-TeXForm"
]

(* GitHub issue #18: the no-weight GT-pattern call must use the exact fallback. *)
VerificationTest[
  GTPatterns[{2, 1}],
  {
    GTPattern[{{2, 1}, {1, 0}, {0, 0}}],
    GTPattern[{{2, 1}, {2, 0}, {0, 0}}]
  },
  TestID -> "OldYoungTableaux-GTPatterns-default-weight"
]

(* GitHub issue #18: adding a box to the empty partition gives {1}. *)
VerificationTest[
  AddBoxToPartition[{}],
  {{1}},
  TestID -> "OldYoungTableaux-AddBoxToPartition-empty"
]

(* GitHub issue #9: OldYoungTableaux uses the CombinatoricTools versions of the
   functions it duplicated, and loading it adds no definitions to CombinatoricTools. *)
VerificationTest[
  Module[{before},
    before = Length /@ DownValues /@ Unevaluated[{CombinatoricTools`ConjugatePartition,
       CombinatoricTools`DescentSet, CombinatoricTools`ZCoefficient}];
    Get["OldYoungTableaux`"];
    {Names["OldYoungTableaux`ConjugatePartition"], Names["OldYoungTableaux`UnimodalQ"],
     Names["OldYoungTableaux`ZCoefficient"],
     before === (Length /@ DownValues /@ Unevaluated[{CombinatoricTools`ConjugatePartition,
       CombinatoricTools`DescentSet, CombinatoricTools`ZCoefficient}])}],
  {{}, {}, {}, True},
  TestID -> "OldYoungTableaux-uses-CombinatoricTools-duplicates"
]
