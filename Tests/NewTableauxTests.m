testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["NewTableaux`"],
  Null,
  TestID -> "NewTableaux-loads-cleanly"
]

(* Regression tests for GitHub issue #14. *)
VerificationTest[
  KnuthRepresentative[{2, 1, 3}],
  {2, 1, 3},
  TestID -> "NewTableaux-KnuthRepresentative-terminates-and-returns-reading-word"
]

VerificationTest[
  And @@ (BiwordRSK[KnuthRepresentative[#]][[1]] === BiwordRSK[#][[1]] & /@
    Permutations[Range[4]]),
  True,
  TestID -> "NewTableaux-KnuthRepresentative-preserves-P-tableau"
]

VerificationTest[
  With[{result = BiwordRSK[{1, 2}, {2.5, 1}]},
    HoldComplete[result] === HoldComplete[BiwordRSK[{1, 2}, {2.5, 1}]]],
  True,
  TestID -> "NewTableaux-BiwordRSK-invalid-two-word-input-does-not-recurse"
]

VerificationTest[
  With[{result = BiwordRSKDual[{1, 2}, {2.5, 1}]},
    HoldComplete[result] === HoldComplete[BiwordRSKDual[{1, 2}, {2.5, 1}]]],
  True,
  TestID -> "NewTableaux-BiwordRSKDual-invalid-two-word-input-does-not-recurse"
]

VerificationTest[
  CrystalSi[{1, 1, 2}, 1],
  {1, 2, 2},
  TestID -> "NewTableaux-CrystalSi-acts-on-words"
]

VerificationTest[
  With[{word = {1, 1, 2}},
    CrystalSi[word, 1] ===
      CrystalSi[YoungTableau[{word}], 1][[1, 1]] &&
    CrystalSi[CrystalSi[word, 1], 1] === word],
  True,
  TestID -> "NewTableaux-CrystalSi-word-tableau-agreement-and-involution"
]

VerificationTest[
  BSTHeightVector[First[BorderStripTableaux[{2, 1}, {2, 1}]]],
  {1, 0},
  TestID -> "NewTableaux-BSTHeightVector-accepts-border-strip-lists"
]

VerificationTest[
  SemiStandardYoungTableaux[{{1}, {1}}, {}],
  {YoungTableau[{{None}}]},
  TestID -> "NewTableaux-SemiStandardYoungTableaux-empty-skew-filling"
]

VerificationTest[
  SemiStandardYoungTableaux[{{2, 1}, {}}, {1, 1, 1}];
  {ValueQ[NewTableaux`Private`g], DownValues[NewTableaux`Private`pathToSSYT]},
  {False, {}},
  TestID -> "NewTableaux-SemiStandardYoungTableaux-does-not-leak-locals"
]

VerificationTest[
  {Length[CylindricTableaux[{2, 1}, 0]],
    Length[CylindricSYT[{2, 1}, 0]]},
  {2, 1},
  TestID -> "NewTableaux-CylindricTableaux-counts-unchanged"
]

(* GitHub issue #51: merge the legacy TeX options and outer-corner behavior
   into the supported YoungTableau representation. *)
VerificationTest[
  {
    YTableauTeX[YoungTableau[{{1, 2}, {None, 3}}], LineBreaks -> False],
    YTableauTeX[YoungTableau[{{1, 2}, {None, 3}}], UseArray -> False],
    TableauShortTeX[YoungTableau[{{1, 2}, {None, 3}}]],
    HasOuterCornerQ[YoungTableau[{{None, 1, 2}, {4, 5}}]],
    HasOuterCornerQ[YoungTableau[{{1, 2}, {3}}]]
  },
  {
    StringJoin["\\begin{ytableau}", "1 & 2", "\\\\", "\\none & 3", "\\\\", "\\end{ytableau}"],
    "\\young(12,:3)",
    "\\ytableaushort{12,{\\none}3}",
    True,
    False
  },
  TestID -> "NewTableaux-TeX-options-and-outer-corner"
]
