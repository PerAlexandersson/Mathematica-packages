testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["CombinatoricTools`"],
  Null,
  TestID -> "CombinatoricTools-loads-cleanly"
]

(* GitHub issue #13: LatticeWordQ must return one Boolean, including for the empty word. *)
VerificationTest[
  And[
    LatticeWordQ[{}] === True,
    LatticeWordQ[{1, 2, 1}] === True,
    LatticeWordQ[{1, 2, 3}] === True,
    LatticeWordQ[{2, 1}] === False
  ],
  True,
  TestID -> "CombinatoricTools-LatticeWordQ-returns-Boolean"
]

(* GitHub issue #13: the UnimodalQ syntax information must not be overwritten. *)
VerificationTest[
  {
    SyntaxInformation[UnimodalQ],
    SyntaxInformation[LatticeWordQ]
  },
  {
    {"ArgumentsPattern" -> {{_...}}},
    {"ArgumentsPattern" -> {{_...}}}
  },
  TestID -> "CombinatoricTools-syntax-information-is-correct"
]

(* GitHub issue #13: the empty composition has no parts, not one zero part. *)
VerificationTest[
  {IntegerCompositions[0], IntegerCompositions[0, 0]},
  {{{}}, {{}}},
  TestID -> "CombinatoricTools-IntegerCompositions-empty"
]

(* GitHub issue #13: compare the determinant formula with the interval enumeration. *)
VerificationTest[
  And[
    PartitionIntervalSize[{3, 1}, {3, 1}] == Length[PartitionInterval[{3, 1}, {3, 1}]],
    And @@ Flatten[
      Table[
        PartitionIntervalSize[lam, mu] == Length[PartitionInterval[lam, mu]],
        {n, 1, 6}, {lam, IntegerPartitions[n]}, {m, 0, n},
        {mu, IntegerPartitions[m]}
      ]
    ]
  ],
  True,
  TestID -> "CombinatoricTools-PartitionIntervalSize-matches-enumeration"
]

(* GitHub issue #13: Durfee must publish its usage message. *)
VerificationTest[
  StringQ[Durfee::usage],
  True,
  TestID -> "CombinatoricTools-Durfee-has-usage"
]

(* GitHub issue #13: negative set partitions terminate, and the empty derangement exists. *)
VerificationTest[
  {SetPartitions[-1], Derangements[0]},
  {{}, {{}}},
  TestID -> "CombinatoricTools-empty-and-negative-boundaries"
]

(* GitHub issue #13: Jack/Macdonald values are unchanged by the column memoization fix. *)
VerificationTest[
  And[
    {JackPsi[{{3, 2}, {2, 1}}, 2], JackPsi[{{3, 1}, {2}}, 2]} === {28/45, 2/3},
    {MacdonaldPsi[{{3, 2}, {2, 1}}, 2, 3], MacdonaldPsi[{{3, 1}, {2}}, 2, 3]} ===
      {2346/1925, 6/5}
  ],
  True,
  TestID -> "CombinatoricTools-JackPsi-MacdonaldPsi-column-memoization"
]

(* GitHub issue #13: return every row where the prefix rank increases. *)
VerificationTest[
  {
    LinearlyIndependentRows[{{1, 0}, {0, 1}, {1, 1}}],
    LinearlyIndependentRows[{{1, 1}, {2, 2}, {0, 1}}]
  },
  {{1, 2}, {1, 3}},
  TestID -> "CombinatoricTools-LinearlyIndependentRows-indices"
]

(* GitHub issue #9: RunSortedPermutations moved here from RunSortedWords.
   Run-sorted permutations of [n] are counted by BellB[n - 1]. *)
VerificationTest[
  {Table[Length[RunSortedPermutations[n]], {n, 1, 7}],
   And @@ (Function[p, Sort[p] === Range[Length[p]] &&
       OrderedQ[First /@ Split[p, Less]]] /@ RunSortedPermutations[5]),
   Length@Union@RunSortedPermutations[5]},
  {BellB /@ Range[0, 6], True, 15},
  TestID -> "CombinatoricTools-RunSortedPermutations-Bell"
]
