testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["GTPatterns`"],
  Null,
  TestID -> "GTPatterns-loads-cleanly"
]

VerificationTest[
  SameQ[First[First[Options[GTPatterns]]], RowFlags],
  True,
  TestID -> "GTPatterns-public-row-flags-option"
]

VerificationTest[
  GTPatterns[{2}, {}, {1, 0, 1}],
  {GTPattern[{{0}, {1}, {1}, {2}}]},
  TestID -> "GTPatterns-zero-content-part"
]

VerificationTest[
  GTPatterns[{1}, {1}, {}],
  {GTPattern[{{1}}]},
  TestID -> "GTPatterns-empty-weight-trivial-skew-shape"
]

VerificationTest[
  GTPatterns[{}, {}, {}],
  {GTPattern[{{}}]},
  TestID -> "GTPatterns-empty-shape-and-weight"
]

VerificationTest[
  GTPatterns[{}, {}, {0, 0}],
  {GTPattern[{{}, {}, {}}]},
  TestID -> "GTPatterns-empty-shape-zero-content"
]

VerificationTest[
  GTPatterns[{1}, {}, {1}, RowFlags -> {{2, Infinity}}],
  {},
  TestID -> "GTPatterns-row-lower-bound-at-final-level"
]

VerificationTest[
  GTPatterns[{1}, {}, {1}, RowFlags -> {{1, 0}}],
  {},
  TestID -> "GTPatterns-row-upper-bound-at-initial-level"
]

VerificationTest[
  GTPatterns[{1}, {}, {1}, RowFlags -> {{1, 1}}],
  {GTPattern[{{0}, {1}}]},
  TestID -> "GTPatterns-valid-single-cell-row-flag"
]

VerificationTest[
  Length[GTPatterns[
    {2, 1}, {}, {1, 1, 1}, RowFlags -> {{1, 2}, {1, Infinity}}]],
  1,
  TestID -> "GTPatterns-interior-row-flag"
]

VerificationTest[
  Length[GTPatterns[
    {2, 1}, {}, {1, 1, 1}, Infinity,
    RowFlags -> {{1, Infinity}, {3, 3}}]],
  1,
  TestID -> "GTPatterns-explicit-cylindric-argument-with-option"
]

(* Regression tests for GitHub issue #14. *)
VerificationTest[
  GTPatterns[{2, 1}, {}, {1, 1, 1}, RowFlags -> {{1, 2}}],
  {GTPattern[{{0, 0}, {1, 0}, {2, 0}, {2, 1}}]},
  TestID -> "GTPatterns-row-flags-pad-by-pairs"
]

VerificationTest[
  Module[{cases, validQ},
    cases = {
      {{{2, 1}, {}, {1, 1, 1}}, {{1, 2}}},
      {{{2, 1}, {}, {1, 1, 1}}, {{1, 1}, {1, Infinity}}},
      {{{2, 2}, {}, {1, 1, 1, 1}}, {{1, 2}}}
    };
    validQ[YoungTableau[tab_], flags_] :=
      And @@ MapThread[
        Function[{row, range},
          With[{lo = range[[1]], hi = range[[2]]},
            AllTrue[DeleteCases[row, None], lo <= # <= hi &]]],
        {tab, PadRight[flags, Length[tab], {{1, Infinity}}]}
      ];
    And @@ (Function[case,
      Module[{shape = case[[1]], flags = case[[2]], all, actual},
        all = SemiStandardYoungTableaux[{shape[[1]], shape[[2]]}, shape[[3]]];
        flags = PadRight[flags, Length[shape[[1]]], {{1, Infinity}}];
        actual = YoungTableau /@ GTPatterns[Sequence @@ shape,
          RowFlags -> case[[2]]];
        SortBy[Select[all, validQ[#, flags] &], ToString[InputForm[#]] &] ===
          SortBy[actual, ToString[InputForm[#]] &]
      ]] /@ cases)
  ],
  True,
  TestID -> "GTPatterns-row-flags-match-brute-force-filter"
]

VerificationTest[
  GTPatterns[{2, 1}, {}, {1, 1, 1}, RowFlags -> {{0, 2}}],
  {},
  {GTPatterns::rowflags},
  TestID -> "GTPatterns-row-flags-reject-invalid-lower-bound"
]
