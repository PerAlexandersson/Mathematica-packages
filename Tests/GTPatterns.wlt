testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

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
