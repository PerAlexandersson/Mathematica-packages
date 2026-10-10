testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

VerificationTest[
  Needs["SymmetricFunctions`"],
  Null,
  TestID -> "SymmetricFunctions-loads-cleanly"
]

VerificationTest[
  SymmetricFunctions`CylindricSchurSymmetric[{{1}, {}}, 0],
  SymmetricFunctions`MonomialSymbol[{1}, None],
  TestID -> "CylindricSchurSymmetric-evaluates-with-protected-public-symbol"
]

VerificationTest[
  SameQ[
    SymmetricFunctions`CylindricSchurSymmetric[{{2}, {}}, 0],
    SymmetricFunctions`CylindricSchurSymmetric[{{2}, {}}, 0]
  ],
  True,
  TestID -> "CylindricSchurSymmetric-repeat-call-without-memoization"
]
