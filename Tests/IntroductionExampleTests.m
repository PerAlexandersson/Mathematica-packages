testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

(* GitHub issue #6: Examples/SymmetricFunctions-Introduction.m runs without messages
   (several of its examples used to abort or emit Set::write), and the identities it
   demonstrates hold. Printing is suppressed. *)

VerificationTest[
  Block[{Print}, Get[FileNameJoin[{testRoot, "Examples", "SymmetricFunctions-Introduction.m"}]]],
  Null,
  TestID -> "IntroductionExample-runs-without-messages"
]

VerificationTest[
  {Table[HallInnerProduct[MonomialSymbol[lam], CompleteHSymbol[mu]],
     {lam, IntegerPartitions[5]}, {mu, IntegerPartitions[5]}] == IdentityMatrix[7],
   OmegaInvolution[SchurSymbol[{4, 1}]] === SchurSymbol[{2, 1, 1, 1}],
   PositiveCoefficientsQ[ToSchurBasis[SkewSchurSymmetric[{{3, 2, 2}, {2}}]], SchurSymbol],
   Expand[HallInnerProduct[NablaOperator[ElementaryESymbol[3], q, t], ElementaryESymbol[3]]] ===
     Expand[qtCatalan[3, q, t]],
   Expand[ToSchurBasis[HallLittlewoodMSymmetric[{2, 1}, t]] -
     ToSchurBasis[MacdonaldHSymmetric[{2, 1}, 0, t]]] === 0},
  {True, True, True, True, True},
  TestID -> "IntroductionExample-identities"
]
