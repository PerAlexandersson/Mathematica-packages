testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

(* GitHub issue #6: the cells of Examples/SymmetricFunctions-Introduction.nb evaluate
   without messages (several used to abort or emit Set::write), and the identities it
   demonstrates hold. The setup cell needs a front end, so it is skipped here. *)

introCells = Rest@Cases[
  Get[FileNameJoin[{testRoot, "Examples", "SymmetricFunctions-Introduction.nb"}]],
  Cell[c_, "Input", ___] :> c, Infinity];

VerificationTest[
  Scan[TimeConstrained[ReleaseHold[ToExpression[#, StandardForm, HoldComplete]], 120] &,
    introCells],
  Null,
  TestID -> "IntroductionNotebook-cells-evaluate-without-messages"
]

VerificationTest[
  {Table[HallInnerProduct[MonomialSymbol[lam], CompleteHSymbol[mu]],
     {lam, IntegerPartitions[5]}, {mu, IntegerPartitions[5]}] == IdentityMatrix[7],
   OmegaInvolution[SchurSymbol[{4, 1}]] === SchurSymbol[{2, 1, 1, 1}],
   PositiveCoefficientsQ[ToSchurBasis[SkewSchurSymmetric[{{3, 2, 2}, {2}}]], SchurSymbol],
   Expand[NablaOperator[ElementaryESymbol[3], q, t] -
     NablaOperator[ElementaryESymbol[3], t, q] /. {q -> t, t -> q}] === 0},
  {True, True, True, True},
  TestID -> "IntroductionNotebook-identities"
]
