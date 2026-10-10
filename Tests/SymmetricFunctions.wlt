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

(* #5: memoized public functions used to store results on protected symbols.
   Store-then-reread functions recursed until $RecursionLimit. *)

VerificationTest[
  ToSchurBasis[MacdonaldHSymmetric[{2, 1}, q, t]],
  SchurSymbol[{3}] + (q + t) SchurSymbol[{2, 1}] + q t SchurSymbol[{1, 1, 1}],
  SameTest -> (Expand[#1 - #2] === 0 &),
  TestID -> "SymmetricFunctions-MacdonaldHSymmetric-21-protected"
]

VerificationTest[
  ToSchurBasis[NablaOperator[ElementaryESymmetric[2], q, t]],
  SchurSymbol[{2}] + (q + t) SchurSymbol[{1, 1}],
  SameTest -> (Expand[#1 - #2] === 0 &),
  TestID -> "SymmetricFunctions-NablaOperator-e2"
]

VerificationTest[
  (* <nabla e_3, e_3> is the q,t-Catalan polynomial C_3(q,t). *)
  Expand@HallInnerProduct[NablaOperator[ElementaryESymmetric[3], q, t],
    ElementaryESymmetric[3]],
  Expand@qtCatalan[3, q, t],
  TestID -> "SymmetricFunctions-NablaOperator-qtCatalan"
]

VerificationTest[
  (* Delta'_{e_{n-1}} e_n = nabla e_n, here n = 2. *)
  Expand[ToSchurBasis[DeltaPrimOperator[ElementaryESymmetric[1], ElementaryESymmetric[2], q, t]
    - NablaOperator[ElementaryESymmetric[2], q, t]]],
  0,
  TestID -> "SymmetricFunctions-DeltaPrimOperator-nabla-identity"
]

VerificationTest[
  (* Delta_{e_1} multiplies H_mu by B_mu = sum of q^(c-1) t^(r-1); B_{21} = 1 + q + t. *)
  Expand[ToSchurBasis[DeltaOperator[ElementaryESymmetric[1], MacdonaldHSymmetric[{2, 1}, q, t], q, t]
    - (1 + q + t) MacdonaldHSymmetric[{2, 1}, q, t]]],
  0,
  TestID -> "SymmetricFunctions-DeltaOperator-eigenvalue"
]

VerificationTest[
  Head[ToMacdonaldHBasis[SchurSymbol[{2}], q, t]],
  Plus,
  TestID -> "SymmetricFunctions-ToMacdonaldHBasis-evaluates"
]

VerificationTest[
  {SkewKostkaCoefficient[{3, 2}, {1}, {2, 1, 1}],
   SkewKostkaCoefficient[{3, 2}, {1}, {1, 2, 1}],
   SkewKostkaCoefficient[{3, 2}, {1}, {2, 2}]},
  {Length[SemiStandardYoungTableaux[{{3, 2}, {1}}, {2, 1, 1}]],
   Length[SemiStandardYoungTableaux[{{3, 2}, {1}}, {1, 2, 1}]],
   Length[SemiStandardYoungTableaux[{{3, 2}, {1}}, {2, 2}]]},
  TestID -> "SymmetricFunctions-SkewKostkaCoefficient-matches-SSYT-count"
]

VerificationTest[
  {KroneckerCoefficient[{2, 1}, {2, 1}, {2, 1}],
   KroneckerCoefficient[{2, 1}, {2, 1}, {1, 1, 1}],
   KroneckerCoefficient[{2, 1}, {2, 1}, {3}],
   KroneckerCoefficient[{2, 1}, {3}, {3}],
   KroneckerCoefficient[{2, 1}, {2}, {2}]},
  {1, 1, 1, 0, 0},
  TestID -> "SymmetricFunctions-KroneckerCoefficient-21"
]

VerificationTest[
  (* Novel calls of memoized functions must not emit Set::write. *)
  {Head[SkewSchurSymmetric[{{3, 1}, {1}}]], Head[JackPSymmetric[{2, 1}, a]],
   Head[HallLittlewoodTSymmetric[{2, 1}, q]], Head[SchursQSymmetric[{2, 1}]],
   Head[LahSymmetricFunction[3, 1]], Head[LLTSymmetric[{{1}, {1}}, q]]},
  {Plus, Plus, Plus, Plus, Plus, Plus},
  TestID -> "SymmetricFunctions-memoized-functions-no-messages"
]

VerificationTest[
  {SameQ[JackPSymmetric[{2, 1}, a], JackPSymmetric[{2, 1}, a]],
   ClearSymmetricFunctionsCache[],
   Expand[ToSchurBasis[JackPSymmetric[{2, 1}, 1]]]},
  {True, Null, SchurSymbol[{2, 1}]},
  TestID -> "SymmetricFunctions-ClearSymmetricFunctionsCache"
]

VerificationTest[
  (* The removed alias block exported protected junk symbols name and rest. *)
  {Names["SymmetricFunctions`name"], Names["SymmetricFunctions`rest"]},
  {{}, {}},
  TestID -> "SymmetricFunctions-no-junk-exports"
]
