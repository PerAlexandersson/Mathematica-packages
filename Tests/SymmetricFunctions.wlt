testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

VerificationTest[
  Needs["SymmetricFunctions`"],
  Null,
  TestID -> "SymmetricFunctions-loads-cleanly"
]

(* #20: basisInElementary must not leak its temporary coefficient vector. *)
VerificationTest[
  ToPowerSumBasis[SchurSymbol[{2}]];
  OwnValues[SymmetricFunctions`Private`vec],
  {},
  TestID -> "SymmetricFunctions-basisInElementary-localizes-vec"
]

(* #20: kSchurSymmetric is a public function and agrees with Schur at large k. *)
VerificationTest[
  {Names["SymmetricFunctions`kSchurSymmetric"] =!= {},
   kSchurSymmetric[{2, 1}, 3]},
  {True, SchurSymbol[{2, 1}]},
  TestID -> "SymmetricFunctions-kSchurSymmetric-exported-and-stable"
]

(* #20: the empty partition is the constant term in the inner products. *)
VerificationTest[
  {HallInnerProduct[1, 1, {q, t}], JackInnerProduct[1, 1, a],
   HallInnerProduct[1 + PowerSumSymbol[{1}], 1 + PowerSumSymbol[{1}]],
   HallInnerProduct[3, SchurSymbol[{1}]]},
  {1, 1, 2, 0},
  TestID -> "SymmetricFunctions-inner-products-handle-constant-terms"
]

(* #20: finite principal specialization at q=1 must convert to monomials. *)
VerificationTest[
  {PrincipalSpecialization[SchurSymbol[{1}], 1, 3],
   PrincipalSpecialization[SchurSymbol[{2, 1}], 1, 3],
   (PrincipalSpecialization[SchurSymbol[{2, 1}], q, 3] /. q -> 1)},
  {3, 8, 8},
  TestID -> "SymmetricFunctions-PrincipalSpecialization-q1-finite"
]

(* #20: generalized LR coefficients vanish outside the degree constraint. *)
VerificationTest[
  {LRCoefficient[{1}, {1}, {1}], LRCoefficient[{2}, {1}, {2}]},
  {0, 0},
  TestID -> "SymmetricFunctions-LRCoefficient-off-degree"
]

VerificationTest[
  And @@ Table[
    LRCoefficient[{2, 1}, {2, 1}, nu] ===
      Coefficient[LRExpand[SchurSymbol[{2, 1}] SchurSymbol[{2, 1}]],
        SchurSymbol[nu]],
    {nu, IntegerPartitions[6]}],
  True,
  TestID -> "SymmetricFunctions-LRCoefficient-agrees-with-LRExpand"
]

(* #20: constants in the first plethysm argument must survive substitution. *)
VerificationTest[
  {Plethysm[3, PowerSumSymbol[{2}]],
   Plethysm[1 + PowerSumSymbol[{1}], PowerSumSymbol[{2}]],
   Plethysm[PowerSumSymbol[{3}], PowerSumSymbol[{2}]]},
  {3, 1 + PowerSumSymbol[{2}], PowerSumSymbol[{6}]},
  TestID -> "SymmetricFunctions-Plethysm-preserves-constants"
]

VerificationTest[
  ToSchurBasis[Plethysm[CompleteHSymbol[2], CompleteHSymbol[2]]],
  SchurSymbol[{4}] + SchurSymbol[{2, 2}],
  TestID -> "SymmetricFunctions-Plethysm-complete-homogeneous"
]

(* #20: SkewMacdonaldESymmetric needs its own permutation-charge helper. *)
VerificationTest[
  SkewMacdonaldESymmetric[{{1}, {}}, q, None],
  SchurSymbol[{1}],
  TestID -> "SymmetricFunctions-SkewMacdonaldESymmetric-resolves-charge"
]

VerificationTest[
  With[{got = SkewMacdonaldESymmetric[{{2, 1}, {}}, 1, None]},
    Expand[got - Sum[
       Length[SemiStandardYoungTableaux[
          {ConjugatePartition[nu], {}}, ConjugatePartition[{2, 1}]]] SchurSymbol[nu],
       {nu, IntegerPartitions[3]}]] === 0],
  True,
  TestID -> "SymmetricFunctions-SkewMacdonaldESymmetric-q1-SSYT-count"
]

(* #20: preserve the requested alphabet in InternalProduct. *)
VerificationTest[
  InternalProduct[PowerSumSymbol[{1}, y], PowerSumSymbol[{1}, y], y],
  PowerSumSymbol[{1}, y],
  TestID -> "SymmetricFunctions-InternalProduct-preserves-alphabet"
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

(* Cases ported from SymmetricFunctionsTestSuite.m. *)
VerificationTest[
  ToMonomialBasis[SchurSymmetric[{4, 3, 2}] -
    SkewSchurSymmetric[{4, 3, 2}]] === 0,
  True,
  TestID -> "SymmetricFunctions-legacy-SchurKostka"
]

VerificationTest[
  And @@ Flatten[Table[
    Expand[LRExpand[SchurSymbol[la] SchurSymbol[mu]] -
      (SchurSymbol[la] SchurSymbol[mu] // ToMonomialBasis // ToSchurBasis)] === 0,
    {la, IntegerPartitions[4]}, {mu, IntegerPartitions[3]}]],
  True,
  TestID -> "SymmetricFunctions-legacy-LRRule"
]

VerificationTest[
  With[{m = 5},
    MExpand[Sum[(-1)^i ElementaryESymmetric[i] CompleteHSymmetric[m - i],
      {i, 0, m}]] === 0],
  True,
  TestID -> "SymmetricFunctions-legacy-ElementaryESymmetric"
]

VerificationTest[
  And[
    ToMonomialBasis[OmegaInvolution[OmegaInvolution[SchurSymmetric[{4, 2, 1}]]]] ===
      SchurSymmetric[{4, 2, 1}],
    ToMonomialBasis[OmegaInvolution[ElementaryESymmetric[{4, 2, 1}]]] ===
      ToMonomialBasis[CompleteHSymmetric[{4, 2, 1}]]],
  True,
  TestID -> "SymmetricFunctions-legacy-OmegaInvolution"
]

VerificationTest[
  Coefficient[ToSchurBasis[HallLittlewoodTSymmetric[{2, 2, 2, 2}, q]],
    SchurSymbol[{3, 3, 2}]] === q^3 + q^4 + q^5,
  True,
  TestID -> "SymmetricFunctions-legacy-HallLittlewoodTSymmetric"
]

VerificationTest[
  ToMonomialBasis[HallLittlewoodPSymmetric[{3, 2}, t]] ===
    ToMonomialBasis[MacdonaldPSymmetric[{3, 2}, 0, t]],
  True,
  TestID -> "SymmetricFunctions-legacy-HallLittlewoodPSymmetric"
]

(* Small, fast versions of the legacy timing checks. *)
VerificationTest[
  And @@ Flatten[Table[Head[func[la]] =!= func,
    {la, IntegerPartitions[5]},
    {func, {SchurSymmetric, ElementaryESymmetric, CompleteHSymmetric,
      PowerSumSymmetric}}]],
  True,
  TestID -> "SymmetricFunctions-legacy-monomial-expansion-smoke"
]

VerificationTest[
  Head[Expand[MonomialSymbol[{3, 1}]^2 + MonomialSymbol[{2, 2}]^2]],
  Plus,
  TestID -> "SymmetricFunctions-legacy-monomial-products-smoke"
]

(* #20: the coefficient of s_nu in E_lam(q) is the Kostka-Foulkes polynomial
   coefficient of s_nu' in the transformed Hall-Littlewood function of lam'. *)
VerificationTest[
  And @@ Flatten@Table[
    With[{e = Expand@SkewMacdonaldESymmetric[{lam, {}}, q, None],
        hl = Expand@ToSchurBasis[HallLittlewoodTSymmetric[ConjugatePartition[lam], q]]},
      Table[Expand[Coefficient[e, SchurSymbol[nu]]
          - Coefficient[hl, SchurSymbol[ConjugatePartition[nu]]]] === 0,
        {nu, IntegerPartitions[Tr@lam]}]],
    {lam, {{2, 1}, {3, 1}, {2, 2}, {2, 1, 1}}}],
  True,
  TestID -> "SymmetricFunctions-SkewMacdonaldESymmetric-Kostka-Foulkes"
]

VerificationTest[
  FreeQ[SkewMacdonaldESymmetric[{{3, 2}, {1}}, q, None],
    s_Symbol /; StringStartsQ[Context[s], "SymmetricFunctions`Private`"]],
  True,
  TestID -> "SymmetricFunctions-SkewMacdonaldESymmetric-skew-fully-evaluated"
]
