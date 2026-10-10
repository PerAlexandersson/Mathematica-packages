VerificationTest[
  Needs["MacdonaldPolynomials`"],
  Null,
  TestID -> "MacdonaldPolynomials-loads-cleanly"
]

(* GitHub issue #9: MacdonaldPolynomials no longer loads the legacy package. *)
VerificationTest[
  MemberQ[$Packages, "OldYoungTableaux`"],
  False,
  TestID -> "MacdonaldPolynomials-no-legacy-dependency"
]

(* GitHub issue #9: use the supported NewTableaux representative. *)
VerificationTest[
  And @@ Table[
    Module[{representative = NewTableaux`KnuthRepresentative[p]},
      Sort[representative] === Range[4] &&
       NewTableaux`BiwordRSK[Range[4], p][[1]] ===
        NewTableaux`BiwordRSK[Range[4], representative][[1]]
    ],
    {p, Permutations[Range[4]]}],
  True,
  TestID -> "MacdonaldPolynomials-KnuthRepresentative-RSK"
]

(* GitHub issue #9: supported packages own these names; Macdonald does not export copies. *)
VerificationTest[
  Names /@ {"MacdonaldPolynomials`KnuthRepresentative",
    "MacdonaldPolynomials`ToPowerSumBasis",
    "MacdonaldPolynomials`PartitionedCompositionCoarsenings"},
  {{}, {}, {}},
  TestID -> "MacdonaldPolynomials-no-duplicate-exports"
]

(* GitHub issue #9: the supported coarsening implementation has the same members,
   although it enumerates them in the opposite order. *)
VerificationTest[
  Sort[QuasiSymmetricFunctions`PartitionedCompositionCoarsenings[{{1}, {2}}]],
  Sort[{{{1}, {2}}, {{1, 2}}}],
  TestID -> "MacdonaldPolynomials-supported-coarsenings-agree"
]

(* GitHub issue #9: preserve the externally used polynomial values from the
   pre-decoupling implementation. *)
VerificationTest[
  AtomTPolynomial[{2, 1}, x, t],
  x[1]^2 x[2],
  TestID -> "MacdonaldPolynomials-AtomTPolynomial-reference-value"
]

VerificationTest[
  Together[MacdonaldEPolynomial[{2, 1}, x, q, t] -
    (((1 - t) x[1]^2 x[2])/(1 - q t) + x[1] x[2]^2)] === 0,
  True,
  TestID -> "MacdonaldPolynomials-MacdonaldEPolynomial-reference-value"
]

VerificationTest[
  OperatorKeyPolynomial[{2, 0, 1}, x],
  x[1]^2 x[2] + x[1] x[2]^2 + x[1]^2 x[3] + x[1] x[2] x[3] + x[1] x[3]^2,
  TestID -> "MacdonaldPolynomials-OperatorKeyPolynomial-reference-value"
]

VerificationTest[
  AtomPolynomial[{2, 1}, x],
  x[1]^2 x[2],
  TestID -> "MacdonaldPolynomials-AtomPolynomial-reference-value"
]

VerificationTest[
  KeyPolynomial[{2, 0, 1}, x],
  x[1]^2 x[2] + x[1] x[2]^2 + x[1]^2 x[3] + x[1] x[2] x[3] + x[1] x[3]^2,
  TestID -> "MacdonaldPolynomials-KeyPolynomial-reference-value"
]

VerificationTest[
  SchubertPolynomial[{3, 2, 1}, x],
  x[1]^2 x[2],
  TestID -> "MacdonaldPolynomials-SchubertPolynomial-reference-value"
]

VerificationTest[
  Expand[MacdonaldHPolynomial[{2, 1}, x, q, t] -
    (x[1]^3 + (1 + q + t) x[1]^2 x[2] +
      (1 + q + t) x[1] x[2]^2 + x[2]^3)] === 0,
  True,
  TestID -> "MacdonaldPolynomials-MacdonaldHPolynomial-reference-value"
]

VerificationTest[
  Expand[ToPowerSumBasisMacdonald[x[1]^2 + x[1] x[2] + x[2]^2, x, pp] -
    (pp[{1, 1}] + pp[{2, 0}])/2] === 0,
  True,
  TestID -> "MacdonaldPolynomials-ToPowerSumBasisMacdonald-reference-value"
]

VerificationTest[
  SkewMacdonaldE[{2, 1}, {1}, 2, x, q],
  x[1]^2 + 2 x[1] x[2] + x[2]^2,
  TestID -> "MacdonaldPolynomials-SkewMacdonaldE-supported-conjugate"
]

(* GitHub issue #18: the slide basis includes all refinements of the flattened composition. *)
VerificationTest[
  FundamentalSlide[{0, 2}, x],
  x[1]^2 + x[1] x[2] + x[2]^2,
  TestID -> "MacdonaldPolynomials-FundamentalSlide-refinements"
]

VerificationTest[
  {
    ToFundamentalSlideBasis[x[1]^2 + x[1] x[2] + x[2]^2, 2, x, ff],
    ToGesselSubsetBasis[x[1]^2 + x[1] x[2] + x[2]^2, x, ff]
  },
  {ff[{0, 2}], ff[{}]},
  TestID -> "MacdonaldPolynomials-slide-basis-conversions"
]

(* GitHub issue #18: summing over rearrangements recovers the ordinary power sum. *)
VerificationTest[
  And @@ Flatten[Table[
    Expand[
      Total[QuasiSymmetricPowerSum2[#, n, x] & /@
        DeleteDuplicates[Permutations[lambda]]] -
       Times @@ (Total[(x /@ Range[n])^#] & /@ lambda)
    ] === 0,
    {lambda, {{1, 1, 1}, {2, 1, 1}}}, {n, {3, 4}}]],
  True,
  TestID -> "MacdonaldPolynomials-QuasiSymmetricPowerSum2-rearrangements"
]

(* The private charge implementation remains equivalent to the imported one. *)
VerificationTest[
  MacdonaldPolynomials`Private`MacdonaldWordCharge[{1, 2, 1}] ===
    CombinatoricTools`WordCharge[{1, 2, 1}],
  True,
  TestID -> "MacdonaldPolynomials-charge-function-preserved"
]

(* GitHub issue #18: loading MacdonaldPolynomials must not add definitions or usage
   strings to symbols of other packages (it used to extend CombinatoricTools`WordCharge,
   ChargeWordDecompose and OldYoungTableaux`PowerSumPolynomial). OldYoungTableaux still
   exports names owned by NewTableaux (issue #9), so only General::shdw is tolerated. *)
VerificationTest[
  Module[{before, after, snapshot},
    snapshot[] := {Length[DownValues[CombinatoricTools`WordCharge]],
      Length[DownValues[CombinatoricTools`ChargeWordDecompose]],
      Length[DownValues[OldYoungTableaux`PowerSumPolynomial]],
      OldYoungTableaux`PowerSumPolynomial::usage,
      OldYoungTableaux`PowerSumPolynomial[{2, 1}, 3][x]};
    Quiet[Needs["OldYoungTableaux`"], General::shdw];
    before = snapshot[];
    Get["MacdonaldPolynomials`"];
    after = snapshot[];
    before === after],
  True,
  TestID -> "MacdonaldPolynomials-load-preserves-other-packages"
]
