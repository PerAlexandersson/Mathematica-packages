testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

VerificationTest[
  Quiet[
    Needs["OldYoungTableaux`"];
    Needs["CombinatoricTools`"];
    macOldPower = Length[DownValues[OldYoungTableaux`PowerSumPolynomial]];
    macOldWord = Length[DownValues[CombinatoricTools`WordCharge]];
    macOldDecomp = Length[DownValues[CombinatoricTools`ChargeWordDecompose]];
    macPowerBefore = OldYoungTableaux`PowerSumPolynomial[{2, 1}, 3][x];
    macPowerUsage = OldYoungTableaux`PowerSumPolynomial::usage;
    Needs["MacdonaldPolynomials`"]
  ],
  Null,
  TestID -> "MacdonaldPolynomials-loads-cleanly"
]

(* GitHub issue #18: loading MacdonaldPolynomials must not mutate imported APIs. *)
VerificationTest[
  {
    {macOldPower, macOldWord, macOldDecomp},
    {Length[DownValues[OldYoungTableaux`PowerSumPolynomial]],
      Length[DownValues[CombinatoricTools`WordCharge]],
      Length[DownValues[CombinatoricTools`ChargeWordDecompose]]},
    macPowerBefore === OldYoungTableaux`PowerSumPolynomial[{2, 1}, 3][x],
    macPowerUsage === OldYoungTableaux`PowerSumPolynomial::usage
  },
  {{0, 1, 3}, {0, 1, 3}, True, True},
  TestID -> "MacdonaldPolynomials-load-preserves-imported-definitions"
]

(* GitHub issue #18: the representative is a permutation with the same RSK P-tableau. *)
VerificationTest[
  And @@ Table[
    Module[{representative = KnuthRepresentative[p]},
      Sort[representative] === Range[4] &&
       NewTableaux`BiwordRSK[Range[4], p][[1]] ===
        NewTableaux`BiwordRSK[Range[4], representative][[1]]
    ],
    {p, Permutations[Range[4]]}],
  True,
  TestID -> "MacdonaldPolynomials-KnuthRepresentative-RSK"
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
  Private`MacdonaldWordCharge[{1, 2, 1}] ===
    CombinatoricTools`WordCharge[{1, 2, 1}],
  True,
  TestID -> "MacdonaldPolynomials-charge-function-preserved"
]
