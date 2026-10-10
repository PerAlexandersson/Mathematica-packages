testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["PolynomialTools`"],
  Null,
  TestID -> "PolynomialTools-loads-cleanly"
]

(* GitHub issue #13: elementary symmetric polynomials need the standard bases. *)
VerificationTest[
  And[
    And @@ Table[
      ElementarySymmetricPolynomial[d, {1, 3}, x] ===
        SymmetricPolynomial[d, x /@ Range[1, 3]],
      {d, 0, 3}
    ],
    ElementarySymmetricPolynomial[-1, {1, 3}, x] === 0,
    CompleteHomogeneousPolynomial[-1, {1, 1}, x] === 0
  ],
  True,
  TestID -> "PolynomialTools-symmetric-polynomial-boundaries"
]

(* GitHub issue #13: interlacing is false when either input has non-real roots. *)
VerificationTest[
  InterleavingRootsQ[t^2 + 1, t^3 + 2 t + 1, t],
  False,
  TestID -> "PolynomialTools-InterleavingRootsQ-nonreal-input"
]

(* GitHub issue #13: use the degree k coefficient in the ultra-log-concavity test. *)
VerificationTest[
  {UltraLogConcaveQ[1 + 3 t + 5 t^2, t], UltraLogConcaveQ[(1 + t)^3, t]},
  {False, True},
  TestID -> "PolynomialTools-UltraLogConcaveQ-coefficient-weights"
]

(* GitHub issue #13: Hilbert monomial memoization is local to each call and variable set. *)
VerificationTest[
  {
    HilbertFunctionValues[{x^2}, {x, y}, 3],
    HilbertFunctionValues[{a^5}, {a, b, c}, 3]
  },
  {{1, 2, 2, 2}, {1, 3, 6, 10}},
  TestID -> "PolynomialTools-HilbertFunctionValues-local-memoization"
]

(* GitHub issue #13: A(0,0) is one. *)
VerificationTest[
  {EulerianA[0, 0], EulerianA[0, 1], Table[EulerianA[3, m], {m, 0, 3}]},
  {1, 0, {1, 4, 1, 0}},
  TestID -> "PolynomialTools-EulerianA-zero-zero"
]

(* GitHub issue #13: multivariate input is diagnosed and returns a Boolean. *)
VerificationTest[
  RealRootedQ[x + y],
  False,
  {RealRootedQ::poly},
  TestID -> "PolynomialTools-RealRootedQ-rejects-multivariate"
]

(* GitHub issue #13: denominator-degree options publish usage strings. *)
VerificationTest[
  And[StringQ[DenominatorVariableDegree::usage], StringQ[DenominatorIndexDegree::usage]],
  True,
  TestID -> "PolynomialTools-denominator-degree-options-have-usage"
]

(* GitHub issue #13: ruleFormatSolution must not retain a package-level definition. *)
VerificationTest[
  Quiet[FindPolynomialRecurrence[{1, 1, 1}, {t, n}, RulesList -> True]];
  DownValues[PolynomialTools`Private`ruleFormatSolution],
  {},
  TestID -> "PolynomialTools-FindPolynomialRecurrence-localizes-formatter"
]

(* GitHub issue #51: port the sequence interpolator and make it reject
   underdetermined low-degree fits. *)
VerificationTest[
  SequenceToPolynomial[(# - 1) (# - 2) (# - 3) &, x],
  Expand[(x - 1) (x - 2) (x - 3)],
  TestID -> "PolynomialTools-SequenceToPolynomial-uses-enough-values"
]

(* GitHub issue #51, review round 2: non-polynomial sequences must terminate
   after the documented degree bound. *)
VerificationTest[
  Module[{messages = {}, result},
    result = Block[{Message = (AppendTo[messages, {##}] &)},
      SequenceToPolynomial[2^# &, x, 5]];
    {result, Length[messages] == 1 &&
      StringContainsQ[First[First[messages]], "No polynomial of degree"]}],
  {$Failed, True},
  TestID -> "PolynomialTools-SequenceToPolynomial-degree-bound"
]

VerificationTest[
  HVector[1 + 3 x + 2 x^2, x],
  PadRight[CoefficientList[HStarPolynomial[1 + 3 x + 2 x^2, x], x], 3],
  TestID -> "PolynomialTools-HVector-agrees-with-HStarPolynomial"
]

(* GitHub issue #51: the zero sequence gives the zero polynomial, and a cubic with three
   leading zeros is not mistaken for 0. *)
VerificationTest[
  {SequenceToPolynomial[0 &, t], SequenceToPolynomial[(# - 1) (# - 2) (# - 3) &, t],
   SequenceToPolynomial[7 &, t]},
  {0, Expand[(t - 1) (t - 2) (t - 3)], 7},
  TestID -> "PolynomialTools-SequenceToPolynomial-zero-and-leading-zeros"
]
