testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];
weightsUsageBefore = System`Weights::usage;

VerificationTest[
  Needs["ChromaticFunctions`"],
  Null,
  TestID -> "ChromaticFunctions-loads-cleanly"
]

(* GitHub issue #17: the unweighted result must not reuse a weighted cache entry. *)
VerificationTest[
  {
    GraphChromaticSymmetricPolynomial[{{1, 2}, {2, 3}}, 3, x, q] /. x[_] -> 1,
    GraphChromaticSymmetricPolynomial[
      {{1, 2}, {2, 3}}, 3, x, q, Weights -> {2, 1, 1}] /. x[_] -> 1,
    GraphChromaticSymmetricPolynomial[{{1, 2}, {2, 3}}, 3, x, q] /. x[_] -> 1
  },
  {1 + 10 q + q^2, 4 + 28 q + 4 q^2, 1 + 10 q + q^2},
  TestID -> "ChromaticFunctions-GraphChromaticSymmetricPolynomial-weight-cache"
]

(* GitHub issue #17: StripSizesToEdges returns {area, strict}, not {attacking, strict}. *)
VerificationTest[
  With[{data = StripSizesToEdges[{2, 1}]},
    Expand[VerticalStripLLTPolynomial[{2, 1}, x, q]] ===
      Expand[GraphChromaticLLTPolynomial[
        AreaToEdges[First[data]], 3, x, q, StrictEdges -> Last[data]]]
  ],
  True,
  TestID -> "ChromaticFunctions-VerticalStripLLTPolynomial-edge-semantics"
]

(* GitHub issue #17: DinvFromAreaSeq must use the input length as its bound. *)
VerificationTest[
  DinvFromAreaSeq[{0, 1, 1}],
  3,
  TestID -> "ChromaticFunctions-DinvFromAreaSeq-length-bound"
]

(* GitHub issue #17: the formula agrees with the independent AreaDinv formula. *)
VerificationTest[
  And @@ Flatten[
    Table[
      DinvFromAreaSeq[a] === AreaDinv[a],
      {len, 1, 5},
      {a, GraphAreaLists[len, Circular -> False, All -> True]}
    ]
  ],
  True,
  TestID -> "ChromaticFunctions-DinvFromAreaSeq-agrees-with-AreaDinv"
]

(* GitHub issue #17: the exported function name must have its actual definition. *)
VerificationTest[
  AreaRowPermutation[{0, 0, 0}],
  {3, 2, 1},
  TestID -> "ChromaticFunctions-AreaRowPermutation-definition"
]

(* GitHub issue #17: LLTOrientationComposition has no implementation to export. *)
VerificationTest[
  Names["ChromaticFunctions`LLTOrientationComposition"],
  {},
  TestID -> "ChromaticFunctions-LLTOrientationComposition-not-exported"
]

(* GitHub issue #17: loading must leave the System`Weights usage message unchanged. *)
VerificationTest[
  System`Weights::usage,
  weightsUsageBefore,
  TestID -> "ChromaticFunctions-Weights-does-not-overwrite-system-usage"
]

(* GitHub issue #17: deprecation message assignments must store strings. *)
VerificationTest[
  Quiet[Check[Private`DyckDiagramPlot[{}, {}, 1], $Failed]];
  Quiet[Check[Private`ColorPlot[{}, {}, Automatic], $Failed]];
  StringQ[Private`DyckDiagramPlot::deprecated] &&
    StringQ[Private`ColorPlot::deprecated],
  True,
  TestID -> "ChromaticFunctions-deprecation-messages-are-strings"
]

(* GitHub issue #17: an integer in the second slot is the maximum color. *)
VerificationTest[
  ChromaticSymmetricColorings[{1, 0, 0}, 2],
  ChromaticSymmetricColorings[{1, 0, 0}, False, 2],
  TestID -> "ChromaticFunctions-ChromaticSymmetricColorings-integer-max-color"
]

(* GitHub issue #17: at q = 1 a vertical-strip LLT polynomial is the product of
   elementary symmetric polynomials e_sizes; for two single cells it is s2 + q s11. *)
VerificationTest[
  {And @@ Table[
     With[{n = Tr[s]},
       Expand[VerticalStripLLTPolynomial[s, x, 1]
         - Times @@ (SymmetricPolynomial[#, x /@ Range[n]] & /@ s)] === 0],
     {s, {{2, 1}, {1, 1}, {2, 2}, {3, 1}, {1, 2, 1}}}],
   Expand[VerticalStripLLTPolynomial[{1, 1}, x, q]]},
  {True, Expand[x[1]^2 + x[2]^2 + (1 + q) x[1] x[2]]},
  TestID -> "ChromaticFunctions-VerticalStripLLTPolynomial-q1-elementary-product"
]
