testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

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

(* GitHub issue #51, review round 2: migrated symbols have one supported
   package owner, and all three option symbols live with GT patterns. *)
VerificationTest[
  Length /@ {
    Names["CombinatoricTools`ShapeTriplets"],
    Names["CombinatoricTools`BoxCountMatrix"],
    Names["CombinatoricTools`EnableSkew"],
    Names["CombinatoricTools`WeightRange"],
    Names["CombinatoricTools`KostkaRange"],
    Names["GTPatterns`ShapeTriplets"],
    Names["GTPatterns`BoxCountMatrix"],
    Names["GTPatterns`EnableSkew"],
    Names["GTPatterns`WeightRange"],
    Names["GTPatterns`KostkaRange"]
  },
  {0, 0, 0, 0, 0, 1, 1, 1, 1, 1},
  TestID -> "GTPatterns-migration-symbols-have-single-owner"
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

(* Regression tests for GitHub issue #14. *)
VerificationTest[
  GTPatterns[{2, 1}, {}, {1, 1, 1}, RowFlags -> {{1, 2}}],
  {GTPattern[{{0, 0}, {1, 0}, {2, 0}, {2, 1}}]},
  TestID -> "GTPatterns-row-flags-pad-by-pairs"
]

VerificationTest[
  Module[{cases, validQ},
    cases = {
      {{{2, 1}, {}, {1, 1, 1}}, {{1, 2}}},
      {{{2, 1}, {}, {1, 1, 1}}, {{1, 1}, {1, Infinity}}},
      {{{2, 2}, {}, {1, 1, 1, 1}}, {{1, 2}}}
    };
    validQ[YoungTableau[tab_], flags_] :=
      And @@ MapThread[
        Function[{row, range},
          With[{lo = range[[1]], hi = range[[2]]},
            AllTrue[DeleteCases[row, None], lo <= # <= hi &]]],
        {tab, PadRight[flags, Length[tab], {{1, Infinity}}]}
      ];
    And @@ (Function[case,
      Module[{shape = case[[1]], flags = case[[2]], all, actual},
        all = SemiStandardYoungTableaux[{shape[[1]], shape[[2]]}, shape[[3]]];
        flags = PadRight[flags, Length[shape[[1]]], {{1, Infinity}}];
        actual = YoungTableau /@ GTPatterns[Sequence @@ shape,
          RowFlags -> case[[2]]];
        SortBy[Select[all, validQ[#, flags] &], ToString[InputForm[#]] &] ===
          SortBy[actual, ToString[InputForm[#]] &]
      ]] /@ cases)
  ],
  True,
  TestID -> "GTPatterns-row-flags-match-brute-force-filter"
]

VerificationTest[
  GTPatterns[{2, 1}, {}, {1, 1, 1}, RowFlags -> {{0, 2}}],
  {},
  {GTPatterns::rowflags},
  TestID -> "GTPatterns-row-flags-reject-invalid-lower-bound"
]

(* Regression coverage for the OldYoungTableaux migration in GitHub issue #51. *)
Quiet[Needs["OldYoungTableaux`"]];
Quiet[Needs["SymmetricFunctions`"]];

legacyToNew[OldYoungTableaux`GTPattern[rows_]] :=
  GTPatterns`GTPattern[Reverse[rows]];
newToLegacy[GTPatterns`GTPattern[rows_]] :=
  OldYoungTableaux`GTPattern[Reverse[rows]];
sortExpressions[exprs_List] := Sort[ToString[InputForm[#]] & /@ exprs];
newCoordinates[expr_, height_Integer] :=
  expr /. pair : {_Integer, _Integer} :>
    {height + 1 - pair[[1]], pair[[2]]};

VerificationTest[
  And @@ Flatten[Table[
    Length[GTPatterns`BZPatterns[lam, mu, nu]] ===
      SymmetricFunctions`LRCoefficient[mu, nu, lam],
    {n, 1, 6}, {lam, IntegerPartitions[n]}, {m, 0, n},
    {mu, IntegerPartitions[m]}, {nu, IntegerPartitions[n - m]}]],
  True,
  TestID -> "GTPatterns-BZPatterns-agrees-with-LR-coefficients-up-to-degree-six"
]

VerificationTest[
  Module[{b = First[GTPatterns`BZPatterns[{2}, {1}, {1}]], old},
    old = First[OldYoungTableaux`BZPatterns[{2}, {1}, {1}]];
    Head[b] === GTPatterns`BZPattern && b[[1]] === old[[1]]
  ],
  True,
  TestID -> "GTPatterns-BZPattern-representation-matches-legacy"
]

VerificationTest[
  Module[{cases = {
      {{2, 1}, {}, {1, 1, 1}},
      {{3, 1}, {1}, {1, 1}},
      {{2, 2}, {1}, {1, 1}},
      {{3, 2, 1}, {1, 1}, {1, 1, 1, 1}}
    }},
    And @@ (sortExpressions[GTPatterns`GTPatterns @@ #] ===
        sortExpressions[legacyToNew /@
          (OldYoungTableaux`GTPatterns @@ #)] & /@ cases)
  ],
  True,
  TestID -> "GTPatterns-pattern-generation-matches-legacy-orientation"
]

VerificationTest[
  Module[{g = First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]],
    h = First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]], old},
    old = OldYoungTableaux`GTPlus[newToLegacy[g], newToLegacy[h]];
    GTPatterns`GTPlus[g, h] === legacyToNew[old]
  ],
  True,
  TestID -> "GTPatterns-GTPlus-matches-legacy"
]

VerificationTest[
  Module[{g = First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]]},
    GTPatterns`GTMonomial[g, x] ===
      OldYoungTableaux`GTMonomial[newToLegacy[g], x]
  ],
  True,
  TestID -> "GTPatterns-GTMonomial-matches-legacy"
]

VerificationTest[
  {Length[GTPatterns`GogPatterns[#]], Length[GTPatterns`MagogPatterns[#]]} & /@
      Range[5],
  {{1, 1}, {2, 2}, {7, 7}, {42, 42}, {429, 429}},
  TestID -> "GTPatterns-Gog-and-Magog-known-counts"
]

VerificationTest[
  Module[{n = 3},
    sortExpressions[GTPatterns`GogPatterns[n]] ===
      sortExpressions[legacyToNew /@ OldYoungTableaux`GogPatterns[n]] &&
    sortExpressions[GTPatterns`MagogPatterns[n]] ===
      sortExpressions[legacyToNew /@ OldYoungTableaux`MagogPatterns[n]]
  ],
  True,
  TestID -> "GTPatterns-Gog-and-Magog-match-legacy-orientation"
]

VerificationTest[
  Module[{g = First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]], h = 4,
    oldTiles, oldSnakes},
    oldTiles = newCoordinates[OldYoungTableaux`GTTiles[newToLegacy[g]], h];
    oldSnakes = newCoordinates[OldYoungTableaux`GTSnakes[newToLegacy[g]], h];
    GTPatterns`GTTiles[g] === oldTiles &&
      GTPatterns`GTSnakes[g] === oldSnakes
  ],
  True,
  TestID -> "GTPatterns-tiles-and-snakes-match-legacy-orientation"
]

VerificationTest[
  Module[{g = First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]],
    graphics, tikz},
    graphics = GTPatterns`GTPatternForm[g, GTPatterns`GTPartition -> "Tiles"];
    tikz = GTPatterns`GTPatternTikz[g, GTPatterns`GTPartition -> "Snakes"];
    Head[graphics] === Graphics && StringQ[tikz] && StringContainsQ[tikz, "tikzpicture"]
  ],
  True,
  TestID -> "GTPatterns-graphics-and-tikz-evaluate-without-messages"
]

VerificationTest[
  Module[{g = First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]], form, tikz},
    form = GTPatterns`LatticePathForm[g];
    tikz = GTPatterns`LatticePathTikz[g];
    Head[form] === Graphics && StringQ[tikz] && StringContainsQ[tikz, "tikzpicture"]
  ],
  True,
  TestID -> "GTPatterns-lattice-path-forms-evaluate"
]

VerificationTest[
  Module[{g = First[GTPatterns`GTPatterns[{3, 2, 1}, {}, {1, 1, 1, 1, 1, 1}]]},
    (GTPatterns`TilingMatrix[g] === {{}} ||
      MatrixQ[GTPatterns`TilingMatrix[g], IntegerQ]) &&
      GTPatterns`ContainingFaceDimension[g] >= 0
  ],
  True,
  TestID -> "GTPatterns-polytope-data-is-integral"
]

VerificationTest[
  Module[{values, poly},
    values = Table[GTPatterns`GTEhrhartPolynomial[{2, 1}, {}, {1, 1, 1}, k],
      {k, 1, 4}];
    poly = GTPatterns`GTEhrhartPolynomial[{2, 1}, {}, {1, 1, 1}, z];
    values === Table[Length[GTPatterns`GTPatterns[k {2, 1}, {}, k {1, 1, 1}]],
        {k, 1, 4}] && (poly /. z -> Range[1, 4]) === values
  ],
  True,
  TestID -> "GTPatterns-Ehrhart-polynomial-matches-pattern-counts"
]

(* Regression tests for GitHub issue #51, review round 2. *)
oldGraphicCoordinatesToNew[{x_, y_}, height_Integer] :=
  {x - height - 1, y + height};
polygonCycleKey[polygon_List] := Module[{p = polygon, rotations},
  If[Length[p] > 1 && First[p] === Last[p], p = Most[p]];
  rotations = Join[
    RotateLeft[p, #] & /@ Range[0, Length[p] - 1],
    RotateLeft[Reverse[p], #] & /@ Range[0, Length[p] - 1]
  ];
  First[SortBy[rotations, ToString[InputForm[#]] &]]
];

VerificationTest[
  Module[{g, cases, legacyPolygons, newPolygons, skew},
    cases = {
      {First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]], False},
      {First[GTPatterns`GTPatterns[{2, 1}, {1}, {1, 1}]], True}
    };
    And @@ (Function[case,
      {g, skew} = case;
      legacyPolygons = Map[
        OldYoungTableaux`Private`ConnectedComponentPolygon,
        OldYoungTableaux`GTTiles[newToLegacy[g],
          OldYoungTableaux`EnableSkew -> skew], {2}];
      legacyPolygons = Map[
        oldGraphicCoordinatesToNew[#, Length[g[[1]]]] &, legacyPolygons, {3}];
      newPolygons = GTPatterns`Private`gtPatternTiles[g, skew];
      Map[polygonCycleKey, newPolygons, {2}] ===
        Map[polygonCycleKey, legacyPolygons, {2}]
    ] /@ cases)
  ],
  True,
  TestID -> "GTPatterns-tile-polygons-match-legacy-boundaries"
]

VerificationTest[
  Length[GTPatterns`Private`gtConnectedPolygon[{{1, 1}}]],
  4,
  TestID -> "GTPatterns-single-cell-boundary-has-four-vertices"
]

VerificationTest[
  GTPatterns`Private`GTPatternTextLabels[
    First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]], False] // Length,
  Binomial[4, 2],
  TestID -> "GTPatterns-non-skew-label-count-is-triangle-count"
]

VerificationTest[
  Module[{g = First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]],
    labels, tikz},
    labels = GTPatterns`Private`GTPatternTextLabels[g, False];
    tikz = GTPatterns`GTPatternTikz[g, GTPatterns`EnableSkew -> False];
    Sort[First /@ labels] === Sort[GTPatterns`Private`GTIndexToGrahphicsCoordinates /@
        {{2, 1}, {3, 1}, {3, 2}, {4, 1}, {4, 2}, {4, 3}}] &&
      StringContainsQ[tikz, "\\node at (-2,4)"]
  ],
  True,
  TestID -> "GTPatterns-renderers-use-bottom-to-top-coordinates"
]

VerificationTest[
  Length[DownValues[GTPatterns`GTPatternForm]],
  1,
  TestID -> "GTPatterns-form-has-one-unified-downvalue"
]

VerificationTest[
  Module[{g = First[GTPatterns`GTPatterns[{2, 1}, {}, {1, 1, 1}]],
    tiles, formTiles, formShaded, formSnakes},
    tiles = GTPatterns`GTTiles[g, GTPatterns`EnableSkew -> False];
    formTiles = GTPatterns`GTPatternForm[g,
      GTPatterns`EnableSkew -> False, GTPatterns`GTPartition -> "Tiles"];
    formShaded = GTPatterns`GTPatternForm[g,
      GTPatterns`EnableSkew -> False,
      GTPatterns`GTPartition -> "ShadedTiles"];
    formSnakes = GTPatterns`GTPatternForm[g,
      GTPatterns`EnableSkew -> False, GTPatterns`GTPartition -> "Snakes"];
    Length[Cases[formTiles, _Line, Infinity]] === Total[Length /@ tiles] &&
      Length[Cases[formShaded, _Polygon, Infinity]] === Length[Last[tiles]] &&
      Length[Cases[formShaded, _Line, Infinity]] === Length[First[tiles]] &&
      Length[Cases[formSnakes, _Line, Infinity]] === Length[GTPatterns`GTSnakes[g]]
  ],
  True,
  TestID -> "GTPatterns-form-partition-primitives-have-right-counts"
]

VerificationTest[
  StringContainsQ[GTPatterns`GTEhrhartPolynomial::usage, "symbolic k"] &&
    !StringContainsQ[GTPatterns`GTEhrhartPolynomial::usage, "k is omitted"],
  True,
  TestID -> "GTPatterns-Ehrhart-usage-documents-symbolic-k"
]

VerificationTest[
  Length[DownValues[GTPatterns`Private`gtFloodFillTile]],
  1,
  TestID -> "GTPatterns-flood-fill-has-one-three-argument-definition"
]

VerificationTest[
  {
    GTPatterns`ShapeTriplets[{2, 1}, GTPatterns`EnableSkew -> False],
    Sort[GTPatterns`ShapeTriplets[{2, 1}, GTPatterns`WeightRange -> {2, 2}]]
  },
  {
    {{{2, 1}, {0, 0}, {3}}, {{2, 1}, {0, 0}, {2, 1}},
      {{2, 1}, {0, 0}, {1, 1, 1}}},
    {{{2, 1}, {0}, {2, 1}}, {{2, 1}, {1}, {1, 1}}}
  },
  TestID -> "GTPatterns-ShapeTriplets-weight-range"
]

VerificationTest[
  Module[{cases, independentMatrix, patterns},
    cases = {
      {{2, 1}, {}, {1, 1, 1}},
      {{3, 1}, {1}, {1, 1}},
      {{3, 2, 1}, {1, 1}, {1, 1, 1, 1}}
    };
    independentMatrix[g_GTPatterns`GTPattern] := Module[{tab, maxEntry},
      tab = First[NewTableaux`YoungTableau[g]];
      maxEntry = Max[DeleteCases[Flatten[tab], None]];
      Table[Count[tab[[i]], j], {i, Length[tab]}, {j, maxEntry}]
    ];
    And @@ Flatten[
      Function[shape,
        patterns = GTPatterns`GTPatterns @@ shape;
        (Function[g,
          GTPatterns`BoxCountMatrix[g] ===
              OldYoungTableaux`BoxCountMatrix[
                OldYoungTableaux`GTPattern[Reverse[g[[1]]]] ] &&
            GTPatterns`BoxCountMatrix[g] === independentMatrix[g]
        ] /@ patterns)
      ] /@ cases
    ]
  ],
  True,
  TestID -> "GTPatterns-BoxCountMatrix-orientation-and-tableau-counts"
]
