VerificationTest[
  Needs["LegacyConversions`"],
  Null,
  TestID -> "LegacyConversions-loads-cleanly"
]

(* GitHub issue #51: the converter package must not load legacy packages. *)
VerificationTest[
  Complement[{"OldYoungTableaux`", "MacdonaldPolynomials`", "ChromaticFunctions`"}, $Packages],
  {"ChromaticFunctions`", "MacdonaldPolynomials`", "OldYoungTableaux`"},
  TestID -> "LegacyConversions-does-not-load-legacy-packages"
]

(* GitHub issue #51: loading LegacyConversions first leaves the legacy packages loadable without
   shadowing messages, and unknown families are reported. *)
VerificationTest[
  {Intersection[$ContextPath, {"NonsymmetricPolynomials`", "UnicellularChromatics`"}],
   Quiet[FromLegacyIndex["Kye", {1, 0}], FromLegacyIndex::family]},
  {{}, $Failed},
  TestID -> "LegacyConversions-no-dependencies-on-context-path"
]

VerificationTest[
  Needs["MacdonaldPolynomials`"],
  Null,
  TestID -> "LegacyConversions-then-MacdonaldPolynomials-loads-cleanly"
]

VerificationTest[
  Module[{g = OldYoungTableaux`GTPattern[{{2, 1}, {1, 0}, {0, 0}}]},
    ToLegacyGTPattern[FromLegacyGTPattern[g]] === g &&
      FromLegacyGTPattern[g] === GTPatterns`GTPattern[{{0, 0}, {1, 0}, {2, 1}}]
  ],
  True,
  TestID -> "LegacyConversions-GTPattern-round-trip-and-orientation"
]

VerificationTest[
  Quiet[Needs["OldYoungTableaux`"], General::shdw];
  Module[{cases = {
      {{2, 1}, {}, {1, 1, 1}}, {{3, 1}, {1}, {1, 1}},
      {{2, 2}, {1}, {1, 1}}}},
    And @@ (With[{old = OldYoungTableaux`GTPatterns @@ #,
        new = GTPatterns`GTPatterns @@ #},
        Sort[ToString[InputForm[#]] & /@ (FromLegacyGTPattern /@ old)] ===
          Sort[ToString[InputForm[#]] & /@ new]] & /@ cases)
  ],
  True,
  TestID -> "LegacyConversions-GTPatterns-match-supported-generation"
]

VerificationTest[
  Module[{old = OldYoungTableaux`YoungTableau[{
      {OldYoungTableaux`Private`SKEW, 1, 2}, {3, 4}}], new},
    new = FromLegacyYoungTableau[old];
    {new, ToLegacyYoungTableau[new], NewTableaux`YoungTableauSize[new]} ===
      {NewTableaux`YoungTableau[{{None, 1, 2}, {3, 4}}], old, 4}
  ],
  True,
  TestID -> "LegacyConversions-YoungTableau-skew-marker-round-trip"
]

VerificationTest[
  Module[{old, new, oldShape},
    old = OldYoungTableaux`YoungTableau[{{OldYoungTableaux`Private`SKEW, 1}, {2}}];
    new = FromLegacyYoungTableau[old];
    oldShape = OldYoungTableaux`ToTableauShape[old];
    {NewTableaux`YoungTableauShape[new], oldShape[[1]], oldShape[[2]],
     NewTableaux`YoungTableauWeight[new]} ===
      {{2, 1}, {2, 1}, {1, 0}, {1, 1}}],
  True,
  TestID -> "LegacyConversions-YoungTableau-shape-and-content"
]

VerificationTest[
  (* LegacyConversions loads no supported package onto $ContextPath; load the ones compared. *)
  Quiet[Needs["ChromaticFunctions`"]; Needs["UnicellularChromatics`"], General::shdw];
  Module[{areas = Flatten[Table[
      ChromaticFunctions`GraphAreaLists[n, All -> True, Circular -> False],
      {n, 1, 4}], 1]},
    And @@ (ToLegacyAreaList[FromLegacyAreaList[#]] === # & /@ areas) &&
      And @@ Table[
        With[{n = Length[old], new = FromLegacyAreaList[old]},
          Sort[UnicellularChromatics`UnitIntervalEdges[new]] ===
            Sort[FromLegacyEdges[ChromaticFunctions`AreaToEdges[old], n]]],
        {old, areas}]
  ],
  True,
  TestID -> "LegacyConversions-area-and-edge-conversions"
]

VerificationTest[
  Module[{old = {0, 1, 1}, new = FromLegacyAreaList[{0, 1, 1}]},
    Expand[ChromaticFunctions`GraphChromaticSymmetricPolynomial[old, z, q] -
      UnicellularChromatics`GraphChromaticSymmetricPolynomial[new, z, q]] === 0 &&
      ToLegacyAreaList[new] === old &&
      ToLegacyEdges[FromLegacyEdges[{{1, 2}, {2, 3}}, 3], 3] === {{1, 2}, {2, 3}}
  ],
  True,
  TestID -> "LegacyConversions-area-semantic-equivalence"
]

VerificationTest[
  Module[{alpha = {0, 2, 1}},
    {FromLegacyKeyIndex[alpha],
     FromLegacyIndex["Key", alpha], FromLegacyIndex["TKey", alpha],
     FromLegacyIndex["Lock", alpha], FromLegacyIndex["Atom", alpha],
     FromLegacyIndex["TAtom", alpha], FromLegacyIndex["Slide", alpha]}
  ],
  {{1, 2, 0}, {1, 2, 0}, {1, 2, 0}, {1, 2, 0}, {0, 2, 1},
   {0, 2, 1}, {0, 2, 1}},
  TestID -> "LegacyConversions-index-family-rules"
]

VerificationTest[
  Quiet[Needs["MacdonaldPolynomials`"], General::shdw];
  Module[{alphas = {{0, 1}, {1, 0}, {1, 1}, {0, 2, 1}}, x},
    And @@ Table[
      Expand[NonsymmetricPolynomials`KeyPolynomial[
          FromLegacyKeyIndex[a], x] - MacdonaldPolynomials`KeyPolynomial[a, x]] === 0 &&
        Expand[NonsymmetricPolynomials`TKeyPolynomial[
          FromLegacyIndex["TKey", a], x, t] -
          MacdonaldPolynomials`OperatorKeyTPolynomial[a, x, t]] === 0 &&
        Expand[NonsymmetricPolynomials`LockPolynomial[
          FromLegacyIndex["Lock", a], x] -
          MacdonaldPolynomials`LockPolynomial[a, x]] === 0,
      {a, alphas}]
  ],
  True,
  TestID -> "LegacyConversions-key-tkey-lock-semantic-equivalence"
]

VerificationTest[
  Quiet[Needs["MacdonaldPolynomials`"], General::shdw];
  Module[{alphas = {{0, 1}, {1, 0}, {1, 1}, {0, 2, 1}}, x},
    Length[DownValues[LegacyConversions`FromLegacyIndex]] > 0 &&
      And @@ Table[
        Expand[NonsymmetricPolynomials`AtomPolynomial[a, x] -
            MacdonaldPolynomials`AtomPolynomial[a, x]] === 0,
        {a, alphas}]
  ],
  True,
  TestID -> "LegacyConversions-atom-index-unchanged-semantic-equivalence"
]

