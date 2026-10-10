VerificationTest[
  Needs["NewTableaux`"],
  Null,
  TestID -> "NewTableaux-loads-cleanly"
]

Needs["NonsymmetricPolynomials`"];

(* Regression tests for GitHub issue #14. *)
VerificationTest[
  KnuthRepresentative[{2, 1, 3}],
  {2, 1, 3},
  TestID -> "NewTableaux-KnuthRepresentative-terminates-and-returns-reading-word"
]

VerificationTest[
  And @@ (BiwordRSK[KnuthRepresentative[#]][[1]] === BiwordRSK[#][[1]] & /@
    Permutations[Range[4]]),
  True,
  TestID -> "NewTableaux-KnuthRepresentative-preserves-P-tableau"
]

VerificationTest[
  With[{result = BiwordRSK[{1, 2}, {2.5, 1}]},
    HoldComplete[result] === HoldComplete[BiwordRSK[{1, 2}, {2.5, 1}]]],
  True,
  TestID -> "NewTableaux-BiwordRSK-invalid-two-word-input-does-not-recurse"
]

VerificationTest[
  With[{result = BiwordRSKDual[{1, 2}, {2.5, 1}]},
    HoldComplete[result] === HoldComplete[BiwordRSKDual[{1, 2}, {2.5, 1}]]],
  True,
  TestID -> "NewTableaux-BiwordRSKDual-invalid-two-word-input-does-not-recurse"
]

VerificationTest[
  CrystalSi[{1, 1, 2}, 1],
  {1, 2, 2},
  TestID -> "NewTableaux-CrystalSi-acts-on-words"
]

VerificationTest[
  With[{word = {1, 1, 2}},
    CrystalSi[word, 1] ===
      CrystalSi[YoungTableau[{word}], 1][[1, 1]] &&
    CrystalSi[CrystalSi[word, 1], 1] === word],
  True,
  TestID -> "NewTableaux-CrystalSi-word-tableau-agreement-and-involution"
]

VerificationTest[
  BSTHeightVector[First[BorderStripTableaux[{2, 1}, {2, 1}]]],
  {1, 0},
  TestID -> "NewTableaux-BSTHeightVector-accepts-border-strip-lists"
]

VerificationTest[
  SemiStandardYoungTableaux[{{1}, {1}}, {}],
  {YoungTableau[{{None}}]},
  TestID -> "NewTableaux-SemiStandardYoungTableaux-empty-skew-filling"
]

VerificationTest[
  SemiStandardYoungTableaux[{{2, 1}, {}}, {1, 1, 1}];
  {ValueQ[NewTableaux`Private`g], DownValues[NewTableaux`Private`pathToSSYT]},
  {False, {}},
  TestID -> "NewTableaux-SemiStandardYoungTableaux-does-not-leak-locals"
]

VerificationTest[
  {Length[CylindricTableaux[{2, 1}, 0]],
    Length[CylindricSYT[{2, 1}, 0]]},
  {2, 1},
  TestID -> "NewTableaux-CylindricTableaux-counts-unchanged"
]

(* GitHub issue #51: merge the legacy TeX options and outer-corner behavior
   into the supported YoungTableau representation. *)
VerificationTest[
  {
    YTableauTeX[YoungTableau[{{1, 2}, {None, 3}}], LineBreaks -> False],
    YTableauTeX[YoungTableau[{{1, 2}, {None, 3}}], UseArray -> False],
    TableauShortTeX[YoungTableau[{{1, 2}, {None, 3}}]],
    HasOuterCornerQ[YoungTableau[{{None, 1, 2}, {4, 5}}]],
    HasOuterCornerQ[YoungTableau[{{1, 2}, {3}}]]
  },
  {
    StringJoin["\\begin{ytableau}", "1 & 2", "\\\\", "\\none & 3", "\\\\", "\\end{ytableau}"],
    "\\young(12,:3)",
    "\\ytableaushort{12,{\\none}3}",
    True,
    False
  },
  TestID -> "NewTableaux-TeX-options-and-outer-corner"
]

(* Regression tests for GitHub issue #51: migrate semistandard augmented
   fillings from the legacy Macdonald implementation. *)
VerificationTest[
  With[{f = First[SSAFillings[{0, 2, 1}, {1, 2, 3}]]},
    {Head[f], SSAFShape[f], SSAFBasement[f], SSAFWeight[f],
      SSAFMonomial[f, x], SSAFQ[f], Length[SSAFillings[{0, 2, 1}, {1, 2, 3}]]}],
  {SSAF, {0, 2, 1}, {1, 2, 3}, {1, 1, 1}, x[1] x[2] x[3], True, 2},
  TestID -> "NewTableaux-SSAF-representation-and-generation"
]

VerificationTest[
  With[{f = First[AtomFillings[{0, 2, 1}]]},
    {SSAFMajorIndex[f], SSAFInversions[f], SSAFCoInversions[f],
      SSAFDn[f], SSAFColumnSets[f]}],
  {0, 3, 0, 1, {{2, 3}, {1}}},
  TestID -> "NewTableaux-SSAF-statistics-and-column-sets"
]

VerificationTest[
  Module[{alphas, x},
    alphas = DeleteDuplicates[Flatten[Table[
      Permutations[PadRight[#, 3]] & /@ IntegerPartitions[m, {1, 3}],
      {m, 1, 4}], 2]];
    And @@ Flatten[Table[
      Expand[Total[(SSAFMonomial[#, x] &) /@ AtomFillings[a]]] ===
        NonsymmetricPolynomials`AtomPolynomial[a, x] &&
      Expand[Total[(SSAFMonomial[#, x] &) /@ KeyFillings[a]]] ===
        NonsymmetricPolynomials`KeyPolynomial[a, x],
      {a, alphas}]]],
  True,
  TestID -> "NewTableaux-SSAF-atom-and-key-index-convention"
]

VerificationTest[
  Module[{alphas, x, t},
    alphas = DeleteDuplicates[Flatten[Table[
      Permutations[PadRight[#, 3]] & /@ IntegerPartitions[m, {1, 3}],
      {m, 1, 4}], 2]];
    And @@ Table[
      Expand[Total[(t^SSAFCoInversions[#] (1 - t)^SSAFDn[#] SSAFMonomial[#, x]) & /@
        TAtomFillings[a]]] === NonsymmetricPolynomials`TAtomPolynomial[a, x, t],
      {a, alphas}]],
  True,
  TestID -> "NewTableaux-SSAF-t-atom-identity"
]

VerificationTest[
  With[{s = First[AtomFillings[{0, 2, 1}]], tab = YoungTableau[{{1, 2}, {2, 3}}]},
    With[{e = CrystalEi[s, 1]},
      e =!= {} && CrystalFi[e, 1] === s &&
        SSAFWeight[e] - SSAFWeight[s] === {-1, 1, 0} &&
        MemberQ[SSAFillings[SSAFShape[s], SSAFBasement[s]], e] &&
        LascouxSchutzenberger[tab, 1] === CrystalSi[tab, 1]]],
  True,
  TestID -> "NewTableaux-SSAF-crystals-and-tableau-involution"
]

VerificationTest[
  Module[{fillings = AtomFillings[{0, 2, 1}]},
    And @@ Flatten[Table[
      MemberQ[SSAFillings[SSAFShape[s], SSAFBasement[s]], CrystalSi[s, i]] &&
        CrystalSi[CrystalSi[s, i], i] === s,
      {s, fillings}, {i, 1, 2}]]],
  True,
  TestID -> "NewTableaux-SSAF-crystal-reflections-preserve-fillings"
]

VerificationTest[
  Module[{comps, fillings, expectedWeight, out},
    comps = DeleteDuplicates[Flatten[Table[
      Permutations[PadRight[#, 3]] & /@ IntegerPartitions[m, {1, 3}],
      {m, 1, 4}], 2]];
    fillings = DeleteDuplicates[Join @@ (Join[AtomFillings[#], KeyFillings[#]] & /@ comps)];
    And @@ Flatten[Table[
      out = LascouxSchutzenberger[s, i];
      expectedWeight = ReplacePart[SSAFWeight[s],
        {i -> SSAFWeight[s][[i + 1]], i + 1 -> SSAFWeight[s][[i]]}];
      If[Select[SSAFillings[SSAFShape[s], SSAFBasement[s]],
          SSAFWeight[#] === expectedWeight &] === {}, True,
        SSAFQ[out] &&
          MemberQ[SSAFillings[SSAFShape[s], SSAFBasement[s]], out] &&
          SSAFWeight[out] === expectedWeight &&
          LascouxSchutzenberger[out, i] === s],
      {s, fillings}, {i, 1, 2}]]],
  True,
  TestID -> "NewTableaux-SSAF-LascouxSchutzenberger-exhaustive-valid-fillings"
]

VerificationTest[
  Module[{comps, fillings, check},
    comps = DeleteDuplicates[Flatten[Table[
      Permutations[PadRight[#, 3]] & /@ IntegerPartitions[m, {1, 3}],
      {m, 1, 4}], 2]];
    fillings = DeleteDuplicates[Join @@ (Join[AtomFillings[#], KeyFillings[#]] & /@ comps)];
    check[s_SSAF] := Module[{w, p, target, out},
      out = SSAFWeightNormalize[s];
      w = SSAFWeight[out];
      p = FirstPosition[Table[w[[j]] < w[[j + 1]],
          {j, Length[w] - 1}], True, Missing[]];
      target = If[MissingQ[p], {},
        ReplacePart[w, {First[p] -> w[[First[p] + 1]],
          First[p] + 1 -> w[[First[p]]]}]];
      SSAFQ[out] && SSAFShape[out] === SSAFShape[s] &&
        SSAFBasement[out] === SSAFBasement[s] &&
        (SSAFWeight[out] === Sort[SSAFWeight[out], Greater] ||
          target === {} ||
          Select[SSAFillings[SSAFShape[s], SSAFBasement[s]],
            SSAFWeight[#] === target &] === {})];
    And @@ (check /@ fillings)],
  True,
  TestID -> "NewTableaux-SSAF-weight-normalize-exhaustive-reachable-domain"
]

VerificationTest[
  With[{s = First[AtomFillings[{0, 2, 1}]]},
      Names["NewTableaux`SSAFRaising"] === {} &&
      Names["NewTableaux`SSAFLowering"] === {} &&
      Names["NewTableaux`ModifiedLascouxSchutzenberger"] === {} &&
      SSAFQ[CrystalEi[s, 1]] && CrystalFi[CrystalEi[s, 1], 1] === s],
  True,
  TestID -> "NewTableaux-SSAF-legacy-operator-names-not-exported"
]

VerificationTest[
  With[{s = First[AtomFillings[{0, 2, 1}]]},
    {LascouxSchutzenberger[s, 1] === CrystalSi[s, 1],
      SSAFWeight[SSAFWeightNormalize[s]],
      SSAFKnownCharge[First[AtomFillings[{2, 1, 0}]]],
      ChargeToMajMap[First[AtomFillings[{2, 1, 0}]]]}],
  {True, {1, 1, 1}, 0,
    First[AtomFillings[{2, 1, 0}]]},
  TestID -> "NewTableaux-SSAF-involutions-and-charge"
]

VerificationTest[
  With[{tab = YoungTableau[{{1, 1, 2}, {2, 3}}]},
    {SSYTToAtom[tab], SSAFWeight[SSYTToAtom[tab]]}],
  {SSAF[{{1}, {2, 2, 2, 1}, {3, 3, 1}}], {2, 2, 1}},
  TestID -> "NewTableaux-SSYTToAtom-Mason-insertion"
]

VerificationTest[
  Module[{fillMax, tabs, images, atoms, check, n, lam, size},
    fillMax[SSAF[rows_]] := Max[Flatten[Rest /@ rows]];
    check[n_, lam_] := Module[{},
      size = Total[lam];
      tabs = DeleteDuplicates[Join @@ Table[
        SemiStandardYoungTableaux[{lam, {}}, w],
        {w, Select[Tuples[Range[0, size], n], Total[#] == size &]}]];
      images = SSYTToAtom /@ tabs;
      atoms = Select[DeleteDuplicates[Join @@ Flatten[Table[
        AtomFillings /@ DeleteDuplicates[Permutations[PadRight[lam, k]]],
        {k, Max[1, Length[lam]], n}], 1]],
        fillMax[#] === Length[SSAFBasement[#]] &];
      DuplicateFreeQ[images] &&
        Length[Complement[images, atoms]] === 0 &&
        Length[Complement[atoms, images]] === 0 &&
        And @@ MapThread[
          SSAFQ[#1] && SSAFBasement[#1] === Range[Max[Flatten[#2[[1]]]]] &&
            Sort[DeleteCases[SSAFShape[#1], 0], Greater] === Sort[lam, Greater] &&
            SSAFWeight[#1] ===
              PadRight[YoungTableauWeight[#2], Max[Flatten[#2[[1]]]]] &,
          {images, tabs}]
    ];
    And @@ Flatten[Table[
      check[n, lam], {n, 1, 3}, {size, 1, 4},
      {lam, Select[IntegerPartitions[size], Length[#] <= n &]}]]],
  True,
  TestID -> "NewTableaux-SSYTToAtom-exhaustive-Mason-bijection"
]

VerificationTest[
  Module[{parts, fillings},
    parts = Join @@ Table[Select[IntegerPartitions[m], Length[#] <= 3 &], {m, 1, 5}];
    fillings = Join @@ (Select[AtomFillings[#],
        SSAFWeight[#] === Sort[SSAFWeight[#], Greater] &] & /@ parts);
    And @@ (SSAFQ[ChargeToMajMap[#]] &&
        SSAFMajorIndex[ChargeToMajMap[#]] === SSAFKnownCharge[#] & /@ fillings)],
  True,
  TestID -> "NewTableaux-SSAF-charge-major-map-exhaustive"
]

VerificationTest[
  With[{rpp = SSAF[{{1, 1}, {2, 2}, {3}}]},
    SSAFColumnSets[RPPToAtom[rpp]] === SSAFColumnSets[rpp]],
  True,
  TestID -> "NewTableaux-RPPToAtom-preserves-column-sets"
]
