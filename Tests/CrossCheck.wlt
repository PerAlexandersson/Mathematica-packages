testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["SymmetricFunctions`"],
  Null,
  TestID -> "SymmetricFunctions-loads-cleanly"
]

VerificationTest[
  Needs["UnicellularChromatics`"],
  Null,
  TestID -> "UnicellularChromatics-loads-cleanly"
]

VerificationTest[
  Needs["QuasiSymmetricFunctions`"],
  Null,
  TestID -> "QuasiSymmetricFunctions-loads-cleanly"
]

VerificationTest[
  Needs["MacdonaldPolynomials`"],
  Null,
  TestID -> "MacdonaldPolynomials-loads-cleanly"
]

VerificationTest[
  Needs["PolynomialTools`"],
  Null,
  TestID -> "PolynomialTools-loads-cleanly"
]

VerificationTest[
  Needs["CombinatoricTools`"],
  Null,
  TestID -> "CombinatoricTools-loads-cleanly"
]

VerificationTest[
  Needs["PermutationTools`"],
  Null,
  TestID -> "PermutationTools-loads-cleanly"
]

VerificationTest[
  Quiet[Needs["PosetData`"]],
  Null,
  TestID -> "PosetData-loads-cleanly"
]

VerificationTest[
  Needs["GraphTools`"],
  Null,
  TestID -> "GraphTools-loads-cleanly"
]

VerificationTest[
  Needs["MatroidTools`"],
  Null,
  TestID -> "MatroidTools-loads-cleanly"
]

fixture[name_String] := Import[
  FileNameJoin[{DirectoryName[$InputFileName], "fixtures", "rust", name}],
  "RawJSON"
];

parseCoefficient[value_] := If[StringQ[value], ToExpression[value], value];

termsExpression[terms_List, basis_, variable_: None] := Total[
  (parseCoefficient[#[[2]]] basis[#[[1]], variable]) & /@ terms
];

qTermsExpression[terms_List, basis_, q_] := Total[
  (Function[term, Total[MapIndexed[#1 q^(First[#2] - 1) &, term[[2]]]] basis[term[[1]]]]) /@ terms
];

qTermsExpressionWithAlphabet[terms_List, basis_, q_] := Total[
  (Function[term, Total[MapIndexed[#1 q^(First[#2] - 1) &, term[[2]]]] basis[term[[1]], None]]) /@ terms
];

sameExpressionQ[left_, right_] := Expand[Together[left - right]] === 0;

basisFunction["m"] := MonomialSymmetric;
basisFunction["e"] := ElementaryESymmetric;
basisFunction["h"] := CompleteHSymmetric;
basisFunction["p"] := PowerSumSymmetric;
basisFunction["s"] := SchurSymmetric;

macdonaldRecordQ[record_] := Module[{mu, b, nabla},
  mu = Lookup[record, "partition"];
  b = Total[Function[term, term[[3]] q^term[[1]] t^term[[2]]] /@
    Lookup[record, "B_terms"]];
  nabla = Total[Function[term, term[[3]]/term[[4]] q^term[[1]] t^term[[2]]] /@
    Lookup[record, "nabla_terms"]];
  (* Both sides use Haglund's convention B_mu = sum q^a'(c) t^l'(c) (issue #35). *)
  sameExpressionQ[
    ToMacdonaldHBasis[
      DeltaOperator[ElementaryESymmetric[1], MacdonaldHSymmetric[mu, q, t], q, t], q, t],
    b MacdonaldHSymbol[mu, None]] &&
   sameExpressionQ[
    ToMacdonaldHBasis[NablaOperator[MacdonaldHSymmetric[mu, q, t], q, t], q, t],
    nabla MacdonaldHSymbol[mu, None]]
];

lltRecordQ[record_] := Module[{area, raw, shifted},
  area = Lookup[record, "area"];
  raw = Lookup[record, "monomial_q_terms"];
  shifted = Lookup[record, "q_plus_one_elementary_terms"];
  sameExpressionQ[UnicellularLLTSymmetric[area, q],
    qTermsExpression[raw, MonomialSymbol, q]] &&
   sameExpressionQ[ToElementaryEBasis[UnicellularLLTSymmetric[area, q + 1]],
    qTermsExpression[shifted, ElementaryESymbol, q]]
];

chromaticRecordQ[record_] :=
  sameExpressionQ[ChromaticSymmetric[Lookup[record, "area"]],
    termsExpression[Lookup[record, "monomial_terms"], MonomialSymbol]];

quasisymmetricRecordQ[record_] := Module[{alpha, expected},
  alpha = Lookup[record, "alpha"];
  expected = Lookup[record, "monomial_terms"];
  sameExpressionQ[
    FundamentalQSymmetric[alpha],
    termsExpression[expected, MonomialQSymbol]]
];

multiTermsExpression[terms_List, x_] := Total[
  Function[term,
    term[[2]] Times @@ MapIndexed[x[First[#2]]^#1 &, term[[1]]]] /@ terms
];

coefficientPolynomial[coefficients_List, x_] := Total[
  MapIndexed[#1 x^(First[#2] - 1) &, coefficients]
];

keyRecordQ[record_, data_, x_] := Module[{alpha, terms},
  alpha = Lookup[record, "alpha"];
  terms = Lookup[First@Select[Lookup[data, "key_atom"],
      Function[item, Lookup[item, "alpha"] === alpha]], "key_terms"];
  Expand[KeyPolynomial[Reverse[alpha], x] - multiTermsExpression[terms, x]] === 0
];

schubertRecordQ[record_, x_] :=
  Expand[SchubertPolynomial[Lookup[record, "permutation"], x] -
    multiTermsExpression[Lookup[record, "terms"], x]] === 0;

lahRecordQ[record_] := Module[{n, k},
  n = Lookup[record, "n"];
  k = Lookup[record, "k"];
  sameExpressionQ[ToElementaryEBasis[LahSymmetricFunction[n, k]],
    termsExpression[Lookup[record, "elementary_terms"], ElementaryESymbol]] &&
   sameExpressionQ[ToMonomialBasis[LahSymmetricFunction[n, k]],
    termsExpression[Lookup[record, "monomial_terms"], MonomialSymbol]]
];

petrieRecordQ[record_] := Module[{k, n},
  k = Lookup[record, "k"];
  n = Lookup[record, "n"];
  sameExpressionQ[PetrieSymmetric[k, n],
    termsExpression[Lookup[record, "monomial_terms"], MonomialSymbol]]
];

partitionRecordQ[record_] := Module[{partition, add, remove},
  partition = Lookup[record, "partition"];
  add = Sort[Lookup[record, "add_box"]];
  remove = Sort[Lookup[record, "remove_box"]];
  ConjugatePartition[partition] === Lookup[record, "conjugate"] &&
   HookLengths[partition] === Lookup[record, "hook_lengths"] &&
   Sort[PartitionAddBox[partition]] === add &&
   Sort[PartitionRemoveBox[partition]] === remove
];

compositionRecordQ[record_] := Module[{composition},
  composition = Lookup[record, "composition"];
  composition =!= {} &&
   CompositionToDescentSet[composition] === Lookup[record, "descent_set"] &&
   Sort[(Flatten[#, 1] &) /@ CompositionRefinements[composition]] ===
    Sort[Lookup[record, "refinements"]] &&
   CompositionWord[composition] === Lookup[record, "word"] &&
   Sort[composition] === Sort[Lookup[record, "partition"]]
];

setPartitionRecordQ[record_] := Module[{n, expected, actual},
  n = Lookup[record, "n"];
  expected = Sort[Sort /@ (Lookup[#, "blocks"] & /@ Lookup[record, "records"])];
  actual = Sort[Sort /@ SetPartitions[n]];
  expected === actual
];

permutationRecordQ[record_] := Module[{permutation, stats, sets},
  permutation = Lookup[record, "permutation"];
  stats = Lookup[record, "stats"];
  sets = Lookup[record, "sets"];
  And[
    PermutationType[permutation] === Lookup[record, "cycle_type"],
    Descents[permutation] === Lookup[stats, "descents"],
    MajorIndex[permutation] === Lookup[stats, "major_index"],
    Inversions[permutation] === Lookup[stats, "inversions"],
    Excedances[permutation] === Lookup[stats, "excedances"],
    PermutationPeaks[permutation] === Lookup[stats, "peaks"],
    PermutationValleys[permutation] === Lookup[stats, "valleys"],
    FixedPoints[permutation] === Lookup[stats, "fixed_points"],
    Length[PermutationAllCycles[permutation]] === Lookup[stats, "cycles"],
    DescentSet[permutation] === Lookup[sets, "descent_set"],
    PermutationPeaksSet[permutation] === Lookup[sets, "peak_set"],
    FoataMap[permutation] === Lookup[record, "foata"],
    PermutationCycleMap[permutation] === Lookup[record, "foata_cycle_word"]
  ]
];

avoidanceRecordQ[record_] := With[{pattern = Lookup[record, "pattern"]},
  Lookup[record, "counts"] ===
   Table[Length@Select[Permutations[Range[n]],
      IsPermutationAvoidingQ[pattern, #] &], {n, 0, 5}]
];

graphFromRecord[record_] := Graph[
  Range[Lookup[record, "vertices"]],
  UndirectedEdge @@@ (Lookup[record, "edges"] + 1)
];

posetFromRecord[record_] := Poset[
  Lookup[record, "vertices"],
  (# + 1) & /@ Lookup[record, "covers"]
];

normalizeBases[bases_] := Sort[Sort /@ bases];

matroidRecordQ[record_] := Module[
  {ground, bases, x, y, rankTerms, rankPolynomial, recordName},
  ground = Lookup[record, "ground"];
  bases = Lookup[record, "bases"];
  x = Unique["x"]; y = Unique["y"];
  rankTerms = Lookup[record, "tutte_terms"];
  rankPolynomial = Total[(#[[3]] (x - 1)^#[[1]] (y - 1)^#[[2]]) & /@ rankTerms];
  recordName = Lookup[record, "name"];
  And[
    IsMatroidQ[bases] === Lookup[record, "is_matroid"],
    MatroidLoops[ground, bases] === Lookup[record, "loops"],
    MatroidColoops[ground, bases] === Lookup[record, "coloops"],
    normalizeBases[MatroidDual[ground, bases]] === normalizeBases[Lookup[record, "dual_bases"]],
    normalizeBases[IndependentSets[bases]] === normalizeBases[Lookup[record, "independent_sets"]],
    And @@ ((MatroidSetRank[bases, First[#]] === Last[#]) & /@ Lookup[record, "rank_queries"]),
    normalizeBases[MatroidDeletion[bases, Lookup[record, "delete_label"]]] ===
      normalizeBases[Lookup[record, "delete_bases"]],
    normalizeBases[MatroidContraction[bases, Lookup[record, "contract_label"]]] ===
      normalizeBases[Lookup[record, "contract_bases"]],
    Expand[MatroidTuttePolynomial[{ground, bases}, {x, y}] - rankPolynomial] === 0,
    Switch[recordName,
      "uniform_U24", normalizeBases[bases] === normalizeBases[UniformBases[2, 4]],
      "transversal_12_23_34", normalizeBases[bases] ===
        normalizeBases[TransversalBases[{{1, 2}, {2, 3}, {3, 4}}]],
      "graphic_triangle_with_loop", normalizeBases[bases] === {{1, 2}, {1, 3}, {2, 3}},
      True, True]
  ]
];

latticePathRecordQ[record_] := Module[
  {lambda, mu, intervals, bases, pathSystem},
  lambda = Lookup[record, "lambda"];
  mu = Lookup[record, "mu"];
  intervals = Lookup[record, "intervals"];
  bases = Lookup[record, "bases"];
  pathSystem = ({First[#], Last[#]} &) /@ PathSetSystem[lambda, mu];
  Sort[pathSystem] === Sort[intervals] &&
   normalizeBases[PathBases[lambda, mu]] === normalizeBases[bases] &&
   normalizeBases[TransversalBases[(Range[First[#], Last[#]] &) /@ intervals]] ===
    normalizeBases[bases] &&
   Length[bases] === ToExpression[Lookup[record, "num_bases"]]
];

plethysmRecordQ[record_] := sameExpressionQ[
  ToSchurBasis[Plethysm[
    SchurSymbol[Lookup[record, "outer"]],
    SchurSymbol[Lookup[record, "inner"]]
  ]],
  termsExpression[Lookup[record, "schur_terms"], SchurSymbol]
];

VerificationTest[
  With[{records = Lookup[fixture["kostka.json"], "records"]},
    And @@ ((KostkaCoefficient[Lookup[#, "lambda"], Lookup[#, "mu"]] ===
        Lookup[#, "value"]) & /@ records)],
  True,
  TestID -> "CrossCheck-Kostka"
]

VerificationTest[
  With[{records = Lookup[fixture["lr.json"], "records"]},
    And @@ ((LRCoefficient[Lookup[#, "lambda"], Lookup[#, "mu"], Lookup[#, "nu"]] ===
        Lookup[#, "value"]) & /@ records)],
  True,
  TestID -> "CrossCheck-LittlewoodRichardson"
]

VerificationTest[
  With[{records = Lookup[fixture["characters.json"], "records"]},
    And @@ ((SnCharacter[Lookup[#, "lambda"], Lookup[#, "mu"]] ===
        Lookup[#, "value"]) & /@ records)],
  True,
  TestID -> "CrossCheck-SymmetricGroupCharacters"
]

VerificationTest[
  With[{data = fixture["transitions.json"]},
    And @@ Flatten[
      Table[
        With[{degree = Lookup[record, "degree"], partitions = Lookup[record, "partitions"],
            matrix = Lookup[Lookup[record, "matrices"], from <> "->" <> to]},
          SymFuncTransMat[basisFunction[from], basisFunction[to], degree] ===
            (matrix /. value_?StringQ :> ToExpression[value])],
        {record, Lookup[data, "records"]},
        {from, Lookup[data, "basis_order"]},
        {to, Lookup[data, "basis_order"]}]]],
  True,
  TestID -> "CrossCheck-ClassicalTransitions"
]

VerificationTest[
  With[{records = Lookup[fixture["macdonald-operators.json"], "records"]},
    And @@ (macdonaldRecordQ /@ records)],
  True,
  TestID -> "CrossCheck-MacdonaldOperators"
]

VerificationTest[
  With[{records = Lookup[fixture["llt.json"], "records"]},
    And @@ (lltRecordQ /@ records)],
  True,
  TestID -> "CrossCheck-UnicellularLLT"
]

VerificationTest[
  With[{records = Lookup[fixture["chromatic.json"], "records"]},
    And @@ (chromaticRecordQ /@ records)],
  True,
  TestID -> "CrossCheck-ChromaticSymmetric"
]

VerificationTest[
  With[{data = fixture["quasisymmetric.json"], records =
      Lookup[fixture["quasisymmetric.json"], "records"]},
     And @@ (quasisymmetricRecordQ /@ records) &&
     sameExpressionQ[
       QuasiSymmetricFunctions`Private`QMonomialProduct[{1, 2}, {1}] +
        QuasiSymmetricFunctions`Private`QMonomialProduct[{1, 1, 1}, {1}],
       termsExpression[Lookup[Lookup[data, "product"], "monomial_terms"], MonomialQSymbol]]],
  True,
  TestID -> "CrossCheck-Quasisymmetric"
]

VerificationTest[
  With[{data = fixture["nonsymmetric.json"], x = Unique["x"]},
    And @@ (keyRecordQ[#, data, x] & /@ Lookup[data, "key_atom"])],
  True,
  TestID -> "CrossCheck-KeyPolynomial"
]

VerificationTest[
  With[{data = fixture["nonsymmetric.json"], x = Unique["x"]},
    And @@ (schubertRecordQ[#, x] & /@ Lookup[data, "schubert"])],
  True,
  TestID -> "CrossCheck-SchubertPolynomial"
]

VerificationTest[
  With[{data = fixture["eulerian.json"], t = Unique["t"]},
    Lookup[data, "eulerian"] ===
      Table[CoefficientList[EulerianAPolynomial[n, t], t], {n, 1, 6}] &&
     And @@ ((RealRootedQ[coefficientPolynomial[Lookup[#, "coefficients"], t], t] ===
          Lookup[#, "real_rooted"]) & /@ Lookup[data, "root_cases"]) &&
     And @@ ((InterleavingRootsQ[
          coefficientPolynomial[Lookup[#, "left"], t],
          coefficientPolynomial[Lookup[#, "right"], t], t] === Lookup[#, "value"]) & /@
          Lookup[data, "interlacing_cases"])],
  True,
  TestID -> "CrossCheck-EulerianAndRoots"
]

VerificationTest[
  With[{data = fixture["lah-petrie.json"]},
    And @@ (lahRecordQ /@ Lookup[data, "lah"]) &&
     And @@ (petrieRecordQ /@ Lookup[data, "petrie"])],
  True,
  TestID -> "CrossCheck-LahAndPetrie"
]

VerificationTest[
  With[{data = fixture["combinatorics.json"]},
    And @@ (partitionRecordQ /@ Lookup[data, "partitions"]) &&
     And @@ (Function[record,
          PartitionLessEqualQ[Lookup[record, "left"], Lookup[record, "right"]] ===
            Lookup[record, "contained"] &&
           PartitionDominatesQ[Lookup[record, "left"], Lookup[record, "right"]] ===
            Lookup[record, "right_dominates_left"]] /@
        Lookup[data, "partition_relations"]) &&
     And @@ (compositionRecordQ /@ Lookup[data, "compositions"]) &&
     Lookup[data, "bell_counts"] === Table[Length[SetPartitions[n]], {n, 0, 4}] &&
     Lookup[data, "ordered_set_partition_counts"] ===
      Table[Length[OrderedSetPartitions[n]], {n, 0, 4}] &&
     And @@ (Function[record,
          With[{n = Lookup[record, "n"], blocks = Lookup[record, "records"]},
           setPartitionRecordQ[record] &&
            Sort[Lookup[data, "stirling_counts"][[n + 1]]] ===
             Sort[Table[{k, Count[Length /@ SetPartitions[n], k]}, {k, 0, n}]]]] /@
        Lookup[data, "set_partitions"])],
  True,
  TestID -> "CrossCheck-Combinatorics"
]

VerificationTest[
  With[{data = fixture["permutations.json"]},
    And @@ (permutationRecordQ /@ Lookup[data, "records"]) &&
     And @@ (avoidanceRecordQ /@ Lookup[data, "avoidance_counts"])],
  True,
  TestID -> "CrossCheck-Permutations"
]

VerificationTest[
  With[{records = Lookup[fixture["graphs.json"], "records"]},
    And @@ (Function[record,
          With[{graph = graphFromRecord[record], t = Unique["t"]},
           CoefficientList[GraphIndependencePolynomial[graph, t], t] ===
              Lookup[record, "independence"] &&
            CoefficientList[GraphMatchingPolynomial[graph, t], t] ===
              Lookup[record, "matching"] &&
            CoefficientList[ChromaticPolynomial[graph, t], t] ===
              Lookup[record, "chromatic"]]] /@ records)],
  True,
  TestID -> "CrossCheck-GraphPolynomials"
]

VerificationTest[
  With[{records = Lookup[fixture["posets.json"], "records"]},
    And @@ (Function[record,
          With[{poset = posetFromRecord[record], t = Unique["t"], n = Lookup[record, "vertices"]},
           Length[JordanHolderSet[poset]] === Lookup[record, "linear_extensions"] &&
            Table[OrderPolynomial[poset, t] /. t -> k, {k, 0, n}] ===
             Lookup[record, "order_values"] &&
            Rest[CoefficientList[PEulerianPolynomial[poset, t], t]] ===
             Lookup[record, "p_eulerian"]]] /@ records)],
  True,
  TestID -> "CrossCheck-Posets"
]

VerificationTest[
  With[{records = Lookup[fixture["matroids.json"], "records"]},
    And @@ (matroidRecordQ /@ records)],
  True,
  TestID -> "CrossCheck-Matroids"
]

VerificationTest[
  With[{records = Lookup[fixture["lattice-path-matroids.json"], "records"]},
    And @@ (latticePathRecordQ /@ records)],
  True,
  TestID -> "CrossCheck-LatticePathMatroids"
]

VerificationTest[
  With[{records = Lookup[fixture["plethysm.json"], "records"]},
    And @@ (plethysmRecordQ /@ records)],
  True,
  TestID -> "CrossCheck-Plethysm"
]
