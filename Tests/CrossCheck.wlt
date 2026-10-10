testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

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

(* MacdonaldPolynomials still exports names owned by other packages (issue #9). *)
VerificationTest[
  Quiet[Needs["MacdonaldPolynomials`"], General::shdw],
  Null,
  TestID -> "MacdonaldPolynomials-loads-cleanly"
]

VerificationTest[
  Needs["PolynomialTools`"],
  Null,
  TestID -> "PolynomialTools-loads-cleanly"
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
  (* The Rust library labels the diagram statistics with q=a' and t=l',
     whereas this Mathematica package uses the opposite q/t naming here. *)
  b = b /. {q -> t, t -> q};
  nabla = nabla /. {q -> t, t -> q};
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
