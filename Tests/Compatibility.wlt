(* Contract tests for CONVENTIONS.md (issue #51): objects produced by one package are
   accepted by the others, and the same quantity computed through different packages.
   TODO P3/P5: key and atom polynomials versus augmented fillings in NewTableaux. *)

VerificationTest[
  Scan[Needs, {"CombinatoricTools`", "NewTableaux`", "SymmetricFunctions`",
    "QuasiSymmetricFunctions`", "GTPatterns`",
    "PermutationTools`", "CatalanObjects`", "UnicellularChromatics`", "GraphTools`"}],
  Null,
  TestID -> "Compatibility-packages-load-together"
]

(* Kostka numbers agree across CombinatoricTools, NewTableaux, GTPatterns and
   SymmetricFunctions. *)
VerificationTest[
  And @@ Flatten@Table[
    With[{k = KostkaCoefficient[lam, mu]},
      k === Length[SemiStandardYoungTableaux[{lam, {}}, mu]] &&
      k === Length[GTPatterns[lam, {}, mu]] &&
      k === Coefficient[ToMonomialBasis[SchurSymbol[lam]], MonomialSymbol[mu]]],
    {lam, IntegerPartitions[5]}, {mu, IntegerPartitions[5]}],
  True,
  TestID -> "Compatibility-Kostka-numbers-agree"
]

(* Tableaux from NewTableaux pass through GTPatterns and back, and are accepted by the
   NewTableaux crystal operators. *)
VerificationTest[
  With[{tabs = SemiStandardYoungTableaux[{{3, 2}, {1}}, {2, 1, 1}]},
    {And @@ (YoungTableau[GTPattern[#]] === # & /@ tabs),
     And @@ (With[{f = CrystalFi[#, 1]}, f === Undefined || CrystalEi[f, 1] === #] & /@ tabs)}],
  {True, True},
  TestID -> "Compatibility-tableaux-GTPatterns-crystals"
]

(* RSK on permutations (from Permutations) produces standard tableaux whose shapes are
   distributed as (f^lam)^2, with f^lam counted by NewTableaux and CombinatoricTools. *)
VerificationTest[
  With[{shapes = Counts[YoungTableauShape[First[BiwordRSK[#]]] & /@ Permutations[Range[5]]]},
    And @@ (Lookup[shapes, Key[#], 0] === Length[StandardYoungTableaux[#]]^2 &&
        Length[StandardYoungTableaux[#]] === SnCharacter[#, ConstantArray[1, 5]] & /@
      IntegerPartitions[5])],
  True,
  TestID -> "Compatibility-RSK-shapes-and-SYT-counts"
]

(* Area lists from CatalanObjects are accepted by UnicellularChromatics, and the area-list
   form agrees with the Graph form built from UnitIntervalEdges. *)
VerificationTest[
  And @@ Flatten@Table[
    Expand[ChromaticSymmetric[area] -
      ChromaticSymmetric[Graph[Range[Length[area]], UndirectedEdge @@@ UnitIntervalEdges[area]]]] === 0,
    {n, 1, 4}, {area, DyckAreaLists[n]}],
  True,
  TestID -> "Compatibility-area-lists-and-graphs"
]

(* Symmetric functions returned by UnicellularChromatics are SymmetricFunctions symbols:
   the unicellular LLT at q = 1 is e_1^n = h_1^n. *)
VerificationTest[
  And @@ Table[
    Expand[ToSchurBasis[UnicellularLLTSymmetric[area, 1]] -
      ToSchurBasis[CompleteHSymbol[ConstantArray[1, Length[area]]]]] === 0,
    {area, DyckAreaLists[4]}],
  True,
  TestID -> "Compatibility-LLT-symbols-are-SymmetricFunctions-symbols"
]

(* GitHub issue #51: every symmetric core basis round-trips through n variables. *)
VerificationTest[
  And @@ Flatten@Table[
    With[{f = basis[lam], p = SymmetricFunctionToPolynomial[basis[lam], x, n]},
      Expand[PolynomialToSymmetricFunction[p, x, basis, n] - f] === 0],
    {basis, {MonomialSymbol, SchurSymbol, ElementaryESymbol, CompleteHSymbol,
      PowerSumSymbol, ForgottenSymbol}}, {d, 0, 4}, {n, Max[1, d], 5},
    {lam, Select[IntegerPartitions[d], Length[#] <= n &]}],
  True,
  TestID -> "Compatibility-symmetric-polynomial-round-trips"
]

(* GitHub issue #51: every QSym core basis round-trips through n variables. *)
VerificationTest[
  And @@ Flatten@Table[
    With[{f = basis[alpha], p = QuasiSymmetricFunctionToPolynomial[basis[alpha], x, n]},
      Expand[PolynomialToQuasiSymmetricFunction[p, x, basis, n] - f] === 0],
    {basis, {MonomialQSymbol, FundamentalQSymbol}}, {d, 0, 4},
    {n, Max[1, d], 5}, {alpha, Select[IntegerCompositions[d], Length[#] <= n &]}],
  True,
  TestID -> "Compatibility-quasisymmetric-polynomial-round-trips"
]

(* GitHub issue #51: s_lam is the sum of F_D(T) over standard Young tableaux. *)
VerificationTest[
  And @@ Table[
    Expand[ToQuasiSymmetric[SchurSymbol[lam]] -
      Total[FundamentalQSymmetric[
          DescentSetToComposition[SYTDescentSet[#], Total[lam]]] & /@
        StandardYoungTableaux[lam]]] === 0,
    {lam, IntegerPartitions[4]}],
  True,
  TestID -> "Compatibility-Schur-SYT-fundamental-identity"
]

(* GitHub issue #51: the legacy polynomial APIs agree with their new bridges. *)
VerificationTest[
  Quiet[Scan[Needs, {"MacdonaldPolynomials`", "OldYoungTableaux`"}]];
  And[
    Expand[MacdonaldPolynomials`QSymMonomial[{2, 1}, 3, x] -
      QuasiSymmetricFunctionToPolynomial[MonomialQSymbol[{2, 1}], x, 3]] === 0,
    Expand[MacdonaldPolynomials`QSymSchur[{2, 1}, 3, x] -
      QuasiSymmetricFunctionToPolynomial[QuasiSchurQSymmetric[{2, 1}], x, 3]] === 0,
    Expand[MacdonaldPolynomials`QuasiSymmetricPowerSum[{2, 1}, 3, x] -
      QuasiSymmetricFunctionToPolynomial[PowerSumQSymbol[{2, 1}], x, 3]] === 0,
    Expand[MacdonaldPolynomials`QuasiSymmetricPowerSum2[{2, 1}, 3, x] -
      QuasiSymmetricFunctionToPolynomial[PowerSumAltQSymmetric[{2, 1}], x, 3]] === 0,
    Expand[MacdonaldPolynomials`GesselFundamental[{1}, 3, x] -
      QuasiSymmetricFunctionToPolynomial[FundamentalQSymbol[{1, 2}], x, 3]] === 0,
    Expand[OldYoungTableaux`SchurPolynomial[{2, 1}, {}, 3][x] -
      SymmetricFunctionToPolynomial[SchurSymbol[{2, 1}], x, 3]] === 0,
    Expand[OldYoungTableaux`SchurPolynomial[{2, 1}, {1}, 3][x] -
      SymmetricFunctionToPolynomial[SkewSchurSymmetric[{{2, 1}, {1}}], x, 3]] === 0,
    Expand[OldYoungTableaux`MonomialSymmetricPolynomial[{2, 1}, 3][x] -
      SymmetricFunctionToPolynomial[MonomialSymbol[{2, 1}], x, 3]] === 0,
    Expand[OldYoungTableaux`PowerSumPolynomial[{2, 1}, 3][x] -
      SymmetricFunctionToPolynomial[PowerSumSymbol[{2, 1}], x, 3]] === 0,
    Expand[OldYoungTableaux`HallLittlewoodP[{2, 1}, 3, x, t] -
      SymmetricFunctionToPolynomial[HallLittlewoodPSymmetric[{2, 1}, t], x, 3]] === 0,
    Together[OldYoungTableaux`JackPPolynomial[{2, 1}, 3, x, a] -
      SymmetricFunctionToPolynomial[JackPSymmetric[{2, 1}, a], x, 3]] === 0,
    Together[OldYoungTableaux`JackJPolynomial[{2, 1}, 3, x, a] -
      SymmetricFunctionToPolynomial[JackJSymmetric[{2, 1}, a], x, 3]] === 0,
    (MacdonaldPolynomials`ToGesselSubsetBasis[x[1]^2 + x[1] x[2] + x[2]^2, x, ff] /.
        ff[{}] :> QuasiSymmetricFunctionToPolynomial[FundamentalQSymbol[{2}], x, 2]) ===
      QuasiSymmetricFunctionToPolynomial[
        PolynomialToQuasiSymmetricFunction[x[1]^2 + x[1] x[2] + x[2]^2, x,
          FundamentalQSymbol, 2], x, 2],
    (MacdonaldPolynomials`ToElementaryBasis[x[1]^2 + x[2]^2 + x[3]^2, x, ee] /.
        ee[lam_List] :> ElementaryESymbol[DeleteCases[lam, 0]]) ===
      PolynomialToSymmetricFunction[x[1]^2 + x[2]^2 + x[3]^2, x, ElementaryESymbol, 3],
    (MacdonaldPolynomials`ToCompleteHomogeneousBasis[x[1]^2 + x[2]^2 + x[3]^2, x, hh] /.
        hh[lam_List] :> CompleteHSymbol[DeleteCases[lam, 0]]) ===
      PolynomialToSymmetricFunction[x[1]^2 + x[2]^2 + x[3]^2, x, CompleteHSymbol, 3],
    (MacdonaldPolynomials`ToPowerSumBasisMacdonald[x[1]^2 + x[2]^2 + x[3]^2, x, pp] /. 
        pp[lam_List] :> PowerSumSymbol[DeleteCases[lam, 0]]) ===
      PolynomialToSymmetricFunction[x[1]^2 + x[2]^2 + x[3]^2, x, PowerSumSymbol, 3],
    And @@ Flatten@Table[
      Expand[MacdonaldPolynomials`QSymMonomial[alpha, n, x] -
        QuasiSymmetricFunctionToPolynomial[MonomialQSymbol[alpha], x, n]] === 0,
      {d, 1, 4}, {alpha, Select[IntegerCompositions[d], Length[#] <= 4 &]},
      {n, Max[1, Length[alpha]], 4}],
    And @@ Flatten@Table[
      Expand[MacdonaldPolynomials`QSymSchur[alpha, n, x] -
        QuasiSymmetricFunctionToPolynomial[QuasiSchurQSymmetric[alpha], x, n]] === 0,
      {d, 1, 4}, {alpha, Select[IntegerCompositions[d], Length[#] <= 4 &]},
      {n, Max[1, Length[alpha]], 4}],
    And @@ Flatten@Table[
      Expand[MacdonaldPolynomials`QuasiSymmetricPowerSum[alpha, n, x] -
        QuasiSymmetricFunctionToPolynomial[PowerSumQSymbol[alpha], x, n]] === 0,
      {d, 1, 4}, {alpha, Select[IntegerCompositions[d], Length[#] <= 4 &]},
      {n, Max[1, Length[alpha]], 4}],
    And @@ Flatten@Table[
      Expand[MacdonaldPolynomials`QuasiSymmetricPowerSum2[alpha, n, x] -
        QuasiSymmetricFunctionToPolynomial[PowerSumAltQSymmetric[alpha], x, n]] === 0,
      {d, 1, 4}, {alpha, Select[IntegerCompositions[d], Length[#] <= 4 &]},
      {n, Max[1, Length[alpha]], 4}],
    And @@ Flatten@Table[
      Expand[MacdonaldPolynomials`GesselFundamental[des, d, n, x] -
        QuasiSymmetricFunctionToPolynomial[
          FundamentalQSymbol[DescentSetToComposition[des, d]], x, n]] === 0,
      {d, 1, 4}, {des, Subsets[Range[d - 1]]}, {n, 1, 4}],
    And @@ Flatten@Table[
      Expand[OldYoungTableaux`SchurPolynomial[lam, {}, n][x] -
        SymmetricFunctionToPolynomial[SchurSymbol[lam], x, n]] === 0,
      {d, 1, 4}, {lam, IntegerPartitions[d]}, {n, Max[1, Length[lam]], 4}],
    And @@ Flatten@Table[
      Expand[OldYoungTableaux`MonomialSymmetricPolynomial[lam, n][x] -
        SymmetricFunctionToPolynomial[MonomialSymbol[lam], x, n]] === 0,
      {d, 1, 4}, {lam, IntegerPartitions[d]}, {n, Max[1, Length[lam]], 4}],
    And @@ Flatten@Table[
      Expand[OldYoungTableaux`PowerSumPolynomial[lam, n][x] -
        SymmetricFunctionToPolynomial[PowerSumSymbol[lam], x, n]] === 0,
      {d, 1, 4}, {lam, IntegerPartitions[d]}, {n, Max[1, Length[lam]], 4}],
    And @@ Flatten@Table[
      Expand[OldYoungTableaux`HallLittlewoodP[lam, n, x, t] -
        SymmetricFunctionToPolynomial[HallLittlewoodPSymmetric[lam, t], x, n]] === 0,
      {d, 1, 4}, {lam, IntegerPartitions[d]}, {n, Max[1, Length[lam]], 4}],
    And @@ Flatten@Table[
      Expand[Together[OldYoungTableaux`JackPPolynomial[lam, n, x, a] -
        SymmetricFunctionToPolynomial[JackPSymmetric[lam, a], x, n]]] === 0,
      {d, 1, 4}, {lam, IntegerPartitions[d]}, {n, Max[1, Length[lam]], 4}],
    And @@ Flatten@Table[
      Expand[Together[OldYoungTableaux`JackJPolynomial[lam, n, x, a] -
        SymmetricFunctionToPolynomial[JackJSymmetric[lam, a], x, n]]] === 0,
      {d, 1, 4}, {lam, IntegerPartitions[d]}, {n, Max[1, Length[lam]], 4}]],
  True,
  TestID -> "Compatibility-legacy-polynomial-oracles"
]

(* GitHub issue #51: old symmetric-basis reducers agree after returning to polynomials. *)
VerificationTest[
  Quiet[Needs["MacdonaldPolynomials`"]];
  And[
    Expand[SymmetricFunctionToPolynomial[
      PolynomialToSymmetricFunction[x[1]^2 + x[2]^2 + x[3]^2, x, ElementaryESymbol, 3], x, 3] -
      (x[1]^2 + x[2]^2 + x[3]^2)] === 0,
    Expand[SymmetricFunctionToPolynomial[
      PolynomialToSymmetricFunction[x[1]^2 + x[2]^2 + x[3]^2, x, CompleteHSymbol, 3], x, 3] -
      (x[1]^2 + x[2]^2 + x[3]^2)] === 0,
    Expand[SymmetricFunctionToPolynomial[
      PolynomialToSymmetricFunction[x[1]^2 + x[2]^2 + x[3]^2, x, PowerSumSymbol, 3], x, 3] -
      (x[1]^2 + x[2]^2 + x[3]^2)] === 0],
  True,
  TestID -> "Compatibility-legacy-symmetric-basis-bridges"
]
