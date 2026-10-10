(* ::Package:: *)

BeginPackage["ShiftedSymmetricFunctions`", {
  "CombinatoricTools`", "NewTableaux`", "GTPatterns`",
  "PermutationTools`", "SymmetricFunctions`"
}];

Unprotect["`*"];
ClearAll["`*"];

ShiftedSchurPolynomial;
ShiftedSchurEvaluate;
ShiftedJackPPolynomial;
ShiftedJackJPolynomial;
ShiftedJackPEvaluate;
ShiftedJackJEvaluate;
ShiftedJackPSymmetric;
NormalizedCharacter;
StanleyCharacterPolynomial;

Begin["`Private`"];

shiftedCache = <||>;

SetAttributes[cached, HoldRest];
cached[key_, expr_] := If[KeyExistsQ[shiftedCache, key], shiftedCache[key], shiftedCache[key] = expr];

normalizeShiftedPartition[mu_List] := DeleteCases[mu, 0];

fallingFactorial[z_, r_Integer] := Product[z - i, {i, 0, r - 1}];

shiftedSchurDet[mu_List, n_Integer, values_List] := Module[{mup},
  If[n == 0, Return[If[mu === {}, 1, 0]]];
  If[Length[mu] > n, Return[0]];
  mup = PadRight[mu, n];
  Cancel[
    Det[Table[
      fallingFactorial[values[[i]] + n - i, mup[[j]] + n - j],
      {i, n}, {j, n}]] /
      Det[Table[fallingFactorial[values[[i]] + n - i, n - j],
      {i, n}, {j, n}]]
  ]
];

ShiftedSchurPolynomial::usage =
  "ShiftedSchurPolynomial[mu,n,x] returns the Okounkov--Olshanski shifted Schur polynomial s*_mu in x[1],...,x[n].";
ShiftedSchurPolynomial[mu_List, n_Integer, x_] := cached[
  {ShiftedSchurPolynomial, mu, n, x},
  shiftedSchurDet[normalizeShiftedPartition[mu], n, x /@ Range[n]]
];

ShiftedSchurEvaluate::usage =
  "ShiftedSchurEvaluate[mu,lam] evaluates s*_mu at the partition lam using the determinant formula.";
ShiftedSchurEvaluate[mu_List, lam_List] := cached[
  {ShiftedSchurEvaluate, mu, lam},
  Module[{m = normalizeShiftedPartition[mu], l = normalizeShiftedPartition[lam]},
    If[!PartitionLessEqualQ[m, l], 0,
      shiftedSchurDet[m, Length[l], l]]
  ]
];

gtPatternsForShifted[{}, 0] := {GTPattern[{{}}]};
gtPatternsForShifted[mu_List, n_Integer] := If[n <= 0, {},
  Flatten[GTPatterns[mu, {}, #] & /@ WeakIntegerCompositions[Tr[mu], n]]
];

jackPsiForShifted[ GTPattern[rows_], a_] := Product[
  JackPsi[{rows[[i + 1]], rows[[i]]}, a], {i, Length[rows] - 1}
];

shiftedJackPPolynomialInternal[mu_List, n_Integer, x_, a_] := Module[{patterns, tableau},
  If[n == 0, Return[If[mu === {}, 1, 0]]];
  If[Length[mu] > n, Return[0]];
  patterns = gtPatternsForShifted[mu, n];
  Total[
    (tableau = First[YoungTableau[#] /. (m_Integer :> n - m + 1)];
      jackPsiForShifted[#, a] Product[
        x[tableau[[r, c]]] - (c - 1) + (r - 1)/a,
        {r, Length[tableau]}, {c, Length[tableau[[r]]]}
      ]) & /@ patterns
  ]
];

ShiftedJackPPolynomial::usage =
  "ShiftedJackPPolynomial[mu,n,x,a] returns the shifted Jack P polynomial P*_mu in x[1],...,x[n] with Jack parameter a.";
ShiftedJackPPolynomial[mu_List, n_Integer, x_, a_] := cached[
  {ShiftedJackPPolynomial, mu, n, x, a},
  shiftedJackPPolynomialInternal[normalizeShiftedPartition[mu], n, x, a]
];

ShiftedJackJPolynomial::usage =
  "ShiftedJackJPolynomial[mu,n,x,a] returns the hook-normalized shifted Jack J polynomial in x[1],...,x[n].";
ShiftedJackJPolynomial[mu_List, n_Integer, x_, a_] := cached[
  {ShiftedJackJPolynomial, mu, n, x, a},
  JackLowerHook[normalizeShiftedPartition[mu], a] ShiftedJackPPolynomial[mu, n, x, a]
];

ShiftedJackPEvaluate::usage =
  "ShiftedJackPEvaluate[mu,lam,a] evaluates P*_mu at the partition lam with Jack parameter a.";
ShiftedJackPEvaluate[mu_List, lam_List, a_] := cached[
  {ShiftedJackPEvaluate, mu, lam, a},
  Module[{n = Length[lam], x},
    ShiftedJackPPolynomial[mu, n, x, a] /. Thread[x /@ Range[n] -> lam]
  ]
];

ShiftedJackJEvaluate::usage =
  "ShiftedJackJEvaluate[mu,lam,a] evaluates J*_mu at the partition lam with Jack parameter a.";
ShiftedJackJEvaluate[mu_List, lam_List, a_] := cached[
  {ShiftedJackJEvaluate, mu, lam, a},
  JackLowerHook[normalizeShiftedPartition[mu], a] ShiftedJackPEvaluate[mu, lam, a]
];

shiftedJackSymmetricInternal[mu_List, a_] := Module[{n = Tr[mu], tableaux, asPoly, rules},
  If[n == 0, Return[1]];
  asPoly = Sum[
    Product[
      z[n + 1 - Extract[ssyt[[1]], s]] - (s[[2]] - 1) + (s[[1]] - 1)/a,
      {s, DiagramBoxes[mu]}] *
    With[{ribbons = Table[YoungTableauShape[ssyt, i], {i, Max[ssyt]}]},
      Product[JackPsi[rib, a], {rib, Partition[Reverse[ribbons], 2, 1]}]],
    {w, WeakIntegerCompositions[n, n]},
    {ssyt, SemiStandardYoungTableaux[{mu, {}}, w]}
  ];
  asPoly = asPoly /. z[i_] :> (z[i] + i)/a;
  rules = CoefficientRules[asPoly, z /@ Range[n]];
  Total[(With[{nu = First[#], coeff = Last[#]},
      Boole[OrderedQ[nu]] coeff MonomialSymmetric[nu]] &) /@ rules]
];

ShiftedJackPSymmetric::usage =
  "ShiftedJackPSymmetric[mu,a,x] returns the legacy shifted Jack P symmetric function obtained from the Okounkov--Olshanski tableau formula. The alphabet x defaults to None.";
ShiftedJackPSymmetric[mu_List, a_, x_: None] := ChangeFunctionAlphabet[
  cached[{ShiftedJackPSymmetric, mu, a}, shiftedJackSymmetricInternal[mu, a]], x
];

permutationOfType[type_List] := PermutationList[Cycles[
  (Range[#1 + 1, #2] & @@@ Partition[Prepend[Accumulate[type], 0], 2, 1])
], Total[type]];

ferayN[sigmaPerm_List, tauPerm_List, d_Integer, p_, q_] := cached[
  {ferayN, sigmaPerm, tauPerm, d, p, q},
  Module[{sigma, tau, edges, sv, tv, inequalities, sVertices, tVertices,
    solutions, generalProduct},
    If[d <= 0, Return[If[sigmaPerm === {} && tauPerm === {}, 1, 0]]];
    sigma = PermutationAllCycles[sigmaPerm];
    tau = PermutationAllCycles[tauPerm];
    edges = Flatten[Table[
      If[Intersection[sigma[[s]], tau[[t]]] =!= {}, {{s, t}}, {}],
      {s, Length[sigma]}, {t, Length[tau]}], 2];
    sVertices = sv /@ Range[Length[sigma]];
    tVertices = tv /@ Range[Length[tau]];
    inequalities = And[
      And @@ (1 <= # <= d & /@ sVertices),
      And @@ (1 <= # <= d & /@ tVertices),
      And @@ (Function[edge, sv[edge[[1]]] <= tv[edge[[2]]]] /@ edges)
    ];
    generalProduct = Product[p[v], {v, sVertices}] Product[q[v], {v, tVertices}];
    solutions = List@ToRules[Reduce[inequalities, Join[sVertices, tVertices], Integers]];
    Total[generalProduct /. solutions]
  ]
];

NormalizedCharacter::usage =
  "NormalizedCharacter[mu,lam] returns n(n-1)...(n-|mu|+1) chi^lam(mu 1^(n-|mu|))/dim(lam), for |mu|<=n=|lam|, and 0 otherwise.";
NormalizedCharacter[mu_List, lam_List] := cached[
  {NormalizedCharacter, mu, lam},
  Module[{m = normalizeShiftedPartition[mu], l = normalizeShiftedPartition[lam], n, k, cycleType},
    n = Tr[l];
    k = Tr[m];
    If[Tr[l] =!= n || k > n, Return[0]];
    cycleType = Sort[Join[m, ConstantArray[1, n - k]], Greater];
    Factorial[n]/Factorial[n - k] SnCharacter[l, cycleType] /
      SnCharacter[l, ConstantArray[1, n]]
  ]
];

StanleyCharacterPolynomial::usage =
  "StanleyCharacterPolynomial[mu,p,q,d] returns the Stanley--Feray--Sniady normalized character polynomial in multirectangular coordinates p[1],...,p[d] and q[1],...,q[d].";
StanleyCharacterPolynomial[mu_List, p_, q_, d_Integer] := cached[
  {StanleyCharacterPolynomial, mu, p, q, d},
  Module[{m = normalizeShiftedPartition[mu], k, permutations, pi},
    k = Tr[m];
    If[k == 0, Return[1]];
    permutations = Permutations[Range[k]];
    pi = permutationOfType[m];
    (-1)^(k - Length[m]) Total[
      (Signature[#2] ferayN[#1, #2, d, p, q]) & @@@
        ({#, PermutationProduct[InversePermutation[#], pi]} & /@ permutations)]
  ]
];

Protect @@ Select[
  Names["ShiftedSymmetricFunctions`*"],
  !StringMatchQ[#, ___ ~~ "$" ~~ ___] &
];

End[];
EndPackage[];
