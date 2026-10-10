VerificationTest[
  Needs["QuasiSymmetricFunctions`"],
  Null,
  TestID -> "QuasiSymmetricFunctions-loads-cleanly"
]

(* GitHub issue #16: alternate power sums symmetrize to the ordinary power sum. *)
VerificationTest[
  And @@ (Function[lambda,
      With[{alphas = DeleteDuplicates[Permutations[lambda]]},
        Expand[Total[PowerSumAltQSymmetric /@ alphas]] ===
         Expand[Total[PowerSumQSymmetric /@ alphas]]]] /@
    {{1, 1, 1}, {2, 1, 1}}),
  True,
  TestID -> "QuasiSymmetricFunctions-PowerSumAltQSymmetric-symmetrization"
]

(* GitHub issue #16: monomial quasisymmetric products expand in the M basis. *)
VerificationTest[
  Expand[MonomialQSymbol[1] MonomialQSymbol[1]] ===
   MonomialQSymbol[2] + 2 MonomialQSymbol[{1, 1}],
  True,
  TestID -> "QuasiSymmetricFunctions-MonomialQSymbol-product"
]

(* GitHub issue #16: the multiplication-based power implementation is active. *)
VerificationTest[
  Expand[MonomialQSymbol[1]^2] ===
   MonomialQSymbol[2] + 2 MonomialQSymbol[{1, 1}],
  True,
  TestID -> "QuasiSymmetricFunctions-MonomialQSymbol-power"
]

(* GitHub issue #16: fundamental products satisfy the shuffle expansion. *)
VerificationTest[
  Expand[FundamentalQSymmetric[{1, 2}] FundamentalQSymmetric[{1}]] ===
   FundamentalQSymmetric[{1, 3}] + FundamentalQSymmetric[{1, 2, 1}] +
    FundamentalQSymmetric[{2, 2}] + FundamentalQSymmetric[{1, 1, 2}],
  True,
  TestID -> "QuasiSymmetricFunctions-FundamentalQSymmetric-shuffle-product"
]

(* GitHub issue #16: M-basis multiplication agrees with finite-variable polynomials. *)
VerificationTest[
  Module[{vars = Array[z, 4], expandM, polynomial},
    expandM[expr_] := Expand[expr /. HoldPattern[MonomialQSymbol[a_List, None]] :>
       Total[(Times @@ MapThread[Power, {vars[[#]], a}]) & /@
         Subsets[Range[Length[vars]], {Length[a]}]]];
    polynomial = expandM[MonomialQSymbol[{1, 2}] MonomialQSymbol[{1}]];
    polynomial === Expand[expandM[MonomialQSymbol[{1, 2}]] expandM[MonomialQSymbol[{1}]]]
  ],
  True,
  TestID -> "QuasiSymmetricFunctions-MonomialQSymbol-finite-variable-check"
]

(* GitHub issue #16: the exported monomial wrapper delegates to M symbols. *)
VerificationTest[
  MonomialQSymmetric[{2}, x] === MonomialQSymbol[{2}, x],
  True,
  TestID -> "QuasiSymmetricFunctions-MonomialQSymmetric-definition"
]

(* GitHub issue #51: polynomial and QSym basis symbols are inverse in finite variables. *)
VerificationTest[
  And @@ Flatten@Table[
    With[{f = basis[alpha], p = QuasiSymmetricFunctionToPolynomial[basis[alpha], z, n]},
      Expand[PolynomialToQuasiSymmetricFunction[p, z, basis, n] - f] === 0],
    {basis, {MonomialQSymbol, FundamentalQSymbol}}, {d, 0, 4},
    {n, Max[1, d], 5}, {alpha, Select[IntegerCompositions[d], Length[#] <= n &]}],
  True,
  TestID -> "QuasiSymmetricFunctions-polynomial-round-trip-all-bases"
]

(* GitHub issue #51: QSym validation and the symmetric-function embedding. *)
VerificationTest[
  {PolynomialToQuasiSymmetricFunction[z[1]^2 z[2] + z[1] z[2]^2, z, MonomialQSymbol],
   Expand[ToQuasiSymmetric[SchurSymbol[{2, 1}]] -
     (MonomialQSymbol[{2, 1}] + MonomialQSymbol[{1, 2}] +
       2 MonomialQSymbol[{1, 1, 1}])]},
  {MonomialQSymbol[{2, 1}, None] + MonomialQSymbol[{1, 2}, None], 0},
  TestID -> "QuasiSymmetricFunctions-polynomial-bridge-and-embedding"
]

VerificationTest[
  Quiet[PolynomialToQuasiSymmetricFunction[z[1]^2 + z[2], z, MonomialQSymbol, 2],
    PolynomialToQuasiSymmetricFunction::nonquasisymmetric],
  $Failed,
  TestID -> "QuasiSymmetricFunctions-polynomial-bridge-rejects-nonquasisymmetric"
]

(* GitHub issue #51: Schur functions decompose into fundamental QSym functions by SYT descents. *)
VerificationTest[
  And @@ Table[
    Expand[ToQuasiSymmetric[SchurSymbol[lam]] -
      Total[FundamentalQSymmetric[
          DescentSetToComposition[SYTDescentSet[#], Total[lam]]] & /@
        StandardYoungTableaux[lam]]] === 0,
    {lam, {{2, 1}, {3, 1}, {2, 1, 1}}}],
  True,
  TestID -> "QuasiSymmetricFunctions-Schur-SYT-fundamental-identity"
]

(* GitHub issue #51: quasisymmetric Schur functions refine Schur by rearranged shapes. *)
VerificationTest[
  And @@ Table[
    Expand[Total[QuasiSchurQSymmetric /@ DeleteDuplicates[Permutations[lam]]] -
      ToQuasiSymmetric[SchurSymbol[lam]]] === 0,
    {lam, {{2, 1}, {3, 1}, {2, 2}, {2, 1, 1}}}],
  True,
  TestID -> "QuasiSymmetricFunctions-QuasiSchur-rearrangement-identity"
]

(* GitHub issue #51: Tewari-van Willigenburg, Example 2.7: the standard reverse composition
   tableaux of shape (2,1,3) give S_(2,1,3) = F_(2,1,3) + F_(2,2,2) + F_(1,2,1,2). *)
VerificationTest[
  Expand[QuasiSchurQSymmetric[{2, 1, 3}, FundamentalQSymbol] -
    (FundamentalQSymbol[{2, 1, 3}] + FundamentalQSymbol[{2, 2, 2}] +
      FundamentalQSymbol[{1, 2, 1, 2}])],
  0,
  TestID -> "QuasiSymmetricFunctions-QuasiSchur-source-example"
]

(* GitHub issue #51: the atom construction of QuasiSchurQSymmetric agrees with a brute-force
   count of fillings up to degree 4. The filling rules below (rows weakly decreasing, first
   column strictly decreasing downwards, triple rule) are those of composition tableaux with
   the rows listed in reverse order, so they enumerate S_(Reverse[alpha]). *)
VerificationTest[
  Module[{compositionTableauQ, bruteForce},
    compositionTableauQ[alpha_List, values_List] := Module[{cells, f},
      cells = Flatten[Table[{r, k}, {r, Length[alpha]}, {k, alpha[[r]]}], 1];
      f = AssociationThread[cells -> values];
      And[
        And @@ Flatten@Table[f[{r, k}] >= f[{r, k + 1}],
          {r, Length[alpha]}, {k, alpha[[r]] - 1}],
        And @@ Table[f[{r, 1}] > f[{r + 1, 1}], {r, Length[alpha] - 1}],
        And @@ Flatten@Table[If[alpha[[r]] == alpha[[s]],
            Table[f[{r, k}] > f[{s, k}], {k, alpha[[s]]}], True],
          {r, Length[alpha] - 1}, {s, r + 1, Length[alpha]}],
        And @@ Flatten@Table[If[alpha[[r]] > alpha[[s]],
            Table[! (f[{r, k}] >= f[{s, k}]) || f[{r, k + 1}] > f[{s, k}],
              {k, Min[alpha[[s]], alpha[[r]] - 1]}], True],
          {r, Length[alpha] - 1}, {s, r + 1, Length[alpha]}]]];
    bruteForce[alpha_List] := Expand@Total@Flatten@Table[
      If[Union[values] === Range[k] && compositionTableauQ[alpha, values],
        MonomialQSymbol[Table[Count[values, i], {i, k}], None], 0],
      {k, Total[alpha]}, {values, Tuples[Range[k], Total[alpha]]}];
    And @@ Flatten@Table[
      Expand[QuasiSchurQSymmetric[alpha] - bruteForce[Reverse[alpha]]] === 0,
      {d, 1, 4}, {alpha, IntegerCompositions[d]}]],
  True,
  TestID -> "QuasiSymmetricFunctions-QuasiSchur-composition-tableaux"
]

(* GitHub issue #51: variables outside x[1], ..., x[n] are not treated as coefficients. *)
VerificationTest[
  Quiet[{PolynomialToQuasiSymmetricFunction[z[1] + z[2] + z[3], z, MonomialQSymbol, 2],
     PolynomialToQuasiSymmetricFunction[z[1] + z[a], z]},
    PolynomialToQuasiSymmetricFunction::nonquasisymmetric],
  {$Failed, $Failed},
  TestID -> "QuasiSymmetricFunctions-polynomial-bridge-rejects-extra-variables"
]
