testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

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
