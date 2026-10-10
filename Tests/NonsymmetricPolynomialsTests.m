testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["NonsymmetricPolynomials`"],
  Null,
  TestID -> "NonsymmetricPolynomials-loads-cleanly"
]

(* Issue #51, P3: operator-generated families with standard indexing. *)

VerificationTest[
  Expand /@ {KeyPolynomial[{0, 1}, x], AtomPolynomial[{0, 1}, x], KeyPolynomial[{1, 0, 2}, x],
   SchubertPolynomial[{1, 3, 2}, x], SchubertPolynomial[{3, 1, 2}, x], SchubertPolynomial[{2, 3, 1}, x]},
  Expand /@ {x[1] + x[2], x[2],
   x[1]^2 x[2] + x[1] x[2]^2 + x[1]^2 x[3] + x[1] x[2] x[3] + x[1] x[3]^2,
   x[1] + x[2], x[1]^2, x[1] x[2]},
  TestID -> "NonsymmetricPolynomials-small-values"
]

(* Independent checks: keys of increasing compositions are Schur polynomials, and the
   key of a weakly decreasing composition is a monomial. *)
VerificationTest[
  {And @@ Table[
     With[{n = 3, lam = PadRight[lam0, 3]},
       Expand[KeyPolynomial[Reverse[lam], x] -
         Together[Det[Table[x[i]^(lam[[j]] + n - j), {i, n}, {j, n}]]/Det[Table[x[i]^(n - j), {i, n}, {j, n}]]]] === 0],
     {lam0, Join @@ Table[IntegerPartitions[d, 3], {d, 1, 4}]}],
   KeyPolynomial[{3, 1, 1, 0}, x], KeyPolynomial[{0, 0, 2}, x] - Sum[x[i] x[j], {i, 3}, {j, i, 3}] // Expand},
  {True, x[1]^3 x[2] x[3], 0},
  TestID -> "NonsymmetricPolynomials-keys-Schur-and-monomials"
]

(* Operator identities on a generic polynomial. *)
VerificationTest[
  Module[{f = x[1]^3 x[2] + 2 x[2]^2 x[3] + x[1] x[3]^2 + 5 x[4], z = 0},
    {Expand[DividedDifference[DividedDifference[f, x, 1], x, 1]],
     Expand[DemazureOperator[DemazureOperator[f, x, 2], x, 2] - DemazureOperator[f, x, 2]],
     Expand[DemazureOperator[f, x, {1, 2, 1}] - DemazureOperator[f, x, {2, 1, 2}]],
     Expand[DividedDifference[f, x, {1, 2, 1}] - DividedDifference[f, x, {2, 1, 2}]],
     Expand[DemazureAtomOperator[f, x, {1, 2, 1}] - DemazureAtomOperator[f, x, {2, 1, 2}]],
     Expand[TDemazureOperator[f, x, t, {1, 2, 1}] - TDemazureOperator[f, x, t, {2, 1, 2}]],
     Expand[KDividedDifference[KDividedDifference[f, x, b, 1], x, b, 1] + b KDividedDifference[f, x, b, 1]],
     Expand[KDemazureOperator[f, x, 0, 3] - DemazureOperator[f, x, 3]]}],
  ConstantArray[0, 8],
  TestID -> "NonsymmetricPolynomials-operator-relations"
]

(* Keys are sums of atoms (with coefficient 1) over rearrangements below in Bruhat order. *)
VerificationTest[
  And @@ Table[
    With[{c = ToAtomBasis[KeyPolynomial[alpha, x], x, 3]},
      Union[Last /@ CoefficientRules[c, Cases[c, _AtomSymbol, {0, Infinity}]]] === {1} &&
        MemberQ[Cases[c, _AtomSymbol, {0, Infinity}], AtomSymbol[alpha, x]]],
    {alpha, Join @@ (Permutations /@ Join @@ Table[IntegerPartitions[d, {3}, Range[0, d]], {d, 1, 4}])}],
  True,
  TestID -> "NonsymmetricPolynomials-keys-are-sums-of-atoms"
]

(* Basis symbols and conversions round-trip. *)
VerificationTest[
  Module[{p = 3 x[1]^2 x[3] - x[2] x[3] + 7 x[1] + 2},
    {KeySymbol[{0, 2, 0}, x], SchubertSymbol[{2, 1, 3}, x], SchubertSymbol[{1, 2}, x],
     Expand[NonsymmetricToPolynomial[ToKeyBasis[p, x], x] - p],
     Expand[NonsymmetricToPolynomial[ToAtomBasis[p, x], x] - p],
     Expand[NonsymmetricToPolynomial[ToSchubertBasis[p, x], x] - p],
     ToKeyBasis[SchubertPolynomial[{1, 3, 2}, x], x]}],
  {KeySymbol[{0, 2}, x], SchubertSymbol[{2, 1}, x], 1, 0, 0, 0, KeySymbol[{0, 1}, x]},
  TestID -> "NonsymmetricPolynomials-symbols-and-conversions"
]

VerificationTest[
  {PermutationToCode[{3, 1, 4, 2}], CodeToPermutation[{2, 0, 1}],
   And @@ (CodeToPermutation[PermutationToCode[#]] === (# //. {w___, k_} /; k == Length[{w, k}] :> {w}) & /@ Permutations[Range[4]])},
  {{2, 0, 1, 0}, {3, 1, 4, 2}, True},
  TestID -> "NonsymmetricPolynomials-Lehmer-codes"
]

(* Legacy oracle (Legacy/MacdonaldPolynomials.m): its keys use reversed compositions,
   its atoms and Schubert polynomials the standard convention. *)
VerificationTest[
  Quiet[Needs["MacdonaldPolynomials`"], General::shdw];
  Module[{comps = Join @@ (Permutations /@ Join @@ Table[IntegerPartitions[d, {3}, Range[0, d]], {d, 0, 4}])},
    And[
      And @@ (Expand[KeyPolynomial[#, x] - MacdonaldPolynomials`KeyPolynomial[Reverse[#], x]] === 0 & /@ comps),
      And @@ (Expand[AtomPolynomial[#, x] - MacdonaldPolynomials`AtomPolynomial[#, x]] === 0 & /@ comps),
      And @@ (Expand[TKeyPolynomial[#, x, t] - MacdonaldPolynomials`OperatorKeyTPolynomial[Reverse[#], x, t]] === 0 & /@ comps),
      And @@ (Expand[TAtomPolynomial[#, x, t] - MacdonaldPolynomials`AtomTPolynomial[#, x, t]] === 0 & /@ comps),
      And @@ (Expand[SchubertPolynomial[#, x] - MacdonaldPolynomials`SchubertPolynomial[#, x]] === 0 & /@ Permutations[Range[4]])]],
  True,
  TestID -> "NonsymmetricPolynomials-legacy-Macdonald-oracle"
]
