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

(* Issue #51, P3b: K-theoretic families use the package's beta convention. *)
VerificationTest[
  Expand /@ {
    NonsymmetricPolynomials`GrothendieckPolynomial[{1, 3, 2}, x, 0],
    NonsymmetricPolynomials`GrothendieckPolynomial[{1, 3, 2}, x, 1],
    NonsymmetricPolynomials`GrothendieckPolynomial[{1, 3, 2}, x, -1],
    NonsymmetricPolynomials`LascouxPolynomial[{0, 1}, x, 0],
    NonsymmetricPolynomials`LascouxPolynomial[{0, 1}, x, 1],
    NonsymmetricPolynomials`LascouxPolynomial[{0, 1}, x, -1]},
  {x[1] + x[2], x[1] + x[2] + x[1] x[2],
   x[1] + x[2] - x[1] x[2], x[1] + x[2],
   x[1] + x[2] + x[1] x[2], x[1] + x[2] - x[1] x[2]},
  TestID -> "NonsymmetricPolynomials-P3b-K-families-small-values"
]

(* The beta = 0 specializations are the ordinary operator families, and the
   longest permutation is the initial monomial. *)
VerificationTest[
  And[
    And @@ (Expand[NonsymmetricPolynomials`GrothendieckPolynomial[#, x, 0] - NonsymmetricPolynomials`SchubertPolynomial[#, x]] === 0 & /@
      Permutations[Range[3]]),
    And @@ (Expand[NonsymmetricPolynomials`LascouxPolynomial[#, x, 0] - NonsymmetricPolynomials`KeyPolynomial[#, x]] === 0 & /@
      {{0, 2}, {1, 0}, {1, 1}, {0, 1, 0}}),
    NonsymmetricPolynomials`GrothendieckPolynomial[{3, 2, 1}, x, b] === x[1]^2 x[2],
    Expand[NonsymmetricPolynomials`LascouxPolynomial[{2, 1, 0}, x, b] - x[1]^2 x[2]] === 0],
  True,
  TestID -> "NonsymmetricPolynomials-P3b-beta-zero-and-dominant"
]

(* Issue #51, P3b: slides use prefix domination plus refinement after deleting
   zero parts; locks are Kohnert polynomials of right-justified diagrams. *)
VerificationTest[
  Expand /@ {
    NonsymmetricPolynomials`FundamentalSlidePolynomial[{0, 2}, x],
    NonsymmetricPolynomials`FundamentalSlidePolynomial[{1, 0, 1}, x],
    NonsymmetricPolynomials`LockPolynomial[{0, 2}, x],
    NonsymmetricPolynomials`LockPolynomial[{1, 0}, x],
    NonsymmetricPolynomials`LockPolynomial[{2, 1, 0}, x],
    NonsymmetricPolynomials`LockPolynomial[{1, 2}, x]},
  {x[1]^2 + x[1] x[2] + x[2]^2,
   x[1] x[2] + x[1] x[3], x[1]^2 + x[1] x[2] + x[2]^2, x[1], x[1]^2 x[2],
   x[1] x[2]^2},
  TestID -> "NonsymmetricPolynomials-P3b-slides-and-locks-small-values"
]

VerificationTest[
  Quiet[Needs["MacdonaldPolynomials`"], General::shdw];
  Module[{alphas = {{0, 2}, {1, 0}, {1, 1}, {0, 1, 1}, {1, 0, 1}, {2, 1, 0}}},
    And @@ (Expand[NonsymmetricPolynomials`FundamentalSlidePolynomial[#, x] -
          MacdonaldPolynomials`FundamentalSlide[#, x]] === 0 & /@ alphas) &&
      And @@ (Expand[NonsymmetricPolynomials`LockPolynomial[#, x] -
          MacdonaldPolynomials`LockPolynomial[Reverse[#], x]] === 0 & /@ alphas)],
  True,
  TestID -> "NonsymmetricPolynomials-P3b-legacy-slide-lock-oracle"
]

(* Basis symbols and transitions. *)
VerificationTest[
  Module[{p = x[1]^2 x[3] + 2 x[1] x[2] x[3] - x[2]^3 + 4},
    {NonsymmetricPolynomials`LascouxSymbol[{0, 1, 0}, x], NonsymmetricPolynomials`GrothendieckSymbol[{1, 3, 2}, x],
     NonsymmetricPolynomials`FundamentalSlideSymbol[{0, 2}, x], NonsymmetricPolynomials`LockSymbol[{1, 0}, x],
     Expand[NonsymmetricPolynomials`NonsymmetricToPolynomial[NonsymmetricPolynomials`ToFundamentalSlideBasis[p - 4, x], x] - (p - 4)],
     Expand[NonsymmetricPolynomials`NonsymmetricToPolynomial[NonsymmetricPolynomials`ToLascouxBasis[NonsymmetricPolynomials`LascouxPolynomial[{0, 1, 0}, x], x], x] -
       NonsymmetricPolynomials`LascouxPolynomial[{0, 1, 0}, x]],
     Expand[NonsymmetricPolynomials`NonsymmetricToPolynomial[NonsymmetricPolynomials`ToGrothendieckBasis[NonsymmetricPolynomials`GrothendieckPolynomial[{1, 3, 2}, x], x], x] -
       NonsymmetricPolynomials`GrothendieckPolynomial[{1, 3, 2}, x]],
     NonsymmetricPolynomials`ToLockBasis[NonsymmetricPolynomials`LockPolynomial[{1, 1, 1}, x], x]}],
  {NonsymmetricPolynomials`LascouxSymbol[{0, 1}, x], NonsymmetricPolynomials`GrothendieckSymbol[{1, 3, 2}, x],
   NonsymmetricPolynomials`FundamentalSlideSymbol[{0, 2}, x], NonsymmetricPolynomials`LockSymbol[{1}, x], 0, 0, 0,
   NonsymmetricPolynomials`LockSymbol[{1, 1, 1}, x]},
  TestID -> "NonsymmetricPolynomials-P3b-symbols-and-filtered-conversions"
]

VerificationTest[
  Module[{before, after},
    before = NonsymmetricPolynomials`GrothendieckPolynomial[{1, 3, 2}, x];
    NonsymmetricPolynomials`ClearNonsymmetricPolynomialsCache[];
    after = NonsymmetricPolynomials`GrothendieckPolynomial[{1, 3, 2}, x];
    before === after],
  True,
  TestID -> "NonsymmetricPolynomials-P3b-cache-clears-new-families"
]

basisCoefficientsNonnegative[expr_, head_Symbol] := Module[{symbols},
  symbols = DeleteDuplicates[Cases[expr, z_ /; Head[z] === head, {0, Infinity}]];
  And @@ (TrueQ[Coefficient[expr, #] >= 0] & /@ symbols)
]

(* Assaf--Searles positivity: keys and Schubert polynomials expand positively
   in fundamental slides. *)
VerificationTest[
  Module[{comps = Join @@ (Permutations /@ IntegerPartitions[3, {3}, Range[0, 3]])},
    And @@ (basisCoefficientsNonnegative[NonsymmetricPolynomials`ToFundamentalSlideBasis[NonsymmetricPolynomials`KeyPolynomial[#, x], x, 3],
          NonsymmetricPolynomials`FundamentalSlideSymbol] & /@ comps) &&
      And @@ (basisCoefficientsNonnegative[NonsymmetricPolynomials`ToFundamentalSlideBasis[NonsymmetricPolynomials`SchubertPolynomial[#, x], x, 4],
          NonsymmetricPolynomials`FundamentalSlideSymbol] & /@ Permutations[Range[4]])],
  True,
  TestID -> "NonsymmetricPolynomials-P3b-key-schubert-slide-positivity"
]

(* Issue #51, P3b: G_w expands in Lascoux polynomials with coefficients
   beta^(|alpha| - length(w)) times nonnegative integers (here beta = -1), and the expansion
   round-trips, for all w in S_4. The smallest term of positive codegree: G_2143 contains
   -L_(2,0,1). *)
VerificationTest[
  And @@ Table[
    Module[{expansion, terms},
      expansion = NonsymmetricPolynomials`ToLascouxBasis[
        NonsymmetricPolynomials`GrothendieckPolynomial[w, x], x, 4];
      terms = Cases[expansion, NonsymmetricPolynomials`LascouxSymbol[a_, x] :> a, {0, Infinity}];
      And[
        Expand[NonsymmetricPolynomials`NonsymmetricToPolynomial[expansion, x] -
          NonsymmetricPolynomials`GrothendieckPolynomial[w, x]] === 0,
        AllTrue[terms, (-1)^(Total[#] - Count[Subsets[w, {2}], {p_, q_} /; p > q]) *
            Coefficient[expansion, NonsymmetricPolynomials`LascouxSymbol[#, x]] > 0 &]]],
    {w, Permutations[Range[4]]}] &&
    Coefficient[NonsymmetricPolynomials`ToLascouxBasis[
        NonsymmetricPolynomials`GrothendieckPolynomial[{2, 1, 4, 3}, x], x, 4],
      NonsymmetricPolynomials`LascouxSymbol[{2, 0, 1}, x]] === -1,
  True,
  TestID -> "NonsymmetricPolynomials-P3b-grothendieck-lascoux-sign-pattern"
]

(* Issue #51, P3b: independent Kohnert oracle (Kohnert's algorithm, Assaf-Searles): keys
   are Kohnert polynomials of left-justified diagrams and locks of right-justified ones,
   for all weak compositions with at most 3 parts and size at most 4. *)
VerificationTest[
  Module[{moves, kohnert, left, right, comps},
    (* cells {row, column}, row 1 at the bottom; move the rightmost cell of a row down to
       the first empty cell below it in its column *)
    moves[d_] := DeleteDuplicates@Flatten[Table[
        With[{c = Max[Cases[d, {r, cc_} :> cc]]},
          Module[{rr = r - 1},
            While[rr >= 1 && MemberQ[d, {rr, c}], rr--];
            If[rr >= 1, {Sort@Append[DeleteCases[d, {r, c}], {rr, c}]}, {}]]],
        {r, Union[d[[All, 1]]]}], 1];
    kohnert[d0_] := Module[{seen = {Sort@d0}, front = {Sort@d0}},
      While[front =!= {},
        front = Complement[Union @@ (moves /@ front), seen];
        seen = Join[seen, front]];
      Total[Times @@ (x /@ #[[All, 1]]) & /@ seen]];
    left[a_] := Flatten[Table[{i, c}, {i, Length[a]}, {c, a[[i]]}], 1];
    right[a_] := With[{m = Max[a]},
      Flatten[Table[{i, c}, {i, Length[a]}, {c, m - a[[i]] + 1, m}], 1]];
    comps = Flatten[Table[Select[Tuples[Range[0, 4], n], 0 < Total[#] <= 4 &], {n, 1, 3}], 1];
    And @@ Table[
      Expand[kohnert[left[a]] - NonsymmetricPolynomials`KeyPolynomial[a, x]] === 0 &&
        Expand[kohnert[right[a]] - NonsymmetricPolynomials`LockPolynomial[a, x]] === 0,
      {a, comps}]],
  True,
  TestID -> "NonsymmetricPolynomials-P3b-keys-and-locks-from-Kohnert"
]
