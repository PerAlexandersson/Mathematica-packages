(* ::Package:: *)

(* Introduction to the SymmetricFunctions package.

   A plain-text version of SymmetricFunctions-Introduction.nb. Run it with
     wolframscript -file Examples/SymmetricFunctions-Introduction.m
   or evaluate it piece by piece in a notebook. Tests/IntroductionExampleTests.m checks
   that it runs without messages. *)

PacletDirectoryLoad[ParentDirectory[DirectoryName[$InputFileName]]];
Needs["SymmetricFunctions`"];

show[label_String, expr_] := Print[label, "\n    ", expr];


(* ::Section:: *)
(* Basics *)

(* The core bases are the monomial, Schur, elementary, power-sum and complete
   homogeneous symmetric functions. An optional second argument is the alphabet (any
   symbol; the default is None). Products and integer powers of elementary, complete
   homogeneous and power-sum symbols expand automatically, but only when the alphabets
   match. *)

show["e_3 e_322 =", 2 ElementaryESymbol[{3}] ElementaryESymbol[{3, 2, 2}]];
show["m_32(x) m_32(y) =", MonomialSymbol[{3, 2}, x] MonomialSymbol[{3, 2}, y]];
show["s_32 s_211 + p_3^3 =", SchurSymbol[{3, 2}] SchurSymbol[{2, 1, 1}] + PowerSumSymbol[3]^3];
show["FullForm[e_322] =", FullForm[ElementaryESymbol[{3, 2, 2}]]];

(* To expand products of Schur or monomial symmetric functions, use LRExpand and
   MExpand. *)

show["MExpand =", MExpand[MonomialSymbol[{3, 2, 1}] (MonomialSymbol[{3, 2, 1}] + MonomialSymbol[{2, 2, 1}])]];
show["LRExpand =", LRExpand[SchurSymbol[{3, 2, 1}] (SchurSymbol[{3, 2, 1}] + SchurSymbol[{2, 2, 1}])]];


(* ::Section:: *)
(* Standard symmetric functions *)

(* ElementaryESymmetric, CompleteHSymmetric, PowerSumSymmetric and SchurSymmetric
   return monomial expansions. Schur functions indexed by compositions use the
   Jacobi-Trudi (slinky) rule. *)

show["h_221 =", CompleteHSymmetric[{2, 2, 1}]];
show["s_131 (a composition) =", SchurSymmetric[{1, 3, 1}]];


(* ::Section:: *)
(* Converting between bases *)

(* ToMonomialBasis, ToSchurBasis, ToElementaryEBasis, ToCompleteHBasis and
   ToPowerSumBasis convert expressions; an optional last argument selects the
   alphabet to convert. *)

show["e_321 in Schur =", ToSchurBasis[ElementaryESymmetric[{3, 2, 1}]]];
show["h_322 in power sums =", ToPowerSumBasis[CompleteHSymbol[{3, 2, 2}]]];
show["only alphabet x converted =", ToSchurBasis[ElementaryESymmetric[{3, 2, 1}, x] MonomialSymmetric[{2, 2, 1}, y], x]];


(* ::Section:: *)
(* Operators *)

(* The omega involution, the Hall inner product, the principal specialization (as a
   formal power series or in finitely many variables) and plethysm. *)

show["omega(s_41) =", OmegaInvolution[SchurSymbol[{4, 1}]]];
show["<s_3, s_3> =", HallInnerProduct[SchurSymbol[{3}], SchurSymbol[{3}]]];
show["<s_3, s_21> =", HallInnerProduct[SchurSymbol[{3}], SchurSymbol[{2, 1}]]];
show["<m_lam, h_mu> for |lam| = |mu| = 5 is the identity matrix:",
  Table[HallInnerProduct[MonomialSymbol[lam], CompleteHSymbol[mu]],
      {lam, IntegerPartitions[5]}, {mu, IntegerPartitions[5]}] == IdentityMatrix[7]];
show["s_21 in 5 variables at q =", Expand[PrincipalSpecialization[SchurSymbol[{2, 1}], q, 5]]];
show["s_32[1/(1-q)] agrees with the principal specialization:",
  Simplify[ToSchurBasis[Plethysm[SchurSymbol[{3, 2}], 1/(1 - q)]] - PrincipalSpecialization[SchurSymbol[{3, 2}], q]] === 0];
show["s_21[p_1(x) + p_1(y)] =", ToSchurBasis[Plethysm[SchurSymbol[{2, 1}], PowerSumSymbol[1, x] + PowerSumSymbol[1, y]], {x, y}]];

(* SymmetricFunctionToPolynomial shows an actual polynomial in a chosen number of
   variables. *)

show["s_21(x1, x2, x3) =", Nicify[SymmetricFunctionToPolynomial[SchurSymbol[{2, 1}], x, 3]]];


(* ::Section:: *)
(* Other families *)

(* Skew Schur, Hall-Littlewood, Jack and Macdonald functions. MacdonaldHSymmetric uses
   Haglund's convention: H~_{2} = s_2 + q s_11. *)

show["s_{3321/21} =", SkewSchurSymmetric[{{3, 3, 2, 1}, {2, 1}}]];
show["modified Hall-Littlewood H~_{21}(x; t) = H~_{21}(x; 0, t) =", ToSchurBasis[HallLittlewoodMSymmetric[{2, 1}, t]]];
show["Jack P_{21} =", JackPSymmetric[{2, 1}, a]];
show["H~_{21}(q, t) =", ToSchurBasis[MacdonaldHSymmetric[{2, 1}, q, t]]];


(* ::Section:: *)
(* Delta and nabla operators *)

show["nabla e_3 =", NablaOperator[ElementaryESymbol[3], q, t]];
show["<nabla e_3, e_3> = C_3(q, t):", Expand[HallInnerProduct[NablaOperator[ElementaryESymbol[3], q, t], ElementaryESymbol[3]]] === Expand[qtCatalan[3, q, t]]];
show["Delta_{m_21} e_11 =", DeltaOperator[MonomialSymmetric[{2, 1}], ElementaryESymmetric[{1, 1}], q, t]];
show["Delta'_{e_2} e_3 =", DeltaPrimOperator[ElementaryESymmetric[2], ElementaryESymmetric[{3}], q, t]];


(* ::Section:: *)
(* Positivity and transition matrices *)

show["s_{322/2} is Schur positive:", PositiveCoefficientsQ[ToSchurBasis[SkewSchurSymmetric[{{3, 2, 2}, {2}}]], SchurSymbol]];
show["e-to-s transition matrix in degree 5:", MatrixForm[SymFuncTransMat[ElementaryESymmetric, SchurSymmetric, 5]]];
