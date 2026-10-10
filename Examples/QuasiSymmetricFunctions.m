(* ::Package:: *)

(* This script introduces quasisymmetric bases, polynomial bridges, the
   symmetric-function embedding, and quasisymmetric Schur functions. Run from
   the repository root with: wolframscript -file Examples/QuasiSymmetricFunctions.m *)

PacletDirectoryLoad[DirectoryName[DirectoryName[$InputFileName]]];
Needs["QuasiSymmetricFunctions`"];

show[label_String, expr_] := Print[label, "\n    ", expr];

(* ::Section:: *)
(* Quasisymmetric bases *)

(* Compositions index QSym bases. M_alpha is monomial, F_alpha is fundamental,
   and Psi_alpha is the quasisymmetric power-sum basis. *)
show["M_(2,1) =", MonomialQSymmetric[{2, 1}, x]];
show["F_(2,1) =", FundamentalQSymmetric[{2, 1}, x]];
show["Psi_(2,1) =", PowerSumQSymmetric[{2, 1}, x]];
show["M_(2,1) M_(1) =", MonomialQSymmetric[{2, 1}, x] MonomialQSymmetric[{1}, x]];


(* ::Section:: *)
(* Polynomial bridges *)

(* QuasiSymmetricFunctionToPolynomial uses x[1],...,x[n]. The reverse bridge
   recognizes equal coefficients on all increasing index placements. *)
show["F_(2,1) in three variables =",
  QuasiSymmetricFunctionToPolynomial[FundamentalQSymbol[{2, 1}, x], x, 3]];
show["x[1]^2 x[2] + x[1]^2 x[3] + x[2]^2 x[3] in M basis =",
  PolynomialToQuasiSymmetricFunction[
    x[1]^2 x[2] + x[1]^2 x[3] + x[2]^2 x[3], x]];


(* ::Section:: *)
(* Symmetric functions inside QSym *)

(* A symmetric monomial m_lambda embeds as the sum of M_alpha over distinct
   rearrangements of lambda (CONVENTIONS.md). *)
Needs["SymmetricFunctions`"];
show["s_(2,1) embedded in QSym =", ToQuasiSymmetric[SchurSymbol[{2, 1}]]];
show["the same embedding in three variables =",
  QuasiSymmetricFunctionToPolynomial[
    ToQuasiSymmetric[SchurSymbol[{2, 1}]], x, 3]];


(* ::Section:: *)
(* Quasisymmetric Schur functions *)

(* QuasiSchurQSymmetric uses the Haglund--Luoto--Mason--van Willigenburg
   convention; its default output is in the monomial QSym basis. *)
show["S_(2,1) in the monomial basis =", QuasiSchurQSymmetric[{2, 1}]];
show["S_(2,1) in the fundamental basis =",
  QuasiSchurQSymmetric[{2, 1}, FundamentalQSymbol]];
