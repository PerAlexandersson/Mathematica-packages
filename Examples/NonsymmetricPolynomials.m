(* ::Package:: *)

(* This script introduces operators, key and K-theoretic polynomials, basis
   symbols, and nonsymmetric Macdonald polynomials. Run from the repository
   root with: wolframscript -file Examples/NonsymmetricPolynomials.m *)

PacletDirectoryLoad[DirectoryName[DirectoryName[$InputFileName]]];
Needs["NonsymmetricPolynomials`"];

show[label_String, expr_] := Print[label, "\n    ", expr];

(* ::Section:: *)
(* Divided differences *)

(* The simple reflection s_i swaps x[i] and x[i+1], and the divided difference
   is (f-s_i f)/(x[i]-x[i+1]). *)
show["s_1(x[1] + 2 x[2]) =", VariableTransposition[x[1] + 2 x[2], x, 1]];
show["divided difference of x[1]^2 x[2] =", DividedDifference[x[1]^2 x[2], x, 1]];
show["Demazure operator on x[2]^2 =", DemazureOperator[x[2]^2, x, 1]];
show["t-Demazure operator on x[2]^2 =", TDemazureOperator[x[2]^2, x, t, 1]];


(* ::Section:: *)
(* Keys, atoms, and slides *)

(* Weak compositions use the standard key convention: kappa_(0,1) = x[1] + x[2]
   (CONVENTIONS.md). Setting t = 0 specializes t-keys and t-atoms. *)
show["key kappa_(0,1) =", KeyPolynomial[{0, 1}, x]];
show["atom A_(0,1) =", AtomPolynomial[{0, 1}, x]];
show["t-key at t = 1/2 =", TKeyPolynomial[{0, 1}, x, 1/2]];
show["fundamental slide F_(0,1) =", FundamentalSlidePolynomial[{0, 1}, x]];
show["lock of (2,0) =", LockPolynomial[{2, 0}, x]];


(* ::Section:: *)
(* Schubert and Grothendieck families *)

(* Schubert polynomials use one-line permutation notation; beta = -1 gives the
   usual Grothendieck polynomial and its K-theoretic lower-degree terms. *)
show["Schubert polynomial for 132 =", SchubertPolynomial[{1, 3, 2}, x]];
show["Grothendieck polynomial for 132 =", GrothendieckPolynomial[{1, 3, 2}, x]];
show["Lascoux polynomial L_(0,1) =", LascouxPolynomial[{0, 1}, x]];

(* Dual Grothendieck polynomials come from fillings with column weights; for weakly
   increasing alpha they are the symmetric g_lambda (here g_(1,1) in two variables). *)
show["dual Grothendieck g_(1,1) =", DualGrothendieckPolynomial[{1, 1}, x]];


(* ::Section:: *)
(* Basis symbols and conversion *)

(* Basis symbols retain a compact algebraic form; NonsymmetricToPolynomial and
   ToKeyBasis move between that form and ordinary polynomials. *)
show["key basis symbol =", KeySymbol[{0, 1}, x]];
show["expanded key basis symbol =", NonsymmetricToPolynomial[KeySymbol[{0, 1}, x], x]];
show["key-basis expansion of x[1] + x[2] =", ToKeyBasis[x[1] + x[2], x]];


(* ::Section:: *)
(* Nonsymmetric Macdonald and Jack polynomials *)

(* E_alpha has leading monomial x^alpha in the identity basement. At q = 0 it
   becomes the t-atom, while a permuted basement is selected by sigma. *)
show["E_(1,0)(x;q,t) =", MacdonaldEPolynomial[{1, 0}, x, q, t]];
show["E_(1,0) with basement 21 =", MacdonaldEPolynomial[{1, 0}, {2, 1}, x, q, t]];
show["q = 0 gives the t-atom =",
  Expand[MacdonaldEPolynomial[{1, 0}, x, 0, t] - TAtomPolynomial[{1, 0}, x, t]] === 0];
show["nonsymmetric Jack polynomial J_(1,0) =", NonsymmetricJackPolynomial[{1, 0}, x, a]];
