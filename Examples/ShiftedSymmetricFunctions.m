(* ::Package:: *)

(* This script introduces shifted Schur and Jack polynomials, character
   evaluations, and Stanley character polynomials. Run from the repository
   root with: wolframscript -file Examples/ShiftedSymmetricFunctions.m *)

PacletDirectoryLoad[DirectoryName[DirectoryName[$InputFileName]]];
Needs["ShiftedSymmetricFunctions`"];

show[label_String, expr_] := Print[label, "\n    ", expr];

(* ::Section:: *)
(* Shifted Schur polynomials *)

(* Shifted Schur polynomials are factorial analogues of Schur polynomials; their
   vanishing property says s*_mu(lambda) = 0 unless mu is contained in lambda. *)
show["s*_(2,1)(x[1],x[2]) =", ShiftedSchurPolynomial[{2, 1}, 2, x]];
show["s*_(2,1)(2) vanishes =", ShiftedSchurEvaluate[{2, 1}, {2}] === 0];
show["s*_(2,1)(2,1) =", ShiftedSchurEvaluate[{2, 1}, {2, 1}]];


(* ::Section:: *)
(* Shifted Jack polynomials *)

(* The parameter a is the Jack parameter. J*_mu is the lower-hook normalization
   of P*_mu, and the evaluation functions substitute a partition for x. *)
show["P*_(2,1)(x;a) =", ShiftedJackPPolynomial[{2, 1}, 2, x, a]];
show["J*_(2,1)(x;a) =", ShiftedJackJPolynomial[{2, 1}, 2, x, a]];
show["P*_(2,1)(2,1) =", ShiftedJackPEvaluate[{2, 1}, {2, 1}, a]];
show["J*_(2,1)(2,1) =", ShiftedJackJEvaluate[{2, 1}, {2, 1}, a]];


(* ::Section:: *)
(* Characters *)

(* NormalizedCharacter uses the falling-factorial normalization of irreducible
   character values; StanleyCharacterPolynomial uses multirectangular p,q. *)
show["normalized character for mu = (2), lambda = (3) =",
  NormalizedCharacter[{2}, {3}]];
show["Stanley character polynomial for mu = (2) =",
  StanleyCharacterPolynomial[{2}, p, q, 1]];
