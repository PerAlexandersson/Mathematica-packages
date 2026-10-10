(* ::Package:: *)

(* This script introduces Gelfand--Tsetlin patterns, BZ patterns, Gog/Magog
   objects, and GT-polytope data. Run from the repository root with:
   wolframscript -file Examples/GTPatterns.m *)

PacletDirectoryLoad[DirectoryName[DirectoryName[$InputFileName]]];
Needs["GTPatterns`"];

show[label_String, expr_] := Print[label, "\n    ", expr];

(* ::Section:: *)
(* Gelfand--Tsetlin patterns *)

(* Rows are listed bottom to top. The pattern below encodes a tableau of shape
   (2,1) and content (1,1,1), as specified in CONVENTIONS.md. *)
g = First[GTPatterns[{2, 1}, {}, {1, 1, 1}]];
show["a GT pattern =", g];
show["shape and weight =", GTShape[g]];
show["its weight monomial =", GTMonomial[g, x]];
show["entry in row 3, column 2 =", g[3, 2]];


(* ::Section:: *)
(* BZ, Gog, and Magog patterns *)

(* BZ patterns enumerate Littlewood--Richardson tableaux, while Gog and Magog
   patterns have the same small alternating-sign-matrix counts. *)
show["BZ patterns for c^(2)_(1,1) =", BZPatterns[{2}, {1}, {1}]];
show["Gog and Magog counts at size 4 =", {Length[GogPatterns[4]], Length[MagogPatterns[4]]}];
show["entrywise sum of two GT patterns =", GTPlus[g, g]];


(* ::Section:: *)
(* Geometry and Ehrhart data *)

(* Tiles, snakes, and the face dimension describe the polytope containing a GT
   pattern; a symbolic k asks for the stretched Kostka/Ehrhart polynomial. *)
show["tiles and snakes =", {GTTiles[g], GTSnakes[g]}];
show["containing face dimension =", ContainingFaceDimension[g]];
show["Ehrhart polynomial for (2,1) and content (1,1,1) =",
  GTEhrhartPolynomial[{2, 1}, {}, {1, 1, 1}, k]];
show["TikZ output begins with =", StringTake[GTPatternTikz[g, GTPartition -> "Snakes"], 24]];
