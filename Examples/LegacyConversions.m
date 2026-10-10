(* ::Package:: *)

(* This script converts legacy GT patterns, tableaux, area lists, edges, and
   nonsymmetric indices. Run from the repository root with:
   wolframscript -file Examples/LegacyConversions.m *)

PacletDirectoryLoad[DirectoryName[DirectoryName[$InputFileName]]];
Needs["LegacyConversions`"];

(* The legacy package is loaded only here to construct old objects for a
   migration example; Quiet suppresses its known General::shdw load message. *)
Quiet[Needs["OldYoungTableaux`"], General::shdw];

show[label_String, expr_] := Print[label, "\n    ", expr];

(* ::Section:: *)
(* GT-pattern row order *)

(* Legacy GT patterns list rows top to bottom; supported GTPatterns list them
   bottom to top. The conversion preserves the represented tableau. *)
oldGT = OldYoungTableaux`GTPattern[{{2, 1}, {1, 1}, {1}}];
show["legacy GT pattern =", oldGT];
show["supported GT pattern =", FromLegacyGTPattern[oldGT]];
show["round trip to legacy =", ToLegacyGTPattern[FromLegacyGTPattern[oldGT]]];


(* ::Section:: *)
(* Area lists and edges *)

(* Legacy area lists end in 0, while supported Catalan conventions start with
   0; edge conversion reverses the vertex convention on n vertices. *)
show["legacy area list {1,1,0} ->", FromLegacyAreaList[{1, 1, 0}]];
show["supported area list {0,1,1} ->", ToLegacyAreaList[{0, 1, 1}]];
show["legacy edges -> supported edges =", FromLegacyEdges[{{1, 2}, {2, 3}}, 3]];
show["supported edges -> legacy edges =", ToLegacyEdges[{{2, 3}, {1, 2}}, 3]];


(* ::Section:: *)
(* Legacy nonsymmetric indices *)

(* Legacy keys, t-keys, and locks reverse their weak-composition index; atom,
   t-atom, Schubert, and slide indices are unchanged. *)
show["legacy key index {0,2,1} ->", FromLegacyKeyIndex[{0, 2, 1}]];
show["legacy Lock index {0,2,1} ->", FromLegacyIndex["Lock", {0, 2, 1}]];
show["legacy Atom index {0,2,1} ->", FromLegacyIndex["Atom", {0, 2, 1}]];


(* ::Section:: *)
(* Young tableaux *)

(* Legacy YoungTableau objects use the old head; ordinary cells convert directly
   to the supported NewTableaux representation. *)
show["legacy tableau -> supported tableau =",
  FromLegacyYoungTableau[OldYoungTableaux`YoungTableau[{{1, 2}, {3}}]]];
