(* ::Package:: *)

(* This script introduces SSAF fillings, their statistics and crystals, and
   Mason's SSYT-to-atom insertion. Run from the repository root with:
   wolframscript -file Examples/NewTableaux-Fillings.m *)

PacletDirectoryLoad[DirectoryName[DirectoryName[$InputFileName]]];
Needs["NewTableaux`"];
Needs["NonsymmetricPolynomials`"];

show[label_String, expr_] := Print[label, "\n    ", expr];

(* ::Section:: *)
(* SSAF objects and fillings *)

(* SSAFs are augmented fillings with a basement. Their weak-composition index
   follows the standard key/atom convention (CONVENTIONS.md). *)
atoms = AtomFillings[{0, 2, 1}];
keys = KeyFillings[{0, 2, 1}];
show["number of atom fillings =", Length[atoms]];
show["one atom filling =", First[atoms]];
show["number of key fillings =", Length[keys]];
show["sum of atom monomials =", Total[SSAFMonomial[#, x] & /@ atoms]];


(* ::Section:: *)
(* Statistics and crystals *)

(* Major index, inversions, and co-inversions are filling statistics; CrystalEi
   and CrystalFi move between fillings whose weights differ by a simple root. *)
s = First[atoms];
show["shape, basement, and weight =", {SSAFShape[s], SSAFBasement[s], SSAFWeight[s]}];
show["major index, inversions, co-inversions =",
  {SSAFMajorIndex[s], SSAFInversions[s], SSAFCoInversions[s]}];
show["crystal word for i = 1 =", SSAFCrystalWord[s, 1]];
show["raise then lower =", CrystalFi[CrystalEi[s, 1], 1] === s];


(* ::Section:: *)
(* t-atoms and insertion *)

(* The t-atom generating function weights a filling by t^coInv (1-t)^dn. The
   insertion SSYTToAtom sends a semistandard Young tableau to an SSAF. *)
show["t-atom generating function =",
  Total[(t^SSAFCoInversions[#] (1 - t)^SSAFDn[#] SSAFMonomial[#, x]) & /@
    TAtomFillings[{0, 2, 1}]]];
show["SSYT-to-atom insertion =",
  SSYTToAtom[YoungTableau[{{1, 1, 2}, {2, 3}}]]];
