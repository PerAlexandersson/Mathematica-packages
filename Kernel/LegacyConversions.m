(* ::Package:: *)

(* Explicit data conversions for notebooks using the legacy packages. *)

(* No package dependencies on $ContextPath: users mix this package with the legacy ones, and
   names such as KeyPolynomial exist in both NonsymmetricPolynomials and the legacy
   MacdonaldPolynomials. The supported packages whose heads appear below are loaded privately. *)
BeginPackage["LegacyConversions`"];

FromLegacyGTPattern;
ToLegacyGTPattern;
FromLegacyYoungTableau;
ToLegacyYoungTableau;
FromLegacyAreaList;
ToLegacyAreaList;
FromLegacyEdges;
ToLegacyEdges;
FromLegacyKeyIndex;
FromLegacyIndex;

FromLegacyGTPattern::usage =
  "FromLegacyGTPattern[OldYoungTableaux`GTPattern[rows]] converts legacy top-to-bottom rows to a GTPatterns`GTPattern with bottom-to-top rows.";
ToLegacyGTPattern::usage =
  "ToLegacyGTPattern[GTPatterns`GTPattern[rows]] converts bottom-to-top rows to the legacy top-to-bottom representation.";
FromLegacyYoungTableau::usage =
  "FromLegacyYoungTableau[OldYoungTableaux`YoungTableau[rows]] converts legacy skew markers OldYoungTableaux`Private`SKEW to None in a NewTableaux`YoungTableau.";
ToLegacyYoungTableau::usage =
  "ToLegacyYoungTableau[NewTableaux`YoungTableau[rows]] converts None skew cells to OldYoungTableaux`Private`SKEW.";
FromLegacyAreaList::usage =
  "FromLegacyAreaList[a] reverses a ChromaticFunctions area list ending in 0 to the 0-first convention used by CatalanObjects and UnicellularChromatics.";
ToLegacyAreaList::usage =
  "ToLegacyAreaList[a] reverses a 0-first area list to the ChromaticFunctions convention ending in 0.";
FromLegacyEdges::usage =
  "FromLegacyEdges[edges,n] converts a legacy edge list to the supported vertex convention by relabelling every vertex v as n + 1 - v and reversing each ordered edge.";
ToLegacyEdges::usage =
  "ToLegacyEdges[edges,n] applies the inverse of FromLegacyEdges; the vertex relabelling v -> n + 1 - v is an involution.";
FromLegacyKeyIndex::usage =
  "FromLegacyKeyIndex[alpha] reverses a MacdonaldPolynomials key, t-key, or lock index to the standard NonsymmetricPolynomials convention. Atom, t-atom, Schubert, and fundamental-slide indices are unchanged; use FromLegacyIndex for one explicit family.";
FromLegacyIndex::usage =
  "FromLegacyIndex[family,index] converts a legacy nonsymmetric-polynomial index: family \"Key\", \"TKey\", or \"Lock\" reverses index, while \"Atom\", \"TAtom\", \"Schubert\", \"Slide\", and \"FundamentalSlide\" leave it unchanged.";

Begin["`Private`"];

Needs["GTPatterns`"];
Needs["NewTableaux`"];

FromLegacyGTPattern[OldYoungTableaux`GTPattern[rows_]] :=
  GTPatterns`GTPattern[Reverse[rows]];

ToLegacyGTPattern[GTPatterns`GTPattern[rows_]] :=
  OldYoungTableaux`GTPattern[Reverse[rows]];

FromLegacyYoungTableau[OldYoungTableaux`YoungTableau[rows_]] :=
  NewTableaux`YoungTableau[rows /. OldYoungTableaux`Private`SKEW -> None];

ToLegacyYoungTableau[NewTableaux`YoungTableau[rows_]] :=
  OldYoungTableaux`YoungTableau[rows /. None -> OldYoungTableaux`Private`SKEW];

FromLegacyAreaList[area_List] := Reverse[area];

ToLegacyAreaList[area_List] := Reverse[area];

FromLegacyEdges[edges_List, n_Integer] :=
  (n + 1 - Reverse[#]) & /@ edges;

ToLegacyEdges[edges_List, n_Integer] := FromLegacyEdges[edges, n];

FromLegacyKeyIndex[alpha_List] := Reverse[alpha];

FromLegacyIndex::family = "Unknown family `1`; use \"Key\", \"TKey\", \"Lock\", \"Atom\", \"TAtom\", \"Schubert\", \"Slide\" or \"FundamentalSlide\".";
FromLegacyIndex[family_String, index_List] := Switch[family,
  "Key" | "TKey" | "Lock", FromLegacyKeyIndex[index],
  "Atom" | "TAtom" | "Schubert" | "Slide" | "FundamentalSlide", index,
  _, Message[FromLegacyIndex::family, family]; $Failed
];

End[];

Protect @@ Select[Names["LegacyConversions`*"], !StringMatchQ[#, ___ ~~ "$" ~~ ___] &];

EndPackage[];
