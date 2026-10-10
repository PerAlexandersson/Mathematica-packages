testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

(* GitHub issue #3: packages keep helpers in their own private contexts, so the
   result of one package's helper cannot depend on which other packages were
   loaded first. OldYoungTableaux used to redefine the GTPatterns helper below.
   Shadowing warnings for duplicated *public* names are expected until the legacy
   packages stop exporting them (issue #9), so only General::shdw is tolerated. *)

VerificationTest[
  Quiet[Needs["OldYoungTableaux`"]; Needs["GTPatterns`"], General::shdw],
  Null,
  TestID -> "LoadOrder-OldYoungTableaux-then-GTPatterns-loads-cleanly"
]

VerificationTest[
  {GTPatterns`Private`GTIndexToGrahphicsCoordinates[{2, 1}],
   Length@DownValues[GTPatterns`Private`GTIndexToGrahphicsCoordinates]},
  {{0, 2}, 1},
  TestID -> "LoadOrder-GTPatterns-helper-not-redefined"
]

VerificationTest[
  Quiet[Scan[Needs, {"CombinatoricTools`", "NewTableaux`", "SymmetricFunctions`",
    "PolynomialTools`", "PermutationTools`", "QuasiSymmetricFunctions`",
    "GraphTools`", "MatroidTools`", "CatalanObjects`", "UnicellularChromatics`",
    "RookTools`", "PosetData`"}], General::shdw];
  Names["Private`*"],
  {},
  TestID -> "LoadOrder-no-shared-top-level-Private-context"
]

(* GitHub issue #2: supported packages export no name twice. StrictEdges and
   WeakEdges are shared options owned by CombinatoricTools; the CatalanObjects grid
   is RookPlacementGrid, distinct from RookTools`RookPlacementPlot. *)
VerificationTest[
  Module[{supported = {"AlgebraicBases", "CombinatoricTools", "NewTableaux", "SymmetricFunctions",
      "GTPatterns", "PolynomialTools", "PermutationTools", "QuasiSymmetricFunctions",
      "GraphTools", "MatroidTools", "CatalanObjects", "UnicellularChromatics",
      "RookTools", "PosetData", "NonsymmetricPolynomials"}, short},
    Scan[Needs[# <> "`"] &, supported];
    short = Flatten[(Last@StringSplit[#, "`"] & /@ Names[# <> "`*"]) & /@ supported];
    Select[Tally[short], Last[#] > 1 &]],
  {},
  TestID -> "LoadOrder-supported-packages-export-distinct-names"
]

VerificationTest[
  {Context[StrictEdges], Context[WeakEdges], Head[CatalanObjects`RookPlacementGrid[{{1, 1}}, {{1, 1}}]]},
  {"CombinatoricTools`", "CombinatoricTools`", Grid},
  TestID -> "LoadOrder-shared-edge-options-and-rook-grid"
]
