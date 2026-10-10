testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

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
