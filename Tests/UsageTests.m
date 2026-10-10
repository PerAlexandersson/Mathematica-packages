testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

(* GitHub issue #21: every public symbol of every package has a usage string.
   Legacy packages still export duplicated names (issue #9), so only
   General::shdw is tolerated while loading. *)

usagePackages = {"AlgebraicBases", "CombinatoricTools", "NewTableaux", "SymmetricFunctions", "GTPatterns",
  "PolynomialTools", "PermutationTools", "QuasiSymmetricFunctions", "GraphTools",
  "MatroidTools", "CatalanObjects", "UnicellularChromatics", "ChromaticFunctions",
  "RookTools", "PosetData", "TreesData", "OldYoungTableaux", "MacdonaldPolynomials",
  "RunSortedWords", "NonsymmetricPolynomials"};

VerificationTest[
  Quiet[Scan[Needs[# <> "`"] &, usagePackages], General::shdw],
  Null,
  TestID -> "Usage-all-packages-load"
]

VerificationTest[
  Association@Select[
    Table[p -> Select[Names[p <> "`*"],
        !StringQ[ToExpression[#, InputForm, Function[s, MessageName[s, "usage"], HoldAll]]] &],
      {p, usagePackages}],
    Last[#] =!= {} &],
  <||>,
  TestID -> "Usage-every-public-symbol-documented"
]
