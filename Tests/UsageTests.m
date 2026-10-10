testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

(* GitHub issue #21: every public symbol of every package has a usage string.
   Legacy packages still export duplicated names (issue #9), so only
   General::shdw is tolerated while loading. *)

usagePackages = {"AlgebraicBases", "CombinatoricTools", "NewTableaux", "SymmetricFunctions", "ShiftedSymmetricFunctions", "GTPatterns",
  "PolynomialTools", "PermutationTools", "QuasiSymmetricFunctions", "GraphTools",
  "MatroidTools", "CatalanObjects", "UnicellularChromatics", "ChromaticFunctions",
  "RookTools", "PosetData", "TreesData", "OldYoungTableaux", "MacdonaldPolynomials",
  "RunSortedWords", "NonsymmetricPolynomials", "LegacyConversions"};

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

(* Removes (possibly nested) comments, innermost first, so that commented-out code is ignored. *)
withoutComments[text_String] := FixedPoint[
  StringReplace[#, "(*" ~~ c : Shortest[___] ~~ "*)" /; StringFreeQ[c, "(*"] :> ""] &, text];

(* Every name given a usage message in a Kernel/ package file is a public symbol of that
   package. Test files are parsed before they run, which creates the public symbols they
   mention and can hide a missing export (a private definition then attaches to the public
   symbol), so this test works with strings only. *)
VerificationTest[
  Association@Select[
    Table[With[{ctx = FileBaseName[file] <> "`",
        names = DeleteDuplicates@StringCases[withoutComments[Import[file, "Text"]],
          StartOfLine ~~ n : (LetterCharacter ~~ (WordCharacter ...)) ~~ "::usage" :> n]},
      FileBaseName[file] -> Select[names, Names[ctx <> #] === {} &]],
      {file, FileNames["*.m", FileNameJoin[{testRoot, "Kernel"}]]}],
    Last[#] =!= {} &],
  <||>,
  TestID -> "Usage-documented-names-are-exported"
]
