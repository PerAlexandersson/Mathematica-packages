(* GitHub issue #6: Code Inspector baseline. High-confidence (>= 0.9) errors are
   allowed only up to the reviewed counts below; anything new fails the test.
   Reviewed 2026-10-10:
   - UnexpectedDot: SyntaxInformation patterns written {_...}; harmless.
   - ImplicitTimesAcrossLines: intended products split across lines.
   - SetInfixInequality: memoized Boolean tests such as colOk[c] = (... == 0).
   Lower these counts when the code is cleaned up; never raise them. *)

VerificationTest[
  Needs["CodeInspector`"],
  Null,
  TestID -> "Lint-loads-CodeInspector"
]

lintBaseline = <|
  "ChromaticFunctions.m" -> <|"ImplicitTimesAcrossLines" -> 1|>,
  "CombinatoricTools.m" -> <|"UnexpectedDot" -> 25, "SetInfixInequality" -> 2,
    "ImplicitTimesAcrossLines" -> 5|>,
  "MacdonaldPolynomials.m" -> <|"ImplicitTimesAcrossLines" -> 1|>,
  "PermutationTools.m" -> <|"UnexpectedDot" -> 1|>,
  "QuasiSymmetricFunctions.m" -> <|"ImplicitTimesAcrossLines" -> 2|>,
  "SymmetricFunctions.m" -> <|"ImplicitTimesAcrossLines" -> 4|>
|>;

lintErrors[file_String] := Counts[#[[1]] & /@ Select[CodeInspect[File[file]],
    MemberQ[{"Error", "Fatal"}, #[[3]]] && Lookup[#[[4]], ConfidenceLevel, 0] >= 0.9 &]];

VerificationTest[
  Select[
    Association@Table[
      With[{name = FileNameTake[f], found = lintErrors[f]},
        name -> Select[
          Association@KeyValueMap[#1 -> {#2, Lookup[Lookup[lintBaseline, name, <||>], #1, 0]} &, found],
          #[[1]] > #[[2]] &]],
      {f, FileNames["*.m", FileNameJoin[{testRoot, #}] & /@ {"Kernel", "Legacy"}]}],
    # =!= <||> &],
  <||>,
  TestID -> "Lint-no-new-high-confidence-errors"
]
