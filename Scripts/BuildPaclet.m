#!/usr/bin/env wolframscript
(* Builds build/<Name>-<Version>.paclet from PacletInfo.wl, Kernel/, Legacy/ and Data/,
   then verifies the archive: it is extracted to a temporary directory and every context
   registered in PacletInfo.wl is loaded in a fresh kernel. Exits non-zero on failure.
   Usage: wolframscript -file Scripts/BuildPaclet.m *)

root = ParentDirectory[DirectoryName[$InputFileName]];
build = FileNameJoin[{root, "build"}];
staging = FileNameJoin[{build, "staging"}];
Quiet[DeleteDirectory[staging, DeleteContents -> True]];
CreateDirectory[staging, CreateIntermediateDirectories -> True];

CopyFile[FileNameJoin[{root, "PacletInfo.wl"}], FileNameJoin[{staging, "PacletInfo.wl"}]];
Scan[CopyDirectory[FileNameJoin[{root, #}], FileNameJoin[{staging, #}]] &,
  {"Kernel", "Legacy", "Data"}];
(* Old notebooks kept for reference are not part of the paclet. *)
Quiet[DeleteDirectory[FileNameJoin[{staging, "Legacy", "notebooks"}], DeleteContents -> True]];

archive = CreatePacletArchive[staging, build];
If[!StringQ[archive] || !FileExistsQ[archive], Print["Archive creation failed"]; Exit[1]];
Print["Built ", archive, " (", Round[FileByteCount[archive]/10.^6, 0.1], " MB)"];

(* Verify: extract the archive and load each context in its own kernel. *)
check = CreateDirectory[];
extracted = ExtractPacletArchive[archive, check];
contexts = Flatten[Lookup[Rest /@ Cases[PacletObject[File[extracted]]["Extensions"],
    {"Kernel", ___}], "Context"]];
scriptCommand = Module[{ws = Select[{
      FileNameJoin[{$InstallationDirectory, "SystemFiles", "Kernel", "Binaries", $SystemID,
        "wolframscript"}],
      FileNameJoin[{$InstallationDirectory, "Executables", "wolframscript"}]}, FileExistsQ]},
  If[ws =!= {}, {First[ws], "-file"}, {"wolframscript", "-file"}]];
shellQuote[s_String] := "'" <> StringReplace[s, "'" -> "'\\''"] <> "'";
loadCheck = FileNameJoin[{root, "Scripts", "LoadCheck.m"}];
(* A context passes when it loads without messages; shadowing messages from legacy
   packages that still export duplicated names (issue #9) are reported but tolerated. *)
failures = Select[contexts, Function[ctx,
  With[{out = ReadList["!" <> StringRiffle[shellQuote /@ Join[scriptCommand, {loadCheck, extracted, ctx}]] <> " 2>&1", String]},
    With[{line = SelectFirst[out, StringStartsQ[#, "LOADED"] &, "no result"]},
      Print[ctx, ": ", line];
      !StringStartsQ[line, "LOADED True 0 "]]]]];
(* Data assets must be found inside the installed layout. *)
dataOut = ReadList["!" <> StringRiffle[shellQuote /@ Join[scriptCommand,
    {loadCheck, extracted, "GraphTools`", "Length /@ {TreeGraphs[8], ConnectedSimpleGraphs[5], RootedTreeGraphs[6]}"}]] <> " 2>&1", String];
dataLine = SelectFirst[dataOut, StringStartsQ[#, "VALUE"] &, "no result"];
Print["GraphTools data from archive: ", dataLine];
If[dataLine =!= "VALUE {23, 21, 20}", AppendTo[failures, "GraphTools data"]];
DeleteDirectory[check, DeleteContents -> True];
If[failures =!= {}, Print["Contexts failing to load cleanly: ", failures]; Exit[1]];
Print["All ", Length[contexts], " contexts load from the archive."];
Exit[0];
