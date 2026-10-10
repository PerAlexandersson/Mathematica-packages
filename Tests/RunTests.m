#!/usr/bin/env wolframscript
(* Runs every Tests/*Tests.m file in a separate kernel, so that results cannot depend
   on which packages earlier test files happened to load. *)

testDirectory = DirectoryName[$InputFileName];
runner = FileNameJoin[{testDirectory, "RunTestFile.m"}];

(* Locate a command that runs a script in a fresh kernel. *)
scriptCommand = Module[{ws},
  ws = Select[{
      FileNameJoin[{$InstallationDirectory, "SystemFiles", "Kernel", "Binaries",
        $SystemID, "wolframscript"}],
      FileNameJoin[{$InstallationDirectory, "Executables", "wolframscript"}]},
    FileExistsQ];
  If[ws =!= {},
    {First[ws], "-file"},
    {First[Select[{FileNameJoin[{$InstallationDirectory, "Executables", "WolframKernel"}],
      FileNameJoin[{$InstallationDirectory, "Contents", "MacOS", "WolframKernel"}]},
      FileExistsQ], "wolframscript"], "-script"}
  ]
];

shellQuote[s_String] := "'" <> StringReplace[s, "'" -> "'\\''"] <> "'";

(* Test files are named <Name>Tests.m; this runner (RunTests.m) must not run itself. *)
files = Select[FileNames["*Tests.m", testDirectory],
  !MemberQ[{"RunTests.m", "RunTestFile.m"}, FileNameTake[#]] &];

results = Table[
  Module[{proc, lines, summary},
    (* RunProcess is unavailable in some sandboxes, so use a shell pipe. *)
    proc = ReadList["!" <> StringRiffle[shellQuote /@ Join[scriptCommand, {runner, file}]]
      <> " 2>&1", String];
    lines = If[ListQ[proc], proc, {}];
    summary = Select[lines, StringStartsQ[#, "RESULT "] &];
    Scan[Print[FileNameTake[file], ": ", #] &,
      Select[lines, StringStartsQ[#, "FAILED: "] &]];
    If[summary === {},
      Print[FileNameTake[file], ": no result (kernel failed)"];
      Scan[Print, lines];
      {0, 1},
      With[{counts = ToExpression /@ Rest[StringSplit[Last[summary]]]},
        If[Total[counts] == 0,
          Print[FileNameTake[file], ": contains no tests"]; {0, 1},
          counts]]
    ]
  ],
  {file, files}];

{succeeded, failed} = Total[results];
Print["Tests succeeded: ", succeeded, "; failed: ", failed];
Exit[If[failed == 0 && Length[files] > 0, 0, 1]];
