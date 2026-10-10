(* Every example is run in a separate Wolfram process. The shell pipe mirrors
   Tests/RunTests.wls and appends an exit marker so a timeout or kernel failure is
   distinguished from a clean run. *)
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

exampleFiles = Sort[FileNames["*.m", FileNameJoin[{testRoot, "Examples"}]]];

Scan[
  Function[file,
    VerificationTest[
      Module[{command, lines, marker},
        command = StringRiffle[shellQuote /@
            Join[{"timeout", "120"}, scriptCommand, {file}], " "] <>
          " 2>&1; printf '__EXAMPLE_EXIT__%s\\n' \"$?\"";
        lines = ReadList["!" <> command, String];
        marker = Select[lines, StringStartsQ[#, "__EXAMPLE_EXIT__"] &];
        Length[marker] == 1 && Last[marker] === "__EXAMPLE_EXIT__0" &&
          Select[lines, StringContainsQ[#, "::"] &] === {}
      ],
      True,
      TestID -> "Examples-" <> FileBaseName[file] <> "-runs-cleanly"
    ]
  ],
  exampleFiles
];
