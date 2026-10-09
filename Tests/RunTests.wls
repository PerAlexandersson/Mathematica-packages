#!/usr/bin/env wolframscript

testDirectory = DirectoryName[$InputFileName];
report = TestReport[FileNames["*.wlt", testDirectory]];

Print[
  "Tests succeeded: ", report["TestsSucceededCount"],
  "; failed: ", report["TestsFailedCount"]
];
Exit[If[TrueQ[report["ReportSucceeded"]], 0, 1]];
