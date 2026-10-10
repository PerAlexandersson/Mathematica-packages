#!/usr/bin/env wolframscript
(* Runs one test file in this (fresh) kernel and prints a machine-readable summary.
   Usage: RunTestFile.m file [seconds]; a test that takes longer than the limit (default
   120 seconds) is aborted and fails, so a runaway test cannot hang the suite. *)

file = $ScriptCommandLine[[2]];
limit = If[Length[$ScriptCommandLine] >= 3, ToExpression[$ScriptCommandLine[[3]]], 120];
SetOptions[VerificationTest, TimeConstraint -> limit];
report = TestReport[file];

Scan[
  Print["FAILED: ", #["TestID"], "  (", #["Outcome"], ")"] &,
  Values[Join @@ Values[report["TestsFailed"]]]
];
Print[
  "RESULT ", report["TestsSucceededCount"], " ", report["TestsFailedCount"]
];
Exit[0];
