#!/usr/bin/env wolframscript
(* Runs one test file in this (fresh) kernel and prints a machine-readable summary. *)

file = $ScriptCommandLine[[2]];
report = TestReport[file];

Scan[
  Print["FAILED: ", #["TestID"], "  (", #["Outcome"], ")"] &,
  Values[Join @@ Values[report["TestsFailed"]]]
];
Print[
  "RESULT ", report["TestsSucceededCount"], " ", report["TestsFailedCount"]
];
Exit[0];
