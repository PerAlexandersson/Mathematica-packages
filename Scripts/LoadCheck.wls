#!/usr/bin/env wolframscript
(* Usage: wolframscript -file Scripts/LoadCheck.wls <paclet directory> <context> [expression]
   Loads the context from the given paclet directory in this fresh kernel and prints
   "LOADED <True|False> <messages> <shadowing messages>"; an optional expression is then
   evaluated and printed as "VALUE <InputForm>". *)

{dir, context} = $ScriptCommandLine[[2 ;; 3]];
PacletDirectoryLoad[dir];
messages = {};
Internal`HandlerBlock[{"Message", If[Last[#], AppendTo[messages, Extract[#, 1, HoldForm]]] &},
  Needs[context]];
shadowing = Count[messages, HoldForm[Message[MessageName[_, "shdw"], ___]]];
Print["LOADED ", MemberQ[$Packages, context], " ", Length[messages] - shadowing, " ", shadowing];
If[Length[$ScriptCommandLine] >= 4,
  Print["VALUE ", ToString[ToExpression[$ScriptCommandLine[[4]]], InputForm]]];
