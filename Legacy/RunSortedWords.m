(* ::Package:: *)

(* Deprecated: RunSortedPermutations now lives in CombinatoricTools (issue #9).
   This package is kept so that existing code calling Needs["RunSortedWords`"] works. *)

BeginPackage["RunSortedWords`", {"CombinatoricTools`"}];

SetPartitionToRSP;

Begin["`Private`"];

SetPartitionToRSP::usage = "SetPartitionToRSP[sp] is a deprecated name for SetPartitionToRunSortedPermutation[sp] from CombinatoricTools.";
SetPartitionToRSP[sp_List] := SetPartitionToRunSortedPermutation[sp];

End[(* End private *)];

EndPackage[];
