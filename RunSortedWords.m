

Clear["RunSortedWords`*"];

BeginPackage["RunSortedWords`"];

Needs["CombinatoricTools`"];


SetPartitionToRSP;
RunSortedPermutations;


Begin["`Private`"];

SetPartitionToRSP::usage = "SetPartitionToRSP[sp] maps a set partition sp of {1, ..., n-1} to a run-sorted permutation of {1, ..., n}: each block is rotated left, all entries are increased by 1, and 1 is prepended.";
RunSortedPermutations::usage = "RunSortedPermutations[n] returns all run-sorted permutations of {1, ..., n}, that is, permutations whose maximal increasing runs have increasing first entries; there are BellB[n-1] of them for n >= 1.";

SetPartitionToRSP[sp_List] := Prepend[1 + Join @@ (RotateLeft /@ sp), 1];

RunSortedPermutations[1]:={{1}};
RunSortedPermutations[0]:={};
RunSortedPermutations[n_Integer]:=RunSortedPermutations[n]=(SetPartitionToRSP/@SetPartitions[n-1]);


End[(* End private *)];

EndPackage[];

