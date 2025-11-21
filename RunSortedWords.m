

Clear["RunSortedWords`*"];

BeginPackage["RunSortedWords`"];

Needs["CombinatoricTools`"];


SetPartitionToRSP;
RunSortedPermutations;


Begin["Private`"];

SetPartitionToRSP[sp_List] := Prepend[1 + Join @@ (RotateLeft /@ sp), 1];

RunSortedPermutations[1]:={{1}};
RunSortedPermutations[n_Integer]:=RunSortedPermutations[n]=(SetPartitionToRSP/@SetPartitions[n-1]);


End[(* End private *)];

EndPackage[];


