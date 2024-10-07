(* ::Package:: *)

(* MathKernel -script file.m *)


Clear["GTPatterns`*"];

BeginPackage["GTPatterns`",{"CombinatoricTools`","NewTableaux`"}];


GTPattern;
GTPatternForm;

GTShape;
GTPatterns;


Begin["Private`"];

GTPattern::usage = "GTPattern[data] represents a GT-pattern. 
GTPattern[YoungTableau[t]] returns the GT-pattern associated with the tableau.";

GTPattern[YoungTableau[tabIn_]]:=Module[{n,tab},
	tab = tabIn /. None-> 0;
	n = Max[tab];
	GTPattern@PadRight[
		Table[
			Count[ row , b_/;b <= k ]
		, {k, 0, n}
		, {row, tab}]
		]
];

GTPattern/:YoungTableau[GTPattern[gtp_]]:=
With[{
	(* Prepend first row, to accomodate for skew shape.*)
	
	entryCounts = Prepend[Differences[gtp], gtp[[1]]]}
	,
	Join @@@ Transpose@Table[
			ConstantArray[If[k - 1 == 0, None, k - 1]
				, #] & /@ entryCounts[[k]]
			, {k, Length@entryCounts}] // YoungTableau
];


(* Element access. *)
GTPattern[gtp_][r_Integer, c_Integer]  := With[
	{rr=Length@gtp, cc=Length[gtp[[1]]]},
	Which[ 
		1<=r<=rr && 1<=c<=cc, PadRight[gtp][[r,c]], (* Note: we pad here in case of triangle shape.*)
		r<=0, 0,
		c>cc, 0,
		r>rr, Message[Part::partw, r, gtp]
	]
];

(* Access of elements, and compatible with Dimensions *)
GTPattern[gtp_][{r_Integer, c_Integer}] := GTPattern[gtp][r,c];
GTPattern/:Dimensions[GTPattern[gtp_]] := Dimensions[gtp];

(* Convert between coordinate systems. *)
(* Internal function. *)
GTIndexToGrahphicsCoordinates[{r_Integer, c_Integer}] := {2 c - r, r};

(* Helper function for extracting text elements for GT-Patterns *)
GTPatternTextLabels[gtp_GTPattern] := Module[
	{nrows,ncols,theLabels},
	
	nrows = Length[gtp[[1]]]; 
	ncols = Length@First@gtp[[1]];
	
	(* Extract text coordinates. *)
	If[ Total[First@gtp[[1]]] == 0,
		(* Non-skew version *)
		Join@@Table[ GTIndexToGrahphicsCoordinates[{r, c}] -> gtp[r, c], {r, 2, nrows}, {c, r-1}]
		,
		Join@@Table[ GTIndexToGrahphicsCoordinates[{r, c}] -> gtp[r, c], {r, nrows}, {c, ncols}]
	]
];


(* Returns a Graphics representation of a GTpattern. *)
GTPatternForm::usage="GTPatternForm[gtp] returns the graphical representation of the GT-pattern.";
GTPatternForm[gtp_GTPattern] := With[
	{
		rc = Dimensions[gtp],
		theLabels = GTPatternTextLabels[gtp]
	},
		Graphics[
			{Black,theLabels /.  Rule[coord_,val_] :> Text[val, coord] },
			BaseStyle -> {14, FontFamily -> "Computer Modern"},
			ImageSize -> 12*{rc[[2]] + rc[[1]],  rc[[1]] } 
		]
];

(* Formatting rule *)
GTPattern/:Format[GTPattern[gtp_]]:=GTPatternForm[GTPattern[gtp]];



GTPattern/:TeXForm[GTPattern[gtlists_], opts:OptionsPattern[]]:=Module[
	{gtlistsStrings,riffled,add,texTable,texString, lb = "\n"},
	
	gtlistsStrings = Reverse[gtlists] /. n_Integer :> ToString[n];
	
	riffled = Riffle[#," "]&/@gtlistsStrings;
	
	add = Length@riffled;
	texTable = Table[ Join[ ConstantArray["", r-1],  riffled[[r]] ], {r, add } ];
	riffled = Riffle[#, " & "] & /@ texTable;
	
	texString = (Append[#, "\\\\"<>lb]& /@ riffled);
	texString = StringJoin@@#& /@ texString;
	texString = StringJoin@@texString;
	texString = StringJoin["\\begin{matrix}"<>lb,texString,"\\end{matrix}"];
	
	texString
];



(* Return skew shape and weight. *)
GTShape[GTPattern[gtp_]]:=With[
	{w = Differences[ Tr/@gtp ]},
	{DeleteCases[Last@gtp,0],DeleteCases[First@gtp,0],w}
];

(* TODO: add row-flag condition also. *)

GTPatterns::usage = "GTPatterns[lam,mu,w] returns a list of all GT-patterns with these characteristics.";

GTPatterns[lam_List, mu_List:{}, w_List:{},cylindricShift_:Infinity]:=With[
	{lamMu = PadRight[{lam,mu}]},
	
	Which[
		!(Tr[lamMu[[1]]]-Tr[lamMu[[2]]]==Tr[w]), {},
		
		(* Check that first and last row are compatible w shift. *)
		lamMu[[1,1]] > lamMu[[1,-1]] + cylindricShift, {},
		lamMu[[2,1]] > lamMu[[2,-1]] + cylindricShift, {},
		True, quickGTPatterns[Sequence@@lamMu, w, cylindricShift]
	]
];

(* This only works when all parts in w are positive! *)
(* 
The code uses the ideas in 
https://mathematica.stackexchange.com/questions/42745/generating-gelfand-tsetlin-patterns
*)
quickGTPatterns[l_List,mu_List,w_List, cylindricShift_:Infinity] := Module[
	{rowSums, levels, g,  mid, startVert,endVert },
		
		
		(* Create all partitions 'between' lambda and mu,  in layers. *)
			rowSums = Accumulate@w;
		mid = Table[
			(* Level + integer partition *)
			{k, #} & /@ IntegerPartitions[Tr[mu]+rowSums[[k]], {Length@l}, Range[0, Max@l]]
			,{k, Length[rowSums]-1}];
		
		(* First and last level consist of a single vertex. *)
		levels = Join[{{{0,mu}}}, mid, {{{Length[rowSums],l}}}];
		
		If[cylindricShift<Infinity,
			levels = Map[
			(*	We add cylindricShift to last entry in each row and put first.
					This might make it not a partition anymore, so only keep partitions.
			*)
				With[{newFirst = #[[2,-1]] + cylindricShift},
					If[newFirst<#[[2,1]], 
						Nothing
						,
						{#[[1]], Prepend[#[[2]],newFirst] }
					]
				]
				&
				,levels,{2}]
		];
		
		(* Extract start and end vertex for pathfinding *)
		startVert = levels[[1,1]];
		endVert = levels[[-1,1]];
		
		
		graphEdges = Flatten@Table[
				lvlA = levels[[k]];
				lvlB = levels[[k+1]];
				Outer[
					With[{a = #1[[2]], b = #2[[2]]},
							If[Min[b-a] >= 0 && Min[a[[;; -2]] - b[[2 ;;]]] >= 0,
								DirectedEdge[#1, #2],
								Nothing
							]
					]&
					,
					lvlA, lvlB, 1
				]
		, {k, Range[Length@levels - 1]}];
		
		(* The big graph, where every path from startVert, endVert represents a GT-pattern. *)
		gg = Graph@graphEdges;
		
		(* Ensure that the start and end vertices are indeed in the graph. *)
		If[!VertexQ[gg,startVert] || !VertexQ[gg,endVert],
			{}
		,
		
			(* Make kind of a graph in Young's lattice, and find paths. *)
			(* Here, we remove the artificial first (cylindric) entry if it was added before. *)
			GTPattern[ 
				With[{gt=Last/@#},
					If[cylindricShift<Infinity,
						Rest/@gt,
						gt
					]
				]] & /@ FindPath[gg, startVert, endVert, Infinity, All]
		]
];


End[]; (*End private*)

EndPackage[];
