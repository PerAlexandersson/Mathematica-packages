(* ::Package:: *)

(* MathKernel -script file.m *)


Clear["GTPatterns`*"];

BeginPackage["GTPatterns`",{"CombinatoricTools`","NewTableaux`"}];


GTPattern;
GTPatternForm;

GTShape;
GTPatterns;
RowFlags;


Begin["Private`"];

GTPattern::usage = "GTPattern[data] represents a GT-pattern as a list of rows (partitions) ordered bottom to top.
GTPattern[YoungTableau[t]] converts a (skew) SSYT to its GT-pattern.
YoungTableau[gtp] converts a GT-pattern back to a SSYT.
gtp[r,c] accesses the entry at row r, column c (1-indexed; row 1 = bottom = inner shape).";

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
	{nrows,ncols},
	
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



GTShape::usage = "GTShape[gtp] returns {lam, mu, w} for a GT-pattern gtp, where lam is the outer shape, mu is the inner shape (empty list for straight shapes), and w is the weight vector.";

(* Return skew shape and weight. *)
GTShape[GTPattern[gtp_]]:=With[
	{w = Differences[ Tr/@gtp ]},
	{DeleteCases[Last@gtp,0],DeleteCases[First@gtp,0],w}
];

GTPatterns::usage = "GTPatterns[lam,mu,w] returns a list of all GT-patterns with outer shape lam, inner shape mu (default {}), and weight vector w (default {}), corresponding to SSYT of skew shape lam/mu with content w.
Optional argument cylindricShift (default Infinity) restricts to cylindric GT-patterns with the given column shift.
Option RowFlags->{{a1,b1},{a2,b2},...} constrains entries in SSYT row r to the range [ar,br] (default {1,Infinity} = no constraint).";
RowFlags::usage = "RowFlags is an option for GTPatterns that restricts the entries in tableau row r to a specified inclusive interval {ar,br}.";

Options[GTPatterns] = {RowFlags -> Automatic};

GTPatterns[lam_List, opts:OptionsPattern[]] :=
	GTPatterns[lam, {}, {}, Infinity, opts];
GTPatterns[lam_List, mu_List, opts:OptionsPattern[]] :=
	GTPatterns[lam, mu, {}, Infinity, opts];
GTPatterns[lam_List, mu_List, w_List, opts:OptionsPattern[]] :=
	GTPatterns[lam, mu, w, Infinity, opts];
GTPatterns[lam_List, mu_List, w_List,
		cylindricShift:(Infinity | _Integer), opts:OptionsPattern[]] :=
	With[{lamMu = PadRight[{lam, mu}]},
		Module[{flags, n = Length[lamMu[[1]]]},
			flags = With[{rf = OptionValue[RowFlags]},
				If[rf === Automatic,
					ConstantArray[{1, Infinity}, n],
					PadRight[rf, n, {1, Infinity}]
				]
			];
			Which[
				!(Tr[lamMu[[1]]] - Tr[lamMu[[2]]] == Tr[w]), {},
				lamMu[[1]] === {},
					If[lamMu[[2]] === {} && AllTrue[w, IntegerQ[#] && # >= 0 &],
						{GTPattern[ConstantArray[{}, Length[w] + 1]]},
						{}],
				(* Check that first and last row are compatible w shift. *)
				lamMu[[1,1]] > lamMu[[1,-1]] + cylindricShift, {},
				lamMu[[2,1]] > lamMu[[2,-1]] + cylindricShift, {},
				True, quickGTPatterns[Sequence @@ lamMu, w, cylindricShift, flags]
			]
		]
	];

(*
The code uses the ideas in
https://mathematica.stackexchange.com/questions/42745/generating-gelfand-tsetlin-patterns
Vertices are {levelIndex, partition} pairs, so zero entries in w are handled correctly:
equal consecutive shapes are distinguished by their level index.
*)
quickGTPatterns[l_List, mu_List, w_List, cylindricShift_:Infinity, rowFlags_:{}] := Module[
	{rowSums, levels, mid, startVert, endVert, m, n, flags, graphEdges, gg, lvlA, lvlB},

	m = Length[w];
	n = Length[l];

	(* Empty weight: the only valid GT-pattern is the trivial one with lam = mu. *)
	If[m == 0, Return[If[l === mu, {GTPattern[{l}]}, {}]]];

	(* Pad flags to n rows; {1, Infinity} imposes no constraint. *)
	flags = PadRight[rowFlags, n, {1, Infinity}];

	(* Create all partitions 'between' lambda and mu, in layers. *)
	rowSums = Accumulate@w;
	mid = Table[
		{k, #} & /@ IntegerPartitions[Tr[mu] + rowSums[[k]], {n}, Range[0, Max@l]],
		{k, Length[rowSums] - 1}];

	(* First and last level consist of a single vertex. *)
	levels = Join[{{{0, mu}}}, mid, {{{m, l}}}];

	(* Apply row-flag constraints before the cylindric transform.
	   Flag [ar,br] on SSYT row r forces all entries in that row into [ar,br]:
	     - below level ar, shape[[r]] must still equal mu[[r]];
	     - at and above level br, shape[[r]] must equal l[[r]].
	   Checking every level also enforces impossible endpoint flags. *)
	levels = MapIndexed[
		Function[{levelShapes, idx},
			With[{k = idx[[1]] - 1},
				Select[levelShapes, Function[{vert},
					With[{shape = vert[[2]]},
						And @@ Table[
							With[{ar = flags[[r, 1]], br = flags[[r, 2]]},
								And[
									If[k < ar, shape[[r]] == mu[[r]], True],
									If[k >= br, shape[[r]] == l[[r]], True]
								]
							],
							{r, n}
						]
					]
				]]
			]
		],
		levels
	];
	If[AnyTrue[levels, # === {} &], Return[{}]];

	If[cylindricShift < Infinity,
		levels = Map[
		(*	We add cylindricShift to last entry in each row and put first.
				This might make it not a partition anymore, so only keep partitions.
		*)
			With[{newFirst = #[[2,-1]] + cylindricShift},
				If[newFirst < #[[2,1]],
					Nothing,
					{#[[1]], Prepend[#[[2]], newFirst]}
				]
			] &,
			levels, {2}]
	];
	If[AnyTrue[levels, # === {} &], Return[{}]];

	(* Extract start and end vertex for pathfinding *)
	startVert = levels[[1,1]];
	endVert = levels[[-1,1]];

	graphEdges = Flatten@Table[
		lvlA = levels[[k]];
		lvlB = levels[[k+1]];
		Outer[
			With[{a = #1[[2]], b = #2[[2]]},
				If[Min[b-a] >= 0 && Min[a[[;;-2]] - b[[2;;]]] >= 0,
					DirectedEdge[#1, #2],
					Nothing
				]
			] &,
			lvlA, lvlB, 1
		],
		{k, Range[Length@levels - 1]}];

	(* The big graph, where every path from startVert to endVert represents a GT-pattern. *)
	gg = Graph@graphEdges;

	(* Ensure that the start and end vertices are indeed in the graph. *)
	If[!VertexQ[gg, startVert] || !VertexQ[gg, endVert],
		{},
		(* Here, we remove the artificial first (cylindric) entry if it was added before. *)
		GTPattern[
			With[{gt = Last /@ #},
				If[cylindricShift < Infinity, Rest /@ gt, gt]
			]] & /@ FindPath[gg, startVert, endVert, Infinity, All]
	]
];


End[]; (*End private*)

EndPackage[];
