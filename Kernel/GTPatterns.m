(* ::Package:: *)

(* MathKernel -script file.m *)


Clear["GTPatterns`*"];

BeginPackage["GTPatterns`",{"CombinatoricTools`","NewTableaux`"}];


GTPattern;
GTPatternForm;
ShapeTriplets;
BoxCountMatrix;

(* Extensions ported from OldYoungTableaux (GitHub issue #51). *)
BZPattern;
BZPatterns;
BZPlus;
GTPlus;
GTMonomial;
GogPatterns;
MagogPatterns;
GTTiles;
GTSnakes;
GTPartition;
EnableSkew;
GTPatternTikz;
LatticePathForm;
LatticePathTikz;
ContainingFaceDimension;
TilingMatrix;
WeightRange;
KostkaRange;
UpperBoundKostkaDegree;
GTEhrhartPolynomial;

GTShape;
GTPatterns;
RowFlags;


Begin["`Private`"];

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
GTIndexToGrahphicsCoordinates[{r_, c_}] := {2 c - r, r};

(* Helper function for extracting text elements for GT-Patterns *)
GTPatternTextLabels[gtp_GTPattern, mode_:Automatic] := Module[
	{rows = gtp[[1]], h, skew, useSkew},
	h = Length[rows];
	If[rows === {}, Return[{}]];
	skew = Total[First[rows]] =!= 0;
	useSkew = If[mode === Automatic, skew, mode];
	If[useSkew,
		Flatten[Table[
			GTIndexToGrahphicsCoordinates[{r, c}] -> gtp[r, c],
			{r, h}, {c, Length[rows[[r]]]}], 1],
		Flatten[Table[
			GTIndexToGrahphicsCoordinates[{r, c}] -> gtp[r, c],
			{r, 2, h}, {c, r - 1}], 1]
	]
];


(* Returns a Graphics representation of a GTpattern. *)
GTPatternForm::usage="GTPatternForm[gtp] returns the graphical representation of the GT-pattern.";

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
GTPatterns::rowflags = "The value of RowFlags must be Automatic or a list of pairs {a,b}, with integer a >= 1 and integer or Infinity b.";
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
		Module[{flags, n = Length[lamMu[[1]]], rf = OptionValue[RowFlags]},
			If[rf =!= Automatic &&
				(!MatchQ[rf, {{_Integer, (_Integer | Infinity)}...}] ||
					!AllTrue[rf, #[[1]] >= 1 &]),
				Message[GTPatterns::rowflags, rf];
				Return[{}]
			];
			flags = If[rf === Automatic,
					ConstantArray[{1, Infinity}, n],
					PadRight[rf, n, {{1, Infinity}}]
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

(* Public option values are strings: Tiles, Snakes, FreeTiles and
   ShadedTiles are not introduced as new symbols in the System context. *)
GTPartition::usage = "GTPartition is an option for GTPatternForm and GTPatternTikz. Its values are \"Tiles\", \"Snakes\", \"FreeTiles\", \"ShadedTiles\", or None.";
EnableSkew::usage = "EnableSkew is an option for ShapeTriplets, GTTiles, GTPatternForm, and GTPatternTikz controlling whether skew cells are included.";
BZPattern::usage = "BZPattern[data] represents a Berenstein-Zelevinsky pattern.";
BZPatterns::usage = "BZPatterns[lam, mu, nu] returns BZ-patterns counted by the Littlewood-Richardson coefficient c^lam_{mu,nu}.";
BZPlus::usage = "BZPlus[b1,b2,...] adds BZ-patterns entrywise.";
GTPlus::usage = "GTPlus[g1,g2,...] adds Gelfand-Tsetlin patterns entrywise, padding smaller patterns.";
GTMonomial::usage = "GTMonomial[g,x] returns the monomial in x associated with the weight of g.";
GogPatterns::usage = "GogPatterns[n] returns the Gog patterns of size n. GogPatterns[n,k] uses k columns.";
MagogPatterns::usage = "MagogPatterns[n] returns the Magog patterns of size n. MagogPatterns[n,k] uses k columns.";
GTTiles::usage = "GTTiles[g] returns {freeTiles, fixedTiles} for a GT-pattern.";
GTSnakes::usage = "GTSnakes[g] returns the equal-entry snakes of a GT-pattern.";
GTPatternTikz::usage = "GTPatternTikz[g] returns TikZ code for a GT-pattern.";
LatticePathForm::usage = "LatticePathForm[g] or LatticePathForm[tab] returns graphics for the non-intersecting lattice paths.";
LatticePathTikz::usage = "LatticePathTikz[g] or LatticePathTikz[tab] returns TikZ code for the non-intersecting lattice paths.";
ContainingFaceDimension::usage = "ContainingFaceDimension[g] returns the dimension of the face of the GT polytope containing g.";
TilingMatrix::usage = "TilingMatrix[g] returns the tiling matrix of a GT-pattern.";
WeightRange::usage = "WeightRange is an option for ShapeTriplets specifying the minimum and maximum number of parts in a weight.";
KostkaRange::usage = "KostkaRange is an option for ShapeTriplets specifying the minimum and maximum allowed Kostka multiplicity.";
UpperBoundKostkaDegree::usage = "UpperBoundKostkaDegree[lam,mu,w] returns an upper bound for the degree of the stretched Kostka polynomial.";
GTEhrhartPolynomial::usage = "GTEhrhartPolynomial[lam,mu,w,k] counts GT-patterns of (k lam,k mu,k w) when k is an integer; a symbolic k returns the interpolating Ehrhart polynomial.";

ShapeTriplets::usage = "ShapeTriplets[lambda, options] returns {lambda, mu, w} triples for skew shapes lambda/mu and weights w. EnableSkew, WeightRange, and KostkaRange control skew shapes, weight sizes, and Kostka multiplicities.";
BoxCountMatrix::usage = "BoxCountMatrix[GTPattern[rows]] returns the matrix whose entry in row i and column j counts entries j in tableau row i, with GT rows ordered bottom to top.";

generateSkewShapes[lambda_List] := generateSkewShapes[lambda] = If[lambda === {},
   {{}},
   Join @@ Table[
      With[{newLambda = Rest[Min[k, #] & /@ lambda]},
         Prepend[#, k] & /@ generateSkewShapes[newLambda]
      ],
      {k, 0, First[lambda]}]
];

Options[ShapeTriplets] = {
   EnableSkew -> True, WeightRange -> {1, Infinity}, KostkaRange -> {0, Infinity}
};
ShapeTriplets[lambda_List, OptionsPattern[]] := Module[
   {pairs, minBox, maxBox, minKostka, maxKostka, triplets},
   pairs = If[TrueQ[OptionValue[EnableSkew]],
      With[{muList = If[lambda === {}, {{}},
         generateSkewShapes[Max[0, #] & /@ (Most[lambda] - 1)]]},
         ({lambda, #} & /@ muList)],
      {{lambda, ConstantArray[0, Length[lambda]]}}
   ];
   {minBox, maxBox} = OptionValue[WeightRange];
   triplets = Join @@ (
      Function[pair,
         With[{n = Total[pair[[1]]] - Total[pair[[2]]]},
            ({pair[[1]], pair[[2]], #} & /@
               IntegerPartitions[n, {minBox, maxBox}])]
      ] /@ pairs
   );
   {minKostka, maxKostka} = OptionValue[KostkaRange];
   If[minKostka > 0 || maxKostka < Infinity,
      triplets = Select[triplets,
         minKostka <= Length[Apply[GTPatterns, #]] <= maxKostka &]
   ];
   triplets
];

BoxCountMatrix[GTPattern[gtp_List]] := Module[
   {gtpList, maxBox},
   If[gtp === {}, Return[{}]];
   maxBox = Length[gtp] - 1;
   gtpList = PadRight[#, Length[First[gtp]]] & /@ gtp;
   Transpose@Table[gtpList[[j + 1]] - gtpList[[j]], {j, maxBox}]
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
	flags = PadRight[rowFlags, n, {{1, Infinity}}];

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


(* ---------------------------------------------------------------------- *)
(* Extensions ported from OldYoungTableaux.  The old package stores GT rows
   top to bottom; all code below uses the public bottom-to-top convention. *)

gtNormalizePartitions[parts_List] := Module[{clean, n},
	clean = DeleteCases[#, 0] & /@ parts;
	n = If[clean === {}, 0, Max[Length /@ clean]];
	PadRight[#, n] & /@ clean
];

gtSkewQ[GTPattern[rows_]] := Total[First[rows]] > 0;

GTMonomial[g_GTPattern, x_] := With[{w = Last[GTShape[g]]},
	Times @@ ((x /@ Range[Length[w]])^w)
];

gtPad[GTPattern[rows_], nr_Integer, nc_Integer] :=
	GTPattern[PadRight[#, nc] & /@
		Join[ConstantArray[First[rows], nr - Length[rows]], rows]];

GTPlus[g_GTPattern] := g;
GTPlus[gs__GTPattern] := Module[{list = {gs}, dims, nr, nc},
	dims = Transpose[Dimensions /@ list];
	{nr, nc} = Max /@ dims;
	GTPattern[Fold[MapThread[Plus, {#1, #2}, 2] &, First[#], Rest[#]] &[
		(gtPad[#, nr, nc][[1]] & /@ list)]]
];

BZPatterns[lamIn_List, muIn_List, nuIn_List] /;
		Total[lamIn] - Total[muIn] - Total[nuIn] =!= 0 := {};
BZPatterns[lamIn_List, muIn_List, nuIn_List] := Module[
	{lam, mu, nu, n, x, conds, pattern, horizontal, downRight, downLeft,
		sol},
	If[!PartitionLessEqualQ[muIn, lamIn] ||
		!PartitionLessEqualQ[nuIn, lamIn], Return[{}]];
	{lam, mu, nu} = Abs[Differences[Reverse[#]]] & /@
		gtNormalizePartitions[{Reverse[lamIn], muIn, nuIn}];
	n = Length[lam];
	pattern = Table[x[r, c], {r, n + 2}, {c, r}];
	horizontal = Table[Sum[x[r, c], {c, k}], {r, 2, n + 1}, {k, r}];
	conds[1, 1] = Thread[Last /@ horizontal == lam];
	conds[1, 2] = Thread[Flatten[horizontal] >= 0];
	downRight = Table[
		Sum[x[n + 3 - c, r - c + 1], {c, k}], {r, 2, n + 1}, {k, r}];
	conds[2, 1] = Thread[Last /@ downRight == mu];
	conds[2, 2] = Thread[Flatten[downRight] >= 0];
	downLeft = Table[
		Sum[x[n + 2 + c - r, n + 3 - r], {c, k}],
		{r, 2, n + 1}, {k, r}];
	conds[3, 1] = Thread[Last /@ downLeft == nu];
	conds[3, 2] = Thread[Flatten[downLeft] >= 0];
	sol = Reduce[
		And @@ Join[Flatten[Table[conds[i, 1], {i, 1, 3}]],
			Flatten[Table[conds[i, 2], {i, 1, 3}]],
			{x[1, 1] == 0, x[n + 2, 1] == 0, x[n + 2, n + 2] == 0}],
			Flatten[pattern], Integers];
	If[sol === False, {}, BZPattern[pattern] /. List[ToRules[sol]]]
];

BZPlus[b_BZPattern] := b;
BZPlus[bs__BZPattern] := BZPattern[
		Fold[MapThread[Plus, {#1, #2}, 1] &, First[#], Rest[#]] &[
			(List @@ # & /@ {bs})]
	];

GogPatterns[n_Integer] := GogPatterns[n, n];
GogPatterns[n_Integer, k_Integer] := Module[
	{w = k, h = n + 1, x, pattern, boundary, special, inequalities, sol},
	pattern = Table[x[r, c], {r, h, 1, -1}, {c, w}];
	boundary = And @@ Table[x[h, c] == n - c + 1 && x[1, c] == 0,
		{c, w}];
	special = And @@ Table[x[r, w] >= r - w, {r, w + 1, h}];
	inequalities = And @@ Flatten[Table[
		If[r < h, x[r + 1, c] >= x[r, c], True] &&
		If[r < h && c < w, x[r, c] >= x[r + 1, c + 1], True] &&
		If[c < r - 1 && c < w, x[r, c] > x[r, c + 1], True] &&
		If[r > 1 && c < r, x[r, c] >= 1, True],
		{r, h}, {c, w}]];
	sol = Reduce[boundary && special && inequalities,
		Flatten[pattern], Integers];
	If[sol === False, {}, (GTPattern[Reverse[#]] &) /@
		(pattern /. List[ToRules[sol]])]
];

MagogPatterns[n_Integer] := MagogPatterns[n, n];
MagogPatterns[n_Integer, k_Integer] := Module[
	{w = k, h = n + 1, x, pattern, boundary, special, inequalities, sol},
	pattern = Table[x[r, c], {r, h, 1, -1}, {c, w}];
	boundary = And @@ Table[x[1, c] == 0, {c, w}];
	special = And @@ Table[x[r, 1] <= r - 1, {r, 2, h}];
	inequalities = And @@ Flatten[Table[
		If[r < h, x[r + 1, c] >= x[r, c], True] &&
		If[r < h && c < w, x[r, c] >= x[r + 1, c + 1], True] &&
		If[r > 1 && c < r, x[r, c] >= 1, True],
		{r, h}, {c, w}]];
	sol = Reduce[boundary && special && inequalities,
		Flatten[pattern], Integers];
	If[sol === False, {}, (GTPattern[Reverse[#]] &) /@
		(pattern /. List[ToRules[sol]])]
];

(* The tiling and display helpers use the same six-neighbour geometry as the
   legacy implementation. *)
gtConnectedPolygon[component_List] := Module[
	{dirs, lines, segments, merged, polygon},
	dirs[0] = {-1, -1}; dirs[1] = {-1, 0};
	dirs[2] = {1, 1}; dirs[3] = {1, 0};
	dirs[n_] := dirs[Mod[n, 4]];
	lines[pt_] := Table[
		If[!MemberQ[component, pt + dirs[d]],
			{pt + (dirs[d] + dirs[d - 1])/2,
				pt + (dirs[d] + dirs[d + 1])/2}, Sequence @@ {}], {d, 0, 3}];
	segments = If[component === {}, {}, Join @@ (lines /@ component)];
	merged = ReplaceRepeated[segments,
		{{b___, a_}, rest___, {a_, c___}, rest2___} :>
			{rest, rest2, {b, a, c}}];
	polygon = If[Length[merged] === 1 &&
			Length[First[merged]] >= 4 &&
			First[First[merged]] === Last[First[merged]],
			Most[First[merged]],
			$Failed];
	If[polygon === $Failed,
		GTIndexToGrahphicsCoordinates /@ component,
		GTIndexToGrahphicsCoordinates /@ polygon]
];

GTSnakes[GTPattern[rows_]] := Module[
	{h, w, inside, extract, taken = Unique["taken$"], points, current,
		snakes = {}, work, r, c, skew},
	skew = gtSkewQ[GTPattern[rows]]; work = Reverse[rows];
	{h, w} = Dimensions[work];
	If[skew,
		inside[r_Integer, c_Integer] := 1 <= r <= h && 1 <= c <= w,
		h--; inside[r_Integer, c_Integer] := 1 <= r <= h && 1 <= c <= h - r + 1
	];
	extract[grid_, {r_Integer, c_Integer}] := Which[
		inside[r + 1, c - 1] && grid[[r + 1, c - 1]] =!= taken &&
			grid[[r + 1, c - 1]] == grid[[r, c]],
			Prepend[extract[grid, {r + 1, c - 1}], {r, c}],
		inside[r + 1, c] && grid[[r + 1, c]] =!= taken &&
			grid[[r + 1, c]] == grid[[r, c]],
			Prepend[extract[grid, {r + 1, c}], {r, c}],
		True, {{r, c}}
	];
	points = Select[Flatten[Table[{r, c}, {r, h}, {c, w}], 1], inside @@ # &];
	While[points =!= {},
		current = extract[work, First[points]];
		work = ReplacePart[work, Thread[current -> taken]];
		AppendTo[snakes, current]; points = Complement[points, current]
	];
	(({Length[rows] + 1 - First[#], Last[#]} & /@ #) & /@ snakes)
];

GTTiles::skew = "The GT-pattern is a skew pattern.";
Options[GTTiles] = {EnableSkew -> Automatic};
gtFloodFillTile[grid_, {sr_, sc_}, inside_] := Module[
	{seen = {}, todo = {{sr, sc}}, r, c, check},
	check[rr_, cc_, dr_, dc_] := inside[rr + dr, cc + dc] &&
		grid[[rr + dr, cc + dc]] == grid[[rr, cc]] &&
		!MemberQ[seen, {rr + dr, cc + dc}] &&
		!MemberQ[todo, {rr + dr, cc + dc}];
	While[todo =!= {},
		{r, c} = Last[todo]; todo = Most[todo]; AppendTo[seen, {r, c}];
		If[check[r, c, 1, -1], AppendTo[todo, {r + 1, c - 1}]];
		If[check[r, c, 1, 0], AppendTo[todo, {r + 1, c}]];
		If[check[r, c, -1, 1], AppendTo[todo, {r - 1, c + 1}]];
		If[check[r, c, -1, 0], AppendTo[todo, {r - 1, c}]]
	];
	seen
];
GTTiles[GTPattern[rowsIn_], OptionsPattern[]] := Module[
	{rows = rowsIn, h, w, skew, inside, extract, points, tile, tiles = {},
		free, r, c},
	skew = gtSkewQ[GTPattern[rowsIn]]; rows = Reverse[rows];
	{h, w} = Dimensions[rows];
	If[OptionValue[EnableSkew] === False && skew,
		Message[GTTiles::skew]; Return[{}]];
	If[!skew && OptionValue[EnableSkew] =!= True,
		h--; rows = PadRight[#, h + 1] & /@ rows;
		inside = Function[{r, c}, 1 <= r <= h && 1 <= c <= h - r + 1],
		inside = Function[{r, c}, 1 <= r <= h && 1 <= c <= w]
	];
	points = Select[Flatten[Table[{r, c}, {r, h}, {c, w}], 1], inside @@ # &];
	While[points =!= {}, tile = gtFloodFillTile[rows, First[points], inside];
		AppendTo[tiles, tile]; points = Complement[points, tile]];
	free = Select[tiles, Min[First /@ #] > 1 && Max[First /@ #] < h &];
	Map[
		Function[tileList,
			Map[Function[tile,
				({Length[rowsIn] + 1 - First[#], Last[#]} & /@ tile)], tileList]],
		{free, Complement[tiles, free]}]
];

gtPatternTiles[g_GTPattern, skew_: True] :=
	Map[gtConnectedPolygon, GTTiles[g, EnableSkew -> skew], {2}];

GTPatternForm::skew = "The GT-pattern is a skew pattern.";
Options[GTPatternForm] = {EnableSkew -> Automatic, GTPartition -> None};
GTPatternForm[GTPattern[rows_], OptionsPattern[]] := Module[
	{gtp = GTPattern[rows], rc, skew, useSkew, labels, tilePolys, part},
	rc = Dimensions[gtp];
	skew = gtSkewQ[gtp];
	useSkew = If[OptionValue[EnableSkew] === Automatic, skew,
		OptionValue[EnableSkew]];
	If[OptionValue[EnableSkew] === False && skew, Message[GTPatternForm::skew]];
	labels = GTPatternTextLabels[gtp, useSkew];
	part = OptionValue[GTPartition]; tilePolys = gtPatternTiles[gtp, useSkew];
	Graphics[{
		Which[
			part === "ShadedTiles", {{LightGray, EdgeForm[Black], Polygon /@ Last[tilePolys]},
				{Black, Line /@ First[tilePolys]}},
			part === "Tiles", {Line /@ Join @@ tilePolys},
			part === "FreeTiles", {Line /@ First[tilePolys]},
			part === "Snakes", {Line /@ (gtConnectedPolygon /@ GTSnakes[gtp])},
			True, {}],
		{Black, labels /. Rule[p_, v_] :> Text[v, p]}},
		BaseStyle -> {14, FontFamily -> "Computer Modern"},
		ImageSize -> 12*{rc[[2]] + rc[[1]], rc[[1]]}
]];

Options[GTPatternTikz] = {EnableSkew -> Automatic, GTPartition -> None};
GTPatternTikz::skew = "The GT-pattern is a skew pattern.";
GTPatternTikz[GTPattern[rows_], OptionsPattern[]] := Module[
	{skew = gtSkewQ[GTPattern[rows]], useSkew, labels, tilePolys, part,
		polygon, labelText},
	useSkew = If[OptionValue[EnableSkew] === Automatic, skew,
		OptionValue[EnableSkew]];
	If[OptionValue[EnableSkew] === False && skew, Message[GTPatternTikz::skew]];
	polygon[pts_, edge_: "black", fill_: None] :=
		If[pts === {}, "", If[fill === None,
			"\\draw[" <> edge <> "] ",
			"\\filldraw[color=" <> edge <> ",fill=" <> fill <> "] "] ] <>
			StringRiffle[("(" <> ToString[InputForm[#[[1]]]] <> "," <>
				ToString[InputForm[#[[2]]]] <> ")" &) /@ pts, "--"] <> "--cycle;\n";
	labelText[p_, v_] := "\\node at (" <> ToString[InputForm[p[[1]]]] <> "," <>
		ToString[InputForm[p[[2]]]] <> ") {$" <> ToString[TeXForm[v]] <> "$};\n";
	labels = GTPatternTextLabels[GTPattern[rows], useSkew];
	part = OptionValue[GTPartition]; tilePolys = gtPatternTiles[GTPattern[rows], useSkew];
	StringJoin["\\begin{tikzpicture}[scale=0.4]\n",
		Which[
			part === "ShadedTiles", StringJoin @@ Join[polygon[#, "black"] & /@ First[tilePolys],
				polygon[#, "black", "lightgray"] & /@ Last[tilePolys]],
			part === "Tiles", StringJoin @@ (polygon[#, "black"] & /@ Join @@ tilePolys),
			part === "FreeTiles", StringJoin @@ (polygon[#, "black"] & /@ First[tilePolys]),
			part === "Snakes", StringJoin @@ (polygon[#, "black"] & /@
				(gtConnectedPolygon /@ GTSnakes[GTPattern[rows]])),
			True, ""],
		StringJoin @@ (labelText @@ # & /@ labels), "\\end{tikzpicture}\n"]
];

TilingMatrix[g_GTPattern] := Module[{free = First[GTTiles[g]], data},
	If[free === {}, Return[{{}}]];
	data = Join @@ MapIndexed[
		(With[{idx = #2[[1]]},
			({#1 - 1, idx} -> #2) & @@@ Tally[First /@ #1]]) &,
		free];
	Normal[SparseArray[data]]
];
ContainingFaceDimension[g_GTPattern] := Module[{matrix = TilingMatrix[g]},
	Length[First[matrix]] - MatrixRank[matrix]
];

UpperBoundKostkaDegree[lamIn_List, muIn_List, weight_List] := Module[
	{triangle, lam, mu, tt1, tt2, h, free, flex},
	triangle[tower_, height_, dir_] := If[height > 0,
		With[{part = Last[tower]}, triangle[Append[tower,
			Insert[Table[If[part[[i]] == part[[i + 1]], part[[i]], -1],
				{i, Length[part] - 1}], -1, dir]], height - 1, dir]], tower];
	{lam, mu} = gtNormalizePartitions[{lamIn, muIn}]; h = 1 + Length[weight];
	If[Total[mu] == 0,
		If[!PartitionDominatesQ[Reverse@Sort[weight], lam], Return[-Infinity]];
		If[lam === DeleteCases[weight, 0], Return[0]]];
	tt1 = triangle[{lam}, h - 1, -1]; tt2 = Reverse[triangle[{mu}, h - 1, 1]];
	free = MapThread[If[Max[#1, #2] >= 0, 0, 1] &, {tt1, tt2}, 2];
	flex = Select[free, Total[#] >= 2 &]; Total[Flatten[flex]] - Length[flex]
];

gtKostkaCount[lam_, mu_, weight_, k_Integer] :=
	Length[GTPatterns[k # & /@ lam, k # & /@ mu, k # & /@ weight]];
GTEhrhartPolynomial[lam_List, mu_List, weight_List, k_Integer?NonNegative] :=
	gtKostkaCount[lam, mu, weight, k];
GTEhrhartPolynomial[lam_List, mu_List, weight_List, variable_] := Module[
	{degree, values},
	degree = UpperBoundKostkaDegree[lam, mu, weight];
	If[degree === -Infinity, Return[0]];
	degree = Max[0, degree];
	values = Table[{i, gtKostkaCount[lam, mu, weight, i]}, {i, degree + 1}];
	InterpolatingPolynomial[values, variable]
];

(* Non-intersecting lattice paths. *)
gtLatticeLines[YoungTableau[table_]] := Module[
	{ncols, columns, boxes, skew, start, maxBox, xcoords},
	ncols = If[table === {}, 0, Max[Length /@ table]];
	columns = Table[Table[If[c <= Length[row], row[[c]], None], {row, table}],
		{c, ncols}];
	boxes = (Select[#, IntegerQ] &) /@ columns;
	skew = (Count[#, None] &) /@ columns;
	start = 2 (Range[Length[boxes]] - skew);
	maxBox = If[boxes === {} || Flatten[boxes] === {}, 0, Max[0, Max[Flatten[boxes]]]];
	xcoords = (Accumulate[Normal[SparseArray[Thread[# + 1 -> -1],
		{maxBox + 1}, 1]]] &) /@ boxes;
	MapThread[Transpose[{#1 + #2, 1 - Range[maxBox + 1]}] &, {xcoords, start}]
];

LatticePathTikz[g_GTPattern] := LatticePathTikz[YoungTableau[g]];
LatticePathTikz[tab_YoungTableau] := Module[{lines = gtLatticeLines[tab], one, ymin, xmin, xmax},
	If[lines === {}, Return["\\begin{tikzpicture}[thick,scale=0.3]\n\\end{tikzpicture}\n"]];
	one[line_] := "\\path[draw] " <> StringRiffle[
		("(" <> ToString[#[[1]]] <> "," <> ToString[#[[2]]] <> ")" &) /@ line,
		" -- "] <> ";\n";
	ymin = Min[Last /@ First[lines]]; xmin = Min[First /@ First[lines]];
	xmax = Max[First /@ Last[lines]];
	"\\begin{tikzpicture}[thick,scale=0.3]\n" <>
		"\\draw[help lines] (" <> ToString[xmin] <> ", " <> ToString[ymin] <>
		") grid (" <> ToString[xmax] <> ",0);\n" <>
		StringJoin @@ (one /@ lines) <> "\\end{tikzpicture}\n"
];

Options[LatticePathForm] = {RowLabels -> True};
LatticePathForm[g_GTPattern, opts : OptionsPattern[]] :=
	LatticePathForm[YoungTableau[g], opts];
LatticePathForm[tab_YoungTableau, OptionsPattern[]] := Module[
	{lines = gtLatticeLines[tab], minX = 0, maxX = 0, maxBox = 0, labels = {}},
	If[lines =!= {}, minX = Min[First /@ First[lines]];
		maxX = Max[First /@ Last[lines]]; maxBox = Length[First[lines]] - 1;
		If[OptionValue[RowLabels], labels = Table[
			Text[Style[ToString[r], 12], {minX - 0.5, -r + 0.5}], {r, maxBox}]]];
	Graphics[{{Thick, Black, Line[lines]}, labels}, AspectRatio -> Automatic,
		Axes -> False, PlotRange -> {{minX - 1.2, maxX + 0.2},
			{-maxBox - 0.2, 0.2}}, GridLines -> {Range[minX, maxX],
			Range[0, -(maxBox + 1), -1]}, ImageSize -> Automatic]
];

End[]; (*End private*)

EndPackage[];
