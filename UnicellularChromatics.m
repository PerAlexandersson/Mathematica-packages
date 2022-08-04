(* ::Package:: *)


(* MathKernel -script file.m *)


BeginPackage["UnicellularChromatics`",{"SymmetricFunctions`","CombinatoricsUtil`","CatalanObjects`"}];

Unprotect["`*"]
ClearAll["`*"]


AreaLists;
Circular;
Width;
UnitIntervalEdges;
GraphColoringAscents;
StrictEdges;
WeakEdges;
UnicellularLLTSymmetric;
UnicellularLLTSymmetricSchur;
ChromaticSymmetric;


(*****************************)

GraphOrientations;
GraphAcyclicOrientations;

OrientationSinks;

(*****************************)

SchroederWordToArea;
SchroederWordStrictEdges;
SchroederLLTSymmetric;
SchroederPlot;
SchroederOrientations;
SchroederAcyclicOrientations
SchroederColorings;
SchroederColoringAscents;

(*
RecursionVertices; (* This is temporary - should not be in the library after paper is done. *)
*)

BounceList;
BounceEndpoint;
EdgesHRVRule;


GraphColoringMonochromaticEdges;

Begin["`Private`"];


AreaLists::usage = "AreaLists[size, All->False, Circular->True, Width->-1] returns all area lists of size n.";
Circular::usage  = "Option for AreaLists";
Width::usage = "Option for AreaLists";


Options[AreaLists] = {All -> False, Circular -> True, Width->-1};
AreaLists[n_Integer, opts:OptionsPattern[]] := AreaLists[n, opts] = 
	Module[{rec, isOkQ, gData = {}, isMinimal, w = -1 },
		
		(* Max-entry *)
		w = OptionValue[Width];
		If[w == -1, w=n];
		
		rec[gList_List, n] := AppendTo[gData, gList];
		rec[gList_List, i_Integer] :=
		Do[
			rec[ Append[gList, k], i + 1]
		, {k, Max[Last[gList] - 1, 0], w - 1}];
	
	(* Init recursion *)
	Do[rec[{k}, 1], {k, 0, w - 1}];
	
	(* The number of entries in dGata seems to be given by A274969 *)
	
	(* It has to be compatible when wrapped around as well! *)
	(* This extra condition reduces it to A194460 *)
	
	isOkQ[lst_List] := (lst[[1]] >= lst[[-1]] - 1) && (OptionValue[Circular]===True || Last[lst]==0);
	
	(* The cardinalities is not in OEIS. *)
	isMinimal[lst_List] := (lst === Last@Sort[Table[RotateLeft[lst, k], {k, 0, n - 1}]]);
	
	Reverse/@If[ Not@OptionValue[All],
		Select[gData, isOkQ[#] && isMinimal[#] &]
	,
		Select[gData, isOkQ ]
	]
];



UnitIntervalEdges::usage = "UnitIntervalEdges[area] returns the edges of the unit interval graph.";
UnitIntervalEdges[area_List] :=With[{n = Length@area},
	Join @@ Table[
		{Mod[k - i - 1, n] + 1, k}
		,{k, n}, {i, area[[k]]}]
];


GraphColoringAscents[edges_List, col_List] := Sum[Boole[col[[e[[1]]]] < col[[e[[2]]]] ], {e, edges}];

GraphColoringMonochromaticEdges[edges_List, col_List] := Sum[Boole[col[[e[[1]]]] == col[[e[[2]]]] ], {e, edges}];


StrictEdges::usage = "StrictEdges is an option for UnicellularLLTSymmetric.";

Options[UnicellularLLTSymmetric] = {StrictEdges -> {}};
UnicellularLLTSymmetric[attacking_List, n_Integer, q_: 1, opts:OptionsPattern[]] := 
	UnicellularLLTSymmetric[attacking, n, q, opts] = Module[{c,lam,perms},
	
	(* We have a list of strict edges *)
	strict = OptionValue[StrictEdges];
	perms = Select[
		Permutations@Range@n
		,
		And@@Table[
			#[[e[[1]]]] < #[[e[[2]]]],{e,strict}] &
		];
	
	(* Here we use the F-expansion formula *)
	Sum[
		With[{lam = DescentSetToComposition[DescentSet@Reverse@Ordering@c,n]},
			q^GraphColoringAscents[attacking, c] SchurSymmetric[lam]
		]
	,{c, perms}]
];


UnicellularLLTSymmetric::usage = "UnicellularLLTSymmetric[area,q] returns the 
unicellular LLT polynomial associated with given area sequence.";
UnicellularLLTSymmetric[area:{_Integer ..}, q_: 1, opts:OptionsPattern[]] :=
UnicellularLLTSymmetric[UnitIntervalEdges@area, Length@area,q,opts];


UnicellularLLTSymmetricSchur[area_List, q_: 1, ss_] := Module[{c,colorings,lam,n,attacking},
	attacking = UnitIntervalEdges@area;
	n = Length@area;
	Sum[
		With[{lam = DescentSetToComposition[DescentSet@Reverse@Ordering@c,n]},
			q^GraphColoringAscents[attacking, c] ss[lam]
		]
	,{c, Permutations@Range@n}]/. ss[c_] :> ( If[#2!=0, ss[#1] #2, 0]& @@ (CompositionSlinky@c) )
];


(* Convention *)
ChromaticSymmetric[{},q_:1] := 1;

ChromaticSymmetric[attacking_List, q_:1, opts:OptionsPattern[]] := ChromaticSymmetric[attacking, Max@attacking,q,opts];

ChromaticSymmetric[attacking_List, n_Integer, q_, opts:OptionsPattern[]] := 
	ChromaticSymmetric[attacking, n, q, opts] = Module[{c,colorings,lam},
	(* TODO: This is inefficient! Use F-expansion instead! *)
	Sum[
		(* All colorings with lam as weight *)
		colorings = Permutations@(Join @@ MapIndexed[ ConstantArray[#2[[1]], #1 ] &, lam]);
		colorings = Select[colorings,
			GraphColoringMonochromaticEdges[attacking,#]==0&];
		MonomialSymmetric[lam]*
		If[q === 1,
			Length[colorings]
			,
			Sum[q^GraphColoringAscents[attacking, c], {c, colorings}]
		]
		
	,{lam, IntegerPartitions[n] }]
];

ChromaticSymmetric::usage = "ChromaticSymmetric[area,q] returns the chromatic symmetric polynomial associated with given area sequence.";
ChromaticSymmetric[area:{_Integer ..}, q_: 1, opts:OptionsPattern[]] :=
ChromaticSymmetric[UnitIntervalEdges@area, Length@area,q,opts];





(***************************************************)

Options[GraphOrientations] = {StrictEdges -> {}, WeakEdges -> {}};
GraphOrientations[edges_List, opts:OptionsPattern[]] := Module[
	{isConnectedQ, multiEdgedQ, n = Max@edges, orients, nEdges = Length@edges, 
		strict, weak, weakIndicator,strictIndicator},
	
	strict = OptionValue[StrictEdges];
	weak = OptionValue[WeakEdges];
	
	(* There is a 1 at the strict edges. *)
	strictIndicator = Table[ Boole@MemberQ[strict,e], {e,edges}];
	weakIndicator = Table[ Boole@MemberQ[weak,e], {e,edges}];
	
	orients = Tuples[{0, 1}, nEdges];
	If[Length@strict>0,
		orients = Select[ orients, (#.strictIndicator)==Tr[strictIndicator] & ];
	];
	If[Length@weak>0,
		orients = Select[ orients, ((1-#).weakIndicator)==Tr[weakIndicator] & ];
	];
	
	orients = Table[
		Table[
			If[ or[[i]] == 1, 
				edges[[i]],
				Reverse@edges[[i]]]
			,{i, nEdges}]
	, {or, orients}];
	
	orients
];


GraphAcyclicOrientations::usage = "GraphAcyclicOrientations[edges] returns the set of acyclic orientations.";
Options[GraphAcyclicOrientations] = {StrictEdges -> {}};
GraphAcyclicOrientations[edges_List, opts:OptionsPattern[]] := GraphAcyclicOrientations[edges,opts] = Module[{n = Max[edges, 0], orients, strict, strictIndices},
	
	(* Try all colorings with different colors. Such colorings 
		can only result in acyclic orientations. *)
	
	orients = Union@Table[
			(
			If[ Less @@ col[[#]], 
				#, 
				Reverse@#]
			) & /@ edges
	, {col, Permutations@Range@n}];
	
	strict = OptionValue[StrictEdges];
	strictIndices = Select[ Range[Length@edges], MemberQ[strict, edges[[#]]] & ];
	
	If[Length@strict>0,
		orients = Select[ orients, And@@Table[ edges[[i]]==#[[i]], {i,strictIndices }] & ];
	];
	
	orients
];

OrientationSinks::usage = "OrientationSinks[edges,n] return list of sinks.";
OrientationSinks[edges_List, n_: 0] := Module[
   {verts = Union[Join @@ edges]},
   (* Sinks are vertices with no outgoing edges. *)
   Complement[Join[verts, Range[1, n]], First /@ edges]
];

(***************************************************)

(* Function which takes a string, --000++,
and convert to (area, strictEdges) pair.
The diagonal steps do not contribute to area.
*)

SchroederWordToArea[""] := {{}, {}};
SchroederWordToArea[word_String] := 
  SchroederWordToArea[Characters[word]];
SchroederWordToArea[word_List] := 
  SchroederWordToArea[word] = Module[{aRest, strictRest, numSeq,
     coArea, area, strictRows, strictEdges},
    
    (* Minus is up. *)
    numSeq = Replace[word, {
       ("-" | "n") -> {1, 0},
       ("+" | "e") -> {0, 1},
       ("0" | "d") :> Sequence @@ {{0, 1}, {1, 0}}}, 1];
    
    strictRows = Join @@ Position[
       Replace[
        word, {("-" | "n") -> "0", ("+" | "e") -> 
          Nothing, ("0" | "d") :> 1}, 1]
       , 1];
    
    coArea = Last /@ (Last /@ GatherBy[
         Accumulate[Prepend[numSeq, {0, 0}]]
         , First]);
    (* This is now the area. *)
    
    area = Most[Range[Length[coArea]] - 1 - coArea];
    
    strictEdges = Table[
      {r - area[[r]] - 1, r}
      , {r, strictRows}];
    
    {area, strictEdges}
];

SchroederWordStrictEdges[w_]:=SchroederWordToArea[w][[2]];

SchroederLLTSymmetric[word_, q_] := 
  SchroederLLTSymmetric[word, q] = Module[{area, strict},
    {area, strict} = SchroederWordToArea[word];
    UnicellularLLTSymmetric[area, q, StrictEdges -> strict]
    ];

SchroederPlot[word_] := Module[{aa, strict},
   {aa, strict} = SchroederWordToArea[word];
   AreaListPlot[aa, Circular -> False, Labels -> Join[
      # -> "\[Rule]" & /@ strict,
      {#, #} -> # & /@ Range[Length@aa]
      ]
    ]];

SchroederOrientations[word_] := 
  With[{data = SchroederWordToArea[word]},
   GraphOrientations[
	UnitIntervalEdges@data[[1]], StrictEdges -> data[[2]]]
];

SchroederAcyclicOrientations[word_] := 
  With[{data = SchroederWordToArea[word]},
   GraphAcyclicOrientations[
	UnitIntervalEdges@data[[1]], StrictEdges -> data[[2]]]
];


Options[SchroederColorings] = {Partition->True};
SchroederColorings[word_, ncols_: 0,opts:OptionsPattern[SchroederColorings]] := Module[
	{area, strict, n, cols,colorings},
   {area, strict} = SchroederWordToArea[word];
   n = Length@area;
   cols = If[ncols == 0, n, ncols];

   If[OptionValue[Partition]===True,
		Join@@Table[
		colorings = Permutations@(Join @@ MapIndexed[ ConstantArray[#2[[1]], #1 ] &, lam]);
		Select[colorings
		,
		And @@ Table[
		#[[ s[[1]] ]] < #[[ s[[2]] ]]
		, {s, strict}]
		&]
		,{lam, IntegerPartitions[n] }]
	
	,
		
		Select[Tuples[Range@cols, n]
	    ,
		And @@ Table[
			#[[ s[[1]] ]] < #[[ s[[2]] ]]
		, {s, strict}]
		&]
	]
];


SchroederPermutationColorings[word_, ncols_: 0] := 
  Module[{area, strict, n, cols},
   {area, strict} = SchroederWordToArea[word];
   n = Length@area;
   cols = If[ncols == 0, n, ncols];

   Select[Permutations@Range@n
    ,
    And @@ Table[
       #[[ s[[1]] ]] < #[[ s[[2]] ]]
       , {s, strict}]
     &]
   ];

SchroederColoringAscents[word_, col_] := 
  With[{data = SchroederWordToArea[word]},
   GraphColoringAscents[UnitIntervalEdges[data[[1]]], col]
];




(***************************************************)


(* Returns y', where we have started a bounce path from row indexed by y  *)
BounceEndpoint[word_, y_Integer] := Module[{aa, strict, strictRows, x, xlist},
   {aa, strict} = SchroederWordToArea[word];
   strictRows = Last /@ strict;
   x = y - 1;
   Which[
    y == 1, y,
    aa[[y]] != aa[[x]], y,
    ! MemberQ[strictRows, y] || ! MemberQ[strictRows, x], y,
    True,
    BounceEndpoint[word, y - aa[[y]] - 1]
    ]
];

(* The x in the (x,y)-pairs of the bounce path. *)
BounceList[word_, y_Integer] := Module[{aa, strict, strictRows, x},
   {aa, strict} = SchroederWordToArea[word];
   strictRows = Last /@ strict;
   x = y - 1;
   Which[
    y == 1, {x},
 
    aa[[y]] != aa[[x]], {x},
 
    !MemberQ[strictRows, y] || ! MemberQ[strictRows, x], {x},
 
    True,
    Prepend[ BounceList[word, y - aa[[y]] - 1], x ]
    ]
];

(*
RecursionVertices[word_, z_Integer, type_: 2] := Module[
	{aa, strict, strictQ, n, x, y, w, validQ, bounceList},
	
	{aa, strict} = SchroederWordToArea[word];
	n = Length@aa;
	
	(* Return true if row is a diagonal step *)
	
	strictQ[row_] := MemberQ[Last /@ strict, row];

	(* Do the bounce *)
	bounceList = BounceList[word, z - aa[[z]]];
	x = Last[bounceList];
	y = x + 1;
	
	Which[
		type == 2, w = y - aa[[y]];
		type == 3, w = y - aa[[y]] - 1;
		type == 4, w = y - aa[[y]] - 1;
		];

	validQ = And[
		(* Row z must be strict in all 3 cases.*)
		strictQ[z],
		(* Make sure there is at least one east-step going from row z to z+1 *)
		(z == n || 
		aa[[z + 1]] < aa[[z]] || 
		And[aa[[z + 1]] == aa[[z]], ! strictQ[z] ])
		,
		(y > x > w >= 1)
		,
		Or[
		(type == 2 && 
			aa[[y]] == 1 + aa[[x]] && ! strictQ[x] && ! strictQ[y]) 
		,
		(type == 3 && aa[[y]] == aa[[x]] && ! strictQ[x] && 
			strictQ[y]) 
		,
		(type == 4 && aa[[y]] == 1 + aa[[x]] && 
			strictQ[x] && ! strictQ[y])
		]
		];
	
	(*
	{validQ, {w, x, y, z}}
	*)
	
	(* (x,y) is now first diagonal touch pair *)
	(*
	{validQ, {w, bounceList[[1]], bounceList[[1]]+1, z}}
	*)
	
	{validQ, {z, Sequence@@bounceList, w}}
	
];
*)

EdgesHRVRule[edges_List] := EdgesHRVRule[edges] = Module[
    {lrvSteps, stepLength, verts, ascEdges},
    verts = Union[Join @@ edges];
    ascEdges = Select[edges, Less @@ # &];
    
    lrvSteps[k_Integer] := lrvSteps[k] = Module[{out, nxt},
       out = Last /@ Select[ascEdges, First[#] == k &];
       (*Recursive definition.*)
       
       If[Length[out] == 0, {}, 
        Union@Join[out, Join @@ (lrvSteps /@ out)]]
       ];
    Join[# -> Prepend[lrvSteps[#], #] & /@ verts, {i_Integer :> {i}}]
];


End[(* End private *)];
EndPackage[];
