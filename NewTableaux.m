(* ::Package:: *)

(* MathKernel -script file.m *)



(* ::TODO:: *)
(*
	--- Add some flag $French to display tableaux in French.
	
*)



Clear["NewTableaux`*"];
BeginPackage["NewTableaux`"];

Needs["CombinatoricsUtil`"];

SYTSize;
SYTMax;
SYTReadingWord;
SYTMajorIndex;
SYTCharge;
SYTDualMajorIndex;
SYTDescents;
SYTDescentSet;
SuperStandardTableau;
SYTPromotion;
SSYTKPromotion;
SSYTKPromotionInverse;
SYTStandardize;

YoungTableau;
YoungTableauWeight;
YoungTableauSize;
YoungTableauShape;
StandardYoungTableaux;
SemiStandardYoungTableaux;
YoungTableauForm;
CylindricTableaux;
PlanePartitions;

BorderStrips;
BorderStripTableaux;
BSTHeightVector;

TableauShortTeX;
YTableauTeX;
RowLatticePaths;
ColumnLatticePaths;

InvMajStatistic;

BiwordRSK;
BiwordRSKDual;

BinaryMatrixToBiword;

SYTEvacuation;
SYTEvacuationDual;

CrystalEi;
CrystalFi;
CrystalSi;



Begin["Private`"];

SYTSize::usage = "SYTSize[tab] returns the number of boxes in the tableau.";
SYTSize[syt_YoungTableau]:=Length@SYTReadingWord[syt];

SYTMax::usage = "MaxSize[tab] returns the maximum of all boxes in the tableau.";
SYTMax[syt_YoungTableau] := Max@SYTReadingWord[syt];


SYTReadingWord[YoungTableau[syt_]] := DeleteCases[Join @@ Reverse[syt], None];

SYTMajorIndex[syt_YoungTableau] := MajorIndex[Ordering[SYTReadingWord@syt]];


SYTCharge::usage = "SYTCharge[ssyt] returns the charge of a semistandard tableau, with partition weight.";
SYTCharge[ssyt_YoungTableau] := WordCharge@SYTReadingWord@ssyt;

(* Only works on SYTs ! *)
SYTDualMajorIndex[YoungTableau[syt_]] := With[{m=Max[syt]},
Sum[
 j Boole[Position[syt,j][[1,1]]>Position[syt,j+1][[1,1]]]
,{j,m-1}]
];


SYTDescents[syt_YoungTableau] := Descents[Ordering@SYTReadingWord@syt];

YoungTableauSize::usage = "YoungTableauSize[tab] returns the number of boxes in the shape. Does not count skew boxes.";
YoungTableauSize[syt_YoungTableau]:=Length[SYTReadingWord[syt]];

YoungTableauShape::usage = "YoungTableauShape[tab] returns the outer shape of the tableau. 
	YoungTableauShape[tab,m] returns the shape of formed by all entries <=m.";

YoungTableauShape[YoungTableau[syt_]]:=Length/@syt;

YoungTableauShape[syt_YoungTableau, v_Integer]:=YoungTableauShape[syt/.{i_Integer /; i>v :> Nothing}];

SYTDescentSet::usage = "SYTDescentSet[syt] returns the descent set of the standard Young tableau.";
SYTDescentSet[YoungTableau[syt_]] := Module[{lookUp, n = SYTMax@YoungTableau@syt },
	
    lookUp = Last /@ SortBy[
            Join @@ MapIndexed[If[IntegerQ[#1], #1 -> #2 ,Nothing] &, syt, {2}]
        ,
        First];
    
    Select[
        Range[n-1], 
        lookUp[[#,1]] < lookUp[[#+1,1]] &]
];



(* TODO---Ensure output is wrapped in this! *)
YoungTableau::usage = "YoungTableau[data] represents a Young tableau.";

(* Access elements. *)
(* Todo: Add checks. *)
YoungTableau[syt_][{r_Integer, c_Integer}] := syt[[r, c]];
YoungTableau[syt_][r_Integer, c_Integer]   := syt[[r, c]];


StandardYoungTableaux::usage = "StandardYoungTableaux[n], 
StandardYoungTableaux[lam] or StandardYoungTableaux[{lam,mu}] returns a list of SYTs";


(* This is maybe not the most efficient way *)
StandardYoungTableaux[n_Integer]:=Join@@Table[StandardYoungTableaux[ip], {ip, IntegerPartitions@n}];

StandardYoungTableaux[{sh__Integer}]:=StandardYoungTableaux[{{sh},{}}];

StandardYoungTableaux[{sh_List, {sh2___Integer, 0}}] := StandardYoungTableaux[{sh, {sh2}}];

StandardYoungTableaux[{{sh___Integer, 0}, sh2_List}] := StandardYoungTableaux[{{sh}, sh2}];

StandardYoungTableaux[{{sh__Integer}, {sh__Integer}}] := {
	YoungTableau@Table[
		ConstantArray[None, r]
	, {r, {sh}}]
};


StandardYoungTableaux[{{}, {}}]:={YoungTableau@{{}}};

StandardYoungTableaux[{sh_List, sh2_List}] := StandardYoungTableaux[{sh, sh2}] = Module[
	{rows = Length@sh, addIdx, sh22, n = Tr@sh - Tr@sh2},
	
	sh22 = PadRight[sh2, rows];
	
	(* Ensure lex-order *)
	addIdx = Select[Range[rows - 1, 1, -1], sh[[#]] > sh[[# + 1]] && sh22[[#]] < sh[[#]] &];
	If[sh22[[-1]] < sh[[-1]], AppendTo[addIdx, rows]];
	
	Join @@ Table[
		(* Generate all smaller tableaux, and add n in the row. *)
		(* Here, decide if we add to existing row or a new bottom row. *)
	
	Map[
		If[
			Length@# < r,
			Append[#, {n}],
			Insert[#, n, {{r, -1}}]
		] &, 
			StandardYoungTableaux[{ReplacePart[sh, r -> sh[[r]] - 1], sh2}]
		,{2}]
	, {r, addIdx}]
];


SuperStandardTableau::usage = "SuperStandardTableau[{lam,mu}] returns the SYT with 1,2,.. in first row and so on.";
SuperStandardTableau[{lam_List, mu_List}] := Module[
   {n = Tr[lam] - Tr[mu], l = Max[Length@lam, Length@mu], rows},
   rows = 
    PartitionList[Range[n], PadRight[lam, l] - PadRight[mu, l]];
   YoungTableau@MapThread[Join,
     {(ConstantArray[None, #] & /@ PadRight[mu, l]), rows}, 1]
   ];




(* TODO: THERE IS ALSO Shutzenberger's promotion *)

SYTPromotion::usage = "SYTPromotion[syt,[k]] performes the promotion operator k times. Default is 1 time.";
SYTPromotion[t_YoungTableau, k_Integer: 1] :=SYTPromotion[t,k]=Nest[SYTPromotion[#, 1] &, t, k];
SYTPromotion[t_YoungTableau, 1] :=SYTPromotion[t,1]=Module[{pos, n},
	(* Create lookup pos[i]={r,c} for entry i.*)
	MapIndexed[
		If[IntegerQ[#1],
		pos[#1] = #2] &
		, t, {3}];
	
	n = YoungTableauSize@t;
	
	(* Do the promotion, swap {i,i+1} in tableau if possible, i=1,...n-1 *)
	Do[
		(* If entries are in same row, or same column, do not swap. 
		Otherwise, swap*)
		Which[
		pos[i][[2]] == pos[i + 1][[2]], Null,
		pos[i][[3]] == pos[i + 1][[3]], Null,
		True, {pos[i], pos[i + 1]} = {pos[i + 1], pos[i]}
		]
		, {i, n - 1}];
	ReplacePart[t, pos[#] -> # & /@ Range[n]]
];


(* Inverse of the k-promotion defined in 
https://doi.org/10.1016/j.disc.2013.11.024
*)
SSYTKPromotion::usage = "SSYTKPromotion[ssyt,k] performs k-promotion.";
SSYTKPromotion[ssyt_YoungTableau, k_Integer] := Module[{lam, new},
   lam = YoungTableauShape[ssyt];
   new = BiwordRSK[DeleteCases[SYTReadingWord[ssyt], 1]][[1, 1]];
   
   YoungTableau@Table[
     With[{row = If[Length@new >= r, new[[r]], {}]},
      Join[row - 1, ConstantArray[k, lam[[r]] - Length[row]]]
      ], {r, Length@lam}]
   ];
UnitTest[SSYTKPromotion]:=(
SSYTKPromotion[
  YoungTableau[{{1, 1, 1, 4}, {2, 2, 3, 6}, {3, 4}}]
  , 7] ===
 YoungTableau[{{1, 1, 2, 3}, {2, 3, 5, 7}, {7, 7}}]);



SSYTKPromotionInverse::usage = "SSYTKPromotionInverse[ssyt,k] performs the inverse of k-promotion. This also works on skew shapes!";
SSYTKPromotionInverse[ssyt_YoungTableau, k_Integer] := Module[
		{val, localMove, allMove, DOT, out, dots},
			
		(* Wrapper for tableau access. *)
		val[tt_, {r_, c_}] := Which[
			r == 0, -1,
			c == 0, -1,
			tt[[r, c]] === None, -1,
			tt[[r, c]] === DOT, -1,
			IntegerQ[tt[[r, c]]], tt[[r, c]],
			True, -1
			];
		
		(* We have a list of dots to move.
		Performs a move and returns the new tab and new dotList.
			*)
		localMove[{tt_List, {}}] := {tt, {}};
		
		localMove[{tt_List, dotList_List}] := Module[{rc, up, lt, rest = Rest@dotList},
			
			(* Row.Col coord for a dot. *)
			
			rc = First@dotList;
			up = rc - {1, 0};
			lt = rc - {0, 1};
			
			Which[
				
				(* Cannot swap (is in a corner) *)
				val[tt, lt] == -1 && val[tt, up] == -1, 
					{tt, rest},
				
				
				(* Move dot up. *)
				1 <= val[tt, up] >= val[tt, lt],
					{ReplacePart[tt, {rc -> val[tt, up], up -> DOT}], 
						Prepend[rest, up]},
					
				(* Swap lt. *)
				1 <= val[tt, lt] > val[tt, up],
					{ReplacePart[tt, {rc -> val[tt, lt], lt -> DOT}], 
						Prepend[rest, lt]},
				
				
				(* This should not happen. *)
				True, 
					Print["Bad outcome"]; {tt, rest}
				]];
				
		out = If[
			(* If max entry is less than k, then increase all entries by 1. *)
						Max[ssyt] < k, ssyt[[1]],
						dots = SortBy[ Rest /@ Position[ssyt, k], Last];
						FixedPoint[localMove, {ssyt[[1]] /. k -> DOT, dots}][[1]] /. DOT -> 0
			];
		YoungTableau@Map[If[IntegerQ[#], # + 1, #] &, out, {2}]
];



SYTStandardize::usage = "SYTStandardize[ssyt] standardizes the (skew) ssyt.";
SYTStandardize[YoungTableau[ssyt_]] := Module[{wrd, repl},
   wrd = Join @@ MapIndexed[#2 -> #1 &, ssyt, {2}];
   wrd = Select[wrd, IntegerQ@Last[#] &];
   wrd = SortBy[wrd, {-#[[1, 1]] &, #[[1, 2]] &}];
   repl = MapThread[Rule, {(First /@ wrd), StandardizeList@(Last /@ wrd)}, 1];
   YoungTableau@ReplacePart[ssyt, repl]
];



SemiStandardYoungTableaux::usage="SemiStandardYoungTableaux[{lam,mu},w] returns
a list of all SSYT with given shape and weight.";

(* If only max-box is provided,
 then produce all with partition weight *)
SemiStandardYoungTableaux[{lam_List, mu_List}, max_Integer]:=Module[{weights},
	
	weights = IntegerPartitions[Tr[lam]-Tr[mu], max ];
	
	Join@@Table[
		SemiStandardYoungTableaux[{lam,mu},w]
		,{w,weights}]
];

(* The default max-entry ensures that all SYTs of the shape appears *)
SemiStandardYoungTableaux[{lam_List, mu_List}]:=SemiStandardYoungTableaux[{lam,mu},Tr[lam]-Tr[mu]];

(* Weight must match up, otherwise empty set *)
SemiStandardYoungTableaux[{lambdaIn_List, muIn_List}, w_List]/;(Tr[lambdaIn]-Tr[muIn]-Tr[w]!=0):={};

SemiStandardYoungTableaux[{lambdaIn_List, muIn_List}, w_List] := Module[{isEdgeQ,
	partitionLevels, directedEdges, mid,lam,mu,wAcc,ssytPaths,q},
	
	lam = lambdaIn;
	mu = PadRight[muIn,Length@lam];
	
	wAcc = Accumulate@w;
	
	mid = Table[
		IntegerPartitions[Tr@mu + wi, {Length@lam}, Range[0, Max@lam]]
	,{wi,Most@wAcc}];
	
	partitionLevels = Join[{{mu}}, mid, {{lam}}];
	
	(* If partitions interlace, with p above q in GT-pattern *)
	isEdgeQ[p_List,q_List]:=And[
		Min[p-q]>=0 && Min[ q[[;;-2]] - p[[2;;]] ]>=0
	];
	
	(* We add lvl to partition also, to allow for weight being 0 *)
	directedEdges = Join@@Table[
		Join@@Outer[
			If[ isEdgeQ[#2,#1], 
				DirectedEdge[{#1,lvl-1}, {#2,lvl}],
				Nothing
			]&
		,
		partitionLevels[[lvl-1]], partitionLevels[[lvl]], 1]
	,{lvl,2,Length[partitionLevels]}];

	
	g = Graph[directedEdges];
	
	ssytPaths = If[ Or[
		!MemberQ[VertexList[g],{mu,1}],
		!MemberQ[VertexList[g],{lam,Length[w]+1}]]
		,
		{}
		,
		(* Find all paths in the graph. *)
		FindPath[g, {mu,1}, {lam,Length[w]+1}, Infinity, All]
	];
	
	pathToSSYT[pathIn_List]:=Module[{path,nrows,m,e,r},
		{m,nrows} = Dimensions[pathIn];
		
		path = Prepend[pathIn,ConstantArray[0,nrows]];
		
		Table[
			Join@@Table[
				ConstantArray[If[e-1==0,None,e-1], path[[e+1,r]] - path[[e,r]] ]
			,{e,m}]
		,{r,nrows}]
	];
	
	Map[YoungTableau[pathToSSYT[First/@#]]&, ssytPaths]
];

UnitTest[SemiStandardYoungTableaux]:=And[
	Length[SemiStandardYoungTableaux[{{5, 4, 2}, {2, 1}}, ConstantArray[1, 8]]]==
	Length[StandardYoungTableaux[{{5, 4, 2}, {2, 1}}]]
];

YoungTableauWeight[YoungTableau[tableau_]]:=Module[{i,rw = DeleteCases[Join@@tableau,None]},
	Table[Count[rw,i],{i,Max[rw,0]}]
];

YoungTableau/:Transpose[YoungTableau[tableau_]]:= Module[{pad, fill, transposed},
	pad = Length@First@tableau;
	transposed = Transpose[PadRight[#, pad, fill] & /@ tableau];
	transposed = DeleteCases[transposed, fill, {2}];
	YoungTableau[transposed]
];

(* Max *)
YoungTableau/:Max[YoungTableau[tableau_]]:=Max@SYTReadingWord@YoungTableau@tableau;
YoungTableau/:Min[YoungTableau[tableau_]]:=Min@SYTReadingWord@YoungTableau@tableau;

(* Formatting rule for YoungTableau objects *)

YoungTableau/:Format[YoungTableau[tableau_]]:=YoungTableauForm[YoungTableau[tableau]];

YoungTableauForm::usage = "YoungTableauForm[tab] returns a graphical representation of the Young tableau.";

Options[YoungTableauForm]={ItemSize->1, DescentSet->False};
YoungTableauForm[diagram_List, opts:OptionsPattern[]]:=YoungTableauForm[YoungTableau@diagram, opts];

YoungTableauForm[YoungTableau[diagram_], opts:OptionsPattern[]]:= Module[
    {n,is,gridItems},
 
	is = OptionValue[ItemSize];
	
	If[ Max@Select[Flatten[diagram],IntegerQ] >9,
		is = 1.5;
	];
	
	gridItems = Table[
		
		n = diagram[[r,c]];
		
		
		Which[
			n===None,
				 Item["", Frame -> {{LightGray, Black}, {Black, LightGray}}],
			
			OptionValue[DescentSet]===True && 
			IntegerQ[n] && 
			c<Length[diagram[[r]]] && 
			IntegerQ[diagram[[r,c+1]]] && 
			diagram[[r,c]]<diagram[[r,c+1]],
				Item[n, Frame -> Black, Background->LightGray],
			
			MatchQ[n, None[__]],
				Item[First@n],
				
			n==="",
				Item[""],
			
			True,
				Item[n, Frame -> Black]
		]
	,{r,Length@diagram}
	,{c,Length[diagram[[r]]]}];
	
	Grid[gridItems,
		ItemSize -> {is, is},
		Spacings -> {0.1, 0.1},
		ItemStyle -> {Black, FontSize -> 16*1, If[is<1,Bold, Plain]}
	]
];


TableauShortTeX::usage = "YoungTableauTeX[tab] returns the \\tableaushort{..} TeX version of the tableau.";

(* TeX form of Young diagrams. *)
TableauShortTeX[YoungTableau[diagram_]]:= Module[{str, strTbl, tex},
	strTbl = diagram /. {None -> "{\\none}", n_Integer :> ToString[n]};
	
	str = StringJoin @@ Riffle[StringJoin /@ strTbl, ","];
	tex = StringJoin["\\ytableaushort{", str, "}"];
	tex
];


Options[YTableauTeX] = {LineBreaks->True};
YTableauTeX[YoungTableau[diagram_],opts:OptionsPattern[]]:= Module[
	{str, strTbl, strLines, texString, lb},
	
	
	lb = If[OptionValue[LineBreaks],"\n",""];
	
	strTbl = diagram /. {None -> "\\none", n_Integer :> ToString[n]};
	
	strLines = (StringJoin@@Riffle[#, " & "])&/@strTbl;
	
	(* Add line breaks *)
	strLines =  StringJoin[#,"\\\\",lb] & /@ strLines;
	texString = StringJoin@@strLines;
	
	texString = StringJoin["\\begin{ytableau}"<>lb,texString,"\\end{ytableau}"];
	texString
];


RowLatticePaths::usage = "RowLatticePaths[ssyt] returns a graphical representation of the ssyt 
as a set of non-intersecting lattice paths, each path corresponding to a row in the ssyt";
RowLatticePaths[YoungTableau[ssyt_]] := Module[
   {m = Max@YoungTableau@ssyt, rows = Length@ssyt, path, paths},
   
   paths = Table[
     Most@Accumulate@
       Prepend[
        Join @@
         Table[
          Append[ConstantArray[{1, 0}, Count[ssyt[[r]], i]], {0, 1}]
          , {i, m}]
        , {-r + 2 Count[ssyt[[r]], None], 1}]
     , {r, rows}];
   Graphics[
    {
     {Thickness[0.010], Line[#] & /@ paths},
     Table[
      Text[Style[i, FontSize -> Scaled[0.03], 
        Background -> White], {-rows - 1, i}], {i, m}]
     },
    GridLines -> {Range[-3 (rows + m), 3 (rows + m)], Range[m]},
    PlotRange -> All
    ]
   ];
   
ColumnLatticePaths::usage = "ColumnLatticePaths[ssyt] returns a graphical representation of the ssyt 
as a set of non-intersecting lattice paths, each path corresponding to a column in the ssyt";

ColumnLatticePaths[YoungTableau[ssytIn_]] := Module[
   {m = Max@YoungTableau@ssytIn, cols = Length@ssytIn[[1]], path, 
    paths, ssyt, labelx},
   ssyt = Transpose[YoungTableau[ssytIn]][[1]];
   
   paths = Table[
     Accumulate@
      Prepend[
       Table[
        If[MemberQ[ssyt[[c]], i], {-1, 1}, {1, 1}]
        , {i, m}]
       , {2 c - 2 Count[ssyt[[c]], None], 1}]
     , {c, cols}];
   labelx = Min[First /@ paths[[1]]];
   Graphics[
    {
     {Thickness[0.010], Line[#] & /@ paths},
     Table[
      Text[Style[i, FontSize -> Scaled[0.03], 
        Background -> White], {labelx - 1, i + 0.5}], {i, m}]
     },
    GridLines -> {Range[-3 (cols + m), 3 (cols + m)], Range[m + 1]},
    PlotRange -> All
    ]
   ];





CylindricTableaux::usage = "CylindricTableaux[{lam,mu},k] produces all cylindric tableaux with partition weight
and shifted up k steps from minimal possible shift.";


CylindricTableaux[lam_List,k_Integer:0]:=CylindricTableaux[{lam,{}},k];

CylindricTableaux[{lam_List, mu_List}, k_Integer: 0] := Module[{
    isValidQ, firstSkew, firstTot, lastBoxes, firstBoxes,
    minShift
    },
   firstSkew = Length[DeleteCases[mu, 0]];
   firstTot = Length[DeleteCases[lam, 0]];
   lastBoxes = Last[ConjugatePartition@lam];
   firstBoxes = firstTot - firstSkew;

   minShift = Max[firstTot - lastBoxes, firstSkew];

   isValidQ[t_] := Module[{tt, firstCol, lastCol, j},
     tt = First@Transpose[t];
     firstCol = First[tt];
     lastCol = Last[tt];

     And @@ Table[
       Or[
        ! (minShift + k + j <= Length[firstCol]),
        firstCol[[minShift + k + j]] == None,
        lastCol[[j]] == None,
        firstCol[[minShift + k + j]] >= lastCol[[j]]
        ]
       , {j, lastBoxes}]
     ];
   Select[SemiStandardYoungTableaux[{lam, mu}], isValidQ]
];


PlanePartitions::usage = "PlanePartitions[shape,max] returns all plane partitions of given shape, with entries <= max.
PlanePartitions[a,b,c] returns all a x b plane partitions with entries bounded by c.";
PlanePartitions[{1, 0 ...}, max_Integer] := YoungTableau[{{#}}] & /@ Range[0, max];
PlanePartitions[sh_List, max_Integer] := PlanePartitions[sh, max] = 
   Module[{r = Length@sh, ri, pp, ppList, shRec},
    ri = Select[Range[r, 0, -1], sh[[#]] > 0 &, 1][[1]];
    shRec = MapAt[# - 1 &, sh, ri];
    ppList = First /@ PlanePartitions[shRec, max];
    ppList = PadRight[#, r, {{}}] & /@ ppList;
    Join @@ Table[
      YoungTableau /@ Table[
        MapAt[Append[#, c] &, pp, ri]
        ,
        {c, 0,
         Min[
          If[sh[[ri]] > 1, pp[[ri, -1]], max],
          If[ri > 1, pp[[ri - 1, sh[[ri]] ]], max],
          max
          ]}]
      , {pp, ppList}]
    ];
PlanePartitions[a_Integer, b_Integer, c_Integer] :=  PlanePartitions[ConstantArray[b, a], c];




(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)




BorderStrips::usage = "BorderStrips[shape,size] returns a list of pairs 
{new-shape,strip} of possible border-strips to remove.";

BorderStrips[shape_List, size_Integer] := BorderStrips[{shape, {}}, size];

BorderStrips[{lam_List, mu_List}, size_Integer] := Module[
   {rows = Length@lam, sh = Append[lam, 0], diffs, span, end, pre, 
    rem, nsh, strip, shSkew},
   shSkew = PadRight[mu, rows + 1];
   
   diffs = Append[ConstantArray[1, rows - 1], 0] - Differences[sh];
   span[s_] := 
    Position[Accumulate[diffs[[s ;;]]], a_ /; a >= size, 1, 1];
   
   Join @@ Table[
     end = span[s];
     Catch[
      
      If[Length[end] == 0, Throw[{}]];
      
      end = s + end[[1, 1]] - 1;
      pre = diffs[[s ;; end]];
      (* How much to remove from each affected row. *)
      
      rem = ReplacePart[pre, -1 -> size - Tr[Most@pre]];
      
      (* If we remove too much from last affected row. *)
      
      If[sh[[end]] - rem[[-1]] < sh[[end + 1]], Throw[{}]];
      
      strip = Join @@ Table[
         Table[{r, c}, {c, sh[[r]], 
           sh[[r]] - rem[[r - s + 1]] + 1, -1}], {r, s, end}];
      
      (* The new shape *)
      
      nsh = lam - 
        SparseArray[Table[s + i - 1 -> rem[[i]], {i, Length@rem}], 
         rows];
      
      (* Must still be skew shape. *)
      
      If[Min[Append[nsh, 0] - shSkew] < 0, Throw[{}]];
      
      nsh = DeleteCases[nsh, 0];
      	
      {{{nsh, mu}, strip}}
      ](* End catch *)
     , {s, rows}]
   
   ];

BorderStripTableaux::usage = "BorderStripTableaux[shape, type] returns a list of all border-strip tableaux of the shape.";
   
BorderStripTableaux[{}, 0] = {{}};
BorderStripTableaux[{}, size_Integer] = {};
BorderStripTableaux[sh_List, size_Integer] := 
  BorderStripTableaux[{sh, {}}, size];
BorderStripTableaux[sh_List, type_List] := 
  BorderStripTableaux[{sh, {}}, type];

BorderStripTableaux[{sh1_List, sh2_List}, size_Integer] :=
BorderStripTableaux[{sh1, sh2}, 
   ConstantArray[size, Floor[(Tr[sh1] - Tr[sh2])/size]]];

BorderStripTableaux[{sh1_List, sh2_List}, type_List] := Module[{res},
   Which[
    Tr[sh1] - Tr[sh2] =!= Tr[type], {},
    Tr[sh1] - Tr[sh2] == 0, {{}},
    True,
    res = BorderStrips[{sh1, sh2}, type[[-1]]];
    Join @@ Table[
      Append[#, Last@r] & /@ BorderStripTableaux[First@r, Most@type]
      , {r, res}]
    ]
   ];

BSTHeightVector::usage = "BSTHeightVector[bst] returns a vector where vi is the height of strip i.";
BSTHeightVector[strips_List] := Table[
	Sort[(First /@ s)][[{-1, 1}]].{1, -1}
, {s, strips}]


(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)



InvMajStatistic::usage = "InvMajStatistic[tab] returns the modified 
Macdonald polynomial statistics {inv,maj}. Works on skew shapes.";

InvMajStatistic[YoungTableau[tab_]] := Module[{isInvQ, fil, mu, muc, inv, maj},

	(* Column 0, basement, gives infinity. *)
	fil[r_Integer, 0] := Infinity;

	(* If skew shape, outside boxes count as infinity also. *)
	fil[r_Integer, c_Integer] := Which[
		tab[[r, c]] === None, Infinity,
		True, tab[[r, c]]
	];

	(* Returns true if the triple parametrized by this column and two rows is a triple *)

	isInvQ[col_Integer, r1_Integer, r2_Integer] := With[
		{a = fil[r1, col - 1],
		b = fil[r1, col],
		c = fil[r2, col]},
		Or[a >= b > c, b > c > a, c > a >= b]
	];
	
	mu = Length /@ tab;
	muc = ConjugatePartition[mu];
	
	inv = Sum[
		(* Box (b) must be part of the shape. *)
		
		If[IntegerQ[fil[r1, c]], Boole[isInvQ[c, r1, r2]], 0]
		, {c, Length[tab[[1]]]},
		{r1, muc[[c]]},
		{r2, r1 + 1, muc[[c]]}];

	maj = Sum[
		(* Box (b) must be part of the shape. *)
		
		If[IntegerQ[fil[r, c]],
		Boole[fil[r, c - 1] < fil[r, c]] (mu[[r]] - c + 1)
		, 0]
		, {r, Length[mu]},
		{c, mu[[r]]}];
	
	{inv, maj}
];







(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)

(***************************** RSK ***********************)

(* This one inserts the pair {a,b} into the tableaux *)

BiwordRSK::usage = "BiwordRSK[{w1,w2} inserts the two words and produces a pair of Young Tableaux."

BiwordRSK[{a_Integer, b_Integer}, {YoungTableau[pTab_], YoungTableau[qTab_]}] := Module[
	{insertInRow, pTabOut = pTab, qTabOut = qTab, swapIndex, newi},
	
	(* Tries to insert element i in row r. If fail, continue with next row. *)
	
	insertInRow[r_, i_] := Which[
		(* There is no row r, create row *)
		Length[pTabOut] < r,
			pTabOut = Append[pTabOut, {i}];
			qTabOut = Append[qTabOut, {a}];
	,
		
		(* Insert at the end of current row. For dual, use less *)
		pTabOut[[r, -1]] <= i,
			pTabOut = Insert[pTabOut, i, {r, -1}];
			qTabOut = Insert[qTabOut, a, {r, -1}];
	,
		
		(* Recurse with swapped element. *)
		True,
			
			swapIndex = First @@ Position[pTabOut[[r]], _?( # > i &), 1, 1];
			newi = pTabOut[[r, swapIndex]];
			pTabOut = ReplacePart[pTabOut, {r, swapIndex} -> i];
			insertInRow[r + 1, newi];
	];
	
	insertInRow[1, b];
	YoungTableau/@{pTabOut, qTabOut}
];

(* Performs the RSK insertion algorithm on the biword, and returns two SSYT of the same shape. *)
BiwordRSK[w1_List, w2_List] := Fold[BiwordRSK[#2, #1] &, YoungTableau/@{{}, {}}, Transpose@{w1,w2}];

(* Add increasing recording word. *)
BiwordRSK[w1_List]:=BiwordRSK[Range[Length@w1],w1];


BiwordRSKDual::usage="BiwordRSKDual[{w1,w2} inserts the two words and produces a pair of Young Tableaux."

BiwordRSKDual[{a_Integer,b_Integer},{YoungTableau[pTab_],YoungTableau[qTab_]}]:=
	Module[{insertInRow,pTabOut=pTab,qTabOut=qTab,swapIndex,newi},
	
	(*Tries to insert element i in row r. If fail,continue with next row.*)
	
	insertInRow[r_,i_]:=Which[
	
		(*There is no row r,create row*)
		Length[pTabOut]<r,
			pTabOut=Append[pTabOut,{i}];
			qTabOut = Append[qTabOut,{a}];
		
		,(*Insert at the end of current row. For dual, use less*)
		pTabOut[[r,-1]]<i,
			pTabOut=Insert[pTabOut,i,{r,-1}];
			qTabOut=Insert[qTabOut,a,{r,-1}];
		
		,(*Recurse with swapped element.*)
		True,
			swapIndex = First@@Position[pTabOut[[r]],_?(#>=i&),1,1];
			newi = pTabOut[[r,swapIndex]];
			pTabOut = ReplacePart[pTabOut, {r,swapIndex}->i];
			insertInRow[ r+1, newi];
	];
	
	insertInRow[1,b];
	YoungTableau/@{pTabOut,qTabOut}
];

(*Performs the RSK insertion algorithm on the biword,and returns two SSYT of the same shape.*)
BiwordRSKDual[w1_List,w2_List]:=Fold[BiwordRSKDual[#2,#1]&,YoungTableau/@{{},{}},Transpose@{w1,w2}];


(* Add increasing recording word. *)
BiwordRSKDual[w1_List]:=BiwordRSKDual[Range[Length@w1],w1];



(* The top row in the biword is row-indices, and bottom row are corresponding column indices, of the ones in the matrix *)
BinaryMatrixToBiword[m_List] := Transpose@SortBy[
    Join @@ 
     MapIndexed[If[#1 == 1, #2, Nothing[]] &, 
      m, {2}], {First, -Last[#] &}];


	  

SYTEvacuation::usage = "SYTEvacuation[syt] performs the evacuation involution on the SYT. Does not work on skew shapes or SSYTs.";
SYTEvacuation[syt_YoungTableau] := SYTEvacuation[syt, Max[syt[[1]]]];
SYTEvacuation[syt_YoungTableau,m_Integer] := With[
   {rw = SYTReadingWord[syt]},
   BiwordRSK[m + 1 - Reverse[rw]][[1]]
];
UnitTest[SYTEvacuation] :=
  And[
   (* Unit-tests from StanleyEC2, p. 428 *)
   SYTEvacuation[
     YoungTableau[{{1, 2, 6}, {3, 4, 7}, {5}}]]
    ===
    YoungTableau[{{1, 3, 5}, {2, 4, 7}, {6}}]
   ,
   (* Unit-tests from StanleyEC2, p. 428 *)
   
   SYTEvacuation[YoungTableau[{{1, 2, 4}, {3, 6, 7}, {5}}]]
    ===
    YoungTableau[{{1, 2, 3}, {4, 5, 7}, {6}}]
   ,
   (* Example 2.11 in https://arxiv.org/pdf/1003.2728.pdf *)
   
   SYTEvacuation[
     YoungTableau[{{1, 3, 8}, {2, 4}, {5, 9}, {6, 10}, {7}}]]
    ===
    YoungTableau[{{1, 4, 9}, {2, 5}, {3, 6}, {7, 10}, {8}}]
   ];

SYTEvacuationDual::usage = "SYTEvacuationDual[syt] is the dual evacuation.";
SYTEvacuationDual[t_YoungTableau] := SYTPromotion[SYTEvacuation@t, SYTSize[t]];
UnitTest[SYTEvacuation] :=
(SYTEvacuationDual[YoungTableau[{{1, 3, 8}, {2, 4}, {5, 9}, {6, 10}, {7}}]]
==YoungTableau[{{1, 3, 8}, {2, 5}, {4, 6}, {7, 10}, {9}}]
);
	  
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)
(****************************************************************************************************)


CrystalOp[YoungTableau[ssyt_], i_Integer, f_Function] := Module[
   {newWord, word, subWord, coord},
   
   (* The subword consisting of elements in {i,i+1} *)
   subWord =
    Join @@ MapIndexed[
      If[IntegerQ[#] && (#1 == i || #1 == i + 1), #2 -> #1, Nothing] &,
      ssyt, {2}];
   (* Make sure order is reading-word order. *)
   
   subWord = SortBy[subWord, {-#[[1, 1]] &, #[[1, 2]] &}];
   
   (* Remove matching parenthesises.  *)
   (* 
   Word is now of the form (i)^a,(i+1)^b *)
   
   subWord = ReplaceRepeated[subWord,
     {first___, {_Integer, _Integer} -> i + 1,
       {_Integer, _Integer} -> i, last___} :> {first, last}];
   
   (* Apply the supplied operator on the matched word *)
   
   word = Last /@ subWord;
   coord = First /@ subWord;
   
   
   newWord = f[word];
   
   (* Map on the SSYT *)
   
   YoungTableau@ReplacePart[ssyt, Thread[Rule[coord, newWord]]]
   ];

CrystalEi::usage = "CrystalEi[ssyt,i] performs the crystal raising operator ei on the tableau. It also works on lists";

CrystalEi[YoungTableau[ssyt_], i_Integer] := 
  CrystalOp[YoungTableau@ssyt, i,
   Function[{w}, If[w == {}, {}, Prepend[Most[w], i]]]];
CrystalEi[w_List, i_Integer] := 
  CrystalEi[YoungTableau[{w}], i][[1, 1]];

CrystalFi::usage = "CrystalFi[ssyt,i] performs the crystal lowering operator fi on the tableau. It also works on lists";
  
CrystalFi[YoungTableau[ssyt_], i_Integer] := 
  CrystalOp[YoungTableau@ssyt, i,
   Function[{w}, If[w == {}, {}, Append[Rest[w], i + 1]]]];
CrystalFi[w_List, i_Integer] := 
  CrystalFi[YoungTableau[{w}], i][[1, 1]];

CrystalSi::usage = "CrystalSi[ssyt,i] performs the crystal 
transposition operator si on the tableau. It also works on lists";
CrystalSi[YoungTableau[ssyt_], i_Integer] := 
  CrystalOp[YoungTableau@ssyt, i, Function[{w}, Reverse[w]/.{i+1->i,i->i+1}]];
CrystalSi[w_List, i_Integer] := CrystalSi[YoungTableau[{w}], i][[1, 1]];


End[]; (*End private*)

EndPackage[];


