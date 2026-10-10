(* ::Package:: *)

(* MathKernel -script file.m *)

(* ::TODO:: *)
(*
	--- Add some flag $French to display tableaux in French.
*)


Clear["NewTableaux`*"];

BeginPackage["NewTableaux`",{"CombinatoricTools`"}];

SYTSize;
SYTMax;
SYTReadingWord;
SYTMajorIndex;
SYTCharge;
SYTCocharge;
SSYTCharge;
SSYTCocharge;
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
HasOuterCornerQ;
StandardYoungTableaux;
SemiStandardYoungTableaux;
YoungTableauForm;
YoungDiagramForm;
CylindricTableaux;
CylindricSYT;
PlanePartitions;

BorderStrips;
BorderStripTableaux;
BSTHeightVector;
SpecialRimHookTableaux;

TableauShortTeX;
YTableauTeX;
LineBreaks;
UseArray;
RowLatticePaths;
ColumnLatticePaths;

InvMajStatistic;

ArrayToBiword;

BiwordRSK;
BiwordRSKDual;
KnuthRepresentative;

BinaryMatrixToBiword;

LongestIncreasingSubsequence;

SYTEvacuation;
SYTEvacuationDual;

CrystalEi;
CrystalFi;
CrystalSi;

SSAF;
SSAFQ;
SSAFShape;
SSAFBasement;
SSAFWeight;
SSAFMonomial;
SSAFForm;
SSAFillings;
AtomFillings;
KeyFillings;
TAtomFillings;
SSAFMajorIndex;
SSAFInversions;
SSAFCoInversions;
SSAFDn;
SSAFColumnSets;
SSAFCrystalWord;
SSAFCrystalString;
LascouxSchutzenberger;
SSAFWeightNormalize;
SSYTToAtom;
RPPToAtom;
SSAFKnownCharge;
ChargeToMajMap;



Begin["`Private`"];

(* Pattern for list of integers *)
iList = {RepeatedNull[_Integer]};

SYTSize::usage = "SYTSize[tab] returns the number of boxes in the tableau.";
SYTSize[syt_YoungTableau]:=Length@SYTReadingWord[syt];

SYTMax::usage = "SYTMax[tab] returns the maximum entry in the tableau.";
SYTMax[syt_YoungTableau] := Max@SYTReadingWord[syt];


SYTReadingWord::usage = "SYTReadingWord[tab] returns the reading word of tab, formed by reading rows from bottom to top and omitting None entries.";
SYTReadingWord[YoungTableau[syt_]] := DeleteCases[Join @@ Reverse[syt], None];

SYTMajorIndex::usage = "SYTMajorIndex[syt] returns the major index of a standard Young tableau.";
SYTMajorIndex[syt_YoungTableau] := MajorIndex[Ordering[SYTReadingWord@syt]];


SYTCharge::usage = "SYTCharge[ssyt] returns the charge of a semistandard tableau, with partition weight.";
SYTCharge[ssyt_YoungTableau] := WordCharge@SYTReadingWord@ssyt;
SYTCocharge::usage = "SYTCocharge[ssyt] returns the cocharge of a semistandard tableau.";
SYTCocharge[ssyt_YoungTableau] := WordCocharge@SYTReadingWord@ssyt;

SSYTCharge::usage = "SSYTCharge[ssyt] returns the charge of a semistandard Young tableau.";
SSYTCharge[ssyt_YoungTableau] := WordCharge@SYTReadingWord@ssyt;
SSYTCocharge::usage = "SSYTCocharge[ssyt] returns the cocharge of a semistandard Young tableau.";
SSYTCocharge[ssyt_YoungTableau] := WordCocharge@SYTReadingWord@ssyt;

(* Only works on SYTs ! *)
SYTDualMajorIndex::usage = "SYTDualMajorIndex[syt] returns the dual major index of a standard Young tableau.";
SYTDualMajorIndex[YoungTableau[syt_]] := With[{m=Max[syt]},
Sum[
 j Boole[Position[syt,j][[1,1]]>Position[syt,j+1][[1,1]]]
,{j,m-1}]
];




YoungTableauSize::usage = "YoungTableauSize[tab] returns the number of boxes in the tableau, excluding skew boxes represented by None.";
YoungTableauSize[syt_YoungTableau]:=Length[SYTReadingWord[syt]];

YoungTableauShape::usage = "YoungTableauShape[tab] returns the outer shape of the tableau. 
	YoungTableauShape[tab,m] returns the shape of formed by all entries <=m.";

YoungTableauShape[YoungTableau[syt_]]:=Length/@syt;

YoungTableauShape[syt_YoungTableau, v_Integer]:=YoungTableauShape[syt/.{i_Integer /; i>v :> Nothing}];

hasOuterCorner[lambda_List, mu_List] := Module[{mup},
   mup = PadRight[mu, Length[lambda]];
   AnyTrue[Range[2, Length[lambda]],
      lambda[[#]] - mup[[#]] >= 2 && mup[[# - 1]] < lambda[[#]] &]
];

HasOuterCornerQ::usage = "HasOuterCornerQ[tab] returns True when the skew shape of tab has an outer corner in the legacy OldYoungTableaux sense.
HasOuterCornerQ[{lam, mu}] applies the same test to a skew shape.";
HasOuterCornerQ[YoungTableau[diagram_]] := hasOuterCorner[
   Length /@ diagram,
   Length[TakeWhile[#, SameQ[#, None] &]] & /@ diagram
];
HasOuterCornerQ[{lambda_List, mu_List}] := hasOuterCorner[lambda, mu];


SYTDescentSet::usage = "SYTDescentSet[syt] returns the descent set of the standard Young tableau.";
(* All i such that i+1 appears South of i *)
SYTDescentSet[YoungTableau[tt_]] := DescentSet@Ordering@Cases[Reverse[tt], _Integer?Positive, {2}];

SYTDescents::usage = "SYTDescents[syt] returns the number of descents of a standard Young tableau.";
SYTDescents[syt_YoungTableau] := Length@SYTDescentSet[syt];



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


StandardYoungTableaux[{}]:={YoungTableau@{{}}};
StandardYoungTableaux[{{}, {}}]:={YoungTableau@{{}}};

StandardYoungTableaux[{sh:iList, sh2:iList}] := StandardYoungTableaux[{sh, sh2}] = Module[
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
			StandardYoungTableaux[{ReplacePart[sh, r -> sh[[r]] - 1], sh2}] (* TODO: USE MapAt instead *)
		,{2}]
	, {r, addIdx}]
];


SuperStandardTableau::usage = "SuperStandardTableau[{lam,mu}] returns the SYT with 1,2,.. in first row and so on.";
SuperStandardTableau[lam:iList]:=SuperStandardTableau[{lam,{}}];
SuperStandardTableau[{lam:iList, mu:iList}] := Module[
   {n = Tr[lam] - Tr[mu], l = Max[Length@lam, Length@mu], rows},
   rows = PartitionList[Range[n], PadRight[lam, l] - PadRight[mu, l]];
   YoungTableau@MapThread[Join,
     {(ConstantArray[None, #] & /@ PadRight[mu, l]), rows}, 1]
];




(* TODO: THERE IS ALSO Shutzenbergers promotion *)

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
SemiStandardYoungTableaux[{lam:iList, mu:iList}, max_Integer]:=Module[{weights},
	
	weights = IntegerPartitions[Tr[lam]-Tr[mu], max ];
	
	Join@@Table[
		SemiStandardYoungTableaux[{lam,mu},w]
		,{w,weights}]
];

(* The default max-entry ensures that all SYTs of the shape appears *)
SemiStandardYoungTableaux[{lam:iList, mu:iList}]:=SemiStandardYoungTableaux[{lam,mu},Tr[lam]-Tr[mu]];

(* Weight must match up, otherwise empty set *)
SemiStandardYoungTableaux[{lambdaIn:iList, muIn:iList}, w:iList]/;(Tr[lambdaIn]-Tr[muIn]-Tr[w]!=0):={};

SemiStandardYoungTableaux[{lambdaIn:iList, muIn:iList}, w:iList] := Module[{isEdgeQ,
	partitionLevels, directedEdges, mid,lam,mu,wAcc,ssytPaths,q,g,pathToSSYT},
	
	lam = lambdaIn;
	mu = PadRight[muIn,Length@lam];
	If[lam === mu,
		Return[{YoungTableau[ConstantArray[None, #] & /@ lam]}]
	];
	
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

YoungTableauWeight::usage = "YoungTableauWeight[tab] returns the weight vector counting entries 1 through the maximum entry of tab.";
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

YoungTableauForm::usage = "YoungTableauForm[tab, options] returns a graphical representation of the Young tableau. ItemSize defaults to 1 and DescentSet defaults to False.";

Options[YoungTableauForm]={ItemSize->1, DescentSet->False};
YoungTableauForm[diagram_List, opts:OptionsPattern[]]:=YoungTableauForm[YoungTableau@diagram, opts];

YoungTableauForm[YoungTableau[diagram_], opts:OptionsPattern[]]:= Module[
    {n,is,gridItems},
 
	is = OptionValue[ItemSize];
	
	If[ Max@Select[Flatten[diagram],IntegerQ] >9,
		is = Max[is,1.5];
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

YoungDiagramForm::usage = "YoungDiagramForm[lam, options] returns a graphical representation of the Young diagram of partition lam. YoungDiagramForm[{lam, mu}, options] uses skew shape lam/mu. ItemSize defaults to 1 and DescentSet defaults to False.";
Options[YoungDiagramForm]={ItemSize->1, DescentSet->False};
YoungDiagramForm[lam:iList, opts:OptionsPattern[]]:=YoungDiagramForm[{lam,{}},opts];

YoungDiagramForm[{lam:iList,mu:iList}, opts:OptionsPattern[]]:= Module[{is,tab,r},
	is = OptionValue[ItemSize];
	tab=Table[
		Join[
			ConstantArray[None, If[Length[mu] >= r, mu[[r]], 0]]
			,
			ConstantArray["\[CenterDot]", If[Length[mu] >= r, lam[[r]] - mu[[r]], lam[[r]]]]
		]
	, {r, Length@lam}];
	YoungTableauForm[tab,ItemSize->is]
];

LineBreaks::usage = "LineBreaks is an option for YTableauTeX; its default is True.";
UseArray::usage = "UseArray is an option for YTableauTeX; its default is True and False selects the legacy \\young representation.";

TableauShortTeX::usage = "TableauShortTeX[tab] returns the \\tableaushort{..} TeX string for the tableau.";

(* TeX form of Young diagrams. *)
TableauShortTeX[YoungTableau[diagram_]]:= Module[{str, strTbl, tex},
	strTbl = diagram /. {None -> "{\\none}", n_Integer :> ToString[n]};
	
	str = StringJoin @@ Riffle[StringJoin /@ strTbl, ","];
	tex = StringJoin["\\ytableaushort{", str, "}"];
	tex
];


YTableauTeX::usage = "YTableauTeX[tab, options] returns a TeX string for tab. LineBreaks defaults to True and UseArray selects the legacy \\young representation when False.";
Options[YTableauTeX] = {LineBreaks -> True, UseArray -> True};
YTableauTeX[YoungTableau[diagram_],opts:OptionsPattern[]]:= Module[
	{str, strTbl, strLines, texString, lb},

	If[!TrueQ[OptionValue[UseArray]],
		strTbl = diagram /. {None -> ":", n_Integer :> ToString[n]};
		str = StringJoin @@ Riffle[StringJoin /@ strTbl, ","];
		Return[StringJoin["\\young(", str, ")"]]
	];
	
	
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
        , {-r + Count[ssyt[[r]], None], 1}]
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
			, {2 c - 2 Count[ssyt[[c]], None], 0}]
		, {c, cols}];
	labelx = Min[First /@ paths[[1]]];
	Graphics[
	{
		{Thickness[0.010], Line[#] & /@ paths},
		Table[
		Text[Style[i, FontSize -> Scaled[0.03], 
			Background -> White], {labelx - 1, i - 0.5}], {i, m}]
		},
	GridLines -> {Range[-3 (cols + m), 3 (cols + m)], Range[0, m]},
	PlotRange -> All
	]
];


CylindricTableaux::usage = "CylindricTableaux[{lam,mu},k] 
produces all cylindric tableaux with partition weight and shifted up k steps from minimal possible shift.";


CylindricTableaux[lam:iList,k_Integer:0]:=CylindricTableaux[{lam,{}},k];

CylindricTableaux[{lam:iList, mu:iList}, k_Integer: 0] := Module[{
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
        firstCol[[minShift + k + j]] === None,
        lastCol[[j]] === None,
        firstCol[[minShift + k + j]] >= lastCol[[j]]
        ]
       , {j, lastBoxes}]
     ];
   Select[SemiStandardYoungTableaux[{lam, mu}], isValidQ]
];

CylindricSYT::usage = "CylindricSYT[lam, k] returns all standard Young tableaux of cylindric shape lam with shift k. CylindricSYT[{lam, mu}, k] uses skew shape lam/mu; k defaults to 0.";
CylindricSYT[lam:iList, k_Integer: 0] := CylindricSYT[{lam, {}}, k];
CylindricSYT[{lam:iList, mu:iList}, k_Integer: 0] :=  Module[{isValidQ, firstSkew, firstTot, lastBoxes, firstBoxes, 
    minShift},
   firstSkew = Length[DeleteCases[mu, 0]];
   firstTot = Length[DeleteCases[lam, 0]];
   lastBoxes = Last[ConjugatePartition@lam];
   firstBoxes = firstTot - firstSkew;
   minShift = Max[firstTot - lastBoxes, firstSkew];

		isValidQ[t_] := Module[{tt, firstCol, lastCol, j},
     tt = First@Transpose[t];
     firstCol = First[tt];
     lastCol = Last[tt];
     And @@ 
      Table[Or[! (minShift + k + j <= Length[firstCol]), 
        firstCol[[minShift + k + j]] === None, lastCol[[j]] === None, 
        firstCol[[minShift + k + j]] >= lastCol[[j]]], {j, 
        lastBoxes}]];
   
   Select[StandardYoungTableaux[{lam, mu}], isValidQ]
];


PlanePartitions::usage = "PlanePartitions[shape,max] returns all plane partitions of given shape, with entries <= max.
PlanePartitions[a,b,c] returns all a x b plane partitions with entries bounded by c.";
PlanePartitions[{1, 0 ...}, max_Integer] := YoungTableau[{{#}}] & /@ Range[0, max];
PlanePartitions[sh:iList, max_Integer] := PlanePartitions[sh, max] = 
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

BorderStrips[shape:iList, size_Integer] := BorderStrips[{shape, {}}, size];

BorderStrips[{lam:iList, mu:iList}, size_Integer] := Module[
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
BorderStripTableaux[sh:iList, size_Integer] := 
  BorderStripTableaux[{sh, {}}, size];
BorderStripTableaux[sh:iList, type:iList] := 
  BorderStripTableaux[{sh, {}}, type];

BorderStripTableaux[{sh1:iList, sh2:iList}, size_Integer] :=
BorderStripTableaux[{sh1, sh2}, 
   ConstantArray[size, Floor[(Tr[sh1] - Tr[sh2])/size]]];

BorderStripTableaux[{sh1:iList, sh2:iList}, type:iList] := Module[{res},
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




SpecialRimHookTableaux::usage= "SpecialRimHookTableaux[shape,type] returns all special rim-hook tableaux of given shape and type.";

SpecialRimHookTableaux[shape:iList, type:iList] := Module[{srhtRec},
   
   (* Hook. *)
   srhtRec[sh_List, {w_Integer}] := List@Join[
      {ConstantArray[1, sh[[1]]]},
      ConstantArray[{1}, Total[Rest@sh]]];
   
   srhtRec[sh_List, t_List] := srhtRec[sh, t] = With[
      {bs =
        Select[BorderStrips[sh, t[[1]]],
         Length[#[[1, 1]]] < Length[sh] &, 1], m = Length@t},
      
      If[Length@bs == 0, {},
       
       Table[
        (* Add last borderstrip to smaller srht. *)
        Table[
         PadRight[If[j <= Length[tab], tab[[j]], {}], sh[[j]], m]
         , {j, Length@sh}]
        
        , {tab, srhtRec[bs[[1, 1, 1]], Rest@t]}]
       ]
      ];
   
   
   Which[
    Total[shape] != Total[type], {},
    Length[type] == 1 && Max[Rest@shape] > 1, {},
    Length[type] == 1, srhtRec[shape, type],(* Hook shape. *)
    True,
    (* All orderings of size of strips.*)
    Join @@ Table[srhtRec[shape, tt], {tt, Permutations@type}]
    ]
   ];

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


ArrayToBiword::usage = "ArrayToBiword[a] converts a nonnegative integer array a to a biword listing the positions of its entries.";
ArrayToBiword[a_] := 
  Transpose@Flatten[MapIndexed[ConstantArray[#2, #1] &, a, {2}], 2];




(* This one inserts the pair {a,b} into the tableaux *)

BiwordRSK::usage = "BiwordRSK[{w1, w2}] applies row-insertion RSK to two equal-length words and returns a pair of YoungTableau objects.
BiwordRSK[w] applies RSK with the increasing word Range[Length[w]] as the first word.";

BiwordRSK[{a_Integer, b_Integer}, {YoungTableau[pTab_], YoungTableau[qTab_]}] := Module[
	{insertInRow, pTabOut = pTab, qTabOut = qTab, swapIndex, newi},
	
	(* Tries to insert element i in row r. If fail, continue with next row. *)
	
	insertInRow[r_Integer, i_Integer] := Which[
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
BiwordRSK[w1:{___Integer}, w2:{___Integer}] /; Length[w1] == Length[w2] :=
	Fold[BiwordRSK[#2, #1] &, YoungTableau/@{{}, {}}, Transpose@{w1,w2}];

(* Add increasing recording word. *)
BiwordRSK[w1_List]:=BiwordRSK[Range[Length@w1],w1];


BiwordRSKDual::usage="BiwordRSKDual[{w1, w2}] applies dual row-insertion RSK to two equal-length words and returns a pair of YoungTableau objects.
BiwordRSKDual[w] applies dual RSK with the increasing word Range[Length[w]] as the first word.";

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
BiwordRSKDual[w1:{___Integer},w2:{___Integer}] /; Length[w1] == Length[w2] :=
	Fold[BiwordRSKDual[#2,#1]&,YoungTableau/@{{},{}},Transpose@{w1,w2}];


(* Add increasing recording word. *)
BiwordRSKDual[w1_List]:=BiwordRSKDual[Range[Length@w1],w1];

KnuthRepresentative::usage = "KnuthRepresentative[pi] returns the unique permutation which is Knuth equivalent to pi, and is the reading word of some SYT.";
KnuthRepresentative[w_List] := KnuthRepresentative[w] =
	SYTReadingWord[BiwordRSK[w][[1]]];



(* The top row in the biword is row-indices, and bottom row are corresponding column indices, of the ones in the matrix *)
BinaryMatrixToBiword::usage = "BinaryMatrixToBiword[m] converts a binary matrix m to its biword of positions of 1 entries.";
BinaryMatrixToBiword[m_List] := Transpose@SortBy[
    Join @@ 
     MapIndexed[If[#1 == 1, #2, Nothing[]] &, 
      m, {2}], {First, -Last[#] &}];

LongestIncreasingSubsequence::usage ="LongestIncreasingSubsequence[w] returns the length of the longest increasing subsequence.";
LongestIncreasingSubsequence[w_List] := Length[BiwordRSK[w][[1, 1, 1]]];
	  

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

CrystalOp[Undefined,_,_]:=Undefined;

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
	(* Word is now of the form (i)^a,(i+1)^b *)
	
	subWord = ReplaceRepeated[subWord,
		{first___, {_Integer, _Integer} -> i + 1,
		{_Integer, _Integer} -> i, last___} :> {first, last}];
	
	(* Apply the supplied operator on the matched word *)
	
	word = Last /@ subWord;
	coord = First /@ subWord;
	
	newWord = f[word];
	
	If[newWord === Undefined,
		Undefined,
		(* Map on the SSYT *)
		YoungTableau@ReplacePart[ssyt, Thread[Rule[coord, newWord]]]
	]
];

CrystalEi::usage = "CrystalEi[ssyt,i] performs the crystal raising operator ei on the tableau. 
It also works on lists";


(* Replaces i+1 with i *)
CrystalEi[YoungTableau[ssyt_], i_Integer,k_Integer:1] := 
CrystalOp[YoungTableau@ssyt, i,
	Function[{w}, 
		With[{a=Count[w,i],b=Count[w,i+1]},
			Which[
				b>=k, Join[ConstantArray[i,a+k],ConstantArray[i+1,b-k]],
				True, Undefined
			]
		]
	]
];
CrystalEi[Undefined,_]:=Undefined;
CrystalEi[Undefined,_,_]:=Undefined;
CrystalEi[w_List, i_Integer,k_Integer:1] := With[
{out = CrystalEi[YoungTableau[{w}], i,k]},
	If[out === Undefined, out, out[[1, 1]] ]
];

CrystalFi::usage = "CrystalFi[ssyt,i] performs the crystal lowering operator fi on the tableau. It also works on words.";

CrystalFi[YoungTableau[ssyt_], i_Integer,k_Integer:1] := 
CrystalOp[YoungTableau@ssyt, i,
	Function[{w}, 
		With[{a=Count[w,i],b=Count[w,i+1]},
			Which[
				a>=k, Join[ConstantArray[i,a-k],ConstantArray[i+1,b+k]],
				True, Undefined
			]
		]
	]
];
CrystalFi[Undefined,_]:=Undefined;
CrystalFi[Undefined,_,_]:=Undefined;
CrystalFi[w_List, i_Integer,k_Integer:1] := With[
	{out = CrystalFi[YoungTableau[{w}], i,k]},
	If[out === Undefined, out, out[[1, 1]] ]
];

CrystalSi[Undefined,_]:=Undefined;
CrystalSi[Undefined,_,_]:=Undefined;
CrystalSi::usage = "CrystalSi[ssyt,i] performs the crystal 
transposition operator si on the tableau. It also works on words.";
CrystalSi[YoungTableau[ssyt_], i_Integer] := 
  CrystalOp[YoungTableau@ssyt, i, Function[{w}, Reverse[w]/.{i+1->i,i->i+1}]];
CrystalSi[w_List, i_Integer] := With[
	{out = CrystalSi[YoungTableau[{w}], i]},
	If[out === Undefined, out, out[[1, 1]] ]
];


(**********************************************************************************)

(* Semistandard augmented fillings.  Rows are listed from top to bottom, so the
   last row is the bottom row; every row begins with its basement entry. *)

SSAF::usage = "SSAF[rows] represents a semistandard augmented filling with basement entries in the first column.";
SSAFQ::usage = "SSAFQ[ssaf] returns True if ssaf is a valid semistandard augmented filling.";
SSAFShape::usage = "SSAFShape[ssaf] returns the weak composition of non-basement row lengths.";
SSAFBasement::usage = "SSAFBasement[ssaf] returns the basement entries of ssaf.";
SSAFWeight::usage = "SSAFWeight[ssaf] returns the weight vector of the non-basement entries of ssaf.";
SSAFMonomial::usage = "SSAFMonomial[ssaf,x] returns the weight monomial of ssaf.";
SSAFForm::usage = "SSAFForm[ssaf, options] returns a graphical representation of an augmented filling.";
SSAFillings::usage = "SSAFillings[alpha, basement] returns all valid augmented fillings of shape alpha and basement.";
AtomFillings::usage = "AtomFillings[alpha] returns the augmented fillings enumerating the Demazure atom indexed by alpha.";
KeyFillings::usage = "KeyFillings[alpha] returns the augmented fillings enumerating the key polynomial indexed by alpha.";
TAtomFillings::usage = "TAtomFillings[alpha] returns the non-attacking augmented fillings used by the t-atom identity.";
SSAFMajorIndex::usage = "SSAFMajorIndex[ssaf] returns the augmented filling major index.";
SSAFInversions::usage = "SSAFInversions[ssaf] returns the number of inversion triples of ssaf.";
SSAFCoInversions::usage = "SSAFCoInversions[ssaf] returns the number of coinversion triples of ssaf.";
SSAFDn::usage = "SSAFDn[ssaf] returns the number of unequal horizontal adjacencies of ssaf.";
SSAFColumnSets::usage = "SSAFColumnSets[ssaf, start] returns sorted column sets, starting at column start (default 2).";
SSAFCrystalWord::usage = "SSAFCrystalWord[ssaf,i] returns the uncancelled i-crystal word of ssaf.";
SSAFCrystalString::usage = "SSAFCrystalString[ssaf,i] returns the i-crystal string containing ssaf.";
LascouxSchutzenberger::usage = "LascouxSchutzenberger[object,i] applies the Lascoux--Schutzenberger involution.";
SSAFWeightNormalize::usage = "SSAFWeightNormalize[ssaf] applies crystal involutions until the weight is a partition.";
SSYTToAtom::usage = "SSYTToAtom[tab] applies Mason's insertion map from a semistandard tableau to an atom filling.";
RPPToAtom::usage = "RPPToAtom[rpp] applies Mason's column-set insertion map to an augmented reverse plane partition.";
SSAFKnownCharge::usage = "SSAFKnownCharge[ssaf] returns the charge when the augmented filling has partition shape.";
ChargeToMajMap::usage = "ChargeToMajMap[ssaf] applies the charge-to-major-index map to an augmented filling.";

ssaClockwiseQ[a_, b_, c_] := Or[a < b < c, b < c < a, c < a < b];
ssaTypeAQ[a_Integer, b_Integer, c_Integer] :=
	ssaClockwiseQ[a + 0.1, c + 0.3, b + 0.2];
ssaTypeBQ[a_Integer, b_Integer, c_Integer] :=
	ssaClockwiseQ[a + 0.2, c + 0.1, b + 0.3];

SSAFShape[SSAF[rows_List]] := (Length /@ rows) - 1;
SSAFBasement[SSAF[rows_List]] := First /@ rows;
SSAFWeight[SSAF[rows_List]] :=
	Table[Count[Join @@ (Rest /@ rows), i], {i, Length[rows]}];
SSAFMonomial[SSAF[rows_List], x_] :=
	Times @@ MapIndexed[x[First[#2]]^#1 &, SSAFWeight[SSAF[rows]]];

Options[SSAFForm] = Options[YoungTableauForm];
SSAFForm[rows_List, opts : OptionsPattern[]] := SSAFForm[SSAF[rows], opts];
SSAFForm[SSAF[rows_List], opts : OptionsPattern[]] := YoungTableauForm[rows, opts];
SSAF /: Format[SSAF[rows_List]] := SSAFForm[SSAF[rows]];

ssaCheckNonAttacking[tab_List, r_Integer, c_Integer] := Module[{b = tab[[r, c]]},
	And @@ Table[
		!(Length[tab[[i]]] >= c && tab[[i, c]] == b) &&
			!(Length[tab[[i]]] >= c - 1 && tab[[i, c - 1]] == b),
		{i, r - 1}]
];

ssaCheckCoinversion[tab_List, r_Integer, c_Integer, shape_List] := Module[{},
	If[!ssaCheckNonAttacking[tab, r, c], Return[False]];
	And @@ Table[
		If[shape[[i]] >= shape[[r]] && Length[tab[[i]]] >= c,
			ssaTypeAQ[tab[[i, c]], tab[[r, c]], tab[[i, c - 1]]],
			If[shape[[i]] < shape[[r]] && Length[tab[[i]]] >= c - 1,
				ssaTypeBQ[tab[[i, c - 1]], tab[[r, c - 1]], tab[[r, c]]],
				True]],
		{i, r - 1}]
];

ssaValidRowsQ[rows_List] := Module[{shape, n},
	If[rows === {} || !VectorQ[rows, ListQ] || !And @@ (Length[#] > 0 & /@ rows), Return[False]];
	shape = (Length /@ rows) - 1;
	n = Length[rows];
	If[!VectorQ[First /@ rows, IntegerQ] ||
		!And @@ (VectorQ[#, IntegerQ] & /@ (Rest /@ rows)), Return[False]];
	If[!And @@ Flatten[Table[
		rows[[r, c - 1]] >= rows[[r, c]],
		{r, n}, {c, 2, Length[rows[[r]]]}]], Return[False]];
	And @@ Flatten[Table[
		ssaCheckCoinversion[rows, r, c, shape],
		{r, n}, {c, 2, Length[rows[[r]]]}]]
];

SSAFQ[SSAF[rows_List]] := ssaValidRowsQ[rows];
SSAFQ[_] := False;

ssaGenerate[alpha_List, basement_List, checker_] := Module[
	{rowSequence, recurse},
	If[Length[alpha] =!= Length[basement] ||
		!VectorQ[alpha, IntegerQ[#] && # >= 0 &] || !VectorQ[basement, IntegerQ], Return[{}]];
	rowSequence = Join @@ Table[ConstantArray[i, alpha[[i]]], {i, Length[alpha]}];
	recurse[tab_List, {}] := {SSAF[tab]};
	recurse[tab_List, seq_List] := Module[{r = First[seq], maxNew, newTab},
		maxNew = tab[[r, -1]];
		Join @@ Table[
			newTab = Append[tab[[r]], b];
			newTab = ReplacePart[tab, r -> newTab];
			If[checker[newTab, r, Length[newTab[[r]]], alpha],
				recurse[newTab, Rest[seq]], {}],
			{b, maxNew}]
	];
	recurse[Transpose[{basement}], rowSequence]
];

SSAFillings[alpha_List, basement_List] := ssaGenerate[alpha, basement, ssaCheckCoinversion];
SSAFillings[alpha_List] := SSAFillings[alpha, Range[Length[alpha]]];
AtomFillings[alpha_List] := SSAFillings[alpha, Range[Length[alpha]]];
KeyFillings[alpha_List] := SSAFillings[Reverse[alpha], Reverse[Range[Length[alpha]]]];
TAtomFillings[alpha_List] :=
	ssaGenerate[alpha, Range[Length[alpha]],
		Function[{tab, r, c, shape}, ssaCheckNonAttacking[tab, r, c]]];

SSAFMajorIndex[SSAF[rows_List]] := Module[{shape = Length /@ rows},
	Sum[Boole[rows[[r, c]] > rows[[r, c - 1]]] (1 + shape[[r]] - c),
		{r, Length[rows]}, {c, 2, shape[[r]]}]
];
SSAFInversions[SSAF[rows_List]] := Module[{shape = Length /@ rows},
	Sum[Boole[Or[
			shape[[i]] >= shape[[r]] && shape[[i]] >= c &&
				ssaTypeAQ[rows[[i, c]], rows[[r, c]], rows[[i, c - 1]]],
			shape[[i]] < shape[[r]] && shape[[i]] >= c - 1 &&
				ssaTypeBQ[rows[[i, c - 1]], rows[[r, c - 1]], rows[[r, c]]]]],
		{r, Length[rows]}, {c, 2, shape[[r]]}, {i, r - 1}]
];
SSAFCoInversions[SSAF[rows_List]] := Module[{shape = Length /@ rows},
	Sum[Boole[Or[
			shape[[i]] >= shape[[r]] && shape[[i]] >= c &&
				!ssaTypeAQ[rows[[i, c]], rows[[r, c]], rows[[i, c - 1]]],
			shape[[i]] < shape[[r]] && shape[[i]] >= c - 1 &&
				!ssaTypeBQ[rows[[i, c - 1]], rows[[r, c - 1]], rows[[r, c]]]]],
		{r, Length[rows]}, {c, 2, shape[[r]]}, {i, r - 1}]
];
SSAFDn[SSAF[rows_List]] :=
	Total[Flatten[Table[Boole[rows[[r, c]] =!= rows[[r, c - 1]]],
		{r, Length[rows]}, {c, 2, Length[rows[[r]]]}]]];

SSAFColumnSets[SSAF[rows_List], startingColumn_Integer : 2] := Module[{len},
	If[rows === {}, Return[{}]];
	len = Max[Length /@ rows];
	Table[Sort[Cases[rows, row_List /; Length[row] >= c :> row[[c]]]],
		{c, startingColumn, len}]
];

ssaCrystalWordRows[rows_List, i_Integer] := Module[
	{len, word, selected, deleted, removePair},
	If[rows === {}, Return[{}]];
	len = Max[Length /@ rows];
	If[len < 2, Return[{}]];
	word = Flatten[Table[
		If[Length[rows[[r]]] >= c && i <= rows[[r, c]] <= i + 1,
			{{r, c} -> rows[[r, c]]}, {}],
		{c, 2, len}, {r, Length[rows], 1, -1}], 2];
	deleted = Join @@ Table[
		selected = Select[word, #[[1, 2]] == c &];
		If[Length[selected] == 2, First /@ selected, {}], {c, 2, len}];
	word = Select[word, !MemberQ[deleted, First[#]] &];
	removePair[w_List] := Catch[
		Do[If[w[[j, 2]] + 1 == w[[j + 1, 2]],
			Throw[Join[w[[;; j - 1]], w[[j + 2 ;;]]]]], {j, Length[w] - 1}]; w];
	FixedPoint[removePair, SortBy[word, #[[1, 2]] &]]
];
SSAFCrystalWord[SSAF[rows_List], i_Integer] := ssaCrystalWordRows[rows, i];

ssaKeySortRows[SSAF[rows_List]] := Module[{shape, basement, columns, candidates},
	shape = SSAFShape[SSAF[rows]];
	basement = SSAFBasement[SSAF[rows]];
	columns = SSAFColumnSets[SSAF[rows]];
	candidates = Select[SSAFillings[shape, basement],
		SSAFColumnSets[#] === columns &];
	If[candidates === {}, {}, First[candidates]]
];

ssaApplyRules[expr_, rules_List] := Fold[ReplacePart[#1, #2] &, expr, rules];

ssaFillingOrder[fillings_List] := SortBy[fillings, ToString[InputForm[#[[1]]]] &];
ssaFillingsOfWeight[ssaf_SSAF, weight_List] :=
	ssaFillingOrder@Select[SSAFillings[SSAFShape[ssaf], SSAFBasement[ssaf]],
		SSAFWeight[#] === weight &];

ssaCrystalStep[ssaf_SSAF, i_Integer, direction_Integer] := Module[
	{weight = SSAFWeight[ssaf], target, sourceFillings, targetFillings, pos},
	If[i < 1 || i >= Length[weight], Return[{}]];
	target = ReplacePart[weight,
		{i -> weight[[i]] - direction, i + 1 -> weight[[i + 1]] + direction}];
	If[Min[target] < 0, Return[{}]];
	sourceFillings = ssaFillingsOfWeight[ssaf, weight];
	targetFillings = ssaFillingsOfWeight[ssaf, target];
	pos = FirstPosition[sourceFillings, ssaf, Missing[]];
	If[MissingQ[pos] || First[pos] > Length[targetFillings], {},
		targetFillings[[First[pos]]]]
];

ssaRaising[ssaf_SSAF, i_Integer] := ssaCrystalStep[ssaf, i, 1];
ssaLowering[ssaf_SSAF, i_Integer] := ssaCrystalStep[ssaf, i, -1];

SSAFCrystalString[ssaf_SSAF, i_Integer] := Module[{raisingPart, loweringPart},
	raisingPart = Most@Rest@NestWhileList[ssaRaising[#, i] &, ssaf, # =!= {} &];
	 loweringPart = Reverse[Most@Rest@NestWhileList[ssaLowering[#, i] &, ssaf, # =!= {} &]];
	Join[loweringPart, {ssaf}, raisingPart]
];

CrystalEi[ssaf_SSAF, i_Integer, k_Integer : 1] := Module[{out = ssaf, j},
	Do[out = ssaRaising[out, i]; If[out === {}, Return[Undefined]], {j, k}]; out
];
CrystalFi[ssaf_SSAF, i_Integer, k_Integer : 1] := Module[{out = ssaf, j},
	Do[out = ssaLowering[out, i]; If[out === {}, Return[Undefined]], {j, k}]; out
];

ssaCrystalReflection[ssaf_SSAF, i_Integer] := Module[
	{weight = SSAFWeight[ssaf], target, sourceFillings, targetFillings, pos},
	If[i < 1 || i >= Length[weight], Return[ssaf]];
	target = ReplacePart[weight,
		{i -> weight[[i + 1]], i + 1 -> weight[[i]]}];
	sourceFillings = ssaFillingsOfWeight[ssaf, weight];
	targetFillings = ssaFillingsOfWeight[ssaf, target];
	pos = FirstPosition[sourceFillings, ssaf, Missing[]];
	If[MissingQ[pos] || First[pos] > Length[targetFillings], ssaf,
		targetFillings[[First[pos]]]]
];

ssaLSTransposeRows[rows_List, i_Integer, modifiedQ_ : False] := Module[
	{word, repl},
	word = ssaCrystalWordRows[rows, i];
	If[word === {}, Return[rows]];
	repl = Reverse[Last /@ word /. {i + 1 -> i, i -> i + 1}];
	ssaApplyRules[rows, Thread[Rule[word[[All, 1]], repl]]]
];

LascouxSchutzenberger[SSAF[rows_List], i_Integer] := CrystalSi[SSAF[rows], i];
LascouxSchutzenberger[t_YoungTableau, i_Integer] := CrystalSi[t, i];

ssaReducedWord[perm_List] := If[Sort[perm] === perm, {},
	With[{d = First[DescentSet[perm]]},
		Join[{d}, ssaReducedWord[ReplacePart[perm, {d -> perm[[d + 1]], d + 1 -> perm[[d]]}]]]]];
LascouxSchutzenberger[ssaf_SSAF, {a_Integer, b_Integer}] := Module[{n = Length[SSAFBasement[ssaf]], p},
	p = ReplacePart[Range[n], {a -> b, b -> a}];
	Fold[LascouxSchutzenberger[#1, #2] &, ssaf, ssaReducedWord[p]]
];
CrystalSi[ssaf_SSAF, i_Integer] := ssaCrystalReflection[ssaf, i];

SSAFWeightNormalize[ssaf_SSAF] := Module[{out = ssaf, w, p},
	w = SSAFWeight[out];
	While[True,
		p = FirstPosition[Table[w[[i]] < w[[i + 1]], {i, Length[w] - 1}], True, Missing[]];
		If[MissingQ[p], Break[]];
		With[{next = CrystalSi[out, First[p]]},
			If[next === out, Break[]];
			out = next];
		w = SSAFWeight[out]
	];
	out
];

ssaInsertElement[atm_List, v_Integer, {r_Integer, c_Integer}, n_Integer] :=
	Module[{nr = r + 1, nc = c},
		If[nr == n + 1, nr = 1; nc = c - 1];
		Which[
			c == 1, $Failed,
			Length[atm[[r]]] + 1 == c && atm[[r, c - 1]] >= v,
				Insert[atm, v, {r, c}],
			Length[atm[[r]]] + 1 > c && atm[[r, c - 1]] >= v && atm[[r, c]] >= v,
				ssaInsertElement[atm, v, {nr, nc}, n],
			Length[atm[[r]]] + 1 > c && atm[[r, c - 1]] >= v && atm[[r, c]] < v,
				ssaInsertElement[ReplacePart[atm, {r, c} -> v], atm[[r, c]], {nr, nc}, n],
			True, ssaInsertElement[atm, v, {nr, nc}, n]]
	];

SSYTToAtom[YoungTableau[rows_List]] := Module[{n, atom, word},
	If[rows === {} || Flatten[rows] === {}, Return[SSAF[{}]]];
	n = Max[Flatten[rows]];
	atom = List /@ Range[n];
	word = Join @@ Reverse[rows];
	Do[atom = ssaInsertElement[atom, v, {1, 1 + Max[Length /@ atom]}, n],
		{v, Reverse[word]}];
	SSAF[atom]
];

RPPToAtom[SSAF[rows_List]] := Module[{n, atom},
	If[rows === {}, Return[SSAF[{}]]];
	n = Length[rows];
	atom = List /@ Range[n];
	Do[Do[atom = ssaInsertElement[atom, v, {1, 1 + Max[Length /@ atom]}, n],
		{v, col}], {col, SSAFColumnSets[SSAF[rows]]}];
	SSAF[atom]
];

ssaNormalizeWord[word_List] := Module[{rows, w, p},
	If[word === {}, Return[{}]];
	rows = {Prepend[word, 0]};
	w = Table[Count[word, i], {i, Max[word]}];
	While[True,
		p = FirstPosition[Table[w[[i]] < w[[i + 1]], {i, Length[w] - 1}], True, Missing[]];
		If[MissingQ[p], Break[]];
		rows = ssaLSTransposeRows[rows, First[p]];
		w = Table[Count[First[rows], i], {i, Max[word]}]
	];
	Rest[First[rows]]
];
ssaWordDecompose[{}] := {};
ssaWordDecompose[word_List] := Module[{n = Length[word], current = 0, find, pos = {}, e},
	find[e_, p_] := Catch[Do[If[word[[j]] == e, Throw[j]],
		{j, Ordering[RotateRight[Range[n], p]]}]; -1];
	Do[With[{q = find[e, current]}, If[q > 0, AppendTo[pos, q]; current = q]],
		{e, Max[word]}];
	pos = Sort[pos];
	Join[{word[[pos]]}, ssaWordDecompose[word[[Complement[Range[n], pos]]]]]
];
SSAFKnownCharge[ssaf_SSAF] /; SSAFShape[ssaf] === Sort[SSAFShape[ssaf], Greater] :=
	Total[MajorIndex[Reverse[Ordering[#]]] & /@
		ssaWordDecompose[ssaNormalizeWord[Join @@ Reverse[ssaf[[1]]]]]];

ChargeToMajMap[SSAF[rows_List]] := Module[
	{indexDecompose, word, indices, standardWords, tagged, maxEntry, maxCol},
	indexDecompose[{}] = {};
	indexDecompose[w_] := {} /; Max[w] == 0;
	indexDecompose[w_List] := Module[{n = Length[w], current = 0, find, pos = {}, e, cmp},
		find[e_, p_] := Catch[Do[If[w[[j]] == e, Throw[j]],
			{j, Ordering[RotateRight[Range[n], p]]}]; -1];
		Do[With[{q = find[e, current]}, If[q > 0, AppendTo[pos, q]; current = q]],
			{e, Max[w]}];
		pos = Sort[pos]; cmp = ReplacePart[w, (# -> 0) & /@ pos];
		Prepend[indexDecompose[cmp], pos]
	];
	word = Join @@ Table[{r, c, rows[[r, c]]},
		{r, Length[rows], 1, -1}, {c, Length[rows[[r]]], 1, -1}];
	indices = indexDecompose[Last /@ word];
	standardWords = word[[#]] & /@ indices;
	tagged = Join @@ Table[Append[#, k] & /@ standardWords[[k]],
		{k, Length[standardWords]}];
	If[tagged === {}, Return[SSAF[{}]]];
	maxEntry = Max[Flatten[rows]]; maxCol = Max[Last /@ tagged];
	SSAF[DeleteCases[Normal[SparseArray[{#3, #4} -> #1 & @@@ tagged,
		{maxEntry, maxCol}]], 0, {2}]]
];


End[]; (*End private*)

EndPackage[];
