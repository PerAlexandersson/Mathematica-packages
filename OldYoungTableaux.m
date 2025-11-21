(* ::Package:: *)

(* MathKernel -script file.m *)

Clear["OldYoungTableaux`*"];
BeginPackage["OldYoungTableaux`"];



(* List of options. *)
LineBreaks::usage="Give as an argument to TeXForm, to enable (default) or disable line breaks.";
UseArray::usage = "Give as an argument to TeXForm, to enable or disable (default) usage of matrix.";


SageForm::usage="Returns Sage representation of object.";

UnimodalQ::usage = "Returns true if the list is unimodal, that is, increasing, then decreasing.";

HVector::usage = "HVector[poly, x] returns the H-vector obtained from the polynomial.";

FullPermutationCycles::usage= "";

PermutationOfType::usage = "";

SetPartitionRefinementQ::usage="SetPartitionRefinementQ[p1,p2] returns true if p1 is a weak refinement of p2.";

SkewShapeQ::usage = 
"ShapeQ[lambda, mu] returns True if lambda/mu defines a valid skew shape. ShapeQ[lambda, mu, weight] is true if the number of boxes matches the total of weight.";

ShapeUnion::usage = "ShapeUnion[{l1,m1},{l2,m2},...] construct {lu,mu} where the latter is the union of shapes.";

UnittestPackage::usage = "Runs the unit tests.";

SequenceToPolynomial::usage = "SequenceToPolynomial[func,x] gives the polynomial that the sequence determines.";

EhrhartPolynomial::usage = "EhrhartPolynomial[polytopeFunc, params={1}, x, (timeout=6) ]";

ConjugatePartition::usage = 
"Returns the conjugate of a partition.";

PartitionList::usage = "PartitionList[list,mu] partitions the list into non-overlapping pieces of sizes given by mu.";

DominatesQ::usage = 
"DominatesQ[p1,p2] returns true if p2 dominates p1.";

YoungLatticePaths::usage="YoungLatticePaths[mu,nu]] returns all lattice paths from mu to nu in Young lattice. mu<=nu.";

PermutationType::usage="";

NormalizePartitions::usage="";

TrimPartition::usage = "Deletes all entries which are 0.";

ToCycleForm::usage = "ToCycleForm[partition] returns the cycle form of the partition.";


(* This is to convert a partition into x1,y1, ... list. Rename this. *)
TopValleyRepresentation::usage = "TODO";

ToTableauShape::usage="";

TableauShape::usage = "Represents a shape and weight.";

HasOuterCornerQ::usage="Returns true if the Tableau shape has outer corner.";

KnopTableaux::usage = "";

ShapeTriplets::usage = "Given a partition lambda, returns all triplets (lambda, mu, w) such that lambda/mu is a shape, and w is a weight.";

(* Options for ShapeTriplets. *)
EnableSkew::usage="";
WeightRange::usage="";
KostkaRange::usage="";


UpperBoundKostkaDegree::usage="KostkaDegreeEstimation[lambda,mu,nu] returns a degree estimate on Ehrhart polynomial. This is always an over-estimation.";

SchurPolynomial::usage = 
"Returns the Schur polynomial SchurPolynomial[lambda, mu][x]. Also, SchurPolynomial[lambda, mu, nvars][x].";


HallLittlewoodP::usage = "The Hall-Littlewood polynomial, HallLittlewoodP[lambda, n, x, t] where n is the number of variables.";


GTPattern::usage = "Represents a GT-pattern.";

GTPlus::usage = "Elementwise addition of GT-patterns";

GTPatterns::usage = "Returns GT-patterns. GTPatterns[s1,s2,w]";

GTMonomial::usage = "GTMonomial[g, x] returns the monomial associated with g, given by the weight/type of g."

BoxCountMatrix::usage = "Returns a matrix, where m[[i,j]] is the number of boxes with content j in row i. m[[i]] therefore describes row i.";

BZPattern::usage = "Represents a BZ-pattern.";
BZPlus::usage = "Elementwise addition of BZ-patterns";
BZPatterns::usage = "Returns BZ-patterns. GTPatterns[lambda,mu,nu], where |mu|+|nu|=|lambda|";

GogPatterns::usage = ""; 
MagogPatterns::usage = ""; 

GTPatternForm::usage = "Returns a graphical representation of the GT-pattern. Options: GTPartition->{ShadedTiles,Tiles,Snakes,None}.";

GTTiles::usage = "Returns {freeTiles, fixedTiles}.";
GTSnakes::usage="Returns {snakes}.";

GTPartition::usage="Option for graphics. Decide what partition of elements to use. Snakes, Tiles, FreeTiles are valid.";
Snakes::usage="";
ShadedTiles::usage = "";
Tiles::usage = "";
FreeTiles::usage = "";


TilingMatrix::usage = "";

ContainingFaceDimension::usage = "";


YoungTableau::usage = 
"Represents a Young tableau. Also used to create Young tableax from GTPatterns.";

YoungTableauForm::usage = 
"Returns a graphical representation of the Young tableau.";

YoungTableauTeX::usage = "";
MatrixTeXForm::usage="";

LatticePathForm::usage = 
"Plots non-intersecting lattice paths. Input: GTpattern or YoungTableau.";

LatticePathTikz::usage = 
"Return tikz code for the lattice paths.";

GTPatternTikz::usage = "Return tikz code for the GT-pattern. Option: GTPartition->{Snakes, Tiles, FreeTiles, ShadedTiles}.";


KostkaCoefficient::usage =
"Returns the Kostka coefficient, K[lambda,weight] or K[lambda,mu,weight]";

ChiCoefficient::usage = "Returns the Chi coefficient, Chi[lambda, mu]. Notice, indexed with partition, not cycle type.";

ZCoefficient::usage = "Returns the Z-coefficient, as p. 299, Enumerative Combinatorics II, Stanley";
(* n!/Zp is the number of permutations of cycle type p *)

LCoefficient::usage="";

PowerSumPolynomial::usage = "Returns p_mu. PowerSumPolynomial[mu,nvars][x].";

MonomialSymmetricPolynomial::usage = "Return m_mu.";

IndexedToFallingBasisRule::usage = "";

FerayN::usage="FerayN[sigma, tau, nvars][p,q].";

ChNormalizedCharacter::usage = "ChNormalizedCharacter[mu_,d_][p_,q_] returns the normalized character on mu, in p1...pd, q1...qd";

ShiftedSchur::usage = "ShiftedSchur[mu,d][p,q]";

KNormalizedCharacter::usage = "KNormalizedCharacter[mu_,d_][p_,q_] returns the normalized character on mu, in p1...pd, q1...qd";

AddBoxToPartition::usage = "AddBoxToPartition[mu, [nu] ] returns all possible partitions obtained from mu [that fits in nu] by adding one box. ";
RemoveBoxFromPartition::usage = "AddBoxToPartition[mu, [nu] ] returns all possible partitions obtained from mu [that nu fits in] by removing one box. ";

MacdonaldPsi::usage = "MacdonaldPsi[lambda,mu,a], MacdonaldPsi[gtpattern,a], MacdonaldPsi[youngtab,a] ";
MacdonaldPsiPrime::usage = "MacdonaldPsiPrime[lambda,mu,a], MacdonaldPsiPrime[gtpattern,a], MacdonaldPsiPrime[youngtab,a] ";
HookFactor::usage = "HookFactor[mu,a=1]";
HookPrimeFactor::usage = "HookPrimeFactor[mu,a=1]";
JackNorm::usage = "JackNorm[mu,a=1]";
JackPPolynomial::usage = "JackPPolynomial[mu,k, x, a] where k is the number of variables";
JackJPolynomial::usage = "JackJPolynomial[mu,k, x, a] where k is the number of variables";
ShiftedJackPPolynomial::usage = "ShiftedJackPPolynomial[mu,k, x,a] where k is the number of variables";
ShiftedJackJPolynomial::usage = "ShiftedJackJPolynomial[mu,k, x,a] where k is the number of variables";
ShiftedJackPEvaluate::usage = "ShiftedJackPEvaluate[mu,nu,a] evaluates the polynomial in one point";
ShiftedJackJEvaluate::usage = "ShiftedJackJEvaluate[mu,nu,a] evaluates the polynomial in one point";
JackPStructureConstant::usage = "JackPStructureConstant[lambda, mu, nu,a] where nu is the largest";
JackJStructureConstant::usage = "JackJStructureConstant[lambda, mu, nu,a] where nu is the largest";


(***************** Switch to private context *********************************)

(* TODO: use integer list patterns for function arguments. *)
(* 
Create functions for iterating over partitions, e.g.
 PartitionTable[ f[p], {p, 1, 7}] will be the same as evaluating f[p] over all integger partitions of 1...7 
 *)

Begin["Private`"];


(* Patterns for matching things we use often. *)
IntegerList = {_Integer ...};
IntegerSets = {{_Integer ...} ...};

UnimodalQ[list_List] := With[{dd = Reverse@DeleteCases[Sign@Differences[list], 0]}, dd === Sort[dd]];


PartitionList[lst_List, mu_List] := 
With[{acc = Accumulate@Prepend[mu, 0]},
DeleteCases[Table[lst[[1 + acc[[i - 1]] ;; acc[[i]]]], {i, 2, Length@acc}], {}]];

(*Change of basis substitutions. *)

ToFallingBasisRule[x_, y_] := {x^k_ :> Sum[StirlingS2[k, d] y^d, {d, 0, k}], x :> y};
IndexedToFallingBasisRule[x_, y_] := {x[i_]^k_ :> Sum[StirlingS2[k, d] y[i]^d, {d, 0, k}], x[i_] :> y[i]};

(*
HVector[poly_,x_] := Module[{d,y},
	d = Exponent[poly, x];
	If[d < 0, 
		0,
		CoefficientList[Together[(1 - y)^(d + 1) Sum[poly y^x, {x, 0, Infinity}]],y]
	]
];
*)

HVector[poly_,x_] := Module[{bb, c, d,y},
	bb[xx_, i_, s_] := 1/i! Product[(xx - k + s), {k, 0, i - 1}];
	
	d = Exponent[poly, x];
	If[d<0, {},
		
		tbz = CoefficientList[poly - Sum[ c[i] bb[x, d, i], {i, 0, d}], x];
		vars = Variables[tbz];
		Reverse[vars] /. Solve[Thread[tbz == 0], vars][[1]]
	]
];



(************************* Ehrhart computations and polytopes ****************)


PolynomialFit[seq_List, x_, degIn_Integer: - 1] := Module[{deg, n, vars, c, poly, eqns, sol},
	deg = If[degIn >= 0, degIn, Length[seq] - 2];
	poly = Sum[c[i] x^i, {i, 0, deg}];
	eqns = Table[seq[[i]] == (poly /. {x -> i}), {i, Length@seq}];
	vars = c /@ Range[0, deg];
	sol = Solve[eqns, vars];
	If[Length@sol == 0,
		False,
		poly /. First[sol]
	]
];

Options[SequenceToPolynomial] = {TimeOut -> 6};
SequenceToPolynomial[func_,x_, OptionsPattern[]] := Module[{poly, k, seq, timeout},
	timeout = OptionValue[TimeOut];
	seq = {func[1]};
	poly = False;

	TimeConstrained[
		For[k = 2, poly === False, k++,
			Check[
				AppendTo[seq, func[k] ];,
				Return[Undefined]
			];

		poly = PolynomialFit[seq,x];
		];
		poly
		,
		60*timeout 
	, Undefined]
];

EhrhartPolynomial[polytopeFunc_, params_List:{1}, x_, timeout_: 6]:=SequenceToPolynomial[Length[polytopeFunc@@(#*params)]& , x, TimeOut->timeout];


(************************* Permutations and set partitions *******************)

SageForm[part:IntegerList]:=StringJoin["[", Riffle[ToString/@part , ","], "]"];

(* Better version of how Signature works. *)

PermutationSignature[list_List]:=Signature[list];
PermutationSignature[Cycles[{{}}]]:=1;
PermutationSignature[Cycles[list_]]:=Signature[PermutationList[Cycles[list]]]; 


(* Returns full cycle representation of a permutation. *)
FullPermutationCycles[perm_List]:=With[{cycles = PermutationCycles[perm][[1]]},
	Join[ cycles, List/@Complement[ Range[Length@perm], Join@@cycles] ]
];


(* Returns the type of the permutation, that is, 
 lengths of the cycles, in decreasing *)
PermutationType[perm_List]:= Reverse@Sort[ Length/@ FullPermutationCycles[perm] ];


(* Returns a canonical permutation of given type, NOT in cycle notation. *)
PermutationOfType[type_List]:=
PermutationList[ Cycles[ 
	(* Here, create the cycles, with Range. *)
	(Range[#1 + 1, #2] & @@@ Partition[Prepend[Accumulate[type], 0], 2, 1]) ], 
Total@type];

UnitTest[PermutationOfType]:=Module[{ip,perm},
	ip = IntegerPartitions[5];
	Do[
		perm = PermutationOfType[p];
		If[p =!= Length/@FullPermutationCycles[perm],
			Return[False];
		]
	,{p, ip}];
	True
];



(* Determines if p1 is a set partition refinement of p2 *)
SetPartitionRefinementQ[p1_List, p2_List] := Module[{IsSubsetOf},
	IsSubsetOf[p_] := Or @@ (Length[Complement[p, #]] == 0 & /@ p2);
	And @@ (IsSubsetOf /@ p1)
];


(************************* Partitions and tableau shapes *******)



(* Make partitions equally long, by padding with zero. *)
NormalizePartitions[p1_List,p2_List:{}]:=With[{len=Max[Length@p1,Length@p2]}, 
	{PadRight[p1,len],PadRight[p2,len]}
];	

NormalizePartitions[partitions_List]:=With[{len=Max[Length/@partitions]}, 
	PadRight[#,len]&/@partitions
];
	
TrimPartition[part_List]:=DeleteCases[part,0];
	
ConjugatePartition[p_List]:=If[Length@p == 0, {}, Table[Count[p, j_ /; j >= m], {m, First[p]}]];

DominatesQ[pp1_List, pp2_List] := Module[{p1,p2},
	{p1,p2} = NormalizePartitions[pp1,pp2];
	 And@@Thread[ Accumulate[p1] <= Accumulate[p2] ] 
];

(* Converts a partition to cycle form. *)
ToCycleForm[{}]:={};
ToCycleForm[partition_List] := Normal[SparseArray[#1 -> #2 & @@@ Tally[ DeleteCases[partition,0]  ]]];


ToTableauShape[lambda_List, mu_List:{}, weight_List:{}]:=Module[{q1,q2},
	{q1,q2} = NormalizePartitions[lambda,mu];
	
	Which[
		Sort[q1,Smaller]!=q1 || Sort[q2,Smaller]!=q2, Null,
		And@@MapThread[#1>=#2&,{q1,q2}] == False, Null,
		Length[weight]>0 && Tr[weight]!=Tr[q1-q2], Null,
		True, TableauShape[q1,q2,weight]
	]
];
ToTableauShape[sh_TableauShape]:=sh;
UnitTest[ToTableauShape]:=(
	ToTableauShape[{3,2,1,1}]===TableauShape[{3,2,1,1},{0,0,0,0},{}] &&
	ToTableauShape[{3,1},{1}]===TableauShape[{3,1},{1,0},{}] && 
	ToTableauShape[{3,3},{3,3},{}]===TableauShape[{3,3},{3,3},{}] && 
	ToTableauShape[{3,2,1},{1},{2,2,2}]===Null &&
	ToTableauShape[{3,1,1},{2,1},{1,1}]===TableauShape[{3,1,1},{2,1,0},{1,1}]
);



(* Compute all possible nu s.t. |mu|+1=|nu|, mu<=nu, and nu<=top *)
AddBoxToPartition[muIn_List, topIn_List: {}] := Module[{top, mu},
   top = If[topIn === {}, mu + 1, topIn];
   mu = muIn;
   If[Last[mu] > 0, AppendTo[mu, 0]];
   {mu, top} = NormalizePartitions[{mu, top}];
   
   Table[
    (* Add box to row j *)
    
    If[(j == 1 || mu[[j]] < mu[[j - 1]]) && mu[[j]] < top[[j]],
     ReplacePart[mu, j -> mu[[j]] + 1],
     Sequence @@ {}
     ]
    , {j, Length@mu}]
   ];

RemoveBoxFromPartition[muIn_List, botIn_List: {}] := 
  Module[{bot, mu},
   bot = If[botIn === {}, Max[#, 0] & /@ (muIn - 1), botIn];
   mu = muIn;
   {mu, bot} = NormalizePartitions[{mu, bot}];
   Table[
    (* Remove box from row j *)
    
    If[(j == Length[mu] || mu[[j + 1]] < mu[[j]]) && 
      mu[[j]] > bot[[j]],
     ReplacePart[mu, j -> mu[[j]] - 1],
     Sequence @@ {}
     ]
    , {j, Length@mu}]
   ];


(*
HookProduct[lambda_List] := HookProduct[lambda, lambda];
HookProduct[lambdaIn_List, muIn_List] := Module[{cLambda, lambda, mu},
	
	{lambda,mu} = NormalizePartitions[{lambdaIn,muIn}];
	cLambda = ConjugatePartition@lambda;
	
	(* Only product of hooks which are in the mu-shape *)
	
	If[ And@@Thread[ mu<=lambda],
		
		Times @@ (Join @@ 
		Table[Table[
			1 - c + lambda[[r]] - r + cLambda[[c]] , 
		{c, mu[[r]]}], {r, Length@mu}])
	,
		0
	]
];
*)

(* Extract shape from YoungTableau. *)
ToTableauShape[YoungTableau[tableau_]]:=Module[{lambda, mu, weight,rules},
	
	lambda = Length /@ tableau;
	
	mu = Length[Select[#, Function[x, x === SKEW]]] & /@ tableau;
	
	rules = #1 -> #2 & @@@ Tally[Sort[Select[Flatten@tableau, NumberQ]]];
	weight = If[Length@rules == 0, {}, Normal@SparseArray[rules]];
	
	ToTableauShape[lambda, mu,weight]
];

(* Returns the shape of a GT-pattern. *)
TableauShape[GTPattern[gtp_]]:=Module[{w},
	
	w = Differences[ Reverse[ Tr/@gtp ]];
	
	TableauShape[First@gtp,Last@gtp,w]
];




(* Multiply with a positive constant. *)
TableauShape/:Times[k_Integer, TableauShape[a_, b_, c_]] := TableauShape[k*a, k*b, k*c] /;k>0;

(* Addition on shapes is done elementwise. *)
TableauShape/:Plus[TableauShape[a1_, b1_, c1_], TableauShape[a2_, b2_, c2_]] := 
Module[{aa1,aa2,bb1,bb2,cc1,cc2},
	{aa1,aa2} = NormalizePartitions[a1,a2];
	{bb1,bb2} = NormalizePartitions[b1,b2];
	{cc1,cc2} = NormalizePartitions[c1,c2];
	TableauShape[aa1+aa2, bb1+bb2, cc1+cc2]
]

UnitTest[TableauShapeMultiply]:=Module[{ts1,ts2},
	ts1 = ToTableauShape[{5,2,1},{2,1},{3,2}];
	ts2 = ToTableauShape[5*{5,2,1},5*{2,1},5*{3,2}];
	5*ts1 === ts2
];


(* Transposing a shape. *)
TableauShape/:Transpose[TableauShape[p1_,p2_,w_]] := Module[{q1,q2},
	q1 = ConjugatePartition[p1];
	q2 = ConjugatePartition[p2];
	{q1,q2} = NormalizePartitions[q1,q2];
	TableauShape[q1,q2,w]
];
UnitTest[Transpose]:=Return[
	Transpose[ToTableauShape[{2,1,1},{1,1}]]===ToTableauShape[{3,1},{2}]
];


HasOuterCornerQ[TableauShape[lambda_,mu_,nu_]]:=Module[{corner=False},
	
	Do[
		If[ lambda[[r]]-mu[[r]] >=2 && mu[[r-1]]< lambda[[r]],
			corner = True;
			Break[];
		]
		
	,{r,2,Length@lambda}];
	
	corner
];


ShapeUnion[{l1_List, m1_List: {}}] := {l1, m1};
ShapeUnion[{l1In_List, m1In_List: {}}, {l2In_List, m2In_List: {}}] := 
	Module[{shift, l1, l2, m1, m2, ln, mn, wn},
	{l1, m1} = NormalizePartitions[l1In, m1In];
	{l2, m2} = NormalizePartitions[l2In, m2In];
	shift = First[l2];
	ln = Join[l1 + shift, l2];
	mn = Join[m1 + shift, m2];
	{ln, mn}
];
ShapeUnion[shapes__List] := With[{list = List@shapes}, Fold[ShapeUnion, First@list, Rest@list]];


YoungLatticePaths[muIn_List, nuIn_List] := Module[{w, mu, nu, dd},
	{mu, nu} = NormalizePartitions[muIn, nuIn];
	dd = Last[mu - nu];
	If[And @@ Thread[nu >= mu] ,
		w = ConstantArray[1, Tr@nu - Tr@mu];
		dd + (Reverse[#[[1]] ] & /@ GTPatterns[nu - dd, mu - dd, w])
		,
		{}
	]
];

(************************* Generating partition triplets **********************)


SkewShapeQ[p1_List, p2_List]:=(And@@(Thread[#1 >= #2]& @@ NormalizePartitions[p1, p2]));

(* Given a partition lambda return a list of all mu such that lambda/mu is a valid shape. *)
GenerateSkewShapes[lambda_List] := 
	GenerateSkewShapes[lambda] = Module[{skewShapes},
	If[lambda == {}, Return[{{}}]];
	Return[
	Join @@ Table[
		With[{newLambda = Rest[Min[k, #] & /@ lambda]},
			skewShapes = GenerateSkewShapes[newLambda];
			Prepend[#1, k] & /@ skewShapes
		], {k, 0, First[lambda]}]
	];
];



Options[ShapeTriplets] = {EnableSkew -> True, WeightRange->{1,Infinity}, KostkaRange->{0,Infinity}};
ShapeTriplets[lambda_List, OptionsPattern[]]:= Module[{ GetTriplet,
	skewEnabled, muList, len, pairs, allowedWeights, 
	triplets, minWeight, maxWeight, minKostka, maxKostka},
	
	skewEnabled = OptionValue[EnableSkew];
	
	len = Length@lambda;
	
	If[skewEnabled,
		muList = GenerateSkewShapes[ Max[0,#]&/@(Most[lambda]-1) ];
		pairs = {lambda, #1}& /@ muList;
	, (* If non skew. *)
		pairs = { {lambda, ConstantArray[0, len]} };
	];
	
	(* Now, we have all pairs that represents proper skew shapes. *)
	(* Redundancies have already been eliminated. *)
	
	
	(* Extract allowed range for box numbering*)
	{minBox,maxBox} = OptionValue[WeightRange];
	
	
	GetTriplet[p1_,p2_]:=Module[{n, weights},
		n = Total[p1] - Total[p2];
		weights = IntegerPartitions[n, {minBox,maxBox} ];
		Return[{p1,p2,#}&/@ weights];
	];
	
	triplets = Join@@(GetTriplet@@@pairs);
	
	{minKostka,maxKostka} = OptionValue[KostkaRange];
	
	(* Here, check how many GTPatterns have the characteristics *)
	If[minKostka>0 || maxKostka<Infinity,
		triplets = Select[triplets, minKostka <= Length[(GTPatterns@@#1)] <= maxKostka & ];
	];
	
	triplets
];



(************************* Schur polynomials and symmetric functions **********************)


SchurPolynomial[partition1_List, partition2_List: {}, nvars_: 0][x_Symbol]:=
Module[{part1, part2, len, n, e, emat, det},
	part1 = ConjugatePartition@partition1;
	part2 = ConjugatePartition@partition2;
	len = Length@part1;
	
	If[len == 0, Return[1]];
	
	part2 = PadRight[part2, len];
	
	(* Yes, it really should be partition1 here. *)
	n = If[nvars == 0, Length@partition1, nvars]; 
	
	(* Elementary symmetric polynomials. *)
	e[j_] := 0 /; (j<0 || j>n);
	e[j_] := SymmetricPolynomial[j, x /@ Range[n]];
	
	emat = Table[ e[ part1[[i]] - part2[[j]] + j - i ], {i, 1, len}, {j, 1, len}];
	det = Det@emat;
	
	If[det === 0, 1, det]
];




PowerSumPolynomial[mu_List,n_Integer][x_]:=PowerSumPolynomial[PadRight[mu,n]][x];

PowerSumPolynomial[mu_List][x_]:=Product[ Sum[ x[i]^p, {i,Length@mu}], {p, TrimPartition@mu} ]

MonomialSymmetricPolynomial[mu_List,n_Integer][x_]:=MonomialSymmetricPolynomial[PadRight[mu,n]][x];

MonomialSymmetricPolynomial[mu_List][x_]:=With[{perms=Permutations[mu]},
	Sum[ Times@@MapIndexed[ x[First[#2]]^#1 &, p ],{p,perms}]
];



(************************* Various coefficients **********************)


(* Skew adoptation of McAllisters "Degrees of stretched Kostka coefficients. *)
(* This always give an over-estimate of the degree. *)
UpperBoundKostkaDegree[lambdaIn_List, muIn_List, weight_List] := 
  UpperBoundKostkaDegree[lambdaIn, muIn, weight] = Module[
    {TriangleTower, lambda, mu, tt1, tt2, h, degree, 
     freeParalellogram, flexRows},
    
    (* Recursively creates the pattern of fixed tiles *)
    
    TriangleTower[tower_List, height_, dir_] := Module[{part, next, n},
      If[height > 0,
       part = Last[tower];
       next = 
        Table[If[part[[i]] == part[[i + 1]], part[[i]], -1], {i, 
          Length@part - 1}];
       (* If dir ==1, then insert first, otherwise, last. *)
       
       next = Insert[next, -1, dir];
       TriangleTower[Append[tower, next], height - 1, dir]
       ,
       tower]
      ];
    {lambda, mu} = NormalizePartitions[lambdaIn, muIn];
    h = 1 + Length[weight];
    
    
    If[Tr@mu == 0,
     (* There are no GT-
     patterns if this is fulfilled solution if this is fulfilled. *)
 
         If[! DominatesQ[Reverse@Sort@weight, lambda], 
      Return[-Infinity]];
     If[lambda === weight, Return[0]];
     ];
    
    
    tt1 = TriangleTower[{lambda}, h - 1, -1];
    tt2 = Reverse@TriangleTower[{mu}, h - 1, 1];
    
    
    (* This produces a GT-pattern (paralellogram),
    where 1 indicates free tiles, and 0 is fixed. *)
    
    freeParalellogram = 
     MapThread[If[Max[#1, #2] >= 0, 0, 1] &, {tt1, tt2}, 2];
    
    (* Select rows with 2 or more free tiles, 
    only values in these rows can vary. *) 
    flexRows = Select[freeParalellogram, Tr[#] >= 2 &];
    
    degree = Tr@Flatten@flexRows - Length[flexRows];
    
    degree
 ];



(* Kostka coefficients. These use memoization. *)
(* Todo: Argument, tells which method to use to compute these. *)
KostkaCoefficient[lambda_List,mu_List,weight_List]:=
	KostkaCoefficient[lambda,mu,weight]=If[Tr@lambda-Tr@mu =!= Tr@weight,0, Length@GTPatterns[lambda,mu,weight]];


(*If lambda does not dominate the weight, then the coefficient is 0. *)
KostkaCoefficient[lambda_List,weight_List]:=0 /; !DominatesQ[weight,lambda];

KostkaCoefficient[lambda_List,weight_List]:=
	KostkaCoefficient[lambda,weight]=KostkaCoefficient[lambda,{},weight];

	

ZCoefficient[part_List]:=Module[{cycle},
	If[part==={}, 1
	,
		cycle = ToCycleForm[part];
		Times@@MapIndexed[ (#1!) * First[#2]^#1 &, cycle]
	]
];


(* Memoized and optimized. *)
LCoefficient[lambda_List,mu_List]:=LCoefficient[lambda,mu]=(DefineLCoefficients[lambda];LCoefficient[lambda,mu]);

(* Helper function that defines the L-coefficients. *)
DefineLCoefficients[lambda_List]:=Module[
	{n, nus, tbz, gList, sol ,zz, g},
	
	n = Total[lambda];
	nus = IntegerPartitions[n];
	
	tbz = PowerSumPolynomial[lambda,n][x] - Sum[ g[lambda,nu] * MonomialSymmetricPolynomial[nu,n][x], {nu,nus}];
	tbz = Last/@CoefficientRules[tbz, x/@Range[n]];
	
	gList = g[lambda, #]&/@nus;
	
	sol = First@Solve[ tbz==0, gList];
	
	(* Define the coefficients. *)
	Do[
		args = First[s]/.{g->List};
		ans = Last[s];
		LCoefficient[args[[1]],args[[2]]] = ans;
	,{s, sol}];
];


(* Memoized and optimized. *)
ChiCoefficient[lambda_List, mu_List]:=ChiCoefficient[lambda, mu]=(DefineChiCoefficients[mu];ChiCoefficient[lambda, mu]);

(* Formula 7.77 Enumerative comb. II, p. 347 *)
DefineChiCoefficients[mu_List] := Module[{PowerSum, n = Tr[mu], x, expr, delta, index},
	PowerSum[n_, p_][x_] := Sum[x[i]^p, {i, n}];
	
	With[{cycle = ToCycleForm[mu]},
		expr = Product[PowerSum[n, j][x]^cycle[[j]], {j, Length[cycle]}]*Product[x[j] - x[i], {j, n}, {i, j + 1, n}];
	];
	delta = Range[n - 1, 0, -1];
	Do[
		index = PadRight[lambda, n] + delta;
		ChiCoefficient[lambda, mu] = Coefficient[expr, Times @@ ((x /@ Range[n])^index)];
	,{lambda, IntegerPartitions[n]}];
];


(************************* Constructing Certain Tableaux **********************)


(* Not enough space. *)
KnopTableaux[lambda_List, k_] := {} /; lambda[[1]] < k;

(* bVec indicates which rows of maxList we will fill *)
(* Return list of vectors indicating positions. 0 means no box. *)
KnopSpecialBoxPositions[{a_},{1}]:=Table[{j},{j,a}];
KnopSpecialBoxPositions[{a_},{0}]:={{0}};
KnopSpecialBoxPositions[maxList_List,bVec_List]:=Module[{tail},
	tail = KnopSpecialBoxPositions[Rest@maxList, Rest@bVec];
	Which[ 
		First@bVec==0, (* If no choice. *)
		Prepend[#,0]& /@ tail,
		
		True,
		Join@@Table[
			Table[Prepend[tp,nb],{nb,Max[1,First@tp],First@maxList}]
		,{tp,tail}]
	]
];

BitTableauToKnopTableaux[lambda_,bitTab_]:=Module[{cLambda,gen},
	
	cLambda = ConjugatePartition[lambda];
	
	gen[1, tbs_] := Prepend[#, {bitTab[[1]], 0} ]& /@ tbs;
	gen[n_, tbs_:{{}}]:= gen[n-1, Prepend[#, {bitTab[[n]],0}]&/@ tbs] /; (bitTab[[n]]==0 || bitTab[[n]]!=bitTab[[n-1]]);
	
	gen[n_, tbs_:{{}}] := With[{row = bitTab[[n]]},
			With[{dds=Join[Range[-(1+cLambda[[n]]-row),-1], Range[1+lambda[[row]]-n ]]},
				gen[n-1, Join@@Table[Prepend[ tab , {row, d} ], {d,dds}, {tab,tbs}]]
				]
			];
	
	gen[Length@bitTab]
];


KnopTableaux[lambda_List, k_] := Module[{bitVecs,cLambda,bitTableaux},
	
	bitVecs = Permutations[ PadRight[ ConstantArray[1,k], lambda[[1]] ] ];
	cLambda = ConjugatePartition[lambda];
	
	bitTableaux = Join@@Table[ KnopSpecialBoxPositions[cLambda,bv], {bv,bitVecs}];
	
	
	Join@@(BitTableauToKnopTableaux[lambda,#]&/@bitTableaux)
];


(************************* Constructing GT-patterns **********************)


GTPattern[tableau_YoungTableau]:=Module[{tab,n,gtp},
	tab = First@tableau;
	tab = tab /. SKEW-> 0;
	n = Max[tab];
	
	gtp = NormalizePartitions@Table[Count[ row , b_/;b <= k ], {k, 0, n}, {row, tab}];
	GTPattern[Reverse@gtp]
];

GTPatterns[Null]:={};

GTPatterns[lambda_List, mu_List:{},weight_List:{}]:=QuickGTPatterns[ToTableauShape[lambda,mu,weight]];
GTPatterns[lambda_List, mu_List:{},maxBox_Integer]:=GTPatterns[ToTableauShape[lambda,mu,{}],maxBox];
GTPatterns[lambda_List, maxBox_Integer]:=GTPatterns[ToTableauShape[lambda,{},{}],maxBox];

GTPatterns[TableauShape[lambda_, mu_, weight_],maxBoxIn_Integer:0]:=Module[
	{w, h, x, gtp, bddconds, wconds=True, ineqs, sol,maxBox=maxBoxIn},
	
	(* Calculate width, height, of GT-pattern. *)
	w = Length[lambda];
	
	(* If no weight or maxBox specified, then maxBox is the number of parts of lambda. *)
	h = 1 + Which[
		Length[weight] >0 , Length[weight],
		maxBox > 0, maxBox,
		True, w];
	
	gtp = Table[x[r][c], {r, h, 1, -1}, {c, w}]; (* The GT-pattern *)
	
	(* Boundary conditions. *)
	bddconds = And @@ Table[x[h][c] == lambda[[c]] && x[1][c] == mu[[c]], {c, w}];
	
	If[Length[weight]>0,
		wconds = And @@ Table[ Sum[x[r + 1][c] - x[r][c], {c, w}] == weight[[r]], {r, h - 1}];
	];
	
	(* Inequalities that must hold. *)
	ineqs = And @@ Flatten[Table[
		If[r < h, x[r + 1][c] >= x[r][c], True] &&
		If[r < h && c < w, x[r][c] >= x[r + 1][c + 1], True] , {r, h}, {c, w}]];
		
	sol = Reduce[bddconds && ineqs && wconds, Flatten@gtp, Integers];
	
	If[sol===False, Return[{}]];
	
	sol = gtp /. List[ToRules[sol]];
	
	GTPattern/@sol
];


GTMonomial[g_GTPattern, x_] := GTMonomial[g, x] = With[{w = Last[TableauShape[g]]}, Times @@ ((x /@ Range[Length[w]])^w)];


QuickGTPatterns[Null]:={};
QuickGTPatterns[TableauShape[l_, mu_, {}]]:={GTPattern[{l}]} /; (l===mu);

(* This only works when all parts in w are positive! *)
QuickGTPatterns[TableauShape[l_, mu_, w_]] := Module[{findPaths, w1, ip, cf, g, de, mid},
	
	(* István Zachar code follows*)
	
   findPaths[a_?MatrixQ, s_Integer, t_Integer] := Module[{child, find},
     child[v_] := 
      Flatten@Position[a[[v]], Except@0, 1, Heads -> False];
     find[v_, list_] := 
      Scan[If[# === t, Sow[Append[list, #]], 
         If[FreeQ[list, #], find[#, Append[list, #]]]] &, child@v];
     If[# =!= {}, First@#, {}] &@Last@Reap@find[s, {s}]
     ];
	 
   findPaths[g_Graph, s_, t_] := 
    Module[{nodes = VertexList@g, convert}, 
     If[nodes === {} || FreeQ[nodes, s] || FreeQ[nodes, t], {}, 
      convert = Thread[nodes -> Range@Length@nodes];
      findPaths[Normal@AdjacencyMatrix@g, s /. convert, 
        t /. convert] /. Reverse /@ convert]
     ];
   
   
   w1 = Accumulate@Reverse@w;
   (* Create all partitions 'between' lambda and mu,  in layers. *)
   (*  Should be able to predict how many parts there are at most in each layer. *)
   
   mid = IntegerPartitions[Tr@l - #, {Length@l}, Range[0, Max@l]] & /@ Most@w1;
   
   ip = Join[{{l}}, mid, {{mu}}];
   
   (* Compute edges. *)
   de = Function[{lvl},
      Outer[
       If[Min[#1 - #2] >= 0 && Min[#2[[;; -2]] - #1[[2 ;;]]] >= 0,
         {#1, #2},
         Sequence @@ {}
         ] &, ip[[lvl]], ip[[lvl + 1]], 1]
      ] /@ Range[Length[ip] - 1];
   
   g = Graph@(DirectedEdge @@@ Flatten[de, 2]);
   
   
   
   (* This should work in Mathematica 10 *)
   (*
   GTPattern /@ FindPath[g, l, mu, Infinity, All]
   *)
   
   GTPattern /@ findPaths[g, l, mu]
   
   
];


BZPatterns[llIn_List, mmIn_List, nnIn_List]:={} /; Total[llIn] - Total[mmIn] - Total[nnIn]!=0;

(* BZ-patterns, counted by the Littlewood-Richardson coefficients *)
BZPatterns[llIn_List, mmIn_List, nnIn_List] := Module[
   {ll, mm, nn, n, x, xvars, conds, expattern, stuffL, stuffM, stuffN,
     sol},
   {ll, mm, nn} = 
    Abs[Differences[Reverse@#]] & /@ 
     NormalizePartitions[{Reverse@llIn, mmIn, nnIn}];
   n = Length@ll;
   
   expattern = Table[x[r][c], {r, n + 2}, {c, r}];
   xvars = Join @@ expattern;
   
   (* Horizontal. *)
   
   stuffL = Table[Sum[x[r][c], {c, k}], {r, 2, n + 1}, {k, r}];
   conds[1][1] = Thread[(Last /@ stuffL) - ll];
   conds[1][2] = Flatten@stuffL;
   
   (* Down-right *)
   
   stuffM = 
    Table[Sum[x[n + 3 - c][r - c + 1], {c, k}], {r, 2, n + 1}, {k, r}];
   conds[2][1] = Thread[(Last /@ stuffM) - mm];
   conds[2][2] = Flatten@stuffM;
   
   (* Down-left *)
   
   stuffN = 
    Table[Sum[x[n + 2 + c - r][n + 3 - r], {c, k}], {r, 2, n + 1}, {k,
       r}];
   conds[3][1] = Thread[(Last /@ stuffN) - nn];
   conds[3][2] = Flatten@stuffN;
   
   conds[-1] = 
    Thread[Flatten[{conds[1][1], conds[2][1], conds[3][1]}] == 0];
   conds[0] = 
    Thread[Flatten[{conds[1][2], conds[2][2], conds[3][2]}] >= 0];
   conds[-2] = {x[1][1] == 0, x[n + 2][1] == 0, 
     x[n + 2][n + 2] == 0};
   sol = Reduce[ 
     And @@ conds[-2] && And @@ conds[-1] && And @@ conds[0], 
     Flatten@expattern, Integers];
   
   If[sol === False, {}, BZPattern[expattern] /. List[ToRules[sol]]]
   
];


(* This requires that all patterns have same dimensions. *)
BZPlus[bzps_BZPattern]:=bzps;
BZPlus[bzps__BZPattern]:=Module[{bzList},
	bzList = List@bzps;
	bzList = #[[1]]& /@ bzList;
	BZPattern@Fold[MapThread[Plus,{#1,#2}, 1 ]&,First@bzList,Rest@bzList]
];





(* Generate all GOG-GT-patterns as in arxiv: 1401.6516 *)
(* First row is always n, n-1, n-2,..., n-k+1, and rows strictly increasing. *)
GogPatterns[n_Integer]:=GogPatterns[n,n];
GogPatterns[n_Integer,k_Integer]:=Module[{w, h,gtp,bddconds,specConds,sol, ineqs},
	
	w = k;
	h = n+1;
	
	gtp = Table[x[r][c], {r, h, 1, -1}, {c, w}]; (* The GT-pattern *)
	
	(* Boundary condition, top & bottom *)
	bddconds = And @@ Table[ x[h][c] == n-c+1 && x[1][c] == 0, {c, w}];
	
	(* To ensure that completion is possible. *)
	specConds = And @@ Table[ x[r][w] >= r-w, {r,w+1,h}];
	
	(* Inequalities that must hold. *)
	ineqs = And @@ Flatten[Table[
		If[r < h, x[r + 1][c] >= x[r][c], True] &&
		If[r < h && c < w, x[r][c] >= x[r + 1][c + 1], True] 
		&& If[ c<r-1 && c < w, x[r][c] > x[r][c+1], True] (* Spec condition. *)
		&& If[r > 1 && c<r, x[r][c] >=1, True] (* Only positive integers! *)
		, {r, h}, {c, w}]];
		
	sol = Reduce[bddconds && ineqs && specConds, Flatten@gtp, Integers];
	
	If[sol===False, Return[{}]];
	
	sol = gtp /. List[ToRules[sol]];
	Return[GTPattern/@sol];
];


MagogPatterns[n_Integer]:=MagogPatterns[n,n];
MagogPatterns[n_Integer,k_Integer]:=Module[{w, h,gtp,bddconds,sol, ineqs, specConds},
	w = k;
	h = n+1;
	gtp = Table[x[r][c], {r, h, 1, -1}, {c, w}];
	
	(* Boundary condition, bottom only *)
	bddconds = And @@ Table[ x[1][c] == 0, {c, w}];
	specConds= And @@ Table[ x[r][1]<=r-1, {r,2,h}];
	
	(* Inequalities that must hold. *)
	ineqs = And @@ Flatten[Table[
		If[r < h, x[r + 1][c] >= x[r][c], True] &&
		If[r < h && c < w, x[r][c] >= x[r + 1][c + 1], True] &&
		If[r > 1 && c<r, x[r][c] >=1, True] (* Only positive integers! *)
		, {r, h}, {c, w}]];
		
	sol = Reduce[bddconds && ineqs && specConds, Flatten@gtp, Integers];
	
	If[sol===False, Return[{}]];
	
	sol = gtp /. List[ToRules[sol]];
	Return[GTPattern/@sol];
];


(* Pads pattern with extra rows and columns, so that it matches given dimensions. *)
GTPad[GTPattern[gtp_], rows_Integer,cols_Integer]:=Module[{gr,gc,gtout},
	{gr,gc} = Dimensions[gtp];
	gtout = Join[ConstantArray[gtp[[1]], rows - gr ],gtp];
	gtout = PadRight[#, cols]& /@ gtout;
	GTPattern[gtout]
];



GTPattern/:Dimensions[GTPattern[gtp_]]:=Dimensions[gtp];

(*SetAttributes[GTPlus,{Flat,OneIdentity}];*)
GTPlus[gtps_GTPattern]:=gtps;
GTPlus[gtps__GTPattern]:=Module[{gtList,dimList,r,c},
	gtList = List@gtps;
	dimList = Transpose[Dimensions/@gtList];
	{r,c} = {Max@First@dimList,Max@Last@dimList};
	
	(* Pad and strip head *)
	gtList = GTPad[#,r,c][[1]] &/@gtList;
	
	GTPattern@Fold[MapThread[Plus,{#1,#2},2]&,First@gtList,Rest@gtList]
];

UnitTest[GTPlus]:=Module[{g1,g2,g3},
	g1 = GTPattern[{{5,3,1},{4,2,0},{4,1,0}}];
	g2 = GTPattern[{{8,2,1},{6,2,0},{2,0,0}}];
	g3 = GTPattern[{{13,5,2},{10,4,0},{6,1,0}}];
	Return[g1+g2==g3 && GTPlus[g1]==g1];
];

(* Define elementwise multiplication. *)
(* No, don't do this, we wish to have an algebra. *)
(*
GTPattern/:Times[n_,GTPattern[gtp_]]:=GTPattern[n*gtp];
*)


(* Algebra relation. *)
GTPattern/:Times[gtp1_GTPattern, gtp2_GTPattern]:=GTPlus[gtp1,gtp2];
GTPattern/:Power[gtp1_GTPattern, n_Integer]:=Nest[GTPlus[gtp1,#]&,gtp1,n-1];


SkewQ[GTPattern[gtp_]]:=Total[Last@gtp]>0;


GTPatternPad[gtp_]:=Module[{h},
	h = Max@Dimensions[gtp];
	Return[PadRight[#,h]&/@gtp ];
];

GTPatternPad[GTPattern[gtp_]]:=GTPatternPad[gtp];


(* Convert between coordinate systems. *)
GTIndexToGrahphicsCoordinates[{r_, c_}] := {2 c + r, 1 - r};


(* Construct polygon that bounds a connected component in a GT-pattern. *)
ConnectedComponentPolygon[component_List]:=Module[{LinesFromPt,lines,dirs,segments,polygon},
	(*DL*)	dirs[0]={1,-1};
	(*UL*)	dirs[1]={1,0};
	(*UR*)	dirs[2]={-1,1};
	(*DR*)	dirs[3]={-1,0};
	
	dirs[n_]:=dirs[Mod[n,4]];
	
	LinesFromPt[pt_]:=Table[
		If[!MemberQ[component,pt+dirs[d]], {pt + 1/2 dirs[d] + 1/2 dirs[d-1], pt+1/2 dirs[d]+1/2 dirs[d+1]  },Sequence@@{}],
	{d,0,3}];
	
	segments = Join@@(LinesFromPt/@component);
	
	(* Here, do some magic to put segments together *)
	polygon = First@ReplaceRepeated[segments,
	{{b___,a_},rest___,{a_,c___},rest2___}:>{rest,rest2,{b, a, c}}];
	
	Map[GTIndexToGrahphicsCoordinates,polygon]
];


(* Helper function for extracting text elements for GT-Patterns *)
GTPatternTextLabels[GTPattern[gtp_], useSkew_:True] := Module[
	{gtp2, h, textLabels, cols, freeTiles, fixedTiles},
	
	h = Length[gtp];
	cols = Length@First@gtp;
	gtp2 = GTPatternPad@gtp;
	
	(* Extract text coordinates. *)
	
	textLabels = If[!useSkew,
		(* Non-skew version *)
		Table[ GTIndexToGrahphicsCoordinates[{r, c}] -> gtp2[[r, c]], {r, h-1}, {c, h-r}]
		,
		Table[ GTIndexToGrahphicsCoordinates[{r, c}] -> gtp2[[r, c]], {r, h}, {c, cols}]
	];
	
	(* Return text positions *)
	textLabels
];


(* Helper function for creating tile elements for GT-Patterns *)
GTPatternTiles[GTPattern[gtp_], useSkew_:True] := Module[
	{gtp2, h, textLabels, cols, freeTiles, fixedTiles},
	
	h = Length[gtp];
	cols = Length@First@gtp;
	
	{freeTiles, fixedTiles} = GTTiles[GTPattern[gtp], EnableSkew->useSkew];
	
	(* Return the polygons for the tiles. *)
	{ConnectedComponentPolygon/@freeTiles, ConnectedComponentPolygon/@fixedTiles}
];



(* Extract snakes. *)
GTSnakes[GTPattern[gtp_]] := Module[
	{gtp2, h, w, isSkew, isInside, FloodFillExtractSnake, TAKEN, snakes={}},
	
	{h, w} = Dimensions[gtp];
	isSkew = SkewQ[GTPattern[gtp]];
	
	If[!isSkew,
		(* Non-skew version. *)
		h--;
		isInside[r_Integer,c_Integer]:=(1 <= r  <= h && 1 <= c  <= h-r+1);
	,
		isInside[r_Integer,c_Integer]:=(1 <= r  <= h && 1 <= c  <= w);
	];
	
	(* Returns the snake that starts in {sr, sc} *)
	FloodFillExtractSnake[gt_List, {r_Integer, c_Integer}] :=Module[{},
		Which[
			(* Down-Left *)
			isInside[r+1,c-1] && gt[[r+1,c-1]]=!=TAKEN && gt[[r+1,c-1]]==gt[[r,c]],
				Prepend[FloodFillExtractSnake[gt, {r+1,c-1}], {r,c}],
			(* Down-Right *)
			isInside[r+1,c] && gt[[r+1,c]]=!=TAKEN && gt[[r+1,c]]==gt[[r,c]],
				 Prepend[FloodFillExtractSnake[gt, {r+1,c}], {r,c}],
			True, 
				{{r,c}}
		]
	];
	
	(* Remove snakes, one by one, by sampling unvisited pts. *)
	pts = Join @@ Table[ If[isInside[r,c],{r, c},Sequence@@{}] , {r, h}, {c, w}];
	
	gtp2 = gtp;
	
	While[Length@pts > 0,
		snake = FloodFillExtractSnake[gtp2, First@pts];
		gtp2 = ReplacePart[gtp2,snake -> TAKEN];
		AppendTo[snakes, snake];
		pts = Complement[pts, snake];
	];
	
	snakes
];



(* Returns a Graphics representation of a GTpattern. *)
Options[GTPatternForm]={EnableSkew->Automatic, GTPartition->None};
GTPatternForm::skew="The GT-pattern is a skew pattern.";
GTPatternForm[GTPattern[gtp_],OptionsPattern[]] := Module[
	{skew, skewAlt, textLabels, theLabels, freeTilePolys, fixedTilePolys, polygons},

	skew = SkewQ[GTPattern@gtp];
	
	If[ (OptionValue[EnableSkew]===Automatic && skew) || OptionValue[EnableSkew]===True,
		skewAlt = True;
	];
	
	If[ (OptionValue[EnableSkew]===Automatic && !skew) || OptionValue[EnableSkew]===False,
		skewAlt = False;
	];
	
	If[ (OptionValue[EnableSkew]===False && skew),
		skewAlt = False;
		Message[GTPatternForm::skew];
	];
	
	(* Get raw data *)
	textLabels = GTPatternTextLabels[GTPattern@gtp, skewAlt];
	
	{freeTilePolys, fixedTilePolys} = GTPatternTiles[ GTPattern@gtp, skewAlt ];
	
	theLabels = textLabels /.  Rule[coord_,val_] :> Text[val,coord];
	
	polygons = 
	Which[
		OptionValue[GTPartition] === None, {},
		OptionValue[GTPartition] === ShadedTiles, 
			{{LightGray,EdgeForm[Black],Polygon/@fixedTilePolys},{Black, Line/@freeTilePolys}},
		OptionValue[GTPartition] === Tiles,  {Line/@fixedTilePolys, Line/@freeTilePolys},
		OptionValue[GTPartition] === Snakes,  {Line/@(ConnectedComponentPolygon/@ GTSnakes[GTPattern@gtp] ) },
		True, {}
	];
	
	Graphics[{polygons,{Black,theLabels}}]
];



Options[GTPatternTikz]={EnableSkew->Automatic, GTPartition->None};
GTPatternTikz::skew="The GT-pattern is a skew pattern.";
GTPatternTikz[GTPattern[gtp_],OptionsPattern[]] := Module[
	{skew, skewAlt, textLabels, theLabels, freeTilePolys, fixedTilePolys, polygons},
	
	skew = SkewQ[GTPattern@gtp];
	
	If[ (OptionValue[EnableSkew]===Automatic && skew) || OptionValue[EnableSkew]===True,
		skewAlt = True;
	];
	If[ (OptionValue[EnableSkew]===Automatic && !skew) || OptionValue[EnableSkew]===False,
		skewAlt = False;
	];
	If[ (OptionValue[EnableSkew]===False && skew),
		skewAlt = False;
		Message[GTPatternTikz::skew];
	];
	
	tikzPoly[pts_List, colorEdge_:"black",colorFill_:None]:=Module[{start,end,coords,mid},
		start = If[colorFill=!=None,
			StringJoin["\\filldraw[color=",colorEdge,",fill=",colorFill,"] "],
			StringJoin["\\draw[",colorEdge,"] "]
		];
		end = "--cycle;";
		coords=Map[ToString@InputForm[#]&,pts,{2}] /. {a_,b_}:>StringJoin["(",a  ,",",b,")"];
		mid = StringJoin@@Riffle[coords,"--"];
		
		StringJoin[start,mid,end,"\n"]
	];
	
	tiksText[txt_String,{x_,y_}]:=
		StringJoin["\\node at (", ToString@InputForm@x,",",ToString@InputForm@y,") {$",txt,"$};\n"];
	
	(* Get raw data *)
	textLabels = GTPatternTextLabels[GTPattern@gtp, skewAlt];
	{freeTilePolys, fixedTilePolys} = GTPatternTiles[ GTPattern@gtp, skewAlt ];
	
	theLabels = textLabels /. Rule[coord_,val_] :> tiksText[ToString@TeXForm@val,coord];
	
	polygons = 
	Which[
		OptionValue[GTPartition] === None, {},
		OptionValue[GTPartition] === ShadedTiles, 
			Join[	tikzPoly[#,"black"]&/@freeTilePolys,
				tikzPoly[#,"black","lightgray"]&/@fixedTilePolys],
		OptionValue[GTPartition] === Tiles,  tikzPoly[#,"black"]&/@Join[fixedTilePolys,freeTilePolys],
		OptionValue[GTPartition] === Snakes,  tikzPoly[#,"black"]&/@(ConnectedComponentPolygon/@ GTSnakes[GTPattern@gtp]),
		True, {}
	];
	
	StringJoin["\\begin{tikzpicture}[scale=0.4]\n",StringJoin@@polygons, StringJoin@@theLabels,"\\end{tikzpicture}\n"]
];



(* TeX form of GT-patterns.*)
(* TODO: Make sure to handle non-skew properly. *)
Options[GTPatternTexForm]={LineBreaks->True, UseArray->False, EnableSkew->Automatic};
GTPattern/:TeXForm[GTPattern[gtlists_], opts:OptionsPattern[]]:=Module[
	{gtlistsStrings,riffled,add,texTable,texString, lb = ""},
	
	
	If[OptionValue[GTPatternTexForm, List[opts], LineBreaks], lb="\n";];
	
	gtlistsStrings = gtlists /. n_Integer :> ToString[n];
	
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

(* This only works for non-skew GT-patterns. *)
(* TODO: Make sure to handle skew properly, by padding. *)
SageForm::skew="The GT-pattern is a skew pattern."
SageForm[GTPattern[gtp_]]:=Module[{array,h,rows},
	(* Check if we have non-skew, so raise error message. *)
	
	If[ SkewQ[GTPattern[gtp] ],
		Message[SageForm::skew];
		Return[];
	];
	
	(* Remove parts not needed. *)
	array = Most[gtp];
	
	h = Length[array];
	rows = Table[ StringJoin[ "[", Riffle[ToString/@array[[r]][[1;;h-r+1]] , ","], "]" ], {r,h}];
	StringJoin["[",Riffle[rows,","],"]"]
];


GTTiles::skew="The GT-pattern is a skew pattern."
Options[GTTiles] := {EnableSkew->Automatic};
(* If skew is not enabled, we only select the points that corresponds to the non-skew part. *)
GTTiles[GTPattern[gtpIn_],OptionsPattern[]] := Module[
	{FloodFillExtractTile, isInside, gtp,h, w, isSkew, pts, tile, tiles = {},freeTiles},
	gtp = gtpIn;
	
	{h, w} = Dimensions[gtp];
	
	isSkew = SkewQ[GTPattern[gtp]];
	
	If[OptionValue[EnableSkew] === False && isSkew,
		Message[GTTiles::skew];
		Return[{}];
	];
	
	If[!isSkew && OptionValue[EnableSkew]=!= True,
		(* Non-skew version. *)
		h--;
		gtp = GTPatternPad@gtp;
		isInside[r_Integer,c_Integer]:=(1 <= r  <= h && 1 <= c  <= h-r+1);
	,
		isInside[r_Integer,c_Integer]:=(1 <= r  <= h && 1 <= c  <= w);
	];

	
	(* Returns the tile that contains {sr, sc} *)
	FloodFillExtractTile[gt_List, {sr_Integer, sc_Integer}] := Module[{checkPt, r, c, toExplore, visited = {}},
		toExplore = {{sr, sc}};
		(* If point r+rd,c+dc is good to explore. *)

		checkPt[r_, c_, dr_,dc_] := ( isInside[r+dr,c+dc] &&
			gt[[r + dr, c + dc]] == gt[[r, c]] && 
			!MemberQ[visited, {r + dr, c + dc}] &&
			!MemberQ[toExplore, {r + dr, c + dc}]);
		
		(* Here, recursive search. *)
		While[Length@toExplore > 0,
			{r, c} = Last@toExplore;(* Pop *)
			toExplore = Most[toExplore];
			AppendTo[visited, {r, c}];
			
			(* Down-left *)
			If[checkPt[r, c, 1, -1], AppendTo[toExplore, {r + 1, c - 1}]];
			(* Down-right*)
			If[checkPt[r, c, 1, 0], AppendTo[toExplore, {r + 1, c}]];
			(* Up-right  *)
			If[checkPt[r, c, -1, 1], AppendTo[toExplore, {r - 1, c + 1}]];
			(* Up-left   *)
			If[checkPt[r, c, -1, 0], AppendTo[toExplore, {r - 1, c}]];
		];
		Return@visited;
	];
	(* Remove components, one by one, by sampling unvisited pts. *)
	
	pts = Join @@ Table[ If[isInside[r,c],{r, c},Sequence@@{}] , {r, h}, {c, w}];
	
	While[Length@pts > 0,
		tile = FloodFillExtractTile[gtp, First@pts];
		AppendTo[tiles, tile];
		pts = Complement[pts, tile];
	];

	(* If we do not want all tiles, we only want the free tiles. *)
	
	freeTiles = Select[tiles, Min[First/@#]>1 && Max[First/@#]<h & ];
	 
	(* Return a pair, the free tiles, and the non-free tiles. *)
	{freeTiles, Complement[tiles,freeTiles]}
];

(* Returns the tiling matrix as in McAllister. *)
TilingMatrix[gtp_GTPattern] := Module[{tileToVector, freeTiles, sparse},
	freeTiles = First@GTTiles[gtp];
	
	(* Note that we adjust the coordinates a bit. *)
	tileToVector[tile_, {idx_}] := Module[{levels, tl, sparseData},
		tl = Tally@Map[First, tile, {1}];
		sparseData = {#1 - 1, idx} -> #2 & @@@ tl;
		Return@sparseData;
	];
	If[Length@freeTiles == 0, Return@{{}}];
	
	sparse = Join @@ MapIndexed[tileToVector, freeTiles];
	Return@Normal[SparseArray[sparse]];
];

ContainingFaceDimension[gtp_GTPattern] := Module[{tm},
	tm = TilingMatrix[gtp];
	Return[Length[First[tm]] - MatrixRank[tm]];
];


BoxCountMatrix[GTPattern[gtp_]]:=Module[{maxBox,gtpList,gtw,theTable},
	
	gtpList = gtp;
	
	(* Largest number in a box that appears. *)
	maxBox = Length[gtpList]-1;

	gtw = Length@First@gtpList;
	
	(* Append an extra row of zeros. *)
	gtpList = Append[gtpList, ConstantArray[0,gtw]];
	
	gtpList = Reverse[gtpList];
	
	theTable = Table[ gtpList[[m+2]]-gtpList[[m+1]] ,{m,maxBox}];
	
	Transpose@theTable
	
];


(************************* Constructing Young Diagrams **********************)

(* Converts a GTPattern to the corresponding Young tableau. *)
YoungTableau[gtp_GTPattern]:=Module[{gtpList,gtw,maxBox,specList,theTable,tableau},
	
	gtpList = First[gtp];
	
	(* Largest number in a box that appears. *)
	maxBox = Length[gtpList]-1;
	
	specList = Prepend[Range[maxBox],SKEW];
	
	gtw = Length@First@gtpList;
	
	(* Append an extra row of zeros. *)
	gtpList = Append[gtpList, ConstantArray[0,gtw]];
	
	gtpList = Reverse[gtpList];
	
	(* Create boxes *)
	theTable = Table[ ConstantArray[ specList[[m+1]], #] & /@ (gtpList[[m+2]]-gtpList[[m+1]])   ,{m,0,maxBox}];
	
	tableau = Map[Join@@#&,Transpose@theTable];
	
	YoungTableau[ tableau ]
];


(* Transposing a Young tableau. Note that the result is in general NOT a Young tableau! *)
YoungTableau/:Transpose[YoungTableau[tableau_]]:= Module[{pad, fill, transposed},
	pad = Length@First@tableau;
	transposed = Transpose[PadRight[#, pad, fill] & /@ tableau];
	transposed = DeleteCases[transposed, fill, {2}];
	YoungTableau[transposed]
];



Options[YoungTableauForm]={ItemSize->1, DescentSet->False};

YoungTableauForm[gtp_GTPattern,opts:OptionsPattern[]]:=YoungTableauForm[YoungTableau@gtp,opts];

YoungTableauForm[part:IntegerList,opts:OptionsPattern[]]:=YoungTableauForm[ToTableauShape[part],opts];

YoungTableauForm[part:IntegerList, part2:IntegerList,opts:OptionsPattern[]]:=YoungTableauForm[ToTableauShape[part,part2],opts];

YoungTableauForm[TableauShape[lambda_,mu_,w_],opts:OptionsPattern[]]:=
	YoungTableauForm[YoungTableau@Table[ If[ k <= mu[[r]], SKEW, BOX], {r,Length@lambda}, {k,lambda[[r]]}], opts];


YoungTableauForm[diagram_List, opts:OptionsPattern[]]:=YoungTableauForm[YoungTableau@diagram, opts];

YoungTableauForm[diagramIn_YoungTableau, opts:OptionsPattern[]]:= Module[{n,diagram=First@diagramIn, posSkew, posBox,is,gridItems},
 
	is = OptionValue[ItemSize];
	
	
	gridItems = Table[
		n=diagram[[r,c]];
		Which[
			n===SKEW,
				 Item["", Frame -> {{Directive[Dashed], Black}, {Black,Directive[Dashed]}}],
			n===BOX,
				Item["", Frame -> Black],
			n===EMPTY,
				"",
			OptionValue[DescentSet]===True && c<Length[diagram[[r]]] && IntegerQ[n] && IntegerQ[diagram[[r,c+1]]] && diagram[[r,c]]<diagram[[r,c+1]],
				Item[n, Frame -> Black, Background->LightGray],
			IntegerQ[n],
				Item[n, Frame -> Black]
		]
	,{r,Length@diagram},{c,Length[diagram[[r]]]}];
	
	
	(*
	grid = Grid[diagram /. {
		SKEW -> 	Item["", Frame -> {{Directive[Dashed], Black}, {Black,Directive[Dashed]}}],
		BOX -> Item["", Frame -> Black],
		n_Integer :> Item[n, Frame -> Black],
		EMPTY -> ""
		
		}, 
		ItemSize -> {is, is},
		Spacings -> {0.1, 0.1},
		ItemStyle -> Directive[FontSize -> 16*is, If[is<1,Bold,Normal]]
	];
	*)
	
	Grid[gridItems,
		ItemSize -> {is, is},
		Spacings -> {0.1, 0.1},
		ItemStyle -> Directive[FontSize -> 16*is, If[is<1,Bold, Normal]]
	]
];

(* Formatting rule for YoungTableau Objects *)
YoungTableau/:Format[YoungTableau[tableau_]]:=YoungTableauForm[YoungTableau[tableau]];


(* TeX form of Young diagrams. *)
YoungTableauTeX[YoungTableau[diagram_]]:= Module[{str, strTbl, tex},
	strTbl = diagram /. {SKEW -> ":", BOX -> "\\hfil", EMPTY -> "", n_Integer :> ToString[n]};
	
	str = StringJoin @@ Riffle[StringJoin /@ strTbl, ","];
	tex = StringJoin["\\young(", str, ")"];
	tex
];


Options[MatrixTeXForm] = {LineBreaks->True};
MatrixTeXForm[YoungTableau[diagram_],opts:OptionsPattern[]]:= Module[{str, strTbl, strLines, texString, lb=""},
	
	If[OptionValue[LineBreaks], lb="\n";];
	
	strTbl = diagram /. {SKEW -> "", BOX -> "", EMPTY -> "", n_Integer :> ToString[n]};
	
	strLines = (StringJoin@@Riffle[#, " & "])&/@strTbl;
	
	(* Add line breaks *)
	strLines = #<>"\\\\"<>lb& /@ strLines;
	texString = StringJoin@@strLines;
	
	texString = StringJoin["\\begin{ytableau}"<>lb,texString,"\\end{ytableau}"];
	texString
];


Options[YoungTableauTexForm]={LineBreaks->True, UseArray->False};
YoungTableau/:TeXForm[YoungTableau[diagram_],opts:OptionsPattern[]]:= Module[{str, strTbl, tex},
	
	If[OptionValue[YoungTableauTexForm, List[opts], UseArray], 
	 
		MatrixTeXForm[YoungTableau[diagram], FilterRules[List[opts], Options[MatrixTeXForm]] ]
	,
		YoungTabTeXForm@YoungTableau[diagram]
	]
];



(************************* Non-intersecting Lattice Paths **********************)


(* Internal method. *)
LatticePathsLines[tableau_YoungTableau] := Module[
	{ttableau, columnBoxes, columnSkewCount, startx, maxBox, xcoords, theLines},
	
	ttableau = First@Transpose@tableau;
	columnBoxes = Select[#, IntegerQ]& /@ ttableau;
	columnSkewCount = Count[#, SKEW] & /@ ttableau;
	
	(* Where each lattice path starts. *)
	startx = 2 (Range[Length@columnBoxes] - columnSkewCount);
	
	maxBox = Max[0,columnBoxes];
	
	(* Create x-coordiantes for each path. *)
	(* Default direction is RIGHT, +1, (no box present.) *)
	xcoords = 
		Accumulate[
			Normal[(SparseArray[Thread[# + 1 -> -1], {maxBox + 1}, 1])]] & /@ columnBoxes;
	
	xcoords = MapThread[#1 + #2 &, {xcoords, startx}];
	theLines = Transpose[{#, 1 - Range[maxBox + 1]}] & /@ xcoords;
	
	theLines
];


LatticePathTikz[gtp_GTPattern]:=LatticePathTikz[YoungTableau[gtp]];

LatticePathTikz[tableau_YoungTableau]:=Module[{theLines,toTikzLine,tikzstr,helpLines,ymin,xmin,xmax},
	
	theLines = LatticePathsLines[tableau];
	
	toTikzLine[line_]:=StringJoin@@ Flatten[{"\\path[draw] ",Riffle[{"(",ToString[#1],",",ToString[#2],")"}& @@@line," -- "],";\n"}];
	
	tikzstr = StringJoin@@(toTikzLine/@theLines);
	
	ymin = Min[Last/@First[theLines]];
	xmin = Min[First/@First[theLines]];
	xmax = Max[First/@Last[theLines]];
	
	helpLines = "\\draw[help lines] ("<>ToString[xmin]<>", "<>ToString[ymin]<>") grid ("<>ToString[xmax]<>",0);\n";
	
	tikzstr = "\\begin{tikzpicture}[thick,scale=0.3]\n"<>helpLines<>tikzstr<>"\\end{tikzpicture}\n";
	
	Return[tikzstr];
];




LatticePathForm[gtp_GTPattern, opts:OptionsPattern[]]:=LatticePathForm[YoungTableau[gtp],opts];

Options[LatticePathForm] = {RowLabels -> True};
LatticePathForm[tableau_YoungTableau, OptionsPattern[]] := Module[
	{maxBox=0, minX=0, maxX=0, theLines, rowLabelText = {}},
	
	theLines = LatticePathsLines[tableau];
	
	If[Length@Flatten@theLines>0,
		minX = Min[First/@First[theLines]];
		maxX = Max[First/@Last[theLines]];
		maxBox = Length[First@theLines]-1;
		
		If[OptionValue[RowLabels],
			With[{rowLabelX = minX},
				rowLabelText = Table[
					Text[Style[ToString@r, 12], {rowLabelX - 0.5, -r + 0.5}]
					, {r, 1, maxBox}]
			];
		];
	];
	
	Graphics[
		{{Thick, Black, Line[theLines]},{rowLabelText}},
		AspectRatio -> Automatic,
		Axes -> False,
		PlotRange -> {{minX - 1 - 0.2, maxX + 0.2}, {-(maxBox) - 0.2, 0.2}},
		GridLines -> {Range[minX, maxX], Range[0, -(maxBox + 1), -1]},
		ImageSize -> Automatic
	]
];


(************************* Characters *****************************************)


(* We construct a bipartite graph G, construct all increasing maps on this, and sum. *)
(* HERE: sigma and tau must be ordinary permutations! *)
FerayN[sigmaPerm_List, tauPerm_List, d_Integer][p_,q_] := FerayN[sigmaPerm,tauPerm,d][p,q] = Module[
	{n,sigma, tau, edges, sv, tv , inequalities, sVertices, tVertices, solutions, generalProduct, terms},
	
	(* Convert to cycle representation *)
	sigma = FullPermutationCycles[sigmaPerm];
	tau = FullPermutationCycles[tauPerm];
	
	
	
	(* Gives all directed edges in the graph. *)
	edges = Join@@Table[
				(* We only have an edge if there is a non-empty intersection. *)
				If[ Length[Intersection[ sigma[[s]], tau[[t]] ]] != 0,
					{s,t},
					Sequence@@{}
				], {s, Length@sigma}, {t, Length@tau} ];
	
	(* We now wish to assign numbers phi to the vertices, such that for each {sv,tv}, phi[sv] <= phi[tv] *)
	
	sVertices = sv/@Range[Length@sigma];
	tVertices = tv/@Range[Length@tau];
	
	(* Thus, we wish to have the following inequalities satisfied. *)
	(* The sigma-vertices are smaller than the tau-vertices. *)
	inequalities = And[ And@@( 1<=#<=d & /@ sVertices), And@@( 1<=#<=d & /@ tVertices), And@@(sv[#1]<=tv[#2] & @@@ edges) ];
	
	
	(* The general product we will be substitutiong into. *)
	generalProduct = Product[p[v],{v,sVertices}] * Product[q[v],{v,tVertices}];
	
	(* Find all solutions to the inequalities. *)
	solutions = List@ToRules[ Reduce[ inequalities, Join[sVertices, tVertices], Integers ]];
	
	(* Substitute *)
	terms = generalProduct /. solutions;
	
	Total[terms]
];



(* This works as expected. *)
(* note that the expression ch[p_,q_] returned satisfies (-1)^|mu| ch[p,-q] has non-neg coefficients. *)
ChNormalizedCharacter[mu_List,d_Integer][p_,q_]:=Module[
	{pi, permutations, permPairs, k},
	
	k = Total[mu];
	
	permutations = Permutations[ Range@k ];
	
	pi = PermutationOfType[mu];
	
	(* These are the permutations we sum over. *)
	(* {sigma, tau } *)
	permPairs = {#, PermutationProduct[InversePermutation@#, pi]} & /@ permutations;
	
	(* This should be true, all products of sigma and tau must be pi. *)
	(*
		And@@ ( pi===PermutationProduct[#1,#2] & @@@ permPairs)
	*)
	
	(* Sum over all pairs *)
	Total[  Signature[#2] * FerayN[ #1 ,#2 ,d][p,q]& @@@ permPairs  ]
];



KNormalizedCharacter[mu_List,d_Integer][p_,q_]:=Module[{rhos},
	rhos = IntegerPartitions[Tr@mu];
	
	Sum[ 
		With[{lc = LCoefficient[rho,mu] },
			If[ lc!=0, lc/ZCoefficient[rho] * ChNormalizedCharacter[rho, d][p,q], 0]
		] , {rho, rhos}]
];

(* Alternative definition. This is slower! *)
KNormalizedCharacter2[mu_List,d_Integer][p_,q_]:=Module[
	{perms,m ,ft, fs, lc, pt,rho},
	
	m = Total[mu];
	perms = Permutations[Range@m];
	
	Return@Sum[
		rho = PermutationProduct[si,ta];
		pt = PermutationType[rho];
		lc = LCoefficient[pt, mu];
		If[lc != 0,
			1/m! LCoefficient[pt, mu]  * Signature[ta] * FerayN[ si, ta, d][p,q]
			,
			0
		]
	,{ta,perms},{si,perms}];
];

(* The newest version of normalized character formula. *)
KNormalizedCharacter3[mu_, d_][p_,q_]:=Module[{perms,rhoType,sum,pi},
	
	perms = Permutations[Range[Total@mu]];
	
	pi = PermutationOfType[mu];
	
	
	(* Normalization factor. *)
	factor = 1/(Times@@(mu!));
	
	sum = factor * Sum[
		rhoType = FullPermutationCycles[ PermutationProduct[sigma, tau] ];
		
		(* Sum where the type of  sigma*tau is a set-partition refinement of the type of mu. *)
		Boole[ SetPartitionRefinementQ[ rhoType, FullPermutationCycles@pi ] ] * Signature[tau] * FerayN[ sigma, tau, d][p,q]
		
	, {sigma, perms}, {tau, perms}];
	
	sum
];


(* Another version, this has the wrong scaling, for some mu:s *)
KNormalizedCharacter4[mu_, d_][p_,q_]:=Module[{perms,rhoType,sum,pi,piPerms},
	
	perms = Permutations[Range[Total@mu]];
	
	(* Permutations of type mu. *)
	piPerms = Select[perms, Reverse@Sort[Length /@ FullPermutationCycles[#]] == mu &];
	
	
	(* Normalization factor. *)
	factor = Times@@(ToCycleForm[mu]!)/(Total[mu]! * Times@@((mu-1)!));
	sum = factor * Sum[
		rhoType = FullPermutationCycles[ PermutationProduct[sigma, tau] ];
		
		(* Sum where the type of  sigma*tau is a set-partition refinement of the type of mu. *)
		Boole[ SetPartitionRefinementQ[ rhoType, FullPermutationCycles@pi ] ] * Signature[tau] * FerayN[ sigma, tau, d][p,q]
	, {sigma, perms}, {tau, perms},{pi,piPerms}];
	Return@sum;
];



ShiftedSchur[mu_List,d_Integer][p_,q_]:=Module[{permutations, sigma,tau, k},
	
	k = Tr[mu];
	permutations = Permutations[ Range@k ];
	
	Sum[
	ChiCoefficient[mu,PermutationType@PermutationProduct[sigma, tau]]
	*Signature[tau]
	*FerayN[sigma,tau ,d][p,q]
	*someACoeff[sigma,tau]
	,{sigma, permutations},{tau, permutations}]
];



HallLittlewoodP::argument = "Warning, length of partition exceeds number of variables.";
HallLittlewoodP[lambda_, n_, x_, t_] := HallLittlewoodP[lambda, n, x, t] = Module[
    {vi, lambdaPad, normCoeff, y, expr, perms, vars},
    
    If[Length[lambda] > n, Message[HallLittlewoodP::argument]];
    
    (* As in Macdonald p. 204, but inverted. *)
    vi[m_] := Product[(1 - t)/(1 - t^i), {i, m}];
    lambdaPad = PadRight[lambda, n];
    
    (* The normalizing coefficient *)
    normCoeff = Times @@ (vi[Last[#]] & /@ Tally[lambdaPad]);
    
    perms = Permutations[Range[n]];
    vars = y /@ Range[n];
    
    expr = (Times @@ (vars^lambdaPad))*Product[ (y[i] - t*y[j])/(y[i] - y[j]), {i, n}, {j, i + 1, n}];
    
    normCoeff*Sum[ expr /. {y[i_] :> x[p[[i]]]}, {p, perms}]//Together
];


(***************************** Jack stuff ******************************************)


(* 10.11 p 379 in Macdonald.  *)

MacdonaldPsi[lambdaIn_List, muIn_List, a_] := 
  MacdonaldPsi[lambdaIn, muIn, a] = 
   Module[{lambda, mu, lambdac, muc, armL, legL, armM, legM, bM, bL},
    {lambda, mu} = NormalizePartitions[{lambdaIn, muIn}];
    {lambdac, muc} = 
     NormalizePartitions[ConjugatePartition /@ {lambdaIn, muIn}];
    
    (* Product over all skew boxes, but only some of them count;
    namely those boxes that DO have a proper box to the right, 
    but no proper box below. *)
    Product[
     If[
      mu[[r]] < lambda[[r]] && muc[[c]] == lambdac[[c]],
      armL = lambda[[r]] - c;
      legL = lambdac[[c]] - r;
      armM = mu[[r]] - c;
      legM = muc[[c]] - r;
      
      bM = ((a armM + legM + 1)/(a armM + legM + a));
      bL = ((a armL + legL + 1)/(a armL + legL + a));
      
      bM/bL
      , 1]
     
     , {r, Length@mu}, {c, mu[[r]]}]
    ];

MacdonaldPsi[GTPattern[gtp_], a_] := 
  Product[MacdonaldPsi[ gtp[[i]], gtp[[i + 1]] , a], {i, 
    Length[gtp] - 1}];
MacdonaldPsi[tab_YoungTableau, a_] := 
  MacdonaldPsi[GTPattern[tab][[1]], a];

(* Macdonald p. 340 *)

MacdonaldPsiPrime[lambdaIn_List, muIn_List, a_] := 
  MacdonaldPsi[ConjugatePartition@lambdaIn, ConjugatePartition@muIn, 
   1/a];
MacdonaldPsiPrime[GTPattern[gtp_], a_] := 
  Product[MacdonaldPsiPrime[ gtp[[i]], gtp[[i + 1]] , a], {i, 
    Length[gtp] - 1}];
MacdonaldPsiPrime[tab_YoungTableau, a_] := 
  MacdonaldPsiPrime[GTPattern[tab][[1]], a];


(* OO HookFactor but changed!! multiplied with a^|mu| *)

HookPrimeFactor[mu_List, a_: 1] := Module[{muc, arm, leg, r, c},
   muc = ConjugatePartition@mu;
   Product[
    arm = mu[[r]] - c;
    leg = muc[[c]] - r;
    (a * arm +  leg + a)
    , {r, Length@mu}, {c, mu[[r]]}]
   ];
(* Agree with c_mu(alpha) in stanley *)

HookFactor[mu_List, a_: 1] := Module[{muc, arm, leg, r, c},
   muc = ConjugatePartition@mu;
   Product[
    arm = mu[[r]] - c;
    leg = muc[[c]] - r;
    (a*arm + leg + 1)
    , {r, Length@mu}, {c, mu[[r]]}]
   ];
JackNorm[mu_List, a_: 1] := HookFactor[mu, a]*HookPrimeFactor[mu, a];

JackPPolynomial[mu_List, k_Integer, x_, a_] := JackPPolynomial[mu, k, a] = Module[{gtps, t, arm, leg},
	gtps = GTPatterns[mu, {}, k];
	Sum[
		t = First@(YoungTableau[g] /. {m_Integer :> k - m + 1});
		MacdonaldPsi[g, a]*
		Product[  x[ t[[r, c]] ] , {r, Length@t}, {c, Length@t[[r]]}]
	, {g, gtps}]
];

JackJPolynomial[mu_List, k_Integer, x_, a_] := HookFactor[mu,a]*JackPPolynomial[mu,k,x,a];

(* Combinatorial formula from Okonukov and Olshanski *)
(* This returns the JackPoly in k variables *)
(* The leading term for a=1 should be the Schur polynomial. *)
(* Multiply with HookPrimeFactor[mu,a] in order to recover Lasalle normalization. *)

ShiftedJackPPolynomial[mu_List, k_Integer, x_, a_] := 
  ShiftedJackPPolynomial[mu, k, a] = Module[{gtps, t, arm, leg},
    gtps = GTPatterns[mu, {}, k];
    Sum[
     t = First@(YoungTableau[g] /. {m_Integer :> k - m + 1});
     MacdonaldPsi[g, a]*
      Product[
        x[ t[[r, c]] ] -  (c - 1) + (r - 1)/a  (* 2.4 OO- 
       Shifted Jack + 
       binom *)
       , {r, Length@t}, {c, Length@t[[r]]}]
     , {g, gtps}]
    ];

ShiftedJackJPolynomial[mu_List, k_Integer, x_, a_]:= HookFactor[mu,a]*ShiftedJackPPolynomial[mu,k,x,a];

ShiftedJackPEvaluate[mu_List, nu_List, a_] := ShiftedJackPEvaluate[mu, nu, a] = Module[{poly,x},
    poly = ShiftedJackPPolynomial[mu, Length[nu], x,  a];
    poly /. x[i_] :> nu[[i]]
];

ShiftedJackJEvaluate[mu_List, nu_List, a_] := HookFactor[mu,a]*ShiftedJackPEvaluate[mu, nu, a];


(* Note, nu is the "top" *)
JackPStructureConstant[lambda_List, mu_List, nu_List,a_] := 
	JackPStructureConstant[lambda, mu, nu, a] = Module[{},
	Which[
		! (SkewShapeQ[nu, mu] && SkewShapeQ[nu, lambda]), 0,
		(* Interchange accoring to size. *)
		Tr[lambda] > Tr[mu], 
		JackPStructureConstant[mu, lambda, nu, a],
		(* Make sure all partitions have the same length. *)
		! \
	(Length[lambda] == Length[mu] == Length[nu]), 
		JackPStructureConstant[#1,#2,#3,a]& @@ NormalizePartitions[{lambda, mu, nu}],
		(* Boundary condition *)
		(* THIS IS THE BASE CASE! *)
		
		mu == nu, ShiftedJackPEvaluate[lambda, nu, a],
		lambda == nu, ShiftedPackJEvaluate[mu, nu, a],
		(* Recursion *)
		True,
		1/(Tr@nu - Tr@mu) (
		Sum[
			MacdonaldPsiPrime[mup, mu, a] JackPStructureConstant[lambda,mup, nu,a], 
			{mup, AddBoxToPartition[mu]}] -
		Sum[
			MacdonaldPsiPrime[nu, num, a] JackPStructureConstant[lambda, mu, num,a], 
			{num, RemoveBoxFromPartition[nu]}])]
		];
		
(* The J-Version of the LRCoeffs *)
JackJStructureConstant[lambda_List, mu_List, nu_List,a_] := JackJStructureConstant[lambda, mu, nu,a] = Factor[
    HookFactor[lambda, a] HookFactor[mu, a] HookPrimeFactor[nu,a] JackPStructureConstant[lambda, mu, nu,a]];







(************************* The unit test function *********************)


(** UnittestPackage runs the unit test on every function that has a unittest defined. **)
UnitTestPackage[] := Module[{args, failed},
	args = Cases[DownValues[UnitTest], UnitTest[a_] :> a, 3];
	failed = Select[args, UnitTest[#] == False &];
	If[Length[failed] > 0, Print["UnitTests failed: ", failed]];
	
	Length[failed] == 0
];



(************************* Run the unit tests *********************************************)

(* Run the unit tests *)

(*
Print["Running unittests..."];
UnitTestPackage[];
Print["Done"];
*)


End[(* End private *)];
EndPackage[];


