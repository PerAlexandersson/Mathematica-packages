(* ::Package:: *)

(* Shared infrastructure for packages that represent algebra elements by basis symbols,
   such as SchurSymbol[lam, x] in SymmetricFunctions and FundamentalQSymbol[alpha, x] in
   QuasiSymmetricFunctions (issue #51). This is an internal package for package authors;
   its symbols are not part of the mathematical API. *)

BeginPackage["AlgebraicBases`"];

CreateBasis;
IndexType;
MultiplicationFunction;
PowerFunction;
SortFunction;

Begin["`Private`"];

CreateBasis::usage = "CreateBasis[bb, label, opts] sets up the symbol bb as a basis symbol bb[index, x] with alphabet x (default None): index normalization, formatting as label subscripted by the index, and optional automatic products and powers. Options: IndexType (\"Partition\", \"Composition\" or \"WeakComposition\"), SortFunction, MultiplicationFunction and PowerFunction. Intended for package authors.";
IndexType::usage = "IndexType is an option for CreateBasis: \"Partition\" (index sorted decreasingly, zeros removed, any negative part gives 0), \"Composition\" (zero parts removed) or \"WeakComposition\" (zeros kept, trailing zeros removed).";
MultiplicationFunction::usage = "MultiplicationFunction is an option for CreateBasis. If it is a function f, a product bb[a, x] bb[b, x] is replaced by f[a, b, x]; None leaves products unevaluated.";
PowerFunction::usage = "PowerFunction is an option for CreateBasis. \"Mult\" computes integer powers by repeated squaring with the multiplication function, a function f replaces bb[a, x]^n by f[a, n, x], and None leaves powers unevaluated.";
SortFunction::usage = "SortFunction is an option for CreateBasis. \"Standard\" normalizes indices according to IndexType; a function f is called as f[index, x] for a partition-type index that is not weakly decreasing (used by SchurSymbol for the slinky rule); None performs no reordering.";


(* Formatting: label with the index as subscript, followed by the alphabet unless it is None. *)
defineBasisFormatting[bb_, symb_String] := Module[{},

	bb /: Format[bb[a_List, x_]] := With[
	{r = If[Max[a] < 10, Row[a], Row[a, ","]]},
		If[x === None, Subscript[symb, r],
			Row[{Subscript[symb, r], "(", MakeBoxes[x], ")"}]]
	];

	bb /: HoldPattern[MakeBoxes[bb[a_List, x_], TraditionalForm]] :=
		With[{r =
			If[Max[a] <= 9, RowBox[ToString /@ a],
				RowBox[Riffle[ToString /@ a, ","]]]},
		If[x === None, SubscriptBox[symb, r],
			RowBox[{SubscriptBox[symb, r], "(", MakeBoxes[x, TraditionalForm], ")"}]]];
];


Options[CreateBasis] = {
	IndexType -> "Partition",
	MultiplicationFunction -> None,
	SortFunction -> "Standard",
	PowerFunction -> "Mult"
};

CreateBasis[bb_Symbol, symb_String, opts:OptionsPattern[]] := Module[{type, sort, mult, pow},
	type = OptionValue[IndexType];
	sort = OptionValue[SortFunction];
	mult = OptionValue[MultiplicationFunction];
	pow = OptionValue[PowerFunction];

	bb[lam_List] := bb[lam, None];
	bb[{}, x_: None] := 1;

	Switch[type,
		"Partition",
			bb[i_Integer] := Which[i > 0, bb[{i}, None], i == 0, 1, True, 0];
			bb[i_Integer, x_] := Which[i > 0, bb[{i}, x], i == 0, 1, True, 0];
			bb[{0}, x_: None] := 1;
			bb[{lam__, 0..}, x_: None] := bb[{lam}, x];
			bb[lam_List, x_: None] := 0 /; Min[lam] < 0;
			Which[
				sort === "Standard",
					bb[lam_List, x_: None] := bb[Sort[lam, Greater], x] /; Not[OrderedQ[Reverse@lam]],
				sort =!= None,
					bb[lam_List, x_: None] := (sort[lam, x]) /; Not[OrderedQ[Reverse@lam]]
			],
		"Composition",
			bb[i_Integer] := Which[i > 0, bb[{i}, None], i == 0, 1, True, 0];
			bb[i_Integer, x_] := Which[i > 0, bb[{i}, x], i == 0, 1, True, 0];
			bb[{0}, x_: None] := 1;
			bb[{lam__, 0..}, x_: None] := bb[{lam}, x];
			Which[
				sort === "Standard",
					bb[lam_List, x_: None] := bb[DeleteCases[lam, 0], x] /; Min[lam] == 0,
				sort =!= None,
					bb[lam_List, x_: None] := (sort[lam, x]) /; Min[lam] == 0
			],
		"WeakComposition",
			bb[{0 ..}, x_: None] := 1;
			bb[{lam__, 0..}, x_: None] := bb[{lam}, x],
		_,
			Message[CreateBasis::type, type]
	];

	If[mult =!= None,
		bb /: Times[bb[a_List, x_], bb[b_List, x_]] := mult[a, b, x];
	];

	bb /: Power[bb[a_List, x_], 0] := 1;
	bb /: Power[bb[a_List, x_], 1] := bb[a, x];

	Which[
		(* Use recursion, via multiplication. *)
		pow === "Mult",
			bb /: Power[bb[a_List, x_], 2] := mult[a, a, x];
			bb /: Power[bb[a_List, x_], n_Integer] := Expand@Which[
			EvenQ[n],
				Power[bb[a, x], n/2] * Power[bb[a, x], n/2],
			True,
				Power[bb[a, x], (n-1)/2] * Power[bb[a, x], (n-1)/2]*bb[a, x]
			];
		,
		(* Use custom power function *)
		pow =!= None,
			bb /: Power[bb[a_List, x_], n_Integer] := pow[a, n, x];
	];

	defineBasisFormatting[bb, symb];
];

CreateBasis::type = "Unknown IndexType `1`; use \"Partition\", \"Composition\" or \"WeakComposition\".";

End[];

Protect @@ Select[Names["AlgebraicBases`*"], !StringMatchQ[#, ___ ~~ "$" ~~ ___] &];

EndPackage[];
