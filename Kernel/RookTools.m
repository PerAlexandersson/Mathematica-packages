(* ::Package:: *)
BeginPackage["RookTools`",{"CombinatoricTools`","GraphTools`"}];

Unprotect["`*"];
ClearAll["`*"];

FerrersNonCrossingPlacements;
FerrersPlacementToBasis;

NonCrossingRookPolynomial;

FerrersRookPlacementPlot;
RookPlacementPlot;


Begin["`Private`"];


FerrersNonCrossingPlacements::usage = "FerrersNonCrossingPlacements[lam,mu] returns all non-crossing rook placements on the skew Ferrers board lam/mu; mu defaults to {}.";
FerrersNonCrossingPlacements[lam_List,mu_List:{}]:=GraphNonCrossingMatchings[FerrersBoardGraph[lam, mu]];


NonCrossingRookPolynomial::usage = "NonCrossingRookPolynomial[lam,t] returns the rook polynomial whose coefficient of t^k counts non-crossing placements of k rooks on the Ferrers board lam.";
NonCrossingRookPolynomial[{}, t_] := 1;
NonCrossingRookPolynomial[{m_Integer}, t_] := 1 + m t;
NonCrossingRookPolynomial[lam_List, t_] :=
  NonCrossingRookPolynomial[lam, t] = With[
    {mp = Most@lam},
    Expand[
     (* Last row is empty *)
     NonCrossingRookPolynomial[mp, t]
      +
      (* Place rook in column c, last row *)
      t*Sum[
        NonCrossingRookPolynomial[
         DeleteCases[Max[0, # - c] & /@ mp, 0], t]
        , {c, Last@lam}]
     ]
    ];


FerrersPlacementToBasis::usage = "FerrersPlacementToBasis[lam,mu,p] returns the matroid basis corresponding to the rook placement p on the skew Ferrers board lam/mu.";
FerrersPlacementToBasis[lam_List, mu_List, p_List] := Module[{rowVars, colVars},
   rowVars = Range[Length@lam];
   colVars = Length[lam] + Range[lam[[1]]];
   Join[Intersection[First /@ p, rowVars], 
    Complement[colVars, Last /@ p]]
];


FerrersRookPlacementPlot::usage = "FerrersRookPlacementPlot[lam,mu,rp] returns a graphic of the rook placement rp on the skew Ferrers board lam/mu.";
FerrersRookPlacementPlot[lam_List, mu_List, rp_List] := RookPlacementPlot[
	EdgeList@FerrersBoardGraph[lam, mu], rp, Length[lam]];

(* Squares are edges {r, nrows + c}, as produced by FerrersBoardGraph. Without nrows,
   the number of rows is taken to be the largest row index of a square. *)
RookPlacementPlot::usage = "RookPlacementPlot[boardSquares,rp] returns a graphic of a rook placement, inferring nrows from boardSquares.
RookPlacementPlot[boardSquares,rp,nrows] returns a graphic of a rook placement using the specified number of rows.";
RookPlacementPlot[boardSquares_List, rp_List] := RookPlacementPlot[boardSquares, rp,
	Max[First /@ (boardSquares /. UndirectedEdge -> List)]];

RookPlacementPlot[boardSquares_List, rp_List, nrows_Integer] := Module[{toXY, boardPts, rooks},
	boardPts = boardSquares /. UndirectedEdge -> List;
	toXY[pt_] := {#2, nrows - #1} & @@ (pt - {1, nrows + 1});
	boardPts = toXY /@ boardPts;
	rooks = toXY /@ rp;
	
	Framed@Graphics[{
		Table[{Orange,
		Rectangle[e - {0.44, 0.44}, e + {0.44, 0.44}]}, {e, boardPts}],
		Table[{Text["\[BlackRook]", e]}, {e, rooks}]
		}, ImageSize -> 50
	]
];
   
   
   
End[(* End private *)];
EndPackage[];
