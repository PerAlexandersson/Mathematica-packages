VerificationTest[
  Needs["RookTools`"],
  Null,
  TestID -> "RookTools-loads-cleanly"
]

(* GitHub issue #15: RookPlacementPlot must derive the row count from boardSquares. *)
VerificationTest[
  Module[{plot, rectangles},
    plot = RookPlacementPlot[
      {UndirectedEdge[1, 3], UndirectedEdge[2, 3]},
      {{1, 3}}
    ];
    rectangles = Cases[plot, Rectangle[_, _], Infinity];
    Length[rectangles] === 2 &&
      MemberQ[rectangles, Rectangle[{0 - 0.44, 2 - 0.44},
        {0 + 0.44, 2 + 0.44}]]
  ],
  True,
  TestID -> "RookTools-RookPlacementPlot-board-rectangles"
]

(* GitHub issue #15: on a skew board whose last row is empty, the columns must
   still be offset by the full number of rows. *)
VerificationTest[
  With[{plot = FerrersRookPlacementPlot[{2, 1}, {0, 1}, {}]},
    Sort[Cases[plot, Rectangle[a_, _] :> Round[a + 0.44], Infinity]]],
  {{0, 2}, {1, 2}},
  TestID -> "RookTools-FerrersRookPlacementPlot-skew-board-columns"
]
