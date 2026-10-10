testRoot = DirectoryName[DirectoryName[$InputFileName]];
If[!MemberQ[$Path, testRoot], PrependTo[$Path, testRoot]];

VerificationTest[
  Needs["PosetData`"],
  Null,
  TestID -> "PosetData-loads-cleanly"
]

(* GitHub issue #18: relabel before applying the descent formula. *)
VerificationTest[
  OrderPolynomial[Poset[3, {{2, 1}, {3, 1}}], 3],
  14,
  TestID -> "PosetData-OrderPolynomial-non-natural-labeling"
]

posetBruteCount[Poset[n_, edges_], t_] :=
  Count[Tuples[Range[t], n], v_ /;
    And @@ ((v[[#[[1]]]] <= v[[#[[2]]]]) & /@ edges)];

(* GitHub issue #18: compare all listed 3- and 4-element data with brute force. *)
VerificationTest[
  And @@ Flatten[Table[
    OrderPolynomial[p, t] == posetBruteCount[p, t],
    {p, Join[GetPosets[3], GetPosets[4]]}, {t, 1, 3}]],
  True,
  TestID -> "PosetData-OrderPolynomial-brute-force"
]

(* GitHub issue #18: the Stembridge example uses all 17 declared vertices. *)
VerificationTest[
  First[StembridgePoset[]] == Max[Flatten[Last[StembridgePoset[]]]],
  True,
  TestID -> "PosetData-StembridgePoset-vertex-count"
]

(* GitHub issue #18: the bundled data are connected posets for k=1,...,7. *)
VerificationTest[
  Length /@ (GetPosets /@ Range[1, 7]),
  {1, 1, 3, 10, 44, 238, 1650},
  TestID -> "PosetData-GetPosets-connected-counts"
]

VerificationTest[
  GetPosets[8],
  Missing["NotAvailable", 8],
  TestID -> "PosetData-GetPosets-unavailable"
]
