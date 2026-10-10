testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

(* Contract tests for CONVENTIONS.md (issue #51): objects produced by one package are
   accepted by the others, and the same quantity computed through different packages
   agrees. Further bridges are added as the legacy port proceeds:
   TODO P2: polynomial <-> symmetric/quasisymmetric function round trips;
   s_lam = sum over SYT of fundamental quasisymmetric functions.
   TODO P3/P5: key and atom polynomials versus augmented fillings in NewTableaux. *)

VerificationTest[
  Scan[Needs, {"CombinatoricTools`", "NewTableaux`", "SymmetricFunctions`", "GTPatterns`",
    "PermutationTools`", "CatalanObjects`", "UnicellularChromatics`", "GraphTools`"}],
  Null,
  TestID -> "Compatibility-packages-load-together"
]

(* Kostka numbers agree across CombinatoricTools, NewTableaux, GTPatterns and
   SymmetricFunctions. *)
VerificationTest[
  And @@ Flatten@Table[
    With[{k = KostkaCoefficient[lam, mu]},
      k === Length[SemiStandardYoungTableaux[{lam, {}}, mu]] &&
      k === Length[GTPatterns[lam, {}, mu]] &&
      k === Coefficient[ToMonomialBasis[SchurSymbol[lam]], MonomialSymbol[mu]]],
    {lam, IntegerPartitions[5]}, {mu, IntegerPartitions[5]}],
  True,
  TestID -> "Compatibility-Kostka-numbers-agree"
]

(* Tableaux from NewTableaux pass through GTPatterns and back, and are accepted by the
   NewTableaux crystal operators. *)
VerificationTest[
  With[{tabs = SemiStandardYoungTableaux[{{3, 2}, {1}}, {2, 1, 1}]},
    {And @@ (YoungTableau[GTPattern[#]] === # & /@ tabs),
     And @@ (With[{f = CrystalFi[#, 1]}, f === Undefined || CrystalEi[f, 1] === #] & /@ tabs)}],
  {True, True},
  TestID -> "Compatibility-tableaux-GTPatterns-crystals"
]

(* RSK on permutations (from Permutations) produces standard tableaux whose shapes are
   distributed as (f^lam)^2, with f^lam counted by NewTableaux and CombinatoricTools. *)
VerificationTest[
  With[{shapes = Counts[YoungTableauShape[First[BiwordRSK[#]]] & /@ Permutations[Range[5]]]},
    And @@ (Lookup[shapes, Key[#], 0] === Length[StandardYoungTableaux[#]]^2 &&
        Length[StandardYoungTableaux[#]] === SnCharacter[#, ConstantArray[1, 5]] & /@
      IntegerPartitions[5])],
  True,
  TestID -> "Compatibility-RSK-shapes-and-SYT-counts"
]

(* Area lists from CatalanObjects are accepted by UnicellularChromatics, and the area-list
   form agrees with the Graph form built from UnitIntervalEdges. *)
VerificationTest[
  And @@ Flatten@Table[
    Expand[ChromaticSymmetric[area] -
      ChromaticSymmetric[Graph[Range[Length[area]], UndirectedEdge @@@ UnitIntervalEdges[area]]]] === 0,
    {n, 1, 4}, {area, DyckAreaLists[n]}],
  True,
  TestID -> "Compatibility-area-lists-and-graphs"
]

(* Symmetric functions returned by UnicellularChromatics are SymmetricFunctions symbols:
   the unicellular LLT at q = 1 is e_1^n = h_1^n. *)
VerificationTest[
  And @@ Table[
    Expand[ToSchurBasis[UnicellularLLTSymmetric[area, 1]] -
      ToSchurBasis[CompleteHSymbol[ConstantArray[1, Length[area]]]]] === 0,
    {area, DyckAreaLists[4]}],
  True,
  TestID -> "Compatibility-LLT-symbols-are-SymmetricFunctions-symbols"
]
