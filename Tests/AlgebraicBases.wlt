VerificationTest[
  Needs["AlgebraicBases`"],
  Null,
  TestID -> "AlgebraicBases-loads-cleanly"
]

(* Issue #51, P1: one basis-symbol implementation shared by SymmetricFunctions,
   QuasiSymmetricFunctions and, later, NonsymmetricPolynomials. *)

CreateBasis[testPartitionSymbol, "b", IndexType -> "Partition",
  MultiplicationFunction -> (testPartitionSymbol[Sort[Join[#1, #2], Greater], #3] &)];
CreateBasis[testCompositionSymbol, "c", IndexType -> "Composition",
  MultiplicationFunction -> None, PowerFunction -> None];
CreateBasis[testWeakSymbol, "k", IndexType -> "WeakComposition",
  MultiplicationFunction -> None, PowerFunction -> None];

VerificationTest[
  {testPartitionSymbol[{1, 3, 0}], testPartitionSymbol[{2, -1}], testPartitionSymbol[{}],
   testPartitionSymbol[0], testPartitionSymbol[2, y]},
  {testPartitionSymbol[{3, 1}, None], 0, 1, 1, testPartitionSymbol[{2}, y]},
  TestID -> "AlgebraicBases-partition-index-normalization"
]

VerificationTest[
  {testCompositionSymbol[{1, 0, 3, 0}], testCompositionSymbol[{2, 1}],
   testWeakSymbol[{0, 2, 0, 1, 0, 0}], testWeakSymbol[{0, 0}], testWeakSymbol[{1, 0}]},
  {testCompositionSymbol[{1, 3}, None], testCompositionSymbol[{2, 1}, None],
   testWeakSymbol[{0, 2, 0, 1}, None], 1, testWeakSymbol[{1}, None]},
  TestID -> "AlgebraicBases-composition-index-normalization"
]

VerificationTest[
  {testPartitionSymbol[{2}] testPartitionSymbol[{3, 1}], testPartitionSymbol[{1}]^3,
   testPartitionSymbol[{1}, x] testPartitionSymbol[{1}, y]},
  {testPartitionSymbol[{3, 2, 1}, None], testPartitionSymbol[{1, 1, 1}, None],
   testPartitionSymbol[{1}, x] testPartitionSymbol[{1}, y]},
  TestID -> "AlgebraicBases-products-only-within-an-alphabet"
]

VerificationTest[
  {ToString[testPartitionSymbol[{3, 1}]], ToString[testWeakSymbol[{0, 1}, y]]},
  {ToString[Subscript["b", Row[{3, 1}]]], ToString[Row[{Subscript["k", Row[{0, 1}]], "(", MakeBoxes[y], ")"}]]},
  TestID -> "AlgebraicBases-formatting"
]

(* The existing algebras now build their symbols with CreateBasis: products and
   normalization are unchanged. *)
VerificationTest[
  Needs["SymmetricFunctions`"]; Needs["QuasiSymmetricFunctions`"],
  Null,
  TestID -> "AlgebraicBases-SymmetricFunctions-and-QuasiSymmetricFunctions-load"
]

VerificationTest[
  {ElementaryESymbol[{1, 2}] ElementaryESymbol[{1}], SchurSymbol[{1, 3}],
   MonomialQSymbol[{1, 0, 2}], Expand[MonomialQSymbol[{1}]^2]},
  {ElementaryESymbol[{2, 1, 1}, None], -SchurSymbol[{2, 2}, None],
   MonomialQSymbol[{1, 2}, None], MonomialQSymbol[{2}, None] + 2 MonomialQSymbol[{1, 1}, None]},
  TestID -> "AlgebraicBases-used-by-SymmetricFunctions-and-QuasiSymmetricFunctions"
]
