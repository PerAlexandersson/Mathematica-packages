VerificationTest[
  Needs["MatroidTools`"],
  Null,
  TestID -> "MatroidTools-loads-cleanly"
]

(* GitHub issue #15: list deletion must return the resulting bases, not Null. *)
VerificationTest[
  Module[{bases = UniformBases[2, 4]},
    MatroidDeletion[bases, {1, 2}] ===
      MatroidDeletion[MatroidDeletion[bases, 1], 2]
  ],
  True,
  TestID -> "MatroidTools-MatroidDeletion-list-matches-repeated-single-deletion"
]

(* GitHub issue #15: basis exchange must be checked in both directions and ranks must agree. *)
VerificationTest[
  IsMatroidQ[{{1, 2}, {1, 3}, {2, 3}, {2, 4}}],
  False,
  TestID -> "MatroidTools-IsMatroidQ-rejects-one-way-exchange-counterexample"
]

(* GitHub issue #15: compare the implementation with an independent exchange check
   on every set system of 2-subsets of a four-element ground set. *)
VerificationTest[
  Module[{allBases, systems, exchangeQ},
    allBases = Subsets[Range[4], {2}];
    systems = Subsets[allBases];
    exchangeQ[bb_] :=
      bb =!= {} &&
        Length[Union[Length /@ bb]] === 1 &&
        And @@ Flatten[
          Table[
            And @@ Table[
              Or @@ (MemberQ[bb, Sort[Join[Complement[a, {x}], {#}]]] & /@
                Complement[b, a]),
              {x, Complement[a, b]}],
            {a, bb}, {b, bb}
          ]
        ];
    And @@ Table[IsMatroidQ[systems[[i]]] === exchangeQ[systems[[i]]],
      {i, Length[systems]}]
  ],
  True,
  TestID -> "MatroidTools-IsMatroidQ-agrees-with-brute-force-2-subset-check"
]

(* GitHub issue #15: contracting a dependent set of all elements leaves the empty basis. *)
VerificationTest[
  MatroidContraction[UniformBases[1, 2], {1, 2}],
  {{}},
  TestID -> "MatroidTools-MatroidContraction-dependent-set-returns-empty-basis"
]

(* GitHub issue #15: set contraction agrees with iterated single-element contraction. *)
VerificationTest[
  Module[{bases = UniformBases[2, 4]},
    MatroidContraction[bases, {1, 2}] ===
      MatroidContraction[MatroidContraction[bases, 1], 2]
  ],
  True,
  TestID -> "MatroidTools-MatroidContraction-set-matches-iterated-contraction"
]
