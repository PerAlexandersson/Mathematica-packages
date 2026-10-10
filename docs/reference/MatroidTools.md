---
title: MatroidTools
parent: Reference
nav_order: 12
---

{% raw %}
# MatroidTools

Matroids from bases: rank, duality, deletion/contraction, Tutte polynomials, transversal, lattice path and rook matroids.

Load with `` Needs["MatroidTools`"] ``.

Background on symmetricfunctions.com: [Matroids](https://www.symmetricfunctions.com/matroids.htm), [Lattice path matroids](https://www.symmetricfunctions.com/lattice-path-matroids.htm).

## Functions and symbols

### Basis01Vector

Basis01Vector\[bases\] or Basis01Vector\[groundSet,basis\] returns a 01-vector representing the basis, or a list of such.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### ExternallyActiveElements

ExternallyActiveElements\[groundSet,bases,basis\] returns the externally active elements of basis with respect to the order on groundSet.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### IndependentSets

IndependentSets\[bases\] returns all independent sets contained in the listed bases.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### InternallyActiveElements

InternallyActiveElements\[groundSet,bases,basis\] returns the internally active elements of basis with respect to the order on groundSet.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### IsMatroidQ

Given a list of sets, see if they satisfy the basis exchange axioms.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### IsSubsetClosedQ

IsSubsetClosedQ\[sets\] returns true if every subset of every set in list of sets is also in the list of sets.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### LatticeOfFlats

LatticeOfFlats\[bases\] returns all ordered comparable pairs of distinct flats, represented as pairs of sets.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatchingMatroidBases

MatchingMatroidBases\[g\] returns the bases for the matching matroid associated with g.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidCharacteristicPolynomial

MatroidCharacteristicPolynomial\[{groundSet,bases},t\] returns the characteristic polynomial.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidColoops

MatroidColoops\[groundSet,bases\] returns the list of coloops, namely elements contained in every basis.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidContraction

MatroidContraction\[bases,e\] contracts with respect to element(s) e in the matroid.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidDeletion

MatroidDeletion\[bases,e\] deletes the element(s) e from the matroid.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidDual

MatroidDual\[groundSet,bases\] returns the bases of the dual matroid.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidFlats

MatroidFlats\[bases\] returns the flats of the matroid specified by its bases.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidIsomorphisms

MatroidIsomorphisms\[basesA,basesB\] returns all bijections carrying the bases of one matroid to the bases of the other.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidLoops

MatroidLoops\[groundSet,bases\] returns the list of loops (dependent 1-element sets).

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidSetRank

MatroidSetRank\[bases,set\] returns the matroid rank of set, computed as the maximum intersection size with a basis.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MatroidTuttePolynomial

MatroidTuttePolynomial\[{groundSet,bases},{x,y}\] returns the Tutte polynomial associated with the matroid.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### MConvexSetQ

MConvexSetQ\[sets\] returns true if these sets form an M-convex set (of sets). 

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### PathBases

PathBases\[lam,mu\] returns the path matroid bases associated with the skew shape.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### PathSetSystem

PathSetSystem\[lam,mu\] returns the set system that gives rise to LPMs.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### RookBases

RookBases\[lam,mu\] returns the rook matroid bases associated with the skew shape.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### RookSetSystem

RookSetSystem\[lam,mu\] returns a set system to produce the rook matroid.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### SetIsomorphisms

SetIsomorphisms\[setsA,setsB\] returns all isomorphisms between the two sets.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### SetsGeneratingPolynomial

SetsGeneratingPolynomial\[sets,x\] returns the multivariate set generating polynomial. Each set {a1,a2,...,ak} contributes with a monomial x\[a1\]x\[a2\]...x\[ak\].

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### SetSymmetries

SetSymmetries\[sets\] returns a list of lists, <br>
  each list describes a permutation group which is in the automorphism group the collection of sets.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### TransversalBases

TransversalBases\[sets\] returns all complete transversals of the list of sets.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### UniformBases

UniformBases\[r,n\] returns all r-element bases of the uniform matroid on {1,2,...,n}.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)

### VamosBases

VamosBases\[\] returns the list of bases of the non-realizable Vamos matroid.

Background: [Matroids](https://www.symmetricfunctions.com/matroids.htm)


{% endraw %}
