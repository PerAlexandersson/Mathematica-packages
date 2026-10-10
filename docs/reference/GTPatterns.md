---
title: GTPatterns
parent: Reference
nav_order: 5
---

# GTPatterns

Gelfand–Tsetlin patterns (skew, row-flagged, cylindric), BZ patterns, Gog and Magog patterns, tiles and snakes, lattice paths, stretched Kostka (Ehrhart) data, TikZ output.

Load with `` Needs["GTPatterns`"] ``.

Background on symmetricfunctions.com: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm), [Hives and BZ-polytopes](https://www.symmetricfunctions.com/hivePolytopes.htm), [Ehrhart theory](https://www.symmetricfunctions.com/ehrhart.htm).

## Functions and symbols

### BoxCountMatrix

BoxCountMatrix\[GTPattern\[rows\]\] returns the matrix whose entry in row i and column j counts entries j in tableau row i, with GT rows ordered bottom to top.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### BZPattern

BZPattern\[data\] represents a Berenstein-Zelevinsky pattern.

Background: [Hives and BZ-polytopes](https://www.symmetricfunctions.com/hivePolytopes.htm)

### BZPatterns

BZPatterns\[lam, mu, nu\] returns BZ-patterns counted by the Littlewood-Richardson coefficient c^lam\_{mu,nu}.

Background: [Hives and BZ-polytopes](https://www.symmetricfunctions.com/hivePolytopes.htm)

### BZPlus

BZPlus\[b1,b2,...\] adds BZ-patterns entrywise.

Background: [Hives and BZ-polytopes](https://www.symmetricfunctions.com/hivePolytopes.htm)

### ContainingFaceDimension

ContainingFaceDimension\[g\] returns the dimension of the face of the GT polytope containing g.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### EnableSkew

EnableSkew is an option for ShapeTriplets, GTTiles, GTPatternForm, and GTPatternTikz controlling whether skew cells are included.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GogPatterns

GogPatterns\[n\] returns the Gog patterns of size n. GogPatterns\[n,k\] uses k columns.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTEhrhartPolynomial

GTEhrhartPolynomial\[lam,mu,w,k\] counts GT-patterns of (k lam,k mu,k w) when k is an integer; a symbolic k returns the interpolating Ehrhart polynomial.

Background: [Ehrhart theory](https://www.symmetricfunctions.com/ehrhart.htm)

### GTMonomial

GTMonomial\[g,x\] returns the monomial in x associated with the weight of g.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTPartition

GTPartition is an option for GTPatternForm and GTPatternTikz. Its values are "Tiles", "Snakes", "FreeTiles", "ShadedTiles", or None.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTPattern

GTPattern\[data\] represents a GT-pattern as a list of rows (partitions) ordered bottom to top.<br>
GTPattern\[YoungTableau\[t\]\] converts a (skew) SSYT to its GT-pattern.<br>
YoungTableau\[gtp\] converts a GT-pattern back to a SSYT.<br>
gtp\[r,c\] accesses the entry at row r, column c (1-indexed; row 1 = bottom = inner shape).

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTPatternForm

GTPatternForm\[gtp\] returns the graphical representation of the GT-pattern.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTPatterns

GTPatterns\[lam,mu,w\] returns a list of all GT-patterns with outer shape lam, inner shape mu (default {}), and weight vector w (default {}), corresponding to SSYT of skew shape lam/mu with content w.<br>
Optional argument cylindricShift (default Infinity) restricts to cylindric GT-patterns with the given column shift.<br>
Option RowFlags-&gt;{{a1,b1},{a2,b2},...} constrains entries in SSYT row r to the range \[ar,br\] (default {1,Infinity} = no constraint).

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTPatternTikz

GTPatternTikz\[g\] returns TikZ code for a GT-pattern.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTPlus

GTPlus\[g1,g2,...\] adds Gelfand-Tsetlin patterns entrywise, padding smaller patterns.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTShape

GTShape\[gtp\] returns {lam, mu, w} for a GT-pattern gtp, where lam is the outer shape, mu is the inner shape (empty list for straight shapes), and w is the weight vector.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTSnakes

GTSnakes\[g\] returns the equal-entry snakes of a GT-pattern.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### GTTiles

GTTiles\[g\] returns {freeTiles, fixedTiles} for a GT-pattern.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### KostkaRange

KostkaRange is an option for ShapeTriplets specifying the minimum and maximum allowed Kostka multiplicity.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### LatticePathForm

LatticePathForm\[g\] or LatticePathForm\[tab\] returns graphics for the non-intersecting lattice paths.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### LatticePathTikz

LatticePathTikz\[g\] or LatticePathTikz\[tab\] returns TikZ code for the non-intersecting lattice paths.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### MagogPatterns

MagogPatterns\[n\] returns the Magog patterns of size n. MagogPatterns\[n,k\] uses k columns.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### RowFlags

RowFlags is an option for GTPatterns that restricts the entries in tableau row r to a specified inclusive interval {ar,br}.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### ShapeTriplets

ShapeTriplets\[lambda, options\] returns {lambda, mu, w} triples for skew shapes lambda/mu and weights w. EnableSkew, WeightRange, and KostkaRange control skew shapes, weight sizes, and Kostka multiplicities.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### TilingMatrix

TilingMatrix\[g\] returns the tiling matrix of a GT-pattern.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

### UpperBoundKostkaDegree

UpperBoundKostkaDegree\[lam,mu,w\] returns an upper bound for the degree of the stretched Kostka polynomial.

Background: [Ehrhart theory](https://www.symmetricfunctions.com/ehrhart.htm)

### WeightRange

WeightRange is an option for ShapeTriplets specifying the minimum and maximum number of parts in a weight.

Background: [Gelfand–Tsetlin patterns and polytopes](https://www.symmetricfunctions.com/gtpatterns.htm)

