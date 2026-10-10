---
title: UnicellularChromatics
parent: Reference
nav_order: 10
---

{% raw %}
# UnicellularChromatics

Chromatic symmetric functions and LLT polynomials of unit interval graphs and area sequences, orientations and their statistics.

Load with `` Needs["UnicellularChromatics`"] ``.

Background on symmetricfunctions.com: [Chromatic quasisymmetric functions](https://www.symmetricfunctions.com/chromaticQuasisymmetric.htm), [Chromatic symmetric functions in the elementary basis](https://www.symmetricfunctions.com/chromaticEexpansion.htm), [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm).

## Functions and symbols

### AcyclicAscents

AcyclicAscents\[lambda, coloring\] or AcyclicAscents\[orientation\] counts ascending edges.

### AlternatingChains

AlternatingChains\[lambda, coloring, i\] returns the alternating chains using colors i and i+1.

### AreaConjugate

AreaConjugate\[area\] returns the conjugate of a non-circular 0-first area list.

### AreaDinv

AreaDinv\[area\] returns dinv for a 0-first area list.

### AreaLists

AreaLists\[size, All-&gt;False, Circular-&gt;True, Width-&gt;-1\] returns all area lists of size n.

### AreaRowPermutation

AreaRowPermutation\[area\] returns the row-to-column permutation for a 0-first area list.

### AreaToDyckWord

AreaToDyckWord\[area, corners\] converts a 0-first area list to its Dyck word.

### AreaToTopBounceShape

AreaToTopBounceShape\[area\] returns the top-starting bounce shape for a 0-first area list.

### AthanasiadisS

AthanasiadisS\[lambda\] returns the Athanasiadis partial sums.

### AthanasiadisUnimodalSets

AthanasiadisUnimodalSets\[lambda\] returns lambda-unimodal subsets of \[n-1\].

### AttackingPoset

AttackingPoset\[lambda\] returns the attacking poset edges of a triangular shape.

### AttackingVertices

AttackingVertices\[lambda, coloring\] returns monochromatic incomparability edges.

### AyclicAreaListQ

AyclicAreaListQ\[area\] tests whether an area list has minimum zero.

### BounceEndpoint

BounceEndpoint\[word, y\] returns the endpoint row reached by the bounce path starting at row y for a Schroeder word.

### BounceLengths

BounceLengths\[lambda\] gives the lengths of bounce triangles, from top to bottom.

### BounceList

BounceList\[word, y\] returns the list of x-coordinates visited by the bounce path starting at row y for a Schroeder word.

### ChromaticSymmetric

ChromaticSymmetric\[area,q\] returns the chromatic symmetric polynomial associated with given area sequence. One can also pass a graph object as argument.

Background: [Chromatic quasisymmetric functions](https://www.symmetricfunctions.com/chromaticQuasisymmetric.htm)

### ChromaticSymmetricColorings

ChromaticSymmetricColorings\[lambda, allowAttacking, maxColor\] returns colorings of a shape.

Background: [Chromatic quasisymmetric functions](https://www.symmetricfunctions.com/chromaticQuasisymmetric.htm)

### ChromaticSymmetricPolynomial

ChromaticSymmetricPolynomial\[lambda, x, q, n\] returns the chromatic symmetric polynomial in variables x.

Background: [Chromatic quasisymmetric functions](https://www.symmetricfunctions.com/chromaticQuasisymmetric.htm)

### Circular

Option for AreaLists

### ColorOrientation

ColorOrientation\[lambda, coloring\] gives the orientation induced by a coloring.

### DiagramRookPlacements

DiagramRookPlacements\[diagram, n\] returns all non-attacking placements of n rooks in a diagram.

### DinvFromAreaSeq

DinvFromAreaSeq\[area\] returns dinv for a 0-first Catalan area sequence.

### EdgesHRVRule

EdgesHRVRule\[edges\] returns replacement rules assigning each vertex the list consisting of that vertex and all vertices reachable from it by ascending edge paths.

### GasharovOpPTableauQ

GasharovOpPTableauQ\[lambda, coloring\] tests the opposite P-tableau condition.

### GasharovOpPTableaux

GasharovOpPTableaux\[lambda\] returns all opposite P-tableau colorings.

### GasharovPTableauQ

GasharovPTableauQ\[lambda, coloring\] tests the P-tableau condition.

### GasharovPTableaux

GasharovPTableaux\[lambda\] returns all P-tableau colorings.

### GraphAreaLists

GraphAreaLists\[n, opts\] returns area lists of unit interval graphs on n vertices, starting with 0 as in CatalanObjects (UnitIntervalEdges accepts them). Options: Circular -&gt; True (default) also includes circular (cylindric) area lists; Width -&gt; w bounds the entries by w - 1 (default n); All -&gt; False (default) keeps one representative per rotation class, All -&gt; True returns all.

### GraphAttackingEdges

GraphAttackingEdges\[edges, coloring\] returns monochromatic edges.

### GraphChromaticLLTPolynomial

GraphChromaticLLTPolynomial\[edges, n, x, q\] returns the graph LLT polynomial.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### GraphChromaticLLTPolynomialAttacking

GraphChromaticLLTPolynomialAttacking\[equalEdges, statisticEdges, n, x, q\] returns an LLT polynomial with forced attacking edges.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### GraphChromaticSymmetricColorings

GraphChromaticSymmetricColorings\[edges, n, allowAttacking, maxColor\] returns graph colorings.

Background: [Chromatic quasisymmetric functions](https://www.symmetricfunctions.com/chromaticQuasisymmetric.htm)

### GraphChromaticSymmetricPolynomial

GraphChromaticSymmetricPolynomial\[edges, n, x, q\] returns the graph chromatic symmetric polynomial.

Background: [Chromatic quasisymmetric functions](https://www.symmetricfunctions.com/chromaticQuasisymmetric.htm)

### GraphColoringAscents

GraphColoringAscents\[edges, coloring\] returns the number of edges whose color increases along the ordered edge.

### GraphColoringInversions

GraphColoringInversions\[edges, coloring\] counts descending edges.

### GraphColoringMonochromaticEdges

GraphColoringMonochromaticEdges\[edges, coloring\] returns the number of edges whose endpoints have equal colors.

### GraphColoringOrientation

GraphColoringOrientation\[edges, coloring\] returns the induced orientation.

### GraphOrientationAscents

GraphOrientationAscents\[edges, orientation\] counts correctly oriented edges.

### GraphOrientationHalfSinks

GraphOrientationHalfSinks\[edges, orientation, n\] returns half-sinks.

### GraphOrientationHalfSources

GraphOrientationHalfSources\[edges, orientation, n\] returns half-sources.

### GraphOrientationIntersection

GraphOrientationIntersection\[edges, orientation\] returns equally oriented edges.

### GraphOrientationInversions

GraphOrientationInversions\[edges, orientation\] counts oppositely oriented edges.

### GraphOrientationSinks

GraphOrientationSinks\[orientation, n\] returns the sinks.

### GraphOrientationSources

GraphOrientationSources\[orientation, n\] returns the sources.

### HomogeneousGraphChromaticSymmetricPolynomial

HomogeneousGraphChromaticSymmetricPolynomial\[edges, n, x, q, t\] returns the homogeneous graph chromatic polynomial.

Background: [Chromatic quasisymmetric functions](https://www.symmetricfunctions.com/chromaticQuasisymmetric.htm)

### HomogeneousGraphLLTPolynomial

HomogeneousGraphLLTPolynomial\[edges, n, x, q, t\] returns the homogeneous graph LLT polynomial.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### IncomparabilityGraph

IncomparabilityGraph\[lambda\] returns the incomparability graph edges of a triangular shape.

### InnerCorners

InnerCorners\[area\] returns the inner corners of a 0-first area list.

### LLTOrientationForest

LLTOrientationForest\[area, orientation\] returns the orientation forest map.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### LLTOrientationLowestReachableVertex

LLTOrientationLowestReachableVertex\[area, orientation\] returns the lowest reachable vertex of each vertex.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### LLTOrientationShape

LLTOrientationShape\[area, orientation\] returns the orientation shape.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### LLTOrientationVertexPartition

LLTOrientationVertexPartition\[area, orientation\] returns the induced vertex partition.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### MajFromAreaSeq

MajFromAreaSeq\[area\] returns the major index of a 0-first area sequence.

### NoAscendingCycleOrientations

NoAscendingCycleOrientations\[edges\] returns orientations with no ascending directed cycle.

### OrientationPlot

OrientationPlot\[area, orientation\] returns a Graphics of an oriented unit interval graph.

### OuterCorners

OuterCorners\[area\] returns the outer corners of a 0-first area list.

### PArray

PArray\[coloring\] returns the P-array associated with a coloring.

### PArrayPlot

PArrayPlot\[lambda, coloring\] returns a Graphics of a coloring as a P-array.

### PartitionRookPlacements

PartitionRookPlacements\[lambda\] returns permutations fitting in a partition diagram.

### PathShapes

PathShapes\[n\] gives all partitions that fit inside the size n triangle.

### SchroederAcyclicOrientations

SchroederAcyclicOrientations\[word\] returns all acyclic orientations of the unit interval graph encoded by a Schroeder word, respecting its strict edges.

### SchroederColoringAscents

SchroederColoringAscents\[word, coloring\] returns the number of ascents of a coloring on the graph encoded by a Schroeder word.

### SchroederColorings

SchroederColorings\[word, ncols, Partition -&gt; True\] returns colorings of the graph encoded by a Schroeder word satisfying its strict edges. ncols defaults to 0; with Partition -&gt; False, colors range from 1 to ncols.

### SchroederLLTSymmetric

SchroederLLTSymmetric\[word, q\] returns the LLT symmetric polynomial associated with a Schroeder word and parameter q.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### SchroederOrientations

SchroederOrientations\[word\] returns all orientations of the unit interval graph encoded by a Schroeder word, respecting its strict edges.

### SchroederPlot

SchroederPlot\[word\] returns a plot of the area and strict edges encoded by a Schroeder word.

### SchroederWordStrictEdges

SchroederWordStrictEdges\[word\] returns the strict edge list extracted from a Schroeder word.

### SchroederWordToArea

SchroederWordToArea\[word\] converts a Schroeder word, given as a string or list using -/n, +/e, and 0/d, to {area, strictEdges}.

### SingleCelledLLTPolynomial

SingleCelledLLTPolynomial\[lambda, x, q, n\] returns the single-celled LLT polynomial.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### SouthEastDiagram

SouthEastDiagram\[permutation\] returns the south-east diagram.

### SouthWestDiagram

SouthWestDiagram\[permutation\] returns the south-west diagram.

### StripSizesToEdges

StripSizesToEdges\[sizes\] returns {area, strictEdges}, using a 0-first area list.

### UnicellularLLTSymmetric

UnicellularLLTSymmetric\[area,q\] returns the <br>
unicellular LLT polynomial associated with given area sequence.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### UnicellularLLTSymmetricSchur

UnicellularLLTSymmetricSchur\[area, q, ss\] returns the LLT polynomial for an area sequence in the basis supplied by the function ss; q defaults to 1.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### UnitIntervalData

UnitIntervalData\[area, coloring\] returns the unit-interval representation of a coloring.

### UnitIntervalEdges

UnitIntervalEdges\[area\] returns the edges of the unit interval graph.

### UnitIntervalPlot

UnitIntervalPlot\[area, coloring\] returns a Graphics of a coloring in unit interval order.

### ValleyEdges

ValleyEdges\[edges, n\] returns the edges that are valleys in the diagram.

### VerticalStripLLTColorings

VerticalStripLLTColorings\[sizes\] returns colorings satisfying the strip strictness conditions.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### VerticalStripLLTPolynomial

VerticalStripLLTPolynomial\[sizes, x, q\] returns a vertical-strip LLT polynomial.

Background: [Unicellular LLT polynomials and twin manifolds](https://www.symmetricfunctions.com/unicellular-llt.htm)

### Width

Option for AreaLists


{% endraw %}
