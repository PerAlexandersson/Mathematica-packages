---
title: CatalanObjects
parent: Reference
nav_order: 9
---

{% raw %}
# CatalanObjects

Dyck paths, non-crossing partitions and matchings, parking functions, trees and other Catalan families, with plots.

Load with `` Needs["CatalanObjects`"] ``.

Background on symmetricfunctions.com: [Other combinatorial families](https://www.symmetricfunctions.com/combinatorialObjects.htm), [Parking functions](https://www.symmetricfunctions.com/parking-functions.htm), [qt-Catalan numbers](https://www.symmetricfunctions.com/qtCatalan.htm).

## Functions and symbols

### AbelianDyckAreaQ

AbelianDyckAreaQ\[area\] returns True when the Dyck area sequence has an abelian unit interval graph.

### AreaBounce

AreaBounce\[area\] returns the bounce statistic of a Dyck area sequence.

### AreaListPlot

AreaListPlot\[area,opts\] plots the area. Options are Circular and Labels

### AreaPeaks

AreaPeaks\[area\] returns number of peaks, assuming its a Dyck path.

### AreaRemoveVertices

AreaRemoveVertices\[area, vList\] returns the smaller area, where vertices have been removed.

### AreaTo312Avoiding

AreaTo312Avoiding\[area\] returns a permutation avoiding 312.

### AreaToBounceShape

AreaToBounceShape\[area\] returns the bounce shape associated with a Dyck area sequence.

### AreaToBracketing

DyckAreaToBracketing\[area\] returns a bracketing representation of the Dyck path.

### AreaToDyckPath

AreaToDyckPath\[area\] converts a Dyck area sequence to its DyckPath representation.

### AreaToFerrersBoard

AreaToFerrersBoard\[area\] returns the list of coordinate squares in the Ferrers board associated with an area sequence.

### AreaToIntervalGraph

AreaToIntervalGraph\[area\] returns the unit interval graph associated with a Dyck area sequence.

### AreaToPartition

AreaToPartition\[area\] returns the partition associated with the area sequence.

### AreaTranspose

AreaTranspose\[area\] returns the transposed area list.

### Av123

Av123\[n\] returns all 123-avoiding permutations of size n.

### Av132

Av132\[n\] returns all 132-avoiding permutations of size n.

### Av213

Av213\[n\] returns all 213-avoiding permutations of size n.

### Av231

Av231\[n\] gives the list of 231-avoiding permutations of size n.

### Av312

Av312\[n\] returns all 312-avoiding permutations of size n.

### Av321

Av321\[n\] returns all 321-avoiding permutations of size n.

### BounceLikeArea

BounceLikeArea\[alpha\] generates a Dyck path with disjoint cliques of specified sizes.

### CircularGraph

Head for object representing a circular object. It is of the form CircularGraph\[n,edgeList\].

### CircularGraphComponents

CircularGraphComponents\[gc\] returns lists of connected components. This assumes we have a graph with edges.

### CircularGraphCrossings

CircularGraphCrossings\[gc\] returns the number of crossings.

### CircularGraphEdges

CircularGraphEdges\[circularGraph\] returns the list of edges or blocks stored in a CircularGraph object.

### CircularGraphLoops

CircularGraphLoops\[circularGraph\] returns the distinct loop edges stored in a CircularGraph object.

### CircularGraphPlot

CircularGraphPlot\[circularGraph\] gives a graphical representation of the graph.

### CircularGraphProperEdges

CircularGraphProperEdges\[circularGraph\] returns the distinct non-loop edges or blocks stored in a CircularGraph object.

### CircularGraphReflectLabels

CircularGraphReflectLabels\[cg\] sends vertex j to n+1-j.

### CircularGraphRotate

CircularGraphRotate\[gc,\[steps\]\] rotates the configuration.

### CircularGraphShortEdgeSet

CircularGraphShortEdgeSet\[cg\] returns the list of vertices v, such that (v,v+1) is an edge.

### CompleteBinaryTrees

CompleteBinaryTrees\[n\] returns a list of binary trees.

### DyckAreaCliqueDecomposition

DyckAreaCliqueDecomposition\[area\] returns the clique decomposition of the unit interval graph associated with a Dyck area sequence.

### DyckAreaCliqueNesting

DyckAreaCliqueNesting\[area\] returns the maximum number of cliques in the Dyck area clique decomposition containing a vertex.

### DyckAreaCliqueNestingVector

DyckAreaCliqueNestingVector\[area\] returns the sorted vector of vertex nesting counts in the Dyck area clique decomposition.

### DyckAreaLists

DyckAreaLists\[n\] returns all area sequences of Dyck paths of length n.

### DyckCoordinates

DyckCoordinates\[dp\] returns the list of coordinates of path vertices.

### DyckMajorIndex

DyckMajorIndex\[dp\] where dp is a ne-path, returns sum of indices of valleys. Summing over all these gives the qCatalan number. This is same as major index of the word where n=0, e=1.

### DyckPath

DyckPath\[word\] constructs a DyckPath object from a word as a string of n/e steps or a list of n/e steps. DyckPath\[binary\] accepts a list of 0/1 values, with 0 mapped to n and 1 mapped to e.

### DyckPathHeight

DyckPathHeight\[path\] returns the maximum height of a DyckPath object.

### DyckPathRemoveVertices

DyckPathRemoveVertices\[dyckPath,v\] returns the smaller Dyck path, where north step v and east step v have been removed.

### DyckPaths

DyckPaths\[n\] returns all Dyck paths of length n. A path is a list of n and e steps.

### DyckPathToTikz

DyckPathToTikz\[path\] returns a TikZ source string for a DyckPath object.

### DyckPeaks

DyckPeaks\[path\] returns the number of peaks in a DyckPath object.

### DyckPlot

DyckPlot\[path\] returns a Graphics representation of a DyckPath object or a string of n/e steps.

### DyckValleys

DyckValleys\[path\] returns the number of valleys in a DyckPath object.

### FormatTypeBSetPartition

FormatTypeBSetPartition\[sp\] returns a canonicalized version.

### FromTypeB

FromTypeB\[n, i\] converts a negative type B label -j to n+j, and leaves a positive label unchanged. FromTypeB\[n, list\] applies this conversion to every entry.

### FussCatalanPaths

FussCatalanPaths\[n,k\] returns all Fuss-Catalan paths in the n by (k - 1)n rectangle.

### IncreasingParkingFunctions

IncreasingParkingFunctions\[n\] returns all parking functions which are weakly increasing. This is a Catalan family.

### Labels

Option for AreaListPlot specifying labels as rules from edges or vertices to displayed values.

### LineGraphAreaLists

LineGraphAreaLists\[n\] returns all area lists where the unit interval graph is a line graph.

### LineGraphAreaQ

LineGraphAreaQ\[area\] returns True when the unit interval graph of the Dyck area sequence is a line graph.

### NCFComponents

NCFComponents\[forest\] returns the connected components of a non-crossing forest.

### NCFRotate

NCFRotate\[forest,\[s\]\] rotates the forest.

### NCFVertexDegree

NCFVertexDegree\[forest\] returns a list {d1,...,dn} such that di is the degree at vertex i.

### NCMFaces

NCMFaces\[ncm\] returns the list of faces (as vertex sets)

### NCMMajorIndex

NCMMajorIndex\[matching\] returns the major-index statistic of a non-crossing perfect matching represented as a CircularGraph.

### NCMPeaks

NCMPeaks\[matching\] returns <br>
the number of peaks (in the corresponding Dyck path). Same as number of instances of {i,i+1}.

### NCMRotate

NCMRotate\[matching,s\] rotates the matching s steps. <br>
The default value for s is 2. Promotion correspond to s=1.

### NCMToDyckPath

NCMToDyckPath\[matching\] returns a Dyck path.

### NCPBlocks

NCPBlocks\[ncp\] returns the number of blocks.

### NCPLeftBiggerStatistic

NCPLeftBiggerStatistic\[ncp\] is the lb-statistic. <br>
Summing over all non-crossing partitions with k parts gives the q-Narayana number.

### NCPMajorLikeStat

NCPMajorLikeStat\[ncp\] gives a statistic equidistributed with maj on Dyck paths, and summing over fixed number of blocks give the q-Narayana.

### NCPRestrictedGrowthFunction

NCPRestrictedGrowthFunction\[partition\] returns the restricted-growth word of a non-crossing partition represented as a CircularGraph.

### NCPRightBiggerStatistic

NCPRightBiggerStatistic\[ncp\] is the rb-statistic.

### NCPRotate

NCPRotate\[partition, steps\] rotates a non-crossing partition by steps vertices; steps defaults to 1. The partition is a CircularGraph object.

### NCPToDyckPath

NCPToDyckPath\[ncp\] produces a Dyck path, where number of blocks is sent to the number of peaks. This map is NOT the same as sending NCP to NCM, and then using NCM to Dyck.

### NCPToPerfectMatching

NCPToPerfectMatching maps a non-crossing partition to a non-crossing perfect matching.

### NgonTriangulations

NgonTriangulations\[n\] returns a list of all triangulations of the (n+2)-gon.

### NonCrossingForests

NonCrossingForests\[n\] returns all non-crossing forests on n vertices.

### NonCrossingMatchings

NonCrossingMatchings\[n\] returns a list of <br>
all non-crossing matchings of size n.<br>
Non-crossing partitions with k peaks is Narayana(n,k).

### NonCrossingMatchingsTypeB

NonCrossingMatchingsTypeB\[n\] returns the non-crossing perfect matchings of type B on 2n pairs, represented as CircularGraph objects.

### NonCrossingPartitions

NonCrossingPartitions\[n\] returns all non-crossing set partitions of \[n\], represented as CircularGraph objects. There are Catalan(n) of them; those with k blocks are counted by Narayana(n,k).

### NonCrossingPartitionsTypeB

NonCrossingPartitionsTypeB\[n\] returns the non-crossing set partitions of type B on the labels +/-1,...,+/-n, represented as CircularGraph objects.

### OrderedRootedTreePlot

OrderedRootedTreePlot\[tree\] returns a graphical representation of an ordered rooted tree.

### OrderedRootedTrees

OrderedRootedTrees\[n\] returns all ordered rooted trees with n edges, represented recursively as lists of child trees.

### OrderedRootedTreeSize

OrderedRootedTreeSize\[tree\] returns the number of vertices in an ordered rooted tree represented recursively as a list of child trees.

### OrderedRootedTreeToGraph

OrderedRootedTreeToGraph\[tree\] converts an ordered rooted tree represented recursively to a directed Graph.

### ORTTo231Perm

ORTTo231Perm\[tree\] returns the 231-avoiding permutation associated with an ordered rooted tree.

### ParkingFunctions

ParkingFunctions\[n\] returns all parking functions of length n.

### PerfectMatchingCrossings

PerfectMatchingCrossings\[pm\] returns the vector with number of crossings.<br>
The sum of entries is CircularGraphCrossings\[pm\].

### PerfectMatchings

PerfectMatchings\[n\] returns the list of perfect matchings with 2n vertices. Can also use area sequence as argument.

### RationalDyckPaths

RationalDyckPaths\[{m,n}\] returns all n/e paths from {0,0} to {m,n} staying weakly above the diagonal.

### RookInversionList

RookInversionList\[board, rooks\] returns, for each row of a coordinate board, the number of squares in RookInversions\[board, rooks\].

### RookInversions

RookInversions\[board,rooks\] returns the squares on board which <br>
  are inversions with respect to the rooks. This assumes {r,c} <br>
coordinates.

### RookPlacementGrid

RookPlacementGrid\[board, rooks\] returns a Grid displaying a rook placement on a coordinate board.

### RookPlacements

RookPlacements\[board,r\] returns a list of all possible ways to <br>
place r non-attacking rooks on a board (given as list of coordinates).

### SchroederPaths

SchroederPaths\[n\] returns all Schroeder paths from (0,0) to (n,n) <br>
using steps in (n,e,d), and there are no diagonal steps on the main diagonal. The number of paths of length n is given by A001003.

### SchroederPathUpSteps

SchroederPathUpSteps\[path\] returns the 1-based indices of up or diagonal steps in a Schroeder path given as a string or list.

### SchroederReverse

SchroederReverse\[path\] returns the reversed path.

### SeparatedNonCrossingPartitions

SeparatedNonCrossingPartitions\[n\] returns all Type A non-crossing and separated set partitions. Enumerated by Motzkin numbers.

### SetPartitionForm

SetPartitionForm\[blocks\] formats a set partition, including signed labels, as a Row expression.

### SetPartitionLinePlot

SetPartitionLinePlot\[CircularGraph\] returns the vertices-on-a-line version of set partitions.<br>
Works on both Type A and Type B.

### Stanley60TypeB

Stanley60TypeB\[n\] returns a family where one edge might appear two times.

### StanleyCatalan60

StanleyCatalan60\[n\] gives Catalan elements,<br>
which are certain configurations of n-1 vertices on a circle.<br>
Some vertices are connected by edges or self-loops. <br>
These must be non-intersecting, even at the end-points.<br>
<br>
Example: CircularGraph\[4,{{1,2},{3,3}}\] is a configuraton on 4 vertices,<br>
with an edge, a self-loop.

### StanleyCatalan60Action

StanleyCatalan60Action\[circularGraph\] applies StanleyCatalan60Flip followed by StanleyCatalan60Rotate. StanleyCatalan60Action\[circularGraph, k\] iterates this action k times.

### StanleyCatalan60EdgesAndLoops

StanleyCatalan60EdgesAndLoops\[circularGraph\] returns the number of edges and loops, counted with multiplicity, in a Stanley Catalan object.

### StanleyCatalan60Flip

StanleyCatalan60Flip\[circularGraph\] toggles the loop at vertex 1 when no non-loop edge is incident to that vertex.

### StanleyCatalan60Rotate

StanleyCatalan60Rotate\[circularGraph, steps\] rotates a Stanley Catalan object by steps vertices; steps defaults to 1.

### ToTypeB

ToTypeB\[n, i\] converts a label i&gt;n to n-i, and leaves a label at most n unchanged. ToTypeB\[n, list\] applies this conversion to every entry.

### TypeBSetPartition0

TypeBSetPartition0\[elems\] returns all type B set partitions without 0 block.

### TypeBSetPartitions

TypeBSetPartitions\[n\] returns all type B set partitions of \[n\].

### TypeBSortInterval

TypeBSortInterval\[interval\] sorts an interval in the type B cyclic order, starting after -1 when -1 is present and otherwise starting after 1.


{% endraw %}
