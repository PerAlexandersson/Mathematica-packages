---
title: GraphTools
parent: Reference
nav_order: 11
---

# GraphTools

Graph polynomials, orientations, and datasets of connected graphs (n <= 9), trees (n <= 20) and rooted trees (n <= 10).

Load with `` Needs["GraphTools`"] ``.

## Functions and symbols

### AcyclicSinkPolynomial

AcyclicSinkPolynomial\[g,t\] returns the polynomial whose coefficient of t^k counts acyclic orientations of g with k sinks.

### ClawFreeQ

ClawFreeQ\[Graph\[g\]\] or ClawFreeQ\[edgesList\] returns true iff the edges form a claw-free graph.

### CompleteBipartiteGraph

CompleteBipartiteGraph\[a,b\] or CompleteBipartiteGraph\[{a,b,c,..}\] returns the complete bipartite or n-partite graph with prescribed sizes, as lists of edges.

### ConnectedSimpleGraphs

ConnectedSimpleGraphs\[n\] returns a list of all non-isomorphic simple connected graphs on n vertices, for 1 &lt;= n &lt;= 9 (OEIS A001349); other n give Missing\["NotAvailable", n\]. The data are read from Data/graphs.

### FerrersBoardGraph

FerrersBoardGraph\[lam\] returns the bipartite graph of the Ferrers board lam.<br>
FerrersBoardGraph\[lam,mu\] returns the bipartite graph of the skew Ferrers board lam/mu.

### GraphAcyclicOrientations

GraphAcyclicOrientations\[g\] returns all acyclic orientations of an edge list or graph g.

### GraphContractVertices

GraphContractVertices\[g,v\] contracts all vertices in v into a single vertex named by the first entry of v. v can also be a directed edge, undirected edge, or rule. KeepLoops defaults to False.

### GraphDeleteEdge

GraphDeleteEdge\[g,e\] removes e from graph g. With KeepMultipleEdges -&gt; True, only one matching copy is removed; the default is False.

### GraphIndependencePolynomial

GraphIndependencePolynomial\[g,t\] returns the univariate independence polynomial of the graph g.

### GraphIndependentTriangles

GraphIndependentTriangles\[edges\] returns all collections of pairwise vertex-disjoint triangles in the graph with edge list edges.

### GraphIndependetSets

GraphIndependetSets\[g\] returns a list of all independent vertex sets in graph g.

### GraphMatchingPolynomial

GraphMatchingPolynomial\[g,t\] returns the univariate matching polynomial of the graph g.

### GraphMatchings

GraphMatchings\[edges\] returns all matchings (as subsets of edges).

### GraphNonCrossingMatchings

GraphNonCrossingMatchings\[edges\] returns all non-crossing matchings (as subsets of edges).

### GraphNonNestingMatchings

GraphNonNestingMatchings\[g\] returns all matchings of g with no pair of nested edges.

### GraphOrientations

GraphOrientations\[g\] returns all orientations of an edge list or graph g.

### GraphPerfectMatchings

GraphPerfectMatchings\[g\] returns a list of perfect matchings of g.

### GraphTriangles

GraphTriangles\[edges\] returns all 3-vertex subsets which are 3-cliques.

### KeepLoops

KeepLoops is an option for GraphContractVertices; its default is False, and True preserves loops and multiple edges created by contraction.

### KeepMultipleEdges

KeepMultipleEdges is an option for GraphDeleteEdge; its default is False, and True removes only one matching copy of an edge.

### KnGraph

KnGraph\[n\] gives the complete graph on n vertices.

### OrientationSinks

OrientationSinks\[g\] returns the vertices which are sinks of an edge list or graph g.

### OrientationsSinkPolynomial

OrientationsSinkPolynomial\[g,t\] returns the polynomial whose coefficient of t^k counts orientations of g with k sinks.

### RootedTreeGraphs

RootedTreeGraphs\[n\] returns all non-isomorphic rooted trees on n vertices, for 1 &lt;= n &lt;= 10 (OEIS A000081), as directed graphs on 1, ..., n with root 1 and edges directed from parent to child; other n give Missing\["NotAvailable", n\]. The data are read from Data/trees/rooted-trees-1-10.wl (formerly TreesData\`GetRootedTrees).

### StirlingGraph

StirlingGraph\[n\] returns the edge list of a bipartite graph whose k-edge matchings are counted by S(n,k).

### TilingGraph

TilingGraph\[w,h,tile\] returns the graph where valid placements of the tile on the rectangle are vertices,<br>
and edges are overlaps of tiles.

### TreeGraphs

TreeGraphs\[n\] returns a list of all non-isomorphic unlabeled trees on n vertices, for 1 &lt;= n &lt;= 20 (OEIS A000055); other n give Missing\["NotAvailable", n\]. The data for n &gt;= 5 are read from Data/trees, taken from https://houseofgraphs.org/meta-directory/trees

