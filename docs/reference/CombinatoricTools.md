---
title: CombinatoricTools
parent: Reference
nav_order: 3
---

{% raw %}
# CombinatoricTools

Partitions, compositions, set partitions, permutation statistics, q-analogs, characters of the symmetric group, Kostka numbers.

Load with `` Needs["CombinatoricTools`"] ``.

Background on symmetricfunctions.com: [Partitions, permutations and tableaux](https://www.symmetricfunctions.com/preliminaries.htm), [Q-analogs, q-Lucas theorem and q-Catalan numbers](https://www.symmetricfunctions.com/q-analogs.htm), [Kostka coefficients, Kostka–Foulkes polynomials and charge](https://www.symmetricfunctions.com/kostkaFoulkes.htm).

## Functions and symbols

### AbacusForm

AbacusForm\[abacus\] returns a graphical representation of the abacus.

### Ascents

Ascents\[p\] returns the number of ascents of the list p.

### AscentSet

AscentSet\[p\] returns the 1-based positions i such that p\[\[i\]\] &lt; p\[\[i+1\]\].

### BurrowsWheeler

BurrowsWheeler\[w\] returns the Burrows-Wheeler transform of the list w.

### ChargeWordDecompose

ChargeWordDecompose\[word\] decomposes a word with partition type into a list of standard subwords.

Background: [Kostka coefficients, Kostka–Foulkes polynomials and charge](https://www.symmetricfunctions.com/kostkaFoulkes.htm)

### CoInversions

CoInversions\[p\] returns Binomial\[Length\[p\],2\] minus the number of inversions of p.

### CoMajorIndex

CoMajorIndex\[w\] returns the sum of n-i over descents i of the length-n list w.

### CompositionRefinements

CompositionRefinements\[comp\] returns all refinements of the composition.

### CompositionSlinky

CompositionSlinky\[comp\] applies the slinky rule to the composition.<br>
The return value is {lam,s} where lam is the resulting partition, and s is the sign, i.e, parity of number of 'slinks'<br>
The value of s=0 is a partition could not be reached.

### CompositionToDescentSet

CompositionToDescentSet\[alpha\] returns the set presentation of the composition.

### CompositionToRibbon

CompositionToRibbon\[alpha\] returns a ribbon skew shape, where row j from the bottom has a\_j boxes.

### CompositionWord

CompositionWord\[alpha\] returns a binary word representing the composition.

### ConjugatePartition

ConjugatePartition\[lam\] returns the conjugate of partition.

### Derangements

Derangements\[n\] returns a list of all derangements of 1,2,...,n.

### Descents

Descents\[p\] returns the number of descents of the list p.

### DescentSet

DescentSet\[p\] returns the 1-based positions i such that p\[\[i\]\] &gt; p\[\[i+1\]\].

### DescentSetToComposition

DescentSetToComposition\[des,n\] returns the composition associated with a descent set D subset \[n-1\].

### DiagramBoxes

DiagramBoxes\[{lam,mu}\] returns a list of (r,c)-coords of boxes in the shape.

### Durfee

Durfee\[mu\] or Durfee\[{lam,mu}\] returns the size of largest square that can fit in the diagram.

### Excedances

Excedances\[pi\] returns the number of excedances of the list pi.

### ExcedancesSet

ExcedancesSet\[pi\] returns the 1-based positions i such that i &lt; pi\[\[i\]\].

### FixedPoints

FixedPoints\[pi\] returns the number of fixed points of the list pi.

### FixedPointsSet

FixedPointsSet\[pi\] returns the 1-based positions i such that pi\[\[i\]\] == i.

### HookLengths

HookLengths\[lam\] returns a table with hook values as entries.

### IntegerCompositions

IntegerCompositions\[n\] returns all compositions of n. IntegerCompositions\[n,k\] returns those with k positive parts.

### IntegerPartitionQ

IntegerPartitionQ\[lam\] returns <br>
true only if lam is a weakly decreasing list of positive integers.

### IntervalSplit

IntervalSplit\[p\] returns the maximal contiguous sublists whose successive entries increase by 1.

### InverseKostkaCoefficient

InverseKostkaCoefficient\[lam,mu\] returns the inverse Kostka coefficient.

### Inversions

Inversions\[p\] returns the number of pairs i&lt;j with p\[\[i\]\] &gt; p\[\[j\]\].

### JackPsi

JackPsi\[{lam,mu},a\] returns the Jack branching coefficient for the skew shape lam/mu and parameter a.

### JackPsiPrime

JackPsiPrime\[{lam,mu},a\] returns JackPsi for the conjugate skew shape with parameter 1/a.

### KostantPartitionFunction

KostantPartitionFunction\[w\] lists all ways to <br>
express w as a non-negative combination of type A roots.

### KostkaCoefficient

KostkaCoefficient\[lam,mu,\[a=1\]\] returns the Kostka coefficient. For general a, this gives the JackP symmetric function coefficients.

Background: [Kostka coefficients, Kostka–Foulkes polynomials and charge](https://www.symmetricfunctions.com/kostkaFoulkes.htm)

### LatticeWordQ

LatticeWordQ\[w\] returns true if w is a lattice word.

### LeftToRightMaxima

LeftToRightMaxima\[p\] returns the elements of p that are left-to-right maxima.

### LeftToRightMinima

LeftToRightMinima\[p\] returns the elements of p that are left-to-right minima.

### LexOrder

LexOrder\[a,b\] returns 1, 0, or -1 according as a precedes, equals, or follows b in the package's lexicographic order.

### LexSort

LexSort\[w\] sorts the lists in w using LexOrder.

### LinearlyIndependentRows

LinearlyIndependentRows\[A\] returns indices of rows that span the row space.

### ListSplits

ListSplits\[list\] returns all 2^(n-1) ways to split the list into non-empty sublists

### MacdonaldPsi

MacdonaldPsi\[{lam,mu},q,t\] returns the Macdonald branching coefficient for the skew shape lam/mu.

### MacdonaldPsiPrime

MacdonaldPsiPrime\[{lam,mu},q,t\] returns MacdonaldPsi for the conjugate skew shape with q and t interchanged.

### MajorIndex

MajorIndex\[p\] returns the sum of the 1-based descent positions of p.

### MultiRectangularToPartition

MultiRectangularToPartition\[{rr,ss}\] is the inverse of PartitionToMultirectangular

### MultiSubsets

MultiSubsets\[set,k\] returns all Binomial\[n + k - 1, k\] ways to pick with repetition, unordered.

### Nicify

Rewrites polynomial expression in a nice form

### OperatorConnectedComponent

OperatorConnectedComponent\[init,ops\] returns a list of everything<br>
  that can be obtained from the initial list by applying the operators<br>
  iteratively.

### OrderedSetPartitions

OrderedSetPartitions\[n\] returns all ordered set partitions of {1,2,...,n}. See A000670.

### PackedWords

PackedWords\[n\] returns all packed words of length n.

### PartitionAbacus

PartitionAbacus\[lam,d\] returns the abacus representation of lambda.

### PartitionAddBox

PartitionAddBox\[lam\] returns all partitions with one box more than lambda.<br>
PartitionAddBox\[lam, top\] returns only those partitions contained in the bounding partition top. An empty top is unbounded.

### PartitionArm

PartitionArm\[mu,{r,c}\] returns the arm length mu\[\[r\]\]-c of the cell {r,c}, or 0 when r is outside mu.

### PartitionCore

PartitionCore\[lam,d\] returns the d-core of lambda.

### PartitionCores

PartitionCores\[n,p\] returns all partitions of n with no hook-length divisible by p.

### PartitionDominatesQ

PartitionDominatesQ\[lam,mu\] returns True if mu dominates lam in dominance order.

### PartitionDropColumn

PartitionDropColumn\[i\]\[nu\] removes column i from the partition.

### PartitionDropRow

PartitionDropRow\[i\]\[nu\] removes row i from the partition.

### PartitionInterval

PartitionInterval\[lam,mu\] returns all partitions nu, such that mu&lt;=nu&lt;=lam in Young's lattice.

### PartitionIntervalSize

PartitionIntervalSize\[lam,mu\] returns the number of partitions nu, such that mu&lt;=nu&lt;=lam in Young's lattice.

### PartitionJoin

PartitionJoin\[a,b\] returns a new partition whose parts are the union of a and b.

### PartitionLeg

PartitionLeg\[mu,{r,c}\] returns the leg length of the cell {r,c}, computed using the conjugate partition.

### PartitionLessEqualQ

PartitionLessEqualQ\[lam,mu\] returns true if lam is entrywise less-equal to mu.

### PartitionList

PartitionList\[list,mu\] partitions the list into non-overlapping pieces of sizes given by mu.

### PartitionN

PartitionN\[lam\] returns n(lam) = Sum\[(i-1) lam\[\[i\]\], {i,Length\[lam\]}\] for a partition lam.

### PartitionPartCount

PartitionPartCount\[lam\] returns (m1,m2,...) so that mi is then number of parts of size i.

### PartitionPath

PartitionPath\[lam\] returns the North-East path outlining the partition.

### PartitionQuotient

PartitionQuotient\[lam,d\] returns the partition quotient by d. Also works on skew shapes.

### PartitionRemoveBox

PartitionRemoveBox\[lam\] lists all partitions obtainable from lambda with one box removed.<br>
PartitionRemoveBox\[lam, bot\] returns only those partitions containing the lower bounding partition bot. An empty bot is unbounded.

### PartitionRemoveHorizontalStrip

PartitionRemoveHorizontalStrip\[lam, k\] returns all partitions obtainable from lam, by removing a horizontal strip of size k.

### PartitionRemoveVerticalStrip

PartitionRemoveVerticalStrip\[lam,k\] returns all partitions obtainable from lam by removing a vertical strip of size k.

### PartitionStrictDominatesQ

PartitionStrictDominatesQ\[lam,mu\] returns True if mu strictly dominates lam.

### PartitionToMultirectangular

PartitionToMultirectangular\[lam\] returns the widths and heights of the rectangles,<br>
in multirectangular notation.

### PathExceedanceDecreaseMap

PathExceedanceDecreaseMap\[bw\] applies the path exceedance map on a path from (0,0) to (n,n).

### PermutationOfType

PermutationOfType\[mu\] returns a canonical one-line permutation with cycle type mu.

### PermutationPeaks

PermutationPeaks\[pi\] returns the number of indices i with pi\[\[i-1\]\] &lt; pi\[\[i\]\] &gt; pi\[\[i+1\]\].

### PermutationPeaksSet

PermutationPeaksSet\[pi\] returns the 1-based positions of the peaks of pi.

### PermutationPeakValues

PermutationPeakValues\[pi\] returns the sorted values at the peaks of pi.

### PermutationValleys

PermutationValleys\[pi\] returns the number of indices i with pi\[\[i-1\]\] &gt; pi\[\[i\]\] &lt; pi\[\[i+1\]\].

### PermutationValleysSet

PermutationValleysSet\[pi\] returns the 1-based positions of the valleys of pi.

### qAlternatingSignMatrices

qAlternatingSignMatrices\[n,q\] returns the q-enumeration polynomial for alternating sign matrices of size n.

### qBinomial

qBinomial\[n,k,q\] returns the q-binomial coefficient, with q defaulting to 1.

### qCarlitzCatalan

qCarlitzCatalan\[n,q\] is the area-generating q-Catalan number

### qCatalan

qCatalan\[n,q\] returns the q-Catalan polynomial, with q defaulting to 1.

### qFactorial

qFactorial\[n,q\] returns the q-factorial, with q defaulting to 1.

### qHookFormula

qHookFormula\[lam,q\] returns the q-hook formula for the partition lam, with q defaulting to 1.

### qInteger

qInteger\[n,q\] returns the q-integer, with q defaulting to 1.

### qIntegerFactorize

qIntegerFactorize\[expr,q\] returns a q-integer factorization of expr, leaving any residual factor in the result.

### qKreweras

qKreweras\[lam,q\] returns the q-Kreweras polynomial for the partition lam, with q defaulting to 1.

### qMultinomial

qMultinomial\[lam,q\] returns the q-multinomial coefficient for the composition or partition lam, with q defaulting to 1.

### qNarayana

qNarayana\[n,k,q\] returns the q-Narayana polynomial, with q defaulting to 1.

### qtCatalan

qtCatalan\[n,q,t\] is the qt-Catalan polynomial.

### qtFibonacci

qtFibonacci\[n,q,t\] returns the n:th qt-Fibonacci number.

### RandomSetPartition

RandomSetPartition\[n,k\] returns a uniform choice of a set partition of n into exactly k blocks.<br>
RandomSetPartition\[n\] chooses uniformly among all set partitions of \[n\]

### RefineBijection

RefineBijectionQ\[setA,setB,{{f1,g1},{f2.g2}...}\] checks if there is a bijection phi:A-&gt;B <br>
with the property that {f1(a) = g1(phi(a)) , f2(a) = g2(phi(a)) , ...} for all a in A.<br>
If so, we return lists { (A1,B1), (A2,B2),... } where Ak is a part of A that must map to the part Bk.

### RefineBijectionQ

RefineBijectionQ\[setA,setB,{{f1,g1},{f2.g2}...}\] checks if there is a bijection phi:A-&gt;B <br>
with the property that {f1(a) = g1(phi(a)) , f2(a) = g2(phi(a)) , ...} for all a in A.

### RightToLeftMaxima

RightToLeftMaxima\[p\] returns the elements of p that are right-to-left maxima.

### RightToLeftMinima

RightToLeftMinima\[p\] returns the elements of p that are right-to-left minima.

### Runs

Runs\[w\] returns the maximal contiguous weakly increasing runs of the list w.

### RunSort

RunSort\[pi\] sorts each maximal weakly increasing run of pi and concatenates the runs.

### RunSortedPermutations

RunSortedPermutations\[n\] returns all run-sorted permutations of {1, ..., n}, that is, permutations whose maximal increasing runs have increasing first entries; there are BellB\[n-1\] of them for n &gt;= 1. RunSortedPermutations\[0\] is {}.

### SageForm

SageForm\[expr\] returns a modest SageMath syntax string for integer lists, Young tableaux, and supported symmetric-function basis symbols.

### SetPartitionBlockIndex

SetPartitionBlockIndex\[sp\] returns a list whose entry i is the index of the block containing i.

### SetPartitionRefinementQ

SetPartitionRefinementQ\[p1, p2\] returns True when every block of p1 is contained in a block of p2.

### SetPartitions

SetPartitions\[n\] returns all set partitions of {1,2,...,n}. The cardinalities are given by the Bell numbers, A000110.

### SetPartitionsNoZeroBlock

SetPartitionsNoZeroBlock\[elems\] returns all signed set partitions of elems without zero blocks.<br>
A zero block is a block B such that B=-B.

### SetPartitionsTypeB

SetPartitionsTypeB\[n\] returns a list of all set partitions of type B.

### SetPartitionToRunSortedPermutation

SetPartitionToRunSortedPermutation\[sp\] returns a flattened permutation of size n+1, where runs are in lexicographic order. Map due to O. Nabawanda.

### SetsStabilizer

SetsStabilizer\[sets\] returns all permutations (of Union @@ sets ) that preserve each set.

### ShapeUnion

ShapeUnion\[{lam1,mu1}, {lam2,mu2}, ...\] places skew shapes side by side and returns their union.

### Shuffles

Shuffles\[listA,listB,listC,...\] returns all shuffles of A, B, C, and so on.

### SkewShapeQ

SkewShapeQ\[lam, mu\] returns True when mu is contained in lam.<br>
SkewShapeQ\[lam, mu, w\] additionally requires the skew shape to have size Total\[w\].

### SnCharacter

SnCharacter\[lam,mu\] returns the irreducible character of the symmetric group indexed by partition lam, evaluated at cycle type mu.

### StandardizeList

StandardizeList\[list\] standardizes the list. For equal entries, order from the left.

### StrictEdges

StrictEdges is an option for functions on graphs, posets and colorings (UnicellularChromatics, PosetData); its value is a list of edges {a, b} that must be strict, for example a coloring with c\[a\] &lt; c\[b\] or an orientation a -&gt; b.

### SubsetTuples

SubsetTuples\[weight,k\] returns a list of sets, each have size k, <br>
 such that the multiset has the given weight. Thus, we require that the total of weight is a multiple of k.

### TupleDescents

TupleDescents\[s1,s2\] returns the minimal number of descents between the sets if they are put in adjacent columns.

### TupleInversions

TupleInversions\[{s1,s2,...sk}\] returns <br>
the tuple-inv associated with the tuple.

### TupleMajorIndex

TupleMajorIndex\[{s1,s2,...sk}\] <br>
returns the tuple-maj associated with this tuple.

### UnimodalQ

UnimodalQ\[list\] returns True if list is unimodal, allowing equal adjacent entries.

### WeakEdges

WeakEdges is an option for functions on graphs, posets and colorings (UnicellularChromatics, PosetData); its value is a list of edges {a, b} that must be weak, for example a coloring with c\[a\] &lt;= c\[b\].

### WeakIntegerCompositions

WeakIntegerCompositions\[n,k\] gives a list of all weak compositions of n with k parts.

### WeakStandardize

WeakStandardize\[list\] replaces each distinct value by its rank among the distinct values, preserving ties.

### WordCharge

WordCharge\[w\] returns the charge of a word with partition weight.

Background: [Kostka coefficients, Kostka–Foulkes polynomials and charge](https://www.symmetricfunctions.com/kostkaFoulkes.htm)

### WordCocharge

WordCocharge\[w\] returns the cocharge of a word with partition weight.

Background: [Kostka coefficients, Kostka–Foulkes polynomials and charge](https://www.symmetricfunctions.com/kostkaFoulkes.htm)

### WordComposition

WordComposition\[bw\] is the inverse of CompositionWord, and returns a composition.

### YoungLatticePaths

YoungLatticePaths\[mu, nu\] returns all saturated chains from mu to nu in Young's lattice.

### ZCoefficient

ZCoefficient\[lam\] returns the Z-coefficient, as p. 299, Enumerative Combinatorics II, Stanley


{% endraw %}
