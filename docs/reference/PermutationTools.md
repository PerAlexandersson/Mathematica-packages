---
title: PermutationTools
parent: Reference
nav_order: 8
---

# PermutationTools

Pattern avoidance, Foata and related maps, Bruhat and weak order, families of permutations.

Load with `` Needs["PermutationTools`"] ``.

Background on symmetricfunctions.com: [Permutations](https://www.symmetricfunctions.com/permutations.htm), [Permutation patterns](https://www.symmetricfunctions.com/permutationPatterns.htm), [Families of permutations](https://www.symmetricfunctions.com/permutationFamilies.htm).

## Functions and symbols

### AlternatingPermutations

AlternatingPermutations\[n\] returns a list of alternating permutations (up-down permutations), see A000111.

### BruhatLowerOrderIdeal

BruhatLowerOrderIdeal\[permutation\] returns all permutations below the given permutation in strong Bruhat order.

### CanonPermutations

CanonPermutations\[lam\] generates canon permutations with content lambda.

### CarlitzMap

CarlitzMap\[pi\] sends inv to maj.

### FoataMap

FoataMap\[pi\] sends maj to inv.

### FromSubExcedance

FromSubExcedance\[f\] returns the permutation associated with a sub-excedance word f. FromSubExcedance\[f, pi\] applies the word to the initial permutation pi.

### GeneratePAPS

GeneratePAPS\[n\] returns all parity-alternating permutations of size n. GeneratePAPS\[n, condition\] restricts both component permutations and the result using condition.

### GenerateRAPS

GenerateRAPS\[n, k, r\] returns the restricted alternating permutations generated from k component permutations; r defaults to 1.

### GrassmannPermutations

GrassmannPermutations\[n\] returns all permutations with at most one descent. See A000325.

### Is123AvoidingQ

Is123AvoidingQ\[permutation\] returns True if the permutation avoids the pattern 123.

Background: [Permutation patterns](https://www.symmetricfunctions.com/permutationPatterns.htm)

### Is132AvoidingQ

Is132AvoidingQ\[permutation\] returns True if the permutation avoids the pattern 132.

Background: [Permutation patterns](https://www.symmetricfunctions.com/permutationPatterns.htm)

### Is213AvoidingQ

Is213AvoidingQ\[permutation\] returns True if the permutation avoids the pattern 213.

Background: [Permutation patterns](https://www.symmetricfunctions.com/permutationPatterns.htm)

### Is231AvoidingQ

Is231AvoidingQ\[permutation\] returns True if the permutation avoids the pattern 231.

Background: [Permutation patterns](https://www.symmetricfunctions.com/permutationPatterns.htm)

### Is312AvoidingQ

Is312AvoidingQ\[permutation\] returns True if the permutation avoids the pattern 312.

Background: [Permutation patterns](https://www.symmetricfunctions.com/permutationPatterns.htm)

### Is321AvoidingQ

Is321AvoidingQ\[permutation\] returns True if the permutation avoids the pattern 321.

Background: [Permutation patterns](https://www.symmetricfunctions.com/permutationPatterns.htm)

### IsPAPQ

IsPAPQ\[permutation\] returns True if odd positions contain odd values and even positions contain even values.

### IsPermutationAvoidingQ

IsPermutationAvoidingQ\[sigma,pi\] returns true if pi avoids the pattern sigma.

Background: [Permutation patterns](https://www.symmetricfunctions.com/permutationPatterns.htm)

### NQueensPermutations

NQueensPermutations\[n\] returns the permutations solving the n-queens problem on an n by n board.

### PairToPAP

PairToPAP\[{p1, p2}\] interleaves p1 and p2 after mapping their entries to odd and even values, respectively.

### PAPS123

PAPS123\[n\] returns all parity-alternating permutations of size n that avoid 123.

### PAPS132

PAPS132\[n\] returns all parity-alternating permutations of size n that avoid 132.

### PAPS213

PAPS213\[n\] returns all parity-alternating permutations of size n that avoid 213.

### PAPS231

PAPS231\[n\] returns all parity-alternating permutations of size n that avoid 231.

### PAPS312

PAPS312\[n\] returns all parity-alternating permutations of size n that avoid 312.

### PAPS321

PAPS321\[n\] returns all parity-alternating permutations of size n that avoid 321.

### PAPToPair

PAPToPair\[pap\] splits a parity-alternating permutation into its odd-position and even-position subsequences, undoing PairToPAP.

### PermutationAllCycles

PermutationAllCycles\[pi\] returns all cycles of the permutation, including fixed-points.

### PermutationCharge

PermutationCharge\[permutation\] returns the charge statistic of a permutation.

### PermutationCocharge

PermutationCocharge\[permutation\] returns the cocharge statistic of a permutation.

### PermutationCode

PermutationCode\[p\] returns the code of the permutation.

### PermutationCycleMap

PermutationCycleMap\[p\] is the map defined on p.23 Stanley's EC1, where one writes the permutation in cycle form, and removes parenthesises.

### PermutationFromWord

PermutationFromWord\[word, n\] returns the permutation of \[n\] represented by a word of simple transpositions.

### PermutationGenus

PermutationGenus\[pi\] returns the genus of the permutation.

### PermutationMatrixPlot

PermutationMatrixPlot\[pi\] returns a graphical representation of the permutation.

### PermutationSkewSum

PermutationSkewSum\[p1,p2,...\] returns the skew sum of the permutations (places the permutation matrices along the anti-diagonal of a block matrix).

### PermutationSum

PermutationSum\[p1,p2,...\] returns the direct sum of the permutations (places the permutation matrices along the main diagonal of a block matrix).

### PermutationType

PermutationType\[pi\] returns the partition of cycle lengths

### ReducedWord

ReducedWord\[pi\] returns a reduced word for pi.

### SeparablePermutationQ

SeparablePermutationQ\[pi\] returns true if the permutation is separable.

### Si

Si\[pi, i\] swaps entries i and i+1 in a permutation pi. Si\[i\]\[pi\] is the equivalent curried form.

### SimionSchmidtMap

SimionSchmidtMap\[perm\] ,see https://core.ac.uk/download/pdf/82232421.pdf

### SimsunPermutations

SimsunPermutations\[n\] returns all Simsun permutations.

### SkewMergedPermutations

SkewMergedPermutations\[n\] returns the permutations in S\_n avoiding 2143 and 3412.

### SplitSeparablePermutation

SplitSeparablePermutation\[pi\] splits the permutation into smaller blocks. The original permutation is then a sum or skew sum of these blocks. Use option 'False' to only consider direct sum.

### StirlingPermutations

StirlingPermutations\[n\] returns all Stirling permutations of order n.

### StrongBruhatDownSet

StrongBruhatDownSet\[permutation\] returns all permutations below the given permutation in strong Bruhat order.

### StrongBruhatGreaterQ

StrongBruhatGreaterQ\[p1,p2\] returns true iff p1 <br>
is greater-equal than p2 in strong Bruhat order. The identity is smaller than all other, and w0 is largest.

### ToSubExcedance

ToSubExcedance\[pi\] returns the sub-excedance function associated with the permutation pi.

### TupleToRAP

TupleToRAP\[lists, r\] interleaves a list of permutations into a restricted alternating permutation; r defaults to 1.

### TypeBPermutations

TypeBPermutations\[n\] returns all permutations of type B.

### WachsPermutations

WachsPermutations\[n\] returns a list of all Wachs permutations in S\_n. See arxiv:2212.04932.

### WeakLowerOrderIdeal

WeakLowerOrderIdeal\[pi\] returns all permutations below pi in the weak order

### WeakOrderGreaterQ

WeakOrderGreaterQ\[p1,p2\] returns true iff p1 is greater <br>
than p2 in weak order. The identity is smaller than all other, and w0 is largest.

