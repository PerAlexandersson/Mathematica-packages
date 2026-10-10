---
title: NewTableaux
parent: Reference
nav_order: 4
---

# NewTableaux

Standard and semistandard (skew) Young tableaux, RSK, promotion, evacuation, crystal operators, border strips, TeX output; semistandard augmented fillings (`SSAF`) with statistics, crystals and Mason insertion.

Load with `` Needs["NewTableaux`"] ``.

Background on symmetricfunctions.com: [Partitions, permutations and tableaux](https://www.symmetricfunctions.com/preliminaries.htm), [RSK, the Robinson–Schensted–Knuth correspondence](https://www.symmetricfunctions.com/rsk.htm), [Crystals](https://www.symmetricfunctions.com/crystals.htm), [Operations on Young tableaux](https://www.symmetricfunctions.com/tableauOperators.htm), [Border strip tableaux and the Littlewood map](https://www.symmetricfunctions.com/borderStripTableaux.htm).

## Functions and symbols

### ArrayToBiword

ArrayToBiword\[a\] converts a nonnegative integer array a to a biword listing the positions of its entries.

### AtomFillings

AtomFillings\[alpha\] returns the augmented fillings enumerating the Demazure atom indexed by alpha.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### BinaryMatrixToBiword

BinaryMatrixToBiword\[m\] converts a binary matrix m to its biword of positions of 1 entries.

### BiwordRSK

BiwordRSK\[{w1, w2}\] applies row-insertion RSK to two equal-length words and returns a pair of YoungTableau objects.<br>
BiwordRSK\[w\] applies RSK with the increasing word Range\[Length\[w\]\] as the first word.

Background: [RSK, the Robinson–Schensted–Knuth correspondence](https://www.symmetricfunctions.com/rsk.htm)

### BiwordRSKDual

BiwordRSKDual\[{w1, w2}\] applies dual row-insertion RSK to two equal-length words and returns a pair of YoungTableau objects.<br>
BiwordRSKDual\[w\] applies dual RSK with the increasing word Range\[Length\[w\]\] as the first word.

Background: [RSK, the Robinson–Schensted–Knuth correspondence](https://www.symmetricfunctions.com/rsk.htm)

### BorderStrips

BorderStrips\[shape,size\] returns a list of pairs <br>
{new-shape,strip} of possible border-strips to remove.

Background: [Border strip tableaux and the Littlewood map](https://www.symmetricfunctions.com/borderStripTableaux.htm)

### BorderStripTableaux

BorderStripTableaux\[shape, type\] returns a list of all border-strip tableaux of the shape.

Background: [Border strip tableaux and the Littlewood map](https://www.symmetricfunctions.com/borderStripTableaux.htm)

### BSTHeightVector

BSTHeightVector\[bst\] returns a vector where vi is the height of strip i.

### ChargeToMajMap

ChargeToMajMap\[ssaf\] applies the charge-to-major-index map to an augmented filling.

### ColumnLatticePaths

ColumnLatticePaths\[ssyt\] returns a graphical representation of the ssyt <br>
as a set of non-intersecting lattice paths, each path corresponding to a column in the ssyt

### CrystalEi

CrystalEi\[ssyt,i\] performs the crystal raising operator ei on the tableau. <br>
It also works on lists

Background: [Crystals](https://www.symmetricfunctions.com/crystals.htm)

### CrystalFi

CrystalFi\[ssyt,i\] performs the crystal lowering operator fi on the tableau. It also works on words.

Background: [Crystals](https://www.symmetricfunctions.com/crystals.htm)

### CrystalSi

CrystalSi\[ssyt,i\] performs the crystal <br>
transposition operator si on the tableau. It also works on words.

Background: [Crystals](https://www.symmetricfunctions.com/crystals.htm)

### CylindricSYT

CylindricSYT\[lam, k\] returns all standard Young tableaux of cylindric shape lam with shift k. CylindricSYT\[{lam, mu}, k\] uses skew shape lam/mu; k defaults to 0.

### CylindricTableaux

CylindricTableaux\[{lam,mu},k\] <br>
produces all cylindric tableaux with partition weight and shifted up k steps from minimal possible shift.

### HasOuterCornerQ

HasOuterCornerQ\[tab\] returns True when the skew shape of tab has an outer corner in the legacy OldYoungTableaux sense.<br>
HasOuterCornerQ\[{lam, mu}\] applies the same test to a skew shape.

### InvMajStatistic

InvMajStatistic\[tab\] returns the modified <br>
Macdonald polynomial statistics {inv,maj}. Works on skew shapes.

### KeyFillings

KeyFillings\[alpha\] returns the augmented fillings enumerating the key polynomial indexed by alpha.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### KnuthRepresentative

KnuthRepresentative\[pi\] returns the unique permutation which is Knuth equivalent to pi, and is the reading word of some SYT.

### LascouxSchutzenberger

LascouxSchutzenberger\[object,i\] applies the Lascoux--Schutzenberger involution.

### LineBreaks

LineBreaks is an option for YTableauTeX; its default is True.

### LongestIncreasingSubsequence

LongestIncreasingSubsequence\[w\] returns the length of the longest increasing subsequence.

### PlanePartitions

PlanePartitions\[shape,max\] returns all plane partitions of given shape, with entries &lt;= max.<br>
PlanePartitions\[a,b,c\] returns all a x b plane partitions with entries bounded by c.

### RowLatticePaths

RowLatticePaths\[ssyt\] returns a graphical representation of the ssyt <br>
as a set of non-intersecting lattice paths, each path corresponding to a row in the ssyt

### RPPToAtom

RPPToAtom\[rpp\] applies Mason's column-set insertion map to an augmented reverse plane partition.

### SemiStandardYoungTableaux

SemiStandardYoungTableaux\[{lam,mu},w\] returns<br>
a list of all SSYT with given shape and weight.

### SpecialRimHookTableaux

SpecialRimHookTableaux\[shape,type\] returns all special rim-hook tableaux of given shape and type.

### SSAF

SSAF\[rows\] represents a semistandard augmented filling with basement entries in the first column.

### SSAFBasement

SSAFBasement\[ssaf\] returns the basement entries of ssaf.

### SSAFCoInversions

SSAFCoInversions\[ssaf\] returns the number of coinversion triples of ssaf.

### SSAFColumnSets

SSAFColumnSets\[ssaf, start\] returns sorted column sets, starting at column start (default 2).

### SSAFCrystalString

SSAFCrystalString\[ssaf,i\] returns the i-crystal string containing ssaf.

Background: [Crystals](https://www.symmetricfunctions.com/crystals.htm)

### SSAFCrystalWord

SSAFCrystalWord\[ssaf,i\] returns the uncancelled i-crystal word of ssaf.

Background: [Crystals](https://www.symmetricfunctions.com/crystals.htm)

### SSAFDn

SSAFDn\[ssaf\] returns the number of unequal horizontal adjacencies of ssaf.

### SSAFForm

SSAFForm\[ssaf, options\] returns a graphical representation of an augmented filling.

### SSAFillings

SSAFillings\[alpha, basement\] returns all valid augmented fillings of shape alpha and basement.

### SSAFInversions

SSAFInversions\[ssaf\] returns the number of inversion triples of ssaf.

### SSAFKnownCharge

SSAFKnownCharge\[ssaf\] returns the charge when the augmented filling has partition shape.

### SSAFMajorIndex

SSAFMajorIndex\[ssaf\] returns the augmented filling major index.

### SSAFMonomial

SSAFMonomial\[ssaf,x\] returns the weight monomial of ssaf.

### SSAFQ

SSAFQ\[ssaf\] returns True if ssaf is a valid semistandard augmented filling.

### SSAFShape

SSAFShape\[ssaf\] returns the weak composition of non-basement row lengths.

### SSAFWeight

SSAFWeight\[ssaf\] returns the weight vector of the non-basement entries of ssaf.

### SSAFWeightNormalize

SSAFWeightNormalize\[ssaf\] applies crystal involutions until the weight is a partition.

### SSYTCharge

SSYTCharge\[ssyt\] returns the charge of a semistandard Young tableau.

### SSYTCocharge

SSYTCocharge\[ssyt\] returns the cocharge of a semistandard Young tableau.

### SSYTKPromotion

SSYTKPromotion\[ssyt,k\] performs k-promotion.

Background: [Operations on Young tableaux](https://www.symmetricfunctions.com/tableauOperators.htm)

### SSYTKPromotionInverse

SSYTKPromotionInverse\[ssyt,k\] performs the inverse of k-promotion. This also works on skew shapes!

Background: [Operations on Young tableaux](https://www.symmetricfunctions.com/tableauOperators.htm)

### SSYTToAtom

SSYTToAtom\[tab\] applies Mason's insertion map from a semistandard tableau to an atom filling.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### StandardYoungTableaux

StandardYoungTableaux\[n\], <br>
StandardYoungTableaux\[lam\] or StandardYoungTableaux\[{lam,mu}\] returns a list of SYTs

### SuperStandardTableau

SuperStandardTableau\[{lam,mu}\] returns the SYT with 1,2,.. in first row and so on.

### SYTCharge

SYTCharge\[ssyt\] returns the charge of a semistandard tableau, with partition weight.

### SYTCocharge

SYTCocharge\[ssyt\] returns the cocharge of a semistandard tableau.

### SYTDescents

SYTDescents\[syt\] returns the number of descents of a standard Young tableau.

### SYTDescentSet

SYTDescentSet\[syt\] returns the descent set of the standard Young tableau.

### SYTDualMajorIndex

SYTDualMajorIndex\[syt\] returns the dual major index of a standard Young tableau.

### SYTEvacuation

SYTEvacuation\[syt\] performs the evacuation involution on the SYT. Does not work on skew shapes or SSYTs.

Background: [Operations on Young tableaux](https://www.symmetricfunctions.com/tableauOperators.htm)

### SYTEvacuationDual

SYTEvacuationDual\[syt\] is the dual evacuation.

Background: [Operations on Young tableaux](https://www.symmetricfunctions.com/tableauOperators.htm)

### SYTMajorIndex

SYTMajorIndex\[syt\] returns the major index of a standard Young tableau.

### SYTMax

SYTMax\[tab\] returns the maximum entry in the tableau.

### SYTPromotion

SYTPromotion\[syt,\[k\]\] performes the promotion operator k times. Default is 1 time.

Background: [Operations on Young tableaux](https://www.symmetricfunctions.com/tableauOperators.htm)

### SYTReadingWord

SYTReadingWord\[tab\] returns the reading word of tab, formed by reading rows from bottom to top and omitting None entries.

### SYTSize

SYTSize\[tab\] returns the number of boxes in the tableau.

### SYTStandardize

SYTStandardize\[ssyt\] standardizes the (skew) ssyt.

### TableauShortTeX

TableauShortTeX\[tab\] returns the \\tableaushort{..} TeX string for the tableau.

### TAtomFillings

TAtomFillings\[alpha\] returns the non-attacking augmented fillings used by the t-atom identity.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### UseArray

UseArray is an option for YTableauTeX; its default is True and False selects the legacy \\young representation.

### YoungDiagramForm

YoungDiagramForm\[lam, options\] returns a graphical representation of the Young diagram of partition lam. YoungDiagramForm\[{lam, mu}, options\] uses skew shape lam/mu. ItemSize defaults to 1 and DescentSet defaults to False.

### YoungTableau

YoungTableau\[data\] represents a Young tableau.

### YoungTableauForm

YoungTableauForm\[tab, options\] returns a graphical representation of the Young tableau. ItemSize defaults to 1 and DescentSet defaults to False.

### YoungTableauShape

YoungTableauShape\[tab\] returns the outer shape of the tableau. <br>
	YoungTableauShape\[tab,m\] returns the shape of formed by all entries &lt;=m.

### YoungTableauSize

YoungTableauSize\[tab\] returns the number of boxes in the tableau, excluding skew boxes represented by None.

### YoungTableauWeight

YoungTableauWeight\[tab\] returns the weight vector counting entries 1 through the maximum entry of tab.

### YTableauTeX

YTableauTeX\[tab, options\] returns a TeX string for tab. LineBreaks defaults to True and UseArray selects the legacy \\young representation when False.

