---
title: NonsymmetricPolynomials
parent: Reference
nav_order: 13
---

# NonsymmetricPolynomials

Divided difference, Demazure and Demazure–Lusztig operators (also K-theoretic); key, atom, t-key, t-atom, Schubert, Grothendieck, Lascoux, fundamental slide, lock and dual Grothendieck polynomials; nonsymmetric Macdonald polynomials (also with permuted basements) and nonsymmetric Jack polynomials; basis symbols with conversions.

Load with `` Needs["NonsymmetricPolynomials`"] ``.

Background on symmetricfunctions.com: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm), [Schubert polynomials](https://www.symmetricfunctions.com/schubert.htm), [Grothendieck polynomials](https://www.symmetricfunctions.com/grothendieck.htm), [Lascoux polynomials](https://www.symmetricfunctions.com/lascoux.htm), [Slide, forest, and lock polynomials](https://www.symmetricfunctions.com/assaf.htm), [Macdonald E polynomials](https://www.symmetricfunctions.com/macdonaldE.htm), [Permuted basement Macdonald E polynomials](https://www.symmetricfunctions.com/macdonaldEperm.htm).

## Functions and symbols

### AtomPolynomial

AtomPolynomial\[alpha, x\] returns the Demazure atom A\_alpha in x\[1\], x\[2\], ... for a weak composition alpha, computed as theta\_w x^sort(alpha). Standard indexing: AtomPolynomial\[{0, 1}, x\] is x\[2\].

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### AtomSymbol

AtomSymbol\[alpha, x\] represents the Demazure atom A\_alpha in the variables x\[1\], x\[2\], ... as a basis element; trailing zeros of alpha are removed. NonsymmetricToPolynomial expands it.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### ClearNonsymmetricPolynomialsCache

ClearNonsymmetricPolynomialsCache\[\] removes all stored results of NonsymmetricPolynomials, such as computed key polynomials and basis transition matrices.

### CodeToPermutation

CodeToPermutation\[c\] returns the permutation (one-line notation) with Lehmer code c, in the smallest symmetric group that contains it.

Background: [Schubert polynomials](https://www.symmetricfunctions.com/schubert.htm)

### DemazureAtomOperator

DemazureAtomOperator\[f, x, i\] applies theta\_i = pi\_i - 1, the operator generating Demazure atoms.<br>
DemazureAtomOperator\[f, x, {i1, ..., ik}\] applies the composition with i\_k acting first.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### DemazureOperator

DemazureOperator\[f, x, i\] applies the isobaric divided difference (Demazure operator) pi\_i f = DividedDifference\[x\[i\] f, x, i\].<br>
DemazureOperator\[f, x, {i1, ..., ik}\] applies the composition with i\_k acting first.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### DividedDifference

DividedDifference\[f, x, i\] applies the divided difference operator (f - s\_i f)/(x\[i\] - x\[i+1\]).<br>
DividedDifference\[f, x, {i1, ..., ik}\] applies the composition with i\_k acting first.

Background: [Schubert polynomials](https://www.symmetricfunctions.com/schubert.htm)

### DualGrothendieckPolynomial

DualGrothendieckPolynomial\[alpha, x\] returns the dual Grothendieck polynomial indexed by the weak composition alpha, in x\[1\], ..., x\[Length\[alpha\]\]: the sum over fillings of the shape Reverse\[alpha\] (entries in 1..n, rows weakly decreasing, each entry at most the entry above it) of the column weight (each value counted once per column). For weakly increasing alpha it is the symmetric dual Grothendieck polynomial g\_lambda (Lam-Pylyavskyy) of the sorted nonzero parts lambda.

Background: [Grothendieck polynomials](https://www.symmetricfunctions.com/grothendieck.htm)

### FundamentalSlidePolynomial

FundamentalSlidePolynomial\[alpha, x\] returns the fundamental slide polynomial of the weak composition alpha, the sum of x^beta over weak compositions beta whose nonzero flattening refines alpha with every prefix sum at least that of alpha.

Background: [Slide, forest, and lock polynomials](https://www.symmetricfunctions.com/assaf.htm)

### FundamentalSlideSymbol

FundamentalSlideSymbol\[alpha, x\] represents the fundamental slide polynomial indexed by alpha; trailing zeros are removed. NonsymmetricToPolynomial expands it.

Background: [Slide, forest, and lock polynomials](https://www.symmetricfunctions.com/assaf.htm)

### GrothendieckPolynomial

GrothendieckPolynomial\[w, x\] returns the beta = -1 Grothendieck polynomial of the permutation w. GrothendieckPolynomial\[w, x, beta\] uses the K-divided differences KDividedDifference (the divided difference of (1 + beta x\[i+1\]) f), so beta = 0 is SchubertPolynomial and beta = -1 is the usual sign convention. Its expansion in Lascoux polynomials has coefficients beta^(\|alpha\| - length(w)) times nonnegative integers.

Background: [Grothendieck polynomials](https://www.symmetricfunctions.com/grothendieck.htm)

### GrothendieckSymbol

GrothendieckSymbol\[w, x\] represents the beta = -1 Grothendieck polynomial of w; trailing fixed points are removed. NonsymmetricToPolynomial expands it.

Background: [Grothendieck polynomials](https://www.symmetricfunctions.com/grothendieck.htm)

### IntegralMacdonaldE

IntegralMacdonaldE\[alpha, x, q, t\] returns the integral-form nonsymmetric Macdonald polynomial, normalized as IntegralFormFactor\[alpha, q, t\] times MacdonaldEPolynomial\[alpha, x, q, t\].

Background: [Macdonald E polynomials](https://www.symmetricfunctions.com/macdonaldE.htm)

### KDemazureOperator

KDemazureOperator\[f, x, beta, i\] applies the K-theoretic Demazure operator KDividedDifference\[x\[i\] f, x, beta, i\], which defines Lascoux polynomials; beta = 0 gives DemazureOperator.<br>
KDemazureOperator\[f, x, beta, {i1, ..., ik}\] applies the composition with i\_k acting first.

Background: [Lascoux polynomials](https://www.symmetricfunctions.com/lascoux.htm)

### KDividedDifference

KDividedDifference\[f, x, beta, i\] applies the connective K-theoretic divided difference DividedDifference\[(1 + beta x\[i+1\]) f, x, i\]; beta = 0 gives DividedDifference.<br>
KDividedDifference\[f, x, beta, {i1, ..., ik}\] applies the composition with i\_k acting first.

Background: [Grothendieck polynomials](https://www.symmetricfunctions.com/grothendieck.htm)

### KeyPolynomial

KeyPolynomial\[alpha, x\] returns the key polynomial (Demazure character) kappa\_alpha in x\[1\], x\[2\], ... for a weak composition alpha, computed as pi\_w x^sort(alpha) with Demazure operators. Standard indexing: KeyPolynomial\[{0, 1}, x\] is x\[1\] + x\[2\].

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### KeySymbol

KeySymbol\[alpha, x\] represents the key polynomial kappa\_alpha in the variables x\[1\], x\[2\], ... as a basis element; trailing zeros of alpha are removed. NonsymmetricToPolynomial expands it.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### LascouxPolynomial

LascouxPolynomial\[alpha, x\] returns the beta = -1 Lascoux polynomial. LascouxPolynomial\[alpha, x, beta\] applies KDemazureOperator along sortingWord\[alpha\] to x^sort(alpha); beta = 0 is KeyPolynomial.

Background: [Lascoux polynomials](https://www.symmetricfunctions.com/lascoux.htm)

### LascouxSymbol

LascouxSymbol\[alpha, x\] represents the beta = -1 Lascoux polynomial in the variables x\[1\], x\[2\], ...; trailing zeros of alpha are removed. NonsymmetricToPolynomial expands it.

Background: [Lascoux polynomials](https://www.symmetricfunctions.com/lascoux.htm)

### LockPolynomial

LockPolynomial\[alpha, x\] returns the lock polynomial of the weak composition alpha (Assaf-Searles): the Kohnert polynomial of the right-justified diagram with alpha\[\[i\]\] cells in row i. Trailing zeros of alpha do not matter, and a lock with one nonzero row is a key polynomial.

Background: [Slide, forest, and lock polynomials](https://www.symmetricfunctions.com/assaf.htm)

### LockSymbol

LockSymbol\[alpha, x\] represents the lock polynomial indexed by the weak composition alpha; trailing zeros are removed. NonsymmetricToPolynomial expands it.

Background: [Slide, forest, and lock polynomials](https://www.symmetricfunctions.com/assaf.htm)

### MacdonaldEPolynomial

MacdonaldEPolynomial\[alpha, x, q, t\] returns the nonsymmetric Macdonald polynomial E\_alpha in Length\[alpha\] variables, with leading monomial x^alpha and identity basement. MacdonaldEPolynomial\[alpha, sigma, x, q, t\] uses the permutation basement sigma, obtained from the identity basement by t-Demazure operators (Alexandersson 2019, Corollary 16). It is generated from 1 by the Knop-Sahi affine shift and intertwiners (Demazure-Lusztig operators) and agrees with the Haglund-Haiman-Loehr non-attacking filling formula; q = 0 gives TAtomPolynomial\[alpha, x, t\].

Background: [Macdonald E polynomials](https://www.symmetricfunctions.com/macdonaldE.htm)

### NonsymmetricJackPolynomial

NonsymmetricJackPolynomial\[alpha, x, a\] returns the nonsymmetric Jack polynomial obtained as Limit\[MacdonaldEPolynomial\[alpha, x, t^a, t\], t -&gt; 1\].

### NonsymmetricToPolynomial

NonsymmetricToPolynomial\[expr, x\] replaces nonsymmetric basis symbols with alphabet x (or None) in expr by their corresponding polynomials in x\[1\], x\[2\], ..., and expands.

### PermutationToCode

PermutationToCode\[w\] returns the Lehmer code of the permutation w (one-line notation): entry i counts j &gt; i with w\[\[j\]\] &lt; w\[\[i\]\].

Background: [Schubert polynomials](https://www.symmetricfunctions.com/schubert.htm)

### SchubertPolynomial

SchubertPolynomial\[w, x\] returns the Schubert polynomial of the permutation w (one-line notation), computed by divided differences from x\[1\]^(n-1) x\[2\]^(n-2) ... x\[n-1\] for the longest permutation of n letters.

Background: [Schubert polynomials](https://www.symmetricfunctions.com/schubert.htm)

### SchubertSymbol

SchubertSymbol\[w, x\] represents the Schubert polynomial of the permutation w in the variables x\[1\], x\[2\], ... as a basis element; trailing fixed points of w are removed. NonsymmetricToPolynomial expands it.

Background: [Schubert polynomials](https://www.symmetricfunctions.com/schubert.htm)

### TAtomPolynomial

TAtomPolynomial\[alpha, x, t\] returns the t-deformed Demazure atom obtained by applying TDemazureAtomOperator along the sorting word of alpha to x^sort(alpha); t = 0 gives AtomPolynomial.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### TDemazureAtomOperator

TDemazureAtomOperator\[f, x, t, i\] applies the t-deformed atom operator (1 - t) theta\_i f + t s\_i f.<br>
TDemazureAtomOperator\[f, x, t, {i1, ..., ik}\] applies the composition with i\_k acting first.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### TDemazureOperator

TDemazureOperator\[f, x, t, i\] applies the t-deformed Demazure operator (1 - t) pi\_i f + t s\_i f.<br>
TDemazureOperator\[f, x, t, {i1, ..., ik}\] applies the composition with i\_k acting first.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### TKeyPolynomial

TKeyPolynomial\[alpha, x, t\] returns the t-deformed key polynomial obtained by applying TDemazureOperator along the sorting word of alpha to x^sort(alpha); t = 0 gives KeyPolynomial.

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### ToAtomBasis

ToAtomBasis\[poly, x\] writes the polynomial poly in x\[1\], x\[2\], ... in the basis of Demazure atoms, as a combination of AtomSymbol\[alpha, x\].<br>
ToAtomBasis\[poly, x, n\] uses atoms in n variables (default: the largest variable index occurring).

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### ToFundamentalSlideBasis

ToFundamentalSlideBasis\[poly, x\] writes a homogeneous polynomial in the fundamental slide basis. ToFundamentalSlideBasis\[poly, x, n\] uses n variables.

Background: [Slide, forest, and lock polynomials](https://www.symmetricfunctions.com/assaf.htm)

### ToGrothendieckBasis

ToGrothendieckBasis\[poly, x\] writes poly in the beta = -1 Grothendieck basis. It repeatedly expands the lowest-degree residual in the Schubert basis and subtracts the corresponding Grothendieck polynomials. ToGrothendieckBasis\[poly, x, n\] uses n variables.

Background: [Grothendieck polynomials](https://www.symmetricfunctions.com/grothendieck.htm)

### ToKeyBasis

ToKeyBasis\[poly, x\] writes the polynomial poly in x\[1\], x\[2\], ... in the key basis, as a combination of KeySymbol\[alpha, x\].<br>
ToKeyBasis\[poly, x, n\] uses keys in n variables (default: the largest variable index occurring).

Background: [Key polynomials and Demazure atoms](https://www.symmetricfunctions.com/key.htm)

### ToLascouxBasis

ToLascouxBasis\[poly, x\] writes poly in the beta = -1 Lascoux basis. It repeatedly expands the lowest-degree residual in the key basis and subtracts the corresponding Lascoux polynomials. ToLascouxBasis\[poly, x, n\] uses n variables.

Background: [Lascoux polynomials](https://www.symmetricfunctions.com/lascoux.htm)

### ToLockBasis

ToLockBasis\[poly, x\] writes a homogeneous polynomial in the lock basis. ToLockBasis\[poly, x, n\] uses n variables.

Background: [Slide, forest, and lock polynomials](https://www.symmetricfunctions.com/assaf.htm)

### ToSchubertBasis

ToSchubertBasis\[poly, x\] writes the polynomial poly in x\[1\], x\[2\], ... in the Schubert basis, as a combination of SchubertSymbol\[w, x\] (Schubert polynomials whose Lehmer code has length at most the number of variables).<br>
ToSchubertBasis\[poly, x, n\] uses n variables (default: the largest variable index occurring).

Background: [Schubert polynomials](https://www.symmetricfunctions.com/schubert.htm)

### VariableTransposition

VariableTransposition\[f, x, i\] interchanges x\[i\] and x\[i+1\] in f.

