---
title: SymmetricFunctions
parent: Reference
nav_order: 1
---

# SymmetricFunctions

Monomial, elementary, complete homogeneous, power-sum, Schur and forgotten bases with fast transition matrices; several alphabets; Hall and Jack inner products; plethysm; Kostka, inverse Kostka, Littlewood–Richardson and Kronecker coefficients; skew Schur, Schur P and Q, Jack, Hall–Littlewood, Macdonald P/J and modified Macdonald H~ (Haglund's convention), LLT, k-Schur, Lah and Petrie functions; the Delta and nabla operators.

Load with `` Needs["SymmetricFunctions`"] ``.

Background on symmetricfunctions.com: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm), [Schur polynomials](https://www.symmetricfunctions.com/schur.htm), [Littlewood–Richardson coefficients](https://www.symmetricfunctions.com/littlewoodRichardson.htm), [Plethysm](https://www.symmetricfunctions.com/plethysm.htm), [Jack polynomials](https://www.symmetricfunctions.com/jack.htm), [Hall–Littlewood polynomials](https://www.symmetricfunctions.com/hallLittlewood.htm), [Macdonald P polynomials](https://www.symmetricfunctions.com/macdonaldP.htm), [Modified Macdonald polynomials](https://www.symmetricfunctions.com/macdonaldH.htm), [LLT polynomials](https://www.symmetricfunctions.com/llt.htm).

## Functions and symbols

### AugmentedMonomialSymmetric

AugmentedMonomialSymmetric\[lam, x\] returns the augmented monomial symmetric function indexed by the integer vector lam in alphabet x. The alphabet x defaults to None.

### BigSchurSymmetric

BigSchurSymmetric\[lam, x\] returns the big Schur symmetric function indexed by partition lam in alphabet x.

### ChangeFunctionAlphabet

ChangeFunctionAlphabet\[expr, to, from\] changes the symmetric-function expressions in expr from alphabet from to alphabet to. The source alphabet defaults to None.

### ClearSymmetricFunctionsCache

ClearSymmetricFunctionsCache\[\] removes all stored results of memoized symmetric functions, such as SkewSchurSymmetric, MacdonaldHSymmetric and KroneckerCoefficient. Transition matrices between the classical bases are kept.

### CoefficientsSum

CoefficientsSum\[expr\] returns the sum of coefficients.

### CompleteHSymbol

CompleteHSymbol\[lam, x\] represents the complete homogeneous symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### CompleteHSymmetric

CompleteHSymmetric\[lam, x\] returns the complete homogeneous symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### CylindricSchurSymmetric

CylindricSchurSymmetric\[{lam, mu}, d, x\] returns the cylindric Schur symmetric function of shape lam/mu and shift d in alphabet x. The shift d and alphabet x default to 0 and None.

### DeltaOperator

DeltaOperator\[f, g, q, t\] applies the Garsia Delta operator Delta\_f to g: it multiplies each modified Macdonald H~\_mu by f\[B\_mu\], where B\_mu = sum over cells (r, c) of mu of q^(c-1) t^(r-1) (Haglund's convention).

### DeltaPrimOperator

DeltaPrimOperator\[f, g, q, t\] applies the primed Delta operator to symmetric functions f and g with parameters q and t.

### ElementaryESymbol

ElementaryESymbol\[lam, x\] represents the elementary symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### ElementaryESymmetric

ElementaryESymmetric\[lam, x\] returns the elementary symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### ForgottenSymbol

ForgottenSymbol\[lam, x\] represents the forgotten symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### ForgottenSymmetric

ForgottenSymmetric\[lam, x\] returns the forgotten symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### FunctionAlphabets

FunctionAlphabets\[expr\] returns the list of alphabets occurring in a symmetric-function expression.

### HallInnerProduct

HallInnerProduct\[f, g\] returns the Hall inner product of symmetric functions f and g.<br>
HallInnerProduct\[f, g, {q, t}, x\] computes the q,t inner product in alphabet x, which defaults to None.

### HallLittlewoodMSymmetric

HallLittlewoodMSymmetric\[lam, q, x\] returns the Hall-Littlewood M symmetric function indexed by partition lam with parameter q. The alphabet x defaults to None.

Background: [Hall–Littlewood polynomials](https://www.symmetricfunctions.com/hallLittlewood.htm)

### HallLittlewoodPSymbol

HallLittlewoodPSymbol\[lam, x\] represents a Hall-Littlewood P basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.

Background: [Hall–Littlewood polynomials](https://www.symmetricfunctions.com/hallLittlewood.htm)

### HallLittlewoodPSymmetric

HallLittlewoodPSymmetric\[lam, t, x\] returns the Hall-Littlewood P symmetric function indexed by partition lam with parameter t. The alphabet x defaults to None.

Background: [Hall–Littlewood polynomials](https://www.symmetricfunctions.com/hallLittlewood.htm)

### HallLittlewoodTSymmetric

HallLittlewoodTSymmetric\[lam, q, x\] returns the transformed Hall-Littlewood symmetric function indexed by partition lam with parameter q. The alphabet x defaults to None.

Background: [Hall–Littlewood polynomials](https://www.symmetricfunctions.com/hallLittlewood.htm)

### HookFilterMap

HookFilterMap\[f, t, \[x\]\] applies the hook filter map to a symmetric function f, returning a polynomial in t. The map sends SchurSymbol\[lam\] to t\*(t-1)^(lam\[\[1\]\]-1) if lam is a hook shape (first part arbitrary, all remaining parts equal to 1), and to 0 otherwise.

### InternalProduct

InternalProduct\[f, g, x\] computes the internal (Kronecker) product of symmetric functions f and g in alphabet x. The alphabet x defaults to None.

### JackInnerProduct

JackInnerProduct\[f, g, a\] returns the Jack inner product of symmetric functions f and g with parameter a, which defaults to 1.<br>
JackInnerProduct\[f, g, a, x\] computes the inner product in alphabet x, which defaults to None.

Background: [Jack polynomials](https://www.symmetricfunctions.com/jack.htm)

### JackJSymmetric

JackJSymmetric\[lam, a, x\] returns the Jack J symmetric function indexed by partition lam with parameter a. The alphabet x defaults to None.

Background: [Jack polynomials](https://www.symmetricfunctions.com/jack.htm)

### JackLowerHook

JackLowerHook\[mu, a, {r, c}\] returns the Jack lower hook at box {r, c} of partition mu. JackLowerHook\[mu, a\] returns the product of lower hooks over mu. The parameter a defaults to 1.

Background: [Jack polynomials](https://www.symmetricfunctions.com/jack.htm)

### JackPSymmetric

JackPSymmetric\[lam, a, x\] returns the Jack P symmetric function indexed by partition lam with parameter a. The alphabet x defaults to None.

Background: [Jack polynomials](https://www.symmetricfunctions.com/jack.htm)

### JackUpperHook

JackUpperHook\[mu, a, {r, c}\] returns the Jack upper hook at box {r, c} of partition mu. JackUpperHook\[mu, a\] returns the product of upper hooks over mu. The parameter a defaults to 1.

Background: [Jack polynomials](https://www.symmetricfunctions.com/jack.htm)

### KroneckerCoefficient

KroneckerCoefficient\[lam,mu,nu\] returns the Kronecker coefficient.

### kSchurSymmetric

kSchurSymmetric\[mu, k, t, x\] returns the k-Schur function indexed by partition mu, with mu1 &lt;= k. The parameter t defaults to 1 and the alphabet x defaults to None.

### LahSymmetricFunction

LahSymmetricFunction\[n, k, x\] returns the Lah symmetric function with parameters n and k in alphabet x. The alphabet x defaults to None.

Background: [Lah symmetric functions](https://www.symmetricfunctions.com/lah.htm)

### LahSymmetricFunctionNegative

LahSymmetricFunctionNegative\[n, k, x\] returns the negative Lah symmetric function with parameters n and k in alphabet x. The alphabet x defaults to None.

Background: [Lah symmetric functions](https://www.symmetricfunctions.com/lah.htm)

### LLTSymmetric

LLTSymmetric\[nu, q, x\] returns the LLT symmetric function associated with the tuple nu of shapes with parameter q. The alphabet x defaults to None.

Background: [LLT polynomials](https://www.symmetricfunctions.com/llt.htm)

### LoadBasisMatrices

LoadBasisMatrices\[\] loads previously stored transition matrices.<br>
To store transition matrices, use PrecomputeBasisMatrices\[\]

### LRCoefficient

LRCoefficient\[lam, mu, nu\] gives the Littlewood-Richardson coefficient of Schur function S\_nu in S\_lam\*S\_mu. The arguments are partitions.

Background: [Littlewood–Richardson coefficients](https://www.symmetricfunctions.com/littlewoodRichardson.htm)

### LRExpand

LRExpand\[expr, x\] expands products and powers of Schur symmetric functions in expr using the Littlewood-Richardson rule. The alphabet x defaults to None.

Background: [Littlewood–Richardson coefficients](https://www.symmetricfunctions.com/littlewoodRichardson.htm)

### LyndonSymmetric

LyndonSymmetric\[lam, x\] returns the Lyndon symmetric function indexed by lam. The alphabet x defaults to None.

### MacdonaldHSymbol

MacdonaldHSymbol\[lam, x\] represents a modified Macdonald H basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.

Background: [Modified Macdonald polynomials](https://www.symmetricfunctions.com/macdonaldH.htm)

### MacdonaldHSymmetric

MacdonaldHSymmetric\[lam, q, t, x\] returns the modified Macdonald symmetric function H~\_lam(x; q, t) in Haglund's convention, so that H~\_{2} = s\_2 + q s\_11, H~\_{1,1} = s\_2 + t s\_11, and H~\_mu = t^n(mu) J\_mu\[X/(1 - 1/t); q, 1/t\]. The alphabet x defaults to None.<br>
MacdonaldHSymmetric\[{lam, mu}, q, t, x\] gives the skew version.

Background: [Modified Macdonald polynomials](https://www.symmetricfunctions.com/macdonaldH.htm)

### MacdonaldJSymmetric

MacdonaldJSymmetric\[lam, q, t, x\] returns the integral-form Macdonald J symmetric function indexed by partition lam with parameters q and t. The alphabet x defaults to None.

Background: [Macdonald P polynomials](https://www.symmetricfunctions.com/macdonaldP.htm)

### MacdonaldPSymmetric

MacdonaldPSymmetric\[lam, q, t, x\] returns the Macdonald P symmetric function indexed by partition lam with parameters q and t. The alphabet x defaults to None.

Background: [Macdonald P polynomials](https://www.symmetricfunctions.com/macdonaldP.htm)

### MExpand

MExpand\[expr\] expands products and powers of monomial symmetric functions in expr.

### MonomialSymbol

MonomialSymbol\[lam, x\] represents the monomial symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### MonomialSymmetric

MonomialSymmetric\[lam, x\] returns the monomial symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### NablaOperator

NablaOperator\[g, q, t\] applies the nabla operator to symmetric function g with parameters q and t.

### OmegaInvolution

OmegaInvolution\[expr, x\] applies the omega involution to expr. The alphabet x defaults to None. Caution: It only acts on the common symmetric functions.

### OrthogonalSchurSymmetric

OrthogonalSchurSymmetric\[lam, x\] returns the orthogonal Schur symmetric function indexed by partition lam in alphabet x.

### PetrieSymmetric

PetrieSymmetric\[k, m, x\] returns the degree-m part of the kth Petrie symmetric function in alphabet x. The alphabet x defaults to None.

Background: [Petrie symmetric functions](https://www.symmetricfunctions.com/petrie.htm)

### Plethysm

Plethysm\[f, g, xx\] computes the plethysm of symmetric functions f and g, acting on all alphabets in g. The alphabet xx of f defaults to None.

Background: [Plethysm](https://www.symmetricfunctions.com/plethysm.htm)

### PolynomialToSymmetricFunction

PolynomialToSymmetricFunction\[poly, x, basisSymbol, n\] converts a symmetric polynomial in x\[1\], ..., x\[n\] to the requested basis (MonomialSymbol, SchurSymbol, ElementaryESymbol, CompleteHSymbol, PowerSumSymbol or ForgottenSymbol). The basis defaults to SchurSymbol and n is inferred from the variables present when omitted. Only partitions with at most n parts can occur.

### PositiveCoefficientsQ

PositiveCoefficientsQ\[expr, basis, extraSymbols\] returns True if all coefficients of basis terms in expr are nonnegative numbers. The basis defaults to MonomialSymbol and extraSymbols defaults to {}.

### PowerSumSymbol

PowerSumSymbol\[lam, x\] represents the power-sum symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### PowerSumSymmetric

PowerSumSymmetric\[lam, x\] returns the power-sum symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.

Background: [Standard symmetric functions](https://www.symmetricfunctions.com/standardSymmetricFunctions.htm)

### PrecomputeBasisMatrices

PrecomputeBasisMatrices\[Dimensions-&gt;n\] computes the transition matrices for<br>
all the most common elementary symmetric functions indexed by partitions of size at most n.<br>
These matrices are stored as a local object (in your home folder).<br>
Use LoadBasisMatrices\[\] to load these matrices.<br>
Note that time and space used grows rapidly with n!

### PrincipalSpecialization

PrincipalSpecialization\[poly, q, k, x\] gives the principal specialization of poly. The parameter k gives the number of variables and defaults to Infinity; the alphabet x defaults to None.

### SchursPSymmetric

SchursPSymmetric\[lam, x\] returns the Schur P symmetric function indexed by strict partition lam. The alphabet x defaults to None.

Background: [Schur polynomials](https://www.symmetricfunctions.com/schur.htm)

### SchursQSymmetric

SchursQSymmetric\[lam\]

Background: [Schur polynomials](https://www.symmetricfunctions.com/schur.htm)

### SchurSymbol

SchurSymbol\[lam, x\] represents the Schur symmetric-function basis element indexed by partition lam in alphabet x. The alphabet x defaults to None.

Background: [Schur polynomials](https://www.symmetricfunctions.com/schur.htm)

### SchurSymmetric

SchurSymmetric\[lam, x\] returns the Schur symmetric function indexed by the partition lam in alphabet x. The alphabet x defaults to None.

Background: [Schur polynomials](https://www.symmetricfunctions.com/schur.htm)

### SkewKostkaCoefficient

SkewKostkaCoefficient\[lam, mu, w\] returns the skew Kostka coefficient for skew shape lam/mu and weight w. The weight w is sorted to a partition.

Background: [Kostka coefficients, Kostka–Foulkes polynomials and charge](https://www.symmetricfunctions.com/kostkaFoulkes.htm)

### SkewMacdonaldESymmetric

SkewMacdonaldESymmetric\[{lam, mu}, q, x\] returns the skew Macdonald E symmetric function of shape lam/mu with parameter q in alphabet x.

### SkewSchurSymmetric

SkewSchurSymmetric\[lam, x\] returns the Schur symmetric function of partition lam in alphabet x. The alphabet x defaults to None.<br>
SkewSchurSymmetric\[{lam, mu}, x\] returns the skew Schur symmetric function of shape lam/mu in alphabet x. The alphabet x defaults to None.

Background: [Schur polynomials](https://www.symmetricfunctions.com/schur.htm)

### SnModuleCharacters

SnModuleCharacters\[polynomialBasis, x\] takes a list of polynomials in x\[1\]...x\[n\] and returns the Sn-character under the action where Sn acts on the variable indices

### SymFuncTransMat

SymFuncTransMat\[fromBasis, toBasis, d\] returns the transition matrix<br>
in degree d. Here, fromBasis and toBasis are functions which given a partition, returns the monomial expansion.

### SymmetricFunctionDegree

SymmetricFunctionDegree\[expr\] returns the degree of the symmetric function.

### SymmetricFunctionToPolynomial

SymmetricFunctionToPolynomial\[expr, x, n, y\] expresses expr as a polynomial in n variables x\[1\] through x\[n\], using alphabet y in expr. The alphabet x and variable count n default to None and the degree of expr, respectively; y defaults to None.

### SymmetricMonomialList

SymmetricMonomialList\[expr\] returns the list of symmetric-function monomials occurring in expr.

### SymplecticSchurSymmetric

SymplecticSchurSymmetric\[lam, x\] returns the symplectic Schur symmetric function indexed by partition lam in alphabet x.

### ToCompleteHBasis

ToCompleteHBasis\[poly, x\] converts poly to the complete homogeneous basis. The alphabet x defaults to None.

### ToElementaryEBasis

ToElementaryEBasis\[poly, x\] converts poly to the elementary basis. The alphabet x defaults to None.

### ToHallLittlewoodPBasis

ToHallLittlewoodPBasis\[poly, t, x, mh\] converts poly to the Hall-Littlewood P basis with parameter t. The alphabet x defaults to None and the basis symbol mh defaults to HallLittlewoodPSymbol.

### ToMacdonaldHBasis

ToMacdonaldHBasis\[poly, q, t, x, mh\] converts poly to the modified Macdonald H basis with parameters q and t. The alphabet x defaults to None and the basis symbol mh defaults to MacdonaldHSymbol.

### ToMonomialBasis

ToMonomialBasis\[poly, x\] converts poly to the monomial basis. The alphabet x defaults to None.

### toOtherSymmetricBasis

toOtherSymmetricBasis\[{basisFunc, basisSymbol}, poly, x\] converts poly to the basis specified by basisFunc and basisSymbol. The alphabet x defaults to None; a list of alphabets is also accepted.

### ToPowerSumBasis

ToPowerSumBasis\[poly, x\] converts poly to the power-sum basis. The alphabet x defaults to None.

### ToPowerSumZBasis

ToPowerSumZBasis\[poly, x\] converts poly to the power-sum basis with each power-sum term multiplied by its z-coefficient. The alphabet x defaults to None.

### ToSchurBasis

ToSchurBasis\[poly, x\] converts poly to the Schur basis. The alphabet x defaults to None.

