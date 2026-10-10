---
title: QuasiSymmetricFunctions
parent: Reference
nav_order: 6
---

{% raw %}
# QuasiSymmetricFunctions

Monomial, fundamental and power-sum quasisymmetric functions, quasisymmetric Schur functions, and bridges to polynomials and to symmetric functions.

Load with `` Needs["QuasiSymmetricFunctions`"] ``.

Background on symmetricfunctions.com: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm), [Gessel quasisymmetric functions](https://www.symmetricfunctions.com/gessel.htm), [Quasisymmetric Schur functions and variants](https://www.symmetricfunctions.com/qsymSchur.htm).

## Functions and symbols

### FundamentalQSymbol

FundamentalQSymbol\[alpha, x\] represents the fundamental quasisymmetric-function basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Gessel quasisymmetric functions](https://www.symmetricfunctions.com/gessel.htm)

### FundamentalQSymmetric

FundamentalQSymmetric\[alpha, x\] returns the fundamental quasisymmetric function indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Gessel quasisymmetric functions](https://www.symmetricfunctions.com/gessel.htm)

### MonomialQSymbol

MonomialQSymbol\[alpha, x\] represents the monomial quasisymmetric-function basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### MonomialQSymmetric

MonomialQSymmetric\[alpha, x\] returns the monomial quasisymmetric function indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### PartitionedCompositionCoarsenings

PartitionedCompositionCoarsenings\[alpha\] returns all coarsenings of the partitioned composition alpha, represented as a list of lists of compositions.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### PolynomialToQuasiSymmetricFunction

PolynomialToQuasiSymmetricFunction\[poly, x, basisSymbol\] converts a quasisymmetric polynomial in x\[1\], x\[2\], ... to the MonomialQSymbol or FundamentalQSymbol basis; the basis defaults to MonomialQSymbol.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### PowerSumAltQSymmetric

PowerSumAltQSymmetric\[alpha, x\] returns the alternate quasisymmetric power-sum function indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### PowerSumQSymbol

PowerSumQSymbol\[alpha, x\] represents the quasisymmetric power-sum basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### PowerSumQSymmetric

PowerSumQSymmetric\[alpha, x\] returns the quasisymmetric power-sum function indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### QuasiSchurQSymbol

QuasiSchurQSymbol\[alpha, x\] represents the HLMW quasisymmetric Schur basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Quasisymmetric Schur functions and variants](https://www.symmetricfunctions.com/qsymSchur.htm)

### QuasiSchurQSymmetric

QuasiSchurQSymmetric\[alpha, basisSymbol\] returns the HLMW quasisymmetric Schur function indexed by composition alpha in the MonomialQSymbol basis, or in the FundamentalQSymbol basis when requested.

Background: [Quasisymmetric Schur functions and variants](https://www.symmetricfunctions.com/qsymSchur.htm)

### QuasiSymmetricFunctionToPolynomial

QuasiSymmetricFunctionToPolynomial\[expr, x, n\] expresses a quasisymmetric function in the variables x\[1\] through x\[n\].

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### ToFundamentalBasis

ToFundamentalBasis\[poly, x\] converts poly to the fundamental quasisymmetric basis. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### ToOtherQSymmetricBasis

ToOtherQSymmetricBasis\[basis, pol, newSymb, x, mm\] converts pol from the monomial basis to the basis specified by basis and newSymb. The alphabet x defaults to None and the monomial symbol mm defaults to MonomialQSymbol.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### ToPowerSumQSymBasis

ToPowerSumQSymBasis\[poly, x\] converts poly to the quasisymmetric power-sum basis. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### ToQuasiSymmetric

ToQuasiSymmetric\[expr\] embeds a SymmetricFunctions expression in QSym by sending m\_lambda to the sum of M\_alpha over all distinct rearrangements alpha of lambda.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### ToZPowerSumQSymBasis

ToZPowerSumQSymBasis\[poly, x\] converts poly to the z-normalized quasisymmetric power-sum basis. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### ZPowerSumQSymbol

ZPowerSumQSymbol\[alpha, x\] represents the z-normalized quasisymmetric power-sum basis element indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)

### ZPowerSumQSymmetric

ZPowerSumQSymmetric\[alpha, x\] returns the z-normalized quasisymmetric power-sum function indexed by composition alpha in alphabet x. The alphabet x defaults to None.

Background: [Quasisymmetric functions](https://www.symmetricfunctions.com/standardQuasiSymmetricFunctions.htm)


{% endraw %}
