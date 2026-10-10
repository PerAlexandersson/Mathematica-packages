---
title: ShiftedSymmetricFunctions
parent: Reference
nav_order: 2
---

{% raw %}
# ShiftedSymmetricFunctions

Okounkov–Olshanski shifted Schur and Jack polynomials, normalized characters, and Stanley–Feray–Sniady multirectangular character polynomials.

Load with `` Needs["ShiftedSymmetricFunctions`"] ``.

Background on symmetricfunctions.com: [Shifted Schur polynomials](https://www.symmetricfunctions.com/schurShifted.htm), [Shifted Jack polynomials](https://www.symmetricfunctions.com/jackShifted.htm).

## Functions and symbols

### NormalizedCharacter

NormalizedCharacter\[mu,lam\] returns n(n-1)...(n-\|mu\|+1) chi^lam(mu 1^(n-\|mu\|))/dim(lam), for \|mu\|&lt;=n=\|lam\|, and 0 otherwise.

### ShiftedJackJEvaluate

ShiftedJackJEvaluate\[mu,lam,a\] evaluates J\*\_mu at the partition lam with Jack parameter a.

Background: [Shifted Jack polynomials](https://www.symmetricfunctions.com/jackShifted.htm)

### ShiftedJackJPolynomial

ShiftedJackJPolynomial\[mu,n,x,a\] returns the hook-normalized shifted Jack J polynomial in x\[1\],...,x\[n\].

Background: [Shifted Jack polynomials](https://www.symmetricfunctions.com/jackShifted.htm)

### ShiftedJackPEvaluate

ShiftedJackPEvaluate\[mu,lam,a\] evaluates P\*\_mu at the partition lam with Jack parameter a.

Background: [Shifted Jack polynomials](https://www.symmetricfunctions.com/jackShifted.htm)

### ShiftedJackPPolynomial

ShiftedJackPPolynomial\[mu,n,x,a\] returns the shifted Jack P polynomial P\*\_mu in x\[1\],...,x\[n\] with Jack parameter a.

Background: [Shifted Jack polynomials](https://www.symmetricfunctions.com/jackShifted.htm)

### ShiftedJackPSymmetric

ShiftedJackPSymmetric\[mu,a,x\] returns the legacy shifted Jack P symmetric function obtained from the Okounkov--Olshanski tableau formula. The alphabet x defaults to None.

Background: [Shifted Jack polynomials](https://www.symmetricfunctions.com/jackShifted.htm)

### ShiftedSchurEvaluate

ShiftedSchurEvaluate\[mu,lam\] evaluates s\*\_mu at the partition lam using the determinant formula.

Background: [Shifted Schur polynomials](https://www.symmetricfunctions.com/schurShifted.htm)

### ShiftedSchurPolynomial

ShiftedSchurPolynomial\[mu,n,x\] returns the Okounkov--Olshanski shifted Schur polynomial s\*\_mu in x\[1\],...,x\[n\].

Background: [Shifted Schur polynomials](https://www.symmetricfunctions.com/schurShifted.htm)

### StanleyCharacterPolynomial

StanleyCharacterPolynomial\[mu,p,q,d\] returns the Stanley--Feray--Sniady normalized character polynomial in multirectangular coordinates p\[1\],...,p\[d\] and q\[1\],...,q\[d\].


{% endraw %}
