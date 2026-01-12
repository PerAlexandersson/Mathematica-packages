# PolynomialTools Package Examples

## Loading the Package

```mathematica
(* Load package. You can also load by specifying file path. *)
Needs["PolynomialTools`"];
```

## FindPolynomialRecurrence Function

The function `FindPolynomialRecurrence` is used to guess recursions from data.

### Syntax

```mathematica
FindPolynomialRecurrence[listOfPolynomials, {t, n}, opts...]
```

where:
- `t` is the variable used in the polynomials
- `n` is the indexing variable (used only in output)

### Options

- **VariableDegree**: Maximal degree of the variable (t) in the coefficients
- **IndexDegree**: Maximal degree of the index (n)
- **DifferentialDegree**: Maximal derivative to use
- **RecurrenceLength**: Length of the recursion
- **Homogeneous**: True/False - whether the recursion is homogeneous or not
- **DenominatorVariableDegree** and **DenominatorIndexDegree**: Used for recursions where coefficients are rational functions

### Important Notes

- If you get a warning about multiple solutions, this means you have too little data or should reduce `RecurrenceLength`
- If you get `None` as output, there is no recursion with the specified parameters

## Example 1: Peak Polynomials for Permutations

Generating polynomials for peaks among permutations of {1, 2, ..., n}, for n = 1, 2, ..., 12:

```mathematica
peakPolynomials = {1, 2, 4 + 2 t, 8 + 16 t, 16 + 88 t + 16 t^2, 
   32 + 416 t + 272 t^2, 64 + 1824 t + 2880 t^2 + 272 t^3, 
   128 + 7680 t + 24576 t^2 + 7936 t^3, 
   256 + 31616 t + 185856 t^2 + 137216 t^3 + 7936 t^4, 
   512 + 128512 t + 1304832 t^2 + 1841152 t^3 + 353792 t^4, 
   1024 + 518656 t + 8728576 t^2 + 21253376 t^3 + 9061376 t^4 + 353792 t^5, 
   2048 + 2084864 t + 56520704 t^2 + 222398464 t^3 + 175627264 t^4 + 22368256 t^5};
```

Looking for a recursion where P_n depends on P_{n-1} and P'_{n-1}, with coefficients at most degree 2 in t (degree for n is 1 by default):

```mathematica
FindPolynomialRecurrence[peakPolynomials, {t, n},
 RecurrenceLength -> 1, DifferentialDegree -> 1, VariableDegree -> 2]
```

**Output:**
$$P_n = (2 - 2t + nt) P_{n-1} - 2(-1 + t)t P'_{n-1}$$

### Implementing the Recursion

```mathematica
Clear[p];
p[1] := 1;
p[n_] := p[n] = Expand[(2 - 2 t + n t) p[n - 1] + 2 t (1 - t) D[p[n - 1], t]];
```

## Example 2: Narayana Polynomials

Generating function of peaks in Dyck paths:

```mathematica
narayanaPolynomials = {t, t (1 + t), t + 3 t^2 + t^3, 
   t + 6 t^2 + 6 t^3 + t^4, t + 10 t^2 + 20 t^3 + 10 t^4 + t^5, 
   t + 15 t^2 + 50 t^3 + 50 t^4 + 15 t^5 + t^6, 
   t + 21 t^2 + 105 t^3 + 175 t^4 + 105 t^5 + 21 t^6 + t^7, 
   t + 28 t^2 + 196 t^3 + 490 t^4 + 490 t^5 + 196 t^6 + 28 t^7 + t^8, 
   t + 36 t^2 + 336 t^3 + 1176 t^4 + 1764 t^5 + 1176 t^6 + 336 t^7 + 36 t^8 + t^9, 
   t + 45 t^2 + 540 t^3 + 2520 t^4 + 5292 t^5 + 5292 t^6 + 2520 t^7 + 540 t^8 + 45 t^9 + t^10, 
   t + 55 t^2 + 825 t^3 + 4950 t^4 + 13860 t^5 + 19404 t^6 + 13860 t^7 + 4950 t^8 + 825 t^9 + 55 t^10 + t^11, 
   t + 66 t^2 + 1210 t^3 + 9075 t^4 + 32670 t^5 + 60984 t^6 + 60984 t^7 + 32670 t^8 + 9075 t^9 + 1210 t^10 + 66 t^11 + t^12};
```

We need to use `DenominatorIndexDegree -> 1` to find the coefficient in the right-hand side, which is non-constant:

```mathematica
FindPolynomialRecurrence[narayanaPolynomials, {t, n},
 RecurrenceLength -> 1, DifferentialDegree -> 1, VariableDegree -> 2, 
 DenominatorIndexDegree -> 1]
```

**Output:**
$$(1 + n) P_n = (-1 + n - t + 3nt) P_{n-1} - 2(-1 + t)t P'_{n-1}$$
