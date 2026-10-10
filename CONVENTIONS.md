# Conventions shared by all packages

Every supported package uses the representations below, so objects produced by one
package can be passed to another. `Tests/CompatibilityTests.m` checks the contract. If
a function needs a different representation internally, it converts at its boundary.

| Object | Representation | Owner |
|---|---|---|
| Partition | weakly decreasing list of positive integers; trailing zeros are accepted and removed | CombinatoricTools |
| Composition, weak composition | list of positive, respectively nonnegative, integers | CombinatoricTools |
| Skew shape | `{lam, mu}` with `mu` contained in `lam` | CombinatoricTools |
| Young tableau | `YoungTableau[rows]`, English notation, rows listed top to bottom, `None` in skew cells | NewTableaux |
| Augmented filling (with basement) | a NewTableaux object (introduced with the nonsymmetric port, #51) | NewTableaux |
| Gelfand–Tsetlin pattern | `GTPattern[rows]`, rows listed from the bottom (inner shape) to the top (outer shape) | GTPatterns |
| Permutation | one-line list of `1, ..., n`; `Cycles` for cycle notation | PermutationTools |
| Dyck path / unit interval graph | area list starting with 0 (`DyckAreaLists`); graph edges from `UnitIntervalEdges` | CatalanObjects, UnicellularChromatics |
| Poset | `Poset[n, rels]` with pairs `{a, b}` meaning a < b | PosetData |
| Graph | System `Graph`; edge-list forms use pairs `{u, v}` | GraphTools |
| Symmetric function | basis symbols such as `SchurSymbol[lam, x]` (alphabet `x` defaults to `None`) | SymmetricFunctions |
| Quasisymmetric function | basis symbols such as `FundamentalQSymbol[alpha, x]` | QuasiSymmetricFunctions |
| Nonsymmetric polynomial basis | basis symbols such as `KeySymbol[alpha, x]`, `AtomSymbol[alpha, x]`, `SchubertSymbol[w, x]`; the alphabet `x` is the variable symbol | NonsymmetricPolynomials |
| Polynomial in finitely many variables | expression in `x[1], ..., x[n]` with `x` a symbol and `n` explicit | – |

## Indexing and parameter conventions

- Keys and atoms are indexed by weak compositions in the standard convention:
  κ_(0,1) = x[1] + x[2] (as in the literature and the Rust `sym-poly` library). Lascoux
  polynomials use the same index; locks are Kohnert polynomials of right-justified diagrams
  (Assaf–Searles), so the lock of (2, 0) is x[1]^2.
- Nonsymmetric Macdonald polynomials follow Haglund-Haiman-Loehr (identity basement): E_alpha =
  x^alpha + lower terms, E_alpha(x; 0, t) is the t-atom and E_alpha(x; 0, 0) the atom, and
  E_(alpha_2, ..., alpha_n, alpha_1 + 1) = q^(-alpha_1) x_n E_alpha(q x_n, x_1, ..., x_(n-1)).
- K-theoretic families use the divided difference of (1 + beta x[i+1]) f, with beta = -1 by
  default; beta = 0 gives Schubert and key polynomials.
- Modified Macdonald functions follow Haglund: H~_(2) = s_2 + q s_11, and
  B_mu = sum over cells (r, c) of q^(c-1) t^(r-1).
- Quasisymmetric Schur functions (`QuasiSchurQSymmetric`) are those of
  Haglund–Luoto–Mason–van Willigenburg: S_alpha is the sum of the atoms A_gamma over weak
  compositions gamma whose nonzero parts form alpha, so
  S_(2,1,3) = F_(2,1,3) + F_(2,2,2) + F_(1,2,1,2) (Tewari–van Willigenburg, Example 2.7).
- Jack parameter `a` (alpha), Hall–Littlewood parameter `t`, K-theory parameter `beta`.
- Coefficient lists of univariate polynomials are in ascending degree.

## Interoperability rules

- A function that takes one of these objects accepts the representation above. Where
  an alternative form is common (for example `Graph` versus an edge list), both are
  accepted.
- Conversions between algebras go through explicit bridge functions
  (`SymmetricFunctionToPolynomial`, `PolynomialToSymmetricFunction`, their QSym
  analogues and `ToQuasiSymmetric`), not
  through ad hoc substitution rules.
- Two supported packages never export the same name (`Tests/LoadOrderTests.m`).
