# Mathematica-packages

Wolfram Language packages for symmetric functions and algebraic combinatorics:
symmetric and quasisymmetric functions, tableaux, Gelfand–Tsetlin patterns, Catalan
objects, permutations, posets, graphs and matroids. They grew out of research use and
are offered to those who prefer Mathematica over Sage; see also
<https://www.symmetricfunctions.com/>.

## Installation

The repository is one paclet, `PerAlexandersson/MathematicaPackages`. Install the latest
release directly from GitHub,

```wolfram
PacletInstall["https://github.com/PerAlexandersson/Mathematica-packages/releases/download/v0.1.0/PerAlexandersson__MathematicaPackages-0.1.0.paclet"];
Needs["SymmetricFunctions`"]
```

or load it from a checkout,

```wolfram
PacletDirectoryLoad["/path/to/Mathematica-packages"];
Needs["SymmetricFunctions`"]
```

or build and install an archive:

```bash
wolframscript -file Scripts/BuildPaclet.m
```

```wolfram
PacletInstall["/path/to/Mathematica-packages/build/PerAlexandersson__MathematicaPackages-0.1.0.paclet"];
Needs["SymmetricFunctions`"]
```

Compatibility: tested with Wolfram Language 14.3 (the paclet requires 14.3 or later;
earlier versions may work but are untested).

## Getting started

Documentation: <https://peralexandersson.github.io/Mathematica-packages/> (tutorial, reference
for every package with links to symmetricfunctions.com, conventions, migration guide). In the
repository, the [tutorial](TUTORIAL.md) is a tour of the packages and how they fit together; its
code is checked by the test suite. Shared conventions are in [`CONVENTIONS.md`](CONVENTIONS.md), and
[`Examples/`](Examples/) has runnable scripts.

## Packages

Supported packages (in `Kernel/`):

| Context | Contents |
|---|---|
| `SymmetricFunctions`` | Monomial, elementary, complete homogeneous, power-sum, Schur and forgotten bases with fast transition matrices; several alphabets; Hall and Jack inner products; plethysm; Kostka, inverse Kostka, Littlewood–Richardson and Kronecker coefficients; skew Schur, Schur P and Q, Jack, Hall–Littlewood, Macdonald P/J and modified Macdonald H~ (Haglund's convention), LLT, k-Schur, Lah and Petrie functions; the Delta and nabla operators |
| `ShiftedSymmetricFunctions`` | Okounkov–Olshanski shifted Schur and Jack polynomials, normalized characters, and Stanley–Feray–Sniady multirectangular character polynomials |
| `CombinatoricTools`` | Partitions, compositions, set partitions, permutation statistics, q-analogs, characters of the symmetric group, Kostka numbers |
| `NewTableaux`` | Standard and semistandard (skew) Young tableaux, RSK, promotion, evacuation, crystal operators, border strips, TeX output; semistandard augmented fillings (`SSAF`) with statistics, crystals and Mason insertion |
| `GTPatterns`` | Gelfand–Tsetlin patterns (skew, row-flagged, cylindric), BZ patterns, Gog and Magog patterns, tiles and snakes, lattice paths, stretched Kostka (Ehrhart) data, TikZ output |
| `QuasiSymmetricFunctions`` | Monomial, fundamental and power-sum quasisymmetric functions, quasisymmetric Schur functions, and bridges to polynomials and to symmetric functions |
| `PolynomialTools`` | Real-rootedness, interlacing, log-concavity, Eulerian and h*-polynomials, recurrence finding, Hilbert functions |
| `PermutationTools`` | Pattern avoidance, Foata and related maps, Bruhat and weak order, families of permutations |
| `CatalanObjects`` | Dyck paths, non-crossing partitions and matchings, parking functions, trees and other Catalan families, with plots |
| `UnicellularChromatics`` | Chromatic symmetric functions and LLT polynomials of unit interval graphs and area sequences, orientations and their statistics |
| `GraphTools`` | Graph polynomials, orientations, and datasets of connected graphs (n <= 9), trees (n <= 20) and rooted trees (n <= 10) |
| `MatroidTools`` | Matroids from bases: rank, duality, deletion/contraction, Tutte polynomials, transversal, lattice path and rook matroids |
| `NonsymmetricPolynomials`` | Divided difference, Demazure and Demazure–Lusztig operators (also K-theoretic); key, atom, t-key, t-atom, Schubert, Grothendieck, Lascoux, fundamental slide, lock and dual Grothendieck polynomials; nonsymmetric Macdonald polynomials (also with permuted basements) and nonsymmetric Jack polynomials; basis symbols with conversions |
| `LegacyConversions`` | Converters from the data of the legacy packages (GT patterns, tableaux, area lists, key indices) to the supported conventions |

Experimental packages (in `Kernel/`): `PosetData`` (connected posets up to 7 elements, linear extensions, order
and P-Eulerian polynomials), and `RookTools``. `AlgebraicBases`` is internal: it provides the basis
symbols used by the algebra packages.

Legacy packages (in `Legacy/`) remain loadable for old notebooks; `Legacy/README.md`
lists their replacements: `OldYoungTableaux``, `MacdonaldPolynomials``, `ChromaticFunctions``,
`TreesData`` and `RunSortedWords``. Their content has been ported into the supported
packages (issue #51); [`Legacy/MIGRATION.md`](Legacy/MIGRATION.md) maps every legacy name to
its replacement, and `LegacyConversions` converts legacy data.

Every public symbol has a usage message, e.g. `?SchurSymmetric`. Objects are shared
between packages using the representations in `CONVENTIONS.md`.

## Example

```wolfram
Needs["SymmetricFunctions`"];
ToSchurBasis[ElementaryESymmetric[{2, 1}]]
(* s_21 + s_111 *)
ToSchurBasis[MacdonaldHSymmetric[{2}, q, t]]
(* s_2 + q s_11 *)
```

`Examples/SymmetricFunctions-Introduction.m` is a longer tour
(`wolframscript -file Examples/SymmetricFunctions-Introduction.m`); the original notebook
is next to it.

## Tests

```bash
wolframscript -file Tests/RunTests.m
```

runs every `Tests/*Tests.m` file in a fresh kernel. Besides regression tests, the suite
checks load-order independence, usage strings, a Code Inspector baseline, and agreement
with the Rust libraries `sym-poly`, `combinatoric-core`, `combpoly` and `polytool` on
about 25 families (`Tests/fixtures/rust/`). See `Tests/README.md`.

## Layout

`PacletInfo.wl`, `Kernel/`, `Legacy/`, `Data/` (datasets with provenance in
`Data/README.md`), `Examples/`, `Tests/`, `Scripts/`.

## Changes, contributing and license

See `CHANGELOG.md` (the 0.1.0 entry lists breaking changes, such as the switch to
Haglund's convention for modified Macdonald functions), `CONTRIBUTING.md` for package,
test and deprecation conventions, and `RELEASING.md`. Licensed under the MIT license
(`LICENSE`).
