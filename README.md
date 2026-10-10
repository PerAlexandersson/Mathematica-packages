# Mathematica-packages

Wolfram Language packages for symmetric functions and algebraic combinatorics:
symmetric and quasisymmetric functions, tableaux, Gelfand–Tsetlin patterns, Catalan
objects, permutations, posets, graphs and matroids. They grew out of research use and
are offered to those who prefer Mathematica over Sage; see also
<https://www.symmetricfunctions.com/>.

## Installation

The repository is one paclet, `PerAlexandersson/MathematicaPackages`. Load it from a
checkout,

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

## Packages

Supported packages (in `Kernel/`):

| Context | Contents |
|---|---|
| `SymmetricFunctions`` | Monomial, elementary, complete homogeneous, power-sum, Schur and forgotten bases with fast transition matrices; several alphabets; Hall and Jack inner products; plethysm; Kostka, inverse Kostka, Littlewood–Richardson and Kronecker coefficients; skew Schur, Schur P and Q, Jack, Hall–Littlewood, Macdonald P/J and modified Macdonald H~ (Haglund's convention), LLT, k-Schur, Lah and Petrie functions; the Delta and nabla operators |
| `CombinatoricTools`` | Partitions, compositions, set partitions, permutation statistics, q-analogs, characters of the symmetric group, Kostka numbers |
| `NewTableaux`` | Standard and semistandard (skew) Young tableaux, RSK, promotion, evacuation, crystal operators, border strips |
| `GTPatterns`` | Gelfand–Tsetlin patterns, including skew, row-flagged and cylindric patterns |
| `QuasiSymmetricFunctions`` | Monomial, fundamental and power-sum quasisymmetric functions |
| `PolynomialTools`` | Real-rootedness, interlacing, log-concavity, Eulerian and h*-polynomials, recurrence finding, Hilbert functions |
| `PermutationTools`` | Pattern avoidance, Foata and related maps, Bruhat and weak order, families of permutations |
| `CatalanObjects`` | Dyck paths, non-crossing partitions and matchings, parking functions, trees and other Catalan families, with plots |
| `UnicellularChromatics`` | Chromatic symmetric functions and LLT polynomials of unit interval graphs and area sequences, orientations and their statistics |
| `GraphTools`` | Graph polynomials, orientations, and datasets of connected graphs (n <= 9), trees (n <= 20) and rooted trees (n <= 10) |
| `MatroidTools`` | Matroids from bases: rank, duality, deletion/contraction, Tutte polynomials, transversal, lattice path and rook matroids |

Experimental packages (in `Kernel/`): `MacdonaldPolynomials`` (key and atom polynomials,
Schubert and Grothendieck polynomials, non-symmetric Macdonald polynomials, slide
polynomials), `PosetData`` (connected posets up to 7 elements, linear extensions, order
and P-Eulerian polynomials), and `RookTools``.

Legacy packages (in `Legacy/`) remain loadable for old notebooks; `Legacy/README.md`
lists their replacements: `OldYoungTableaux``, `ChromaticFunctions``, `TreesData`` and
`RunSortedWords``.

Every public symbol has a usage message, e.g. `?SchurSymmetric`.

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
