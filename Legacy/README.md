# Legacy packages

These packages are kept loadable for existing notebooks (`Needs["OldYoungTableaux`"]`
etc. still work after `PacletDirectoryLoad` or installation), but they are not maintained
and no supported package loads them. Prefer the replacements below in new code; [`MIGRATION.md`](MIGRATION.md) lists every legacy name
with its replacement, and `LegacyConversions` converts legacy data.

| Package | Replacement |
|---|---|
| `OldYoungTableaux` | `NewTableaux` (tableaux, RSK, crystals, TeX), `GTPatterns` (GT, BZ, Gog/Magog patterns, tiles, Ehrhart data), `CombinatoricTools` (partitions, Kostka numbers), `SymmetricFunctions`, `ShiftedSymmetricFunctions` (shifted Schur/Jack, normalized characters), `PolynomialTools` |
| `MacdonaldPolynomials` | `NonsymmetricPolynomials` (operators; keys, atoms, t-keys, t-atoms, Schubert, Grothendieck, Lascoux, slides, locks, nonsymmetric Macdonald and Jack), `NewTableaux` (SSAF fillings, crystals, Mason insertion), `QuasiSymmetricFunctions` (polynomial bridges, quasisymmetric Schur). Keys and locks are indexed in reverse in the legacy package: new `KeyPolynomial[alpha]` = old `KeyPolynomial[Reverse[alpha]]` |
| `ChromaticFunctions` | `UnicellularChromatics`, which contains its useful functions with the 0-first area-list convention of `CatalanObjects` (ChromaticFunctions area lists end with 0) |
| `TreesData` | `GraphTools`TreeGraphs` (unrooted, n <= 20) and `GraphTools`RootedTreeGraphs` (n <= 10) |
| `RunSortedWords` | `CombinatoricTools`RunSortedPermutations` and `SetPartitionToRunSortedPermutation`; the package is now a shim |

## Names shared with supported packages

Loading a legacy package together with supported packages prints `::shdw` messages for
the following names, which the legacy packages define differently. Whichever context comes
first in `$ContextPath` wins for newly typed input; use full names
(for example `OldYoungTableaux`GTPatterns`) when mixing them.

- `OldYoungTableaux`: `YoungTableau`, `YoungTableauForm` (different tableau objects),
  `GTPattern`, `GTPatterns`, `GTPatternForm` (patterns stored top to bottom),
  `KostkaCoefficient` (third argument is a skew weight, not the Jack parameter),
  `MacdonaldPsi` (different signature), `PartitionList` (drops empty pieces),
  `PermutationType`.
  `ConjugatePartition`, `UnimodalQ` and `ZCoefficient` are no longer duplicated: the
  package uses the CombinatoricTools versions.
- `MacdonaldPolynomials`: `KeyPolynomial`, `AtomPolynomial`, `SchubertPolynomial`, `LockPolynomial`,
  `FundamentalSlide`-related names and `MacdonaldEPolynomial` (NonsymmetricPolynomials; keys and
  locks are reversed in the legacy package, and its four-argument `MacdonaldEPolynomial` uses the
  basement n, ..., 1). `LegacyConversions` converts indices.
- `ChromaticFunctions`: its functions that now live in `UnicellularChromatics` (with the
  opposite area-list convention), `AreaBounce`, `AreaListPlot`, `AreaToBounceShape`,
  `Labels` (CatalanObjects), and `GraphOrientations`, `GraphAcyclicOrientations`
  (GraphTools, which takes `Graph` objects).
