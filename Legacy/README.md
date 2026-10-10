# Legacy packages

These packages are kept loadable for existing notebooks (`Needs["OldYoungTableaux`"]`
etc. still work after `PacletDirectoryLoad` or installation), but they are not maintained
and no supported package loads them. Prefer the replacements below in new code.

| Package | Replacement |
|---|---|
| `OldYoungTableaux` | `NewTableaux` (tableaux, RSK, crystals), `GTPatterns`, `CombinatoricTools` (partitions, Kostka numbers), `SymmetricFunctions` |
| `MacdonaldPolynomials` | `NonsymmetricPolynomials` (keys, atoms, t-keys, t-atoms, Schubert; the remaining families are being ported, #51). Note: its key functions index compositions in reverse; the new ones use the standard convention, `KeyPolynomial[alpha]` = old `KeyPolynomial[Reverse[alpha]]` |
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
- `MacdonaldPolynomials`: `KeyPolynomial`, `AtomPolynomial`, `SchubertPolynomial` (NonsymmetricPolynomials; keys reversed in the legacy package).
- `ChromaticFunctions`: its functions that now live in `UnicellularChromatics` (with the
  opposite area-list convention), `AreaBounce`, `AreaListPlot`, `AreaToBounceShape`,
  `Labels` (CatalanObjects), and `GraphOrientations`, `GraphAcyclicOrientations`
  (GraphTools, which takes `Graph` objects).
