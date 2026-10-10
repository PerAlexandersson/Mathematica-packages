# Changelog

All notable changes to this repository are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and versions follow semantic
versioning as described in `RELEASING.md`. Issue and pull-request numbers refer to
<https://github.com/PerAlexandersson/Mathematica-packages>.

## [0.1.0] - Unreleased

First release as a paclet (`PerAlexandersson/MathematicaPackages`). The state before
this refactoring is tagged `pre-refresh-2026-10`.

### Breaking changes

- `MacdonaldHSymmetric` follows Haglund's convention: H~_{2} = s_2 + q s_11. The former
  `MacdonaldHSymmetric[mu, q, t]` equals the new `MacdonaldHSymmetric[mu, t, q]`.
  `DeltaOperator`, `DeltaPrimOperator`, `NablaOperator` and `ToMacdonaldHBasis` follow
  (B_mu = sum of q^a'(c) t^l'(c)). Results symmetric in q and t are unchanged (#35, #38).
- The repository is a paclet: packages live in `Kernel/` (supported and experimental)
  and `Legacy/`; load them with `PacletDirectoryLoad` or install the paclet. Context
  names and `Needs` calls are unchanged (#7, #42). Tests and scripts are plain `.m` files (#49).
- `CatalanObjects`RookPlacementPlot` (a `Grid`) is renamed `RookPlacementGrid`; the name
  `RookPlacementPlot` belongs to RookTools (#46).
- `StrictEdges` and `WeakEdges` are single shared options in `CombinatoricTools` (#46).
- MacdonaldPolynomials no longer loads OldYoungTableaux, no longer exports its own
  `KnuthRepresentative` and `PartitionedCompositionCoarsenings` (use NewTableaux and
  QuasiSymmetricFunctions), and its 3-argument `ToPowerSumBasis` is renamed
  `ToPowerSumBasisMacdonald` (#43).
- `MonomialQSymbol` products and powers expand automatically (#16, #31).
- Every package uses its own private context; code that reached package helpers through
  the shared top-level ``Private` `` context must use full names such as
  `NewTableaux`Private`helper` (#3, #32).
- ChromaticFunctions, OldYoungTableaux, TreesData and RunSortedWords
  are legacy packages; see `Legacy/README.md` (#9).

### Added

- `AlgebraicBases`, an internal package with `CreateBasis`, shared by the algebra packages
  for basis symbols (formatting, index normalization, products) (#51).
- `CONVENTIONS.md` and `Tests/CompatibilityTests.m`: shared object representations
  between packages (#51).

- `ClearSymmetricFunctionsCache[]` (#23).
- `CombinatoricTools`RunSortedPermutations` (from RunSortedWords, now a shim) (#39).
- `GraphTools`RootedTreeGraphs` for n <= 10 (from TreesData) (#40).
- `kSchurSymmetric` is exported (#27). `MonomialQSymmetric`, `OrderedRootedTreeSize` and
  `RationalDyckPaths` are defined and exported (#31).
- UnicellularChromatics contains the useful ChromaticFunctions API (graph and area-list
  chromatic and LLT polynomials with weights and strict/weak edges, orientations,
  Gasharov P-tableaux, vertical-strip LLT, dinv/maj/bounce statistics), using the 0-first
  area-list convention (#44).
- Usage strings for every public symbol (#21, #36).
- Datasets in `Data/` with provenance (`Data/README.md`), found relative to the paclet (#8, #37).
- Tests: `wolframscript -file Tests/RunTests.m` runs every `Tests/*Tests.m` file in a
  fresh kernel (#22). It includes regression tests for all fixed defects,
  load-order and isolation tests, a Code Inspector baseline (#33), usage-string coverage,
  the introduction example (#48), and a cross-check against the Rust libraries
  `sym-poly`, `combinatoric-core`, `combpoly` and `polytool` covering about 25 families
  (#34, #41).
- `Scripts/BuildPaclet.m` builds and verifies the paclet archive; `RELEASING.md`,
  `CHANGELOG.md`, `CONTRIBUTING.md` and an MIT `LICENSE`.
- `Examples/SymmetricFunctions-Introduction.m`, a plain-text version of the introduction
  notebook.

### Fixed

SymmetricFunctions
- `MacdonaldHSymmetric`, `ToMacdonaldHBasis`, `DeltaOperator`, `DeltaPrimOperator`,
  `NablaOperator`, `SkewKostkaCoefficient` and `KroneckerCoefficient` no longer abort with
  `$RecursionLimit`, and memoized functions no longer emit `Set::write` (regression from
  `eed1b77`, #5, #23).
- Loading no longer exports protected junk symbols `name` and `rest` (#23).
- `HallInnerProduct`/`JackInnerProduct` with constant terms, `Plethysm` with constant terms,
  `LRCoefficient` for mismatched sizes, `PrincipalSpecialization` at q = 1, and
  `SkewMacdonaldESymmetric` (unresolved helper) (#20, #27).

Core utilities
- CombinatoricTools: `LatticeWordQ` returns a Boolean; `IntegerCompositions[0]`,
  `Derangements[0]`, negative `SetPartitions`; `PartitionIntervalSize`;
  `LinearlyIndependentRows`; `Durfee` usage; the `System`Permutations` convenience rule no
  longer caches about 290 MB, and package code no longer relies on it (#4, #13, #24, #32).
- PolynomialTools: `ElementarySymmetricPolynomial`, `CompleteHomogeneousPolynomial`,
  `UltraLogConcaveQ`, `HilbertFunctionValues` (stale results across calls),
  `InterleavingRootsQ`/`RealRootedQ` always return Booleans, `EulerianA[0, m]` (#4, #13, #24).
- NewTableaux: `KnuthRepresentative` and `BiwordRSK` no longer recurse forever,
  `CrystalSi` on words, `BSTHeightVector`, empty skew SSYT, leaked helpers (#14, #25).
- GTPatterns: partial `RowFlags` lists and invalid values (#14, #25).
- GraphTools, MatroidTools, RookTools: `GraphIndependentTriangles`, `GraphDeleteEdge`,
  failed data imports no longer memoized, `MatroidDeletion` by a set, `IsMatroidQ`,
  `MatroidContraction` by dependent sets, `RookPlacementPlot` (#15, #26).
- CatalanObjects, PermutationTools, QuasiSymmetricFunctions: `DyckPaths`,
  `PerfectMatchings`, `FussCatalanPaths`, `ORTTo231Perm`, empty base cases,
  `SeparablePermutationQ`, `SplitSeparablePermutation`, `GeneratePAPS` and
  `PermutationFromWord` caches, `PowerSumAltQSymmetric` (#16, #31).
- ChromaticFunctions and UnicellularChromatics: weighted chromatic cache,
  `VerticalStripLLTPolynomial`, `DinvFromAreaSeq`, `AreaRowPermutation`, `System`Weights`
  usage no longer overwritten, `UnicellularLLTSymmetric` with integer q, Schroeder
  orientations, `ChromaticSymmetric` on arbitrary vertex labels (#17, #28).
- Legacy packages: `GetTrees`, `FundamentalSlide`, `QuasiSymmetricPowerSum2`, loading
  MacdonaldPolynomials no longer modifies other packages, `OrderPolynomial` for non-natural
  labellings, `StembridgePoset` size, `SchurPolynomial` (#18, #29).
- Graph and tree data no longer load from a hard-coded `~/Dropbox` path (#8, #37).

### Removed

- `SymmetricFunctionsTestSuite.m`; its cases are in `Tests/SymmetricFunctionsTests.m` (#27).
- `Tex2WebUtilities` (website tooling of an older project, replaced by other tools); it
  remains available in the `pre-refresh-2026-10` tag.
