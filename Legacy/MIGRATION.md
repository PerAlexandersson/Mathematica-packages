# Migrating from the legacy packages

The legacy packages `OldYoungTableaux`, `MacdonaldPolynomials` and `ChromaticFunctions` are
frozen. Everything useful in them now lives in the supported packages (issue #51), which follow
one set of conventions ([`CONVENTIONS.md`](../CONVENTIONS.md)). This page lists every public
legacy name and its replacement. `LegacyConversions` converts legacy data:

```wolfram
Needs["LegacyConversions`"]   (* loads no legacy package *)
FromLegacyGTPattern[oldPattern]; FromLegacyYoungTableau[oldTableau];
FromLegacyAreaList[oldArea]; FromLegacyEdges[oldEdges, n]; FromLegacyIndex["Key", alpha];
```

## Convention changes

| Object | Legacy | Supported | Conversion |
|---|---|---|---|
| GT patterns | rows top to bottom (`OldYoungTableaux`) | rows bottom to top (`GTPatterns`) | `FromLegacyGTPattern` (reverse the rows) |
| Skew cells in tableaux | private `SKEW` symbol | `None` (`NewTableaux`) | `FromLegacyYoungTableau` |
| Area lists | end with 0 (`ChromaticFunctions`) | start with 0 (`CatalanObjects`, `UnicellularChromatics`) | `FromLegacyAreaList` (reverse); edges: `FromLegacyEdges` (v -> n + 1 - v) |
| Keys, t-keys, locks | reversed index (`MacdonaldPolynomials`) | standard index: κ_(0,1) = x1 + x2; locks as Assaf–Searles | `FromLegacyIndex["Key" \| "TKey" \| "Lock", alpha]` (reverse) |
| Atoms, t-atoms, Schubert, slides | standard | standard | none |
| `MacdonaldEPolynomial[alpha, x, q, t]` | basement n, ..., 1 | identity basement | legacy = new `MacdonaldEPolynomial[alpha, Range[n, 1, -1], x, q, t]` |
| Normalized characters | `ChNormalizedCharacter` signed | positive | legacy = (-1)^(\|mu\| - Length[mu]) `StanleyCharacterPolynomial[mu, p, q, d]` |
| Functions returning functions | e.g. `SchurPolynomial[lam, mu, n][x]` | expressions in `x[1], ..., x[n]` | see the tables |
| `GTPartition` values | symbols `Tiles`, `Snakes`, ... | strings `"Tiles"`, `"Snakes"`, `"FreeTiles"`, `"ShadedTiles"` | |
| `KostkaCoefficient[lam, mu, w]` | third argument a skew weight | third argument the Jack parameter | skew: `SkewKostkaCoefficient[lam, mu, w]` |

## Same name in a supported package

These names exist in the package shown, with the conventions above; the exceptions noted
differ in meaning. When a legacy package is loaded too, use full names such as
`GTPatterns`GTPatterns` ([`README.md`](README.md)).

**OldYoungTableaux**
- CombinatoricTools: `KostkaCoefficient` (third argument differs, see above), `MacdonaldPsi`
  (different signature), `MacdonaldPsiPrime`, `PartitionList` (the legacy version drops empty
  pieces), `PermutationOfType`, `SageForm`, `SetPartitionRefinementQ`, `ShapeUnion`,
  `SkewShapeQ`, `YoungLatticePaths`
- GTPatterns: `BZPattern`, `BZPatterns`, `BZPlus`, `BoxCountMatrix`, `ContainingFaceDimension`,
  `EnableSkew`, `GTMonomial`, `GTPartition`, `GTPattern`, `GTPatternForm`, `GTPatternTikz`,
  `GTPatterns`, `GTPlus`, `GTSnakes`, `GTTiles`, `GogPatterns`, `KostkaRange`, `LatticePathForm`,
  `LatticePathTikz`, `MagogPatterns`, `ShapeTriplets`, `TilingMatrix`, `UpperBoundKostkaDegree`,
  `WeightRange`
- NewTableaux: `HasOuterCornerQ`, `LineBreaks`, `UseArray`, `YoungTableau`, `YoungTableauForm`
- PermutationTools: `PermutationType` (defined differently in the legacy package; check uses)
- PolynomialTools: `HVector`, `SequenceToPolynomial` (now with a degree bound)
- ShiftedSymmetricFunctions: `ShiftedJackJEvaluate`, `ShiftedJackJPolynomial`,
  `ShiftedJackPEvaluate`, `ShiftedJackPPolynomial`

**MacdonaldPolynomials**
- CombinatoricTools: `ChargeWordDecompose`, `WordCharge`
- NewTableaux (fillings are `SSAF[rows]` objects): `AtomFillings`, `ChargeToMajMap`, `KeyFillings`,
  `LascouxSchutzenberger` (the legacy version can leave the filling set), `RPPToAtom`,
  `SSAFCoInversions`, `SSAFColumnSets`, `SSAFCrystalString`, `SSAFCrystalWord`, `SSAFDn`,
  `SSAFInversions`, `SSAFKnownCharge`, `SSAFMajorIndex`, `SSAFMonomial`, `SSAFWeight`,
  `SSAFWeightNormalize`, `SSAFillings`, `SSYTToAtom`, `TAtomFillings`
- NonsymmetricPolynomials: `AtomPolynomial`, `DividedDifference`, `KeyPolynomial` (index
  reversed), `LockPolynomial` (index reversed), `MacdonaldEPolynomial` (basement, see above),
  `SchubertPolynomial`, `ToAtomBasis`, `ToFundamentalSlideBasis`, `ToKeyBasis`, `ToLockBasis`

**ChromaticFunctions** (area lists now start with 0)
- CatalanObjects: `AreaBounce`, `AreaListPlot`, `AreaToBounceShape`, `Labels`
- CombinatoricTools: `StrictEdges`, `WeakEdges`
- GraphTools: `GraphAcyclicOrientations`, `GraphOrientations` (take a `Graph` or an edge list)
- UnicellularChromatics: all other names, including `GraphAreaLists`, `AreaConjugate`,
  `ValleyEdges`, `DiagramRookPlacements`, `OrientationPlot`, `PArrayPlot`, `UnitIntervalPlot`,
  the chromatic symmetric and LLT functions, orientations and Gasharov tableaux.

## Renamed or replaced

| Legacy | Supported replacement |
|---|---|
| `OldYoungTableaux`SchurPolynomial[lam, mu, n][x]` | `SymmetricFunctionToPolynomial[SchurSymbol[lam], x, n]`; skew: `SkewSchurSymmetric[{lam, mu}]` |
| `MonomialSymmetricPolynomial[lam, n][x]` | `SymmetricFunctionToPolynomial[MonomialSymbol[lam], x, n]` |
| `PowerSumPolynomial[lam, n][x]` | `SymmetricFunctionToPolynomial[PowerSumSymbol[lam], x, n]` |
| `HallLittlewoodP[lam, n, x, t]` | `SymmetricFunctionToPolynomial[HallLittlewoodPSymmetric[lam, t], x, n]` |
| `JackPPolynomial[lam, n, x, a]`, `JackJPolynomial[...]` | `SymmetricFunctionToPolynomial[JackPSymmetric[lam, a], x, n]`, `JackJSymmetric` |
| `HookFactor[lam, a]`, `HookPrimeFactor[lam, a]`, `JackNorm[lam, a]` | `JackLowerHook[lam, a]`, `JackUpperHook[lam, a]`, their product |
| `ChiCoefficient[lam, mu]` | `SnCharacter[lam, mu]` |
| `DominatesQ` | `PartitionDominatesQ` |
| `AddBoxToPartition[mu, top]`, `RemoveBoxFromPartition[mu, bot]` | `PartitionAddBox[mu, top]`, `PartitionRemoveBox[mu, bot]` |
| `ToCycleForm` | `PartitionPartCount` |
| `TrimPartition`, `NormalizePartitions` | `DeleteCases[lam, 0]`, `PadRight` |
| `FullPermutationCycles` | `PermutationAllCycles` (PermutationTools) |
| `TableauShape`, `ToTableauShape` | `YoungTableauShape` |
| `YoungTableauTeX`, `MatrixTeXForm` | `YTableauTeX[t, UseArray -> False]`, `YTableauTeX[t, UseArray -> True]` |
| `Tiles`, `Snakes`, `FreeTiles`, `ShadedTiles` | the strings `"Tiles"`, ... as `GTPartition` values |
| `EhrhartPolynomial[...]` (GT polytopes) | `GTEhrhartPolynomial[lam, mu, w, k]` |
| `ChNormalizedCharacter[mu, d][p, q]` | `(-1)^(Total[mu] - Length[mu]) StanleyCharacterPolynomial[mu, p, q, d]`; also `NormalizedCharacter[mu, lam]` |
| `MacdonaldPolynomials`OperatorKeyPolynomial[alpha, x]` | `KeyPolynomial[Reverse[alpha], x]` |
| `OperatorAtomPolynomial[alpha, x]` | `AtomPolynomial[alpha, x]` |
| `OperatorKeyTPolynomial[alpha, x, t]` | `TKeyPolynomial[Reverse[alpha], x, t]` |
| `AtomTPolynomial[alpha, x, t]` | `TAtomPolynomial[alpha, x, t]` |
| `PiOperator`, `ThetaOperator`, `TPiOperator`, `TThetaOperator` | `DemazureOperator`, `DemazureAtomOperator`, `TDemazureOperator`, `TDemazureAtomOperator` (same arguments) |
| `FundamentalSlide[alpha, x]` | `FundamentalSlidePolynomial[alpha, x]` |
| `DualGrothendieckPolynomial[alpha, x]` | `DualGrothendieckPolynomial[Reverse[alpha], x]` (NonsymmetricPolynomials; index reversed as for keys) |
| `IntegralFormFactor[alpha, q, t] MacdonaldEPolynomial[...]` | `IntegralMacdonaldE[alpha, x, q, t]` |
| `SSAFRaising`, `SSAFLowering` | `CrystalEi`, `CrystalFi` on `SSAF` objects |
| `ModifiedLascouxSchutzenberger` | `LascouxSchutzenberger` |
| `SSYTLascouxSchutzenberger[t, i]` | `CrystalSi[t, i]` |
| `QSymMonomial[alpha, n, x]` | `QuasiSymmetricFunctionToPolynomial[MonomialQSymbol[alpha], x, n]` |
| `GesselFundamental[S, d, n, x]` | `QuasiSymmetricFunctionToPolynomial[FundamentalQSymbol[DescentSetToComposition[S, d]], x, n]` |
| `QSymSchur[alpha, n, x]` | `QuasiSymmetricFunctionToPolynomial[QuasiSchurQSymmetric[alpha], x, n]` |
| `QuasiSymmetricPowerSum`, `QuasiSymmetricPowerSum2` | `PowerSumQSymbol`, `PowerSumAltQSymmetric` (via `QuasiSymmetricFunctionToPolynomial`) |
| `ToGesselSubsetBasis[p, x, ff]` | `PolynomialToQuasiSymmetricFunction[p, x, FundamentalQSymbol]` |
| `ToElementaryBasis`, `ToCompleteHomogeneousBasis`, `ToPowerSumBasisMacdonald` | `PolynomialToSymmetricFunction[p, x, ElementaryESymbol]` (or `CompleteHSymbol`, `PowerSumSymbol`) |
| `CompositionIndexedBasisRule`, `PartitionIndexedBasisRule` | the `To...Basis` functions of NonsymmetricPolynomials and `PolynomialToSymmetricFunction` |
| `MacdonaldHPolynomial`, `MacdonaldJPolynomial` (symmetric) | `MacdonaldHSymmetric` (Haglund convention), `MacdonaldJSymmetric` |
| `ChromaticFunctions`AreaToEdges[area]` | `UnitIntervalEdges[FromLegacyAreaList[area]]` |

## Not ported

| Legacy | Reason |
|---|---|
| `ShiftedSchur[mu, d][p, q]`, `KNormalizedCharacter`, `JackPStructureConstant`, `JackJStructureConstant`, `LCoefficient`, `IndexedToFallingBasisRule`, `FerayN` | no stated contract that could be tested independently (`FerayN` is internal to `StanleyCharacterPolynomial`) |
| `KnopTableaux`, `TopValleyRepresentation` | empty or "TODO" usage strings |
| `UnittestPackage` | replaced by `Tests/RunTests.wls` |
| `IntegralFormNonSymmetricJack` | uses the legacy basement; use `IntegralMacdonaldE` and `NonsymmetricJackPolynomial` |
| `ElementaryPolynomial` (q-deformed e), `SkewMacdonaldE`, `ToMacdonaldEBasis` | not needed by the supported packages so far |
| `DualGrothendieckFillings`, `RPPColumnWeight` | filling internals of `DualGrothendieckPolynomial` |
| `LockFillings`, `KeyAsAtomFillings`, `KeyAsPBFs`, `MacdonaldEFillings`, `MacdonaldHFillings`, `MacdonaldJFillings`, `MacdonaldJMonomial`, `MacdonaldMonomial`, `GeneralTableauFillings` | filling internals; the polynomials are generated by operators |
| `WordDecompose`, `IsInversionTripletTypeAQ/BQ`, `KeyCompositionToPermutation`, `AtomCompositionToPermutation` | internals (`ChargeWordDecompose`, operator words) |
| `LascouxSchutzenbergerNormalizeWord`, `SSYTLascouxSchutzenbergerInvolution`, `SSYTWeightNormalize` | use `CrystalSi` and `SSAFWeightNormalize` |
| `QuasiSymmetricCompleteHomogeneous` | had no definition in the legacy package |
| `ColorPlot`, `DyckDiagramPlot` | use `OrientationPlot`, `UnitIntervalPlot`, `AreaListPlot` |

If something you used is missing here or behaves differently, please open an issue.
