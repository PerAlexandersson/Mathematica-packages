# Contributing

## Layout

- `Kernel/` holds the supported and experimental packages, `Legacy/` the legacy ones (see
  `Legacy/README.md`). Every context is registered in `PacletInfo.wl`; add new packages
  there.
- `Data/` holds datasets (documented in `Data/README.md`); find them relative to the
  package file, never through an absolute path.
- `Examples/` holds plain-text example scripts (`.m`); `Tests/` holds the test suite.

## Package conventions

- Use the shared object representations of `CONVENTIONS.md`; accept them as input and
  return them as output. `Tests/CompatibilityTests.m` checks the contract.

- `BeginPackage["Name`", {dependencies}]`, declare every public symbol before
  `Begin["`Private`"]` (note the leading backtick: never use the shared top-level
  ``Private` `` context), and keep helpers in the private section.
- Depend only on supported packages, and list every package whose symbols you use in
  `BeginPackage`. Do not define, in a private section, a name that another package
  exports: it would add definitions to that package's symbol.
- Do not modify `System` symbols (the `Permutations[n]` rule in CombinatoricTools is a
  documented exception) and do not set usage messages on them.
- Do not memoize with `f[x] := f[x] = ...` on symbols that are protected
  (SymmetricFunctions protects its public API; use its private `cached[key, expr]`).
- Use exact arithmetic for combinatorial quantities.
- Every public symbol needs a usage string starting with its call signature; option
  names say which functions they belong to (`Tests/UsageTests.m` enforces this).

## Public API changes and deprecation

- A public symbol of a supported package keeps its name and meaning within a minor
  version. To retire a name, keep it working for at least one minor release with a
  usage string starting "Deprecated:" and naming the replacement, record it under
  "Deprecated" in `CHANGELOG.md`, and remove it in a later release (also recorded).
- Changes of mathematical convention are breaking changes, even when no name changes;
  record them under "Breaking changes" with an example.
- Two supported packages must not export the same name (`Tests/LoadOrderTests.m`
  checks this); share a single definition instead.

## Tests

- Run `wolframscript -file Tests/RunTests.m`; it runs each `Tests/*Tests.m` file in a
  fresh kernel. Conventions are in `Tests/README.md`.
- Every bug fix comes with a regression test that fails before the fix. Compare with an
  independent computation (brute force, known counts, an identity, or the Rust fixtures in
  `Tests/fixtures/rust/`) rather than with the new output.
- Do not raise the counts in `Tests/LintTests.m`.
- Before a release, follow `RELEASING.md`.
