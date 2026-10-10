# Mathematica Packages Handoff

## Current status

- The clean project checkout was created from `PerAlexandersson/Mathematica-packages`
  at commit `664f6d2` on `master`.
- The legacy Dropbox working copy remains untouched. Its Git metadata is incomplete,
  it lacks 23 files present on GitHub, and it has local differences in four package
  files. Recovery is tracked in issue #1.
- The repository-refresh plan is tracked by milestone `Repository refresh` and the
  umbrella issue #11.
- Issue #1 is complete and merged through PR #12 at `bb8eb67`: the four local
  differences are preserved in separate cosmetic and semantic commits, row-flag
  endpoint handling is corrected, `RowFlags` is public, and focused regression
  tests pass.
- The 2026-10-10 deep audit is complete. Its confirmed defects are tracked in
  #13--#20 (one issue per group of files) and missing usage strings in #21;
  audit corrections were added to #3, #4, #5 and #11.
- PR #22 runs every `Tests/*.wlt` file in a fresh kernel; see `Tests/README.md`.
- Orchestrator: Claude session `agent-mathematica-mathem-c-eaac064b`. It owns
  `SymmetricFunctions.m` (#5 hotfix: private result cache, no alias block),
  `Tests/SymmetricFunctions.wlt`, `Tests/RunTests.wls`, `HANDOFF.md`, and all
  integration, commits and PRs.
- Codex `gpt-5.6-luna` workers fix bugs in scratch copies and never touch the
  repository; the orchestrator reviews and integrates their patches:
  - A (#13): `CombinatoricTools.m`, `PolynomialTools.m` and their tests.
  - B (#14): `NewTableaux.m`, `GTPatterns.m`, `Tests/GTPatterns.wlt`, `Tests/NewTableaux.wlt`.
  - C (#15): `GraphTools.m`, `MatroidTools.m`, `RookTools.m` and their tests.
  - D (#16): `CatalanObjects.m`, `PermutationTools.m`, `QuasiSymmetricFunctions.m` and their tests.
  - Queued: E (#17, chromatic packages), F (#18, legacy packages), #20 after the #5 hotfix.

## Planned sequence

1. Define supported contexts and public symbols (#2).
2. Add the test baseline (#6), then repair context isolation and known defects
   (#3--#5).
3. Introduce the paclet layout and portable datasets (#7--#8).
4. Archive or split unsupported material and finish documentation (#9--#10).

## Review evidence

- All published `.m` files load individually under Wolfram 14.3.
- Loading `MacdonaldPolynomials` emits multiple context-shadowing warnings.
- Eleven files use the shared top-level ``Private` `` context.
- Confirmed defects and reproductions are recorded in issues #3--#5.
- Wolfram Code Inspector reported 61 errors and 67 warnings before triage; issue #6
  owns the reviewed baseline and executable tests.

## Verification

- Fresh clone matches `origin/master` at `664f6d2` before this planning update.
- GitHub issues #1--#11 are assigned to milestone `Repository refresh`.
- `wolframscript -file Tests/RunTests.wls`: 12 succeeded, 0 failed.
- Representative unflagged `GTPatterns` calls match the pre-recovery GitHub
  implementation; new tests cover zero content, empty shapes, row flags, and the
  recovered cylindric-Schur behavior.
