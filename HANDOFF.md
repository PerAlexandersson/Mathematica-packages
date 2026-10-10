# Mathematica Packages Handoff

## Current status

- 2026-10-10: the repository refresh is essentially complete. The deep audit's
  defects (#4, #5, #13--#21, #35) are fixed with regression tests; packages use
  private contexts (#3); the repository is a paclet with `Kernel/`, `Legacy/`,
  `Data/`, `Examples/`, `Tests/`, `Scripts/` (#7, #8); legacy packages are
  documented in `Legacy/README.md` (#9); documentation, MIT license, changelog
  and contribution guide are in place (#10). See `CHANGELOG.md` for 0.1.0.
- Owner decisions: Haglund convention for `MacdonaldHSymmetric` (#35); keep the
  `Permutations[n]` convenience (#3); package-status proposal approved (#2);
  keep large tree data (#8); MIT license; plain `.m` files for tests, scripts
  and examples.
- Next: port the legacy packages into the supported structure with cross-package
  compatibility; the plan is issue #51 (milestone `Legacy port`), awaiting owner
  decisions D1--D7. Nothing in it is implemented yet.
- Open for the owner: whether to rename the remaining legacy names that differ
  in meaning from supported ones (PR #47), and when to release 0.1.0
  (`RELEASING.md`).
- Verification: `wolframscript -file Tests/RunTests.m` (252 tests in 26 files)
  and `wolframscript -file Scripts/BuildPaclet.m` (all 19 contexts load from the
  built archive). The Rust cross-check (`Tests/fixtures/rust/`) found no
  disagreement with `sym-poly`, `combinatoric-core`, `combpoly`, `polytool`.
- No worker owns files. Codex workers used during the refresh worked only in
  scratch copies; all integration went through reviewed PRs #22--#50.

## Planned sequence (completed)

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
