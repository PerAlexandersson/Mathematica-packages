# Mathematica Packages Handoff

## Current status

- The clean project checkout was created from `PerAlexandersson/Mathematica-packages`
  at commit `664f6d2` on `master`.
- The legacy Dropbox working copy remains untouched. Its Git metadata is incomplete,
  it lacks 23 files present on GitHub, and it has local differences in four package
  files. Recovery is tracked in issue #1.
- The repository-refresh plan is tracked by milestone `Repository refresh` and the
  umbrella issue #11.
- Active branch: `refactor/recover-local-changes`.
- Host supervisor owns `GTPatterns.m`, `SymmetricFunctions.m`,
  `MacdonaldPolynomials.m`, `PolynomialTools.m`, and the initial recovery tests
  for issue #1. No worker owns overlapping files.

## Planned sequence

1. Recover and test the meaningful unpublished changes (#1).
2. Define supported contexts and public symbols (#2).
3. Add the test baseline (#6), then repair context isolation and known defects
   (#3--#5).
4. Introduce the paclet layout and portable datasets (#7--#8).
5. Archive or split unsupported material and finish documentation (#9--#10).

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
