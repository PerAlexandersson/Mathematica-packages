# Mathematica Packages Handoff Archive

Notes from the 2026 repository refresh (before the legacy port), moved from `HANDOFF.md`.
They describe the state at that time; several items (for example the shadowing warnings and
the shared `Private` contexts) have since been fixed.

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
