# Mathematica Packages Handoff

## Current status

- 2026-10-10: the repository refresh is complete (audit defects fixed with regression
  tests, paclet layout, documentation, MIT license, `CHANGELOG.md` for 0.1.0; PRs #22--#50).
- Legacy port, issue #51 (milestone `Legacy port`), owner decisions D1--D7 recorded there.
  Merged so far: P0 conventions and compatibility tests (#54), `AlgebraicBases` (#55),
  `NonsymmetricPolynomials` core (#56), ChromaticFunctions remainder (#57), polynomial
  bridges and quasisymmetric Schur (#58), GT-pattern extensions (#59), Grothendieck,
  Lascoux, slide and lock polynomials (#60), small helpers (#61).
- In progress (Codex workers in scratch copies, reviewed by the orchestrator before
  integration): P4 nonsymmetric Macdonald E by operators (owns
  `Kernel/NonsymmetricPolynomials.m` and its tests), P5 SSAF objects in NewTableaux (owns
  `Kernel/NewTableaux.m` and its tests), P7 new `ShiftedSymmetricFunctions` (owns the new
  package, `SymmetricFunctions.m` for moving `ShiftedJackPSymmetric`, `PacletInfo.wl`,
  `UsageTests.m`, `LoadOrderTests.m`). Remaining: P9 (`Legacy/MIGRATION.md`, converters,
  release).
- Rust: `sym-poly` `lock_polynomial` indexed locks in reverse; fixed in
  PerAlexandersson/polytool#8. Until the shared `/workspace/rust` checkout includes it,
  `CrossCheck-Grothendieck-Lascoux-Slide-Lock` compares `LockPolynomial[Reverse[a]]` with
  `lock_polynomial_terms`; after it does, regenerate the fixtures and drop the `Reverse`.
- Open for the owner: legacy names that differ in meaning (PR #47), and when to release
  0.1.0 (`RELEASING.md`).
- Verification: `wolframscript -file Tests/RunTests.m` (337 tests) and
  `wolframscript -file Scripts/BuildPaclet.m` (all 20 contexts load from the archive).

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
