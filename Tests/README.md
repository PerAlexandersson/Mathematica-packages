# Tests

Run everything with

```bash
wolframscript -file Tests/RunTests.wls
```

`RunTests.wls` runs each `Tests/*.wlt` file in its own fresh kernel (via
`RunTestFile.wls`), so a test file cannot depend on packages loaded by another
file. The command exits non-zero if any test fails, a kernel fails, or a file
contains no tests.

`Lint.wlt` runs Wolfram Code Inspector on every package and fails on high-confidence
errors beyond the reviewed baseline recorded in that file. `LoadOrder.wlt` checks that
packages do not interfere through shared contexts.

## Conventions

- One file per package, named `<Package>.wlt`.
- Start the file with the `$Path` setup used by the existing files, followed by a
  load test, which fails if loading emits any message:

  ```wolfram
  VerificationTest[
    Needs["CombinatoricTools`"],
    Null,
    TestID -> "CombinatoricTools-loads-cleanly"
  ]
  ```

- Every fixed bug gets a regression test with a descriptive `TestID` of the form
  `<Package>-<function>-<behaviour>`; mention the issue number in a comment.
- `VerificationTest` fails on unexpected messages; use `ExpectedMessages` only when
  a message is the documented behaviour.
- Use exact arithmetic and small inputs; each test should run in well under a second.
- Fully qualify symbols that exist in more than one package context
  (for example `CatalanObjects`AreaBounce`).
