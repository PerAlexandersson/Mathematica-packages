# Tests

Run everything with

```bash
wolframscript -file Tests/RunTests.m
```

`RunTests.m` runs each `Tests/*Tests.m` file in its own fresh kernel (via
`RunTestFile.m`), so a test file cannot depend on packages loaded by another
file. The command exits non-zero if any test fails, a kernel fails, or a file
contains no tests.

`LintTests.m` runs Wolfram Code Inspector on every package and fails on high-confidence
errors beyond the reviewed baseline recorded in that file. `LoadOrderTests.m` checks that
packages do not interfere through shared contexts.

## Conventions

- One file per package, named `<Package>Tests.m`.
- Start the file by loading the working tree as a paclet, followed by a load test,
  which fails if loading emits any message:

  ```wolfram
  testRoot = DirectoryName[DirectoryName[$InputFileName]];
  PacletDirectoryLoad[testRoot];
  ```


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
