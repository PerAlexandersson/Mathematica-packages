# Mathematica/Rust cross-check fixtures

This directory is a standalone Cargo crate. It was generated against the Rust
workspace at commit `4349e40c96d4832aae5320c6c08bcdcc99cbcd0f` and writes the
JSON files in this directory:

```bash
CARGO_TARGET_DIR=/cargo-target/ai-projects \
  timeout 120 nice -n 10 cargo run --manifest-path Cargo.toml
```

The generator uses the public APIs of `sym-poly-sym`, `sym-poly-qsym`,
`sym-poly-multipoly`, `sym-poly-core`, `combinatoric-core`, and `polytool`.
The first attempted manifest used pinned Git dependencies:
`https://github.com/PerAlexandersson/polytool` at the commit above. The
environment could not fetch that revision, so the checked-in manifest uses
read-only path dependencies under `/workspace/rust`; to reproduce from a
network-enabled checkout, replace the path entries with the pinned Git entries
documented in the task brief.

All JSON files have a top-level `family`, `rust_function`, `convention`, and
family-specific records. Partition and composition vectors are written in the
Rust library's displayed order. Polynomial coefficient vectors are ascending
degree order. Rational coefficients are JSON integers when integral and strings
such as `"1/2"` otherwise. Symmetric-function terms are `[index, coefficient]`;
q-polynomial terms use `[index, [c_0,c_1,...]]`; multivariate terms use
`[exponent_vector, coefficient]`.

The generated families are:

- Kostka numbers through degree 7, selected Littlewood--Richardson products
  of total degree at most 8, and symmetric-group characters through degree 7;
- all classical transition matrices between `m`, `e`, `h`, `p`, and `s`
  through degree 6;
- modified-Macdonald `B_mu` and `nabla` eigenvalues through degree 4;
- all Rust-valid area sequences of sizes 1--4 for unicellular LLT, including
  raw, Schur, and `q -> q+1` elementary data, plus their chromatic functions;
- fundamental-to-monomial quasisymmetric conversions through degree 4 and a
  product;
- small key, atom, and Schubert polynomials;
- Eulerian polynomials and exact real-rootedness/interlacing decisions; and
- Lah and Petrie symmetric functions;
- partitions, compositions, set partitions, and their enumeration/refinement
  data;
- permutation statistics, cycle types, Foata maps, and classical pattern
  avoidance counts;
- graph independence, matching, and chromatic polynomials;
- poset linear-extension counts, order-polynomial values, and P-Eulerian
  polynomials;
- basis-list matroid operations, Tutte polynomials, and independent sets;
- lattice-path matroid bases from Dyck area sequences; and
- Schur plethysms.

The Mathematica consumer is `../../CrossCheckTests.m`, which imports each file
relative to the test file and records one verification per family. Ordinary
Hall--Littlewood/Kostka--Foulkes expansions and full modified Macdonald Schur
expansions remain omitted because this Rust revision exposes only
nonsymmetric Hall--Littlewood and modified-Macdonald-basis operator data, not
the corresponding ordinary/full public expansions.
