---
title: Home
nav_order: 1
---

# Mathematica packages for symmetric functions

Wolfram Language packages for symmetric functions and algebraic combinatorics:
symmetric and quasisymmetric functions, tableaux, Gelfand–Tsetlin patterns, Catalan
objects, permutations, posets, graphs and matroids. They grew out of research use and
are offered to those who prefer Mathematica over Sage; see also
<https://www.symmetricfunctions.com/>.

- [Tutorial](tutorial.html): a tour of the packages and how they fit together.
- [Reference](reference/): every package and function, with background links to [symmetricfunctions.com](https://www.symmetricfunctions.com/).
- [Conventions](conventions.html), [migrating from the legacy packages](migration.html), [changelog](changelog.html).
- Source, issues and releases: [GitHub](https://github.com/PerAlexandersson/Mathematica-packages).

## Installation

The repository is one paclet, `PerAlexandersson/MathematicaPackages`. Install the latest
release directly from GitHub,

```wolfram
PacletInstall["https://github.com/PerAlexandersson/Mathematica-packages/releases/download/v0.1.0/PerAlexandersson__MathematicaPackages-0.1.0.paclet"];
Needs["SymmetricFunctions`"]
```

or load it from a checkout,

```wolfram
PacletDirectoryLoad["/path/to/Mathematica-packages"];
Needs["SymmetricFunctions`"]
```

or build and install an archive:

```bash
wolframscript -file Scripts/BuildPaclet.m
```

```wolfram
PacletInstall["/path/to/Mathematica-packages/build/PerAlexandersson__MathematicaPackages-0.1.0.paclet"];
Needs["SymmetricFunctions`"]
```

Compatibility: tested with Wolfram Language 14.3 (the paclet requires 14.3 or later;
earlier versions may work but are untested).

