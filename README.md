# Mathematica-packages

Packages for computing with symmetric functions, and  combinatorics

These are all work in progress, but some of these packages might be useful
for those who prefers Mathematica over Sage.

## Installation

The repository is a single paclet. Either load it from a checkout,

```wolfram
PacletDirectoryLoad["/path/to/Mathematica-packages"];
Needs["SymmetricFunctions`"]
```

or build an archive with `wolframscript -file Scripts/BuildPaclet.wls` and install it
with `PacletInstall["build/PerAlexandersson__MathematicaPackages-0.1.0.paclet"]`.
Supported and experimental packages live in `Kernel/`, legacy packages (kept loadable
for existing notebooks) in `Legacy/`, datasets in `Data/`, and notebooks in `Examples/`.
Tested with Wolfram Language 14.3. Run the tests with
`wolframscript -file Tests/RunTests.wls`.

## SymmetricFunctions.m

This is a package for working with symmetric functions, see https://www.symmetricfunctions.com/. 
It has the core bases, as well as Jack and Macdonald polynomials, LLT polynomials,
SchurP and SchurQ functions, the Hall inner product, plethysm and the Delta operator.

This also has support for working multiple alphabets, which is useful when dealing with plethysm calculus.

Compare with Stembridge's sf package for Maple, or Curtis Greene's symfun13 package.

See `Examples/SymmetricFunctions-Introduction.nb` for a brief introduction.

*Updates*
- 2023-08-08 Code added for storing transition matrices between sessions.
- 2020-08-25 Finished big refactoring. 
  Many functions are now more intuitive, and basis conversion is quicker.


## CombinatoricTools.m

Functions for permutation statistics (major index, inversions, Bruhat order, etc.), 
compositions, partitions, q-analogs, the Foata map,
and various helper functions.


## NewTableaux.m

Functions for generating standard Young tableaux, semi-standard Young tableaux,
RSK, promotion and crystal operators. 


## CatalanObjects.m

Various different families of Catalan objects,
and ways to draw them.



## ChromaticFunctions.m

Chromatic (quasi)symmetric functions and LLT polynomials
associated with unit interval graphs.
Various functions related to unit-interval graphs.

## GTPatterns.m

Methods for generating GT-patterns (Gelfand-Tsetlin patterns) which are in bijection with
semi-standard Young tableaux.


## MacdonaldPolynomials.m

This is an older package, and not very up to date.
It has Schubert polynomials, permuted-basement Macdonald polynomials,
and various non-symmetric polynomials.

## Tex2WebUtilities.m

I use this mainly for generating the bibliography on the Symmetric functions catalog.
It can parse .bib-files and produce an associative array
of the data. 
There are also functions for converting `ytableau` syntax to HTML.


## PosetData.m

This is a file which mainly contains non-isomorphic posets.

There are also plot functions, and functions for computing linear extensions or the Jordan-Hölder set.


## TreesData.m

This is a file which mainly contains non-isomorphic trees.
