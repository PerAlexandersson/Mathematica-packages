(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "PerAlexandersson/MathematicaPackages",
    "Version" -> "0.1.0",
    "WolframVersion" -> "14.3+",
    "Description" -> "Symmetric functions, tableaux, Gelfand-Tsetlin patterns, Catalan objects, permutations, posets, graphs and matroids.",
    "Creator" -> "Per Alexandersson",
    "PublisherID" -> "PerAlexandersson",
    "License" -> "TBD",
    "PrimaryContext" -> "SymmetricFunctions`",
    "Extensions" -> {
      (* Supported and experimental packages. *)
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          "CombinatoricTools`",
          "NewTableaux`",
          "SymmetricFunctions`",
          "GTPatterns`",
          "PolynomialTools`",
          "PermutationTools`",
          "QuasiSymmetricFunctions`",
          "GraphTools`",
          "MatroidTools`",
          "CatalanObjects`",
          "UnicellularChromatics`",
          "RookTools`",
          "PosetData`",
          "MacdonaldPolynomials`"
        }
      },
      (* Legacy packages, kept loadable for existing notebooks (issue #9). *)
      {
        "Kernel",
        "Root" -> "Legacy",
        "Context" -> {
          "OldYoungTableaux`",
          "ChromaticFunctions`",
          "TreesData`",
          "RunSortedWords`",
          "Tex2WebUtilities`"
        }
      },
      {"Asset", "Root" -> "Data", "Assets" -> {{"Data", "."}}}
    }
  |>
]
