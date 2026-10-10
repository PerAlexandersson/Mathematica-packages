# Datasets

Runtime data used by the packages. Paths are resolved relative to the package
files, so the repository can live anywhere.

| Files | Contents | Used by | Provenance |
|---|---|---|---|
| `graphs/graph{n}c.g6`, n = 2..9 | All non-isomorphic connected simple graphs on n vertices, graph6 format; counts follow OEIS A001349 (1, 2, 6, 21, 112, 853, 11117, 261080) | `GraphTools`ConnectedSimpleGraphs` | Generated enumeration in graph6 format (the standard output of nauty `geng -c`); exact generator not recorded |
| `trees/trees{n}.g6`, n = 5..20 | All non-isomorphic unlabeled trees on n vertices, graph6 format; counts follow OEIS A000055 | `GraphTools`TreeGraphs` | House of Graphs, https://houseofgraphs.org/meta-directory/trees |
| `posets/allPosets1-5.txt` | All 1 + 2 + 5 + 16 + 63 = 87 unlabeled posets on 1..5 elements as Hasse diagrams (OEIS A000112) | not loaded by any package | Unknown; validated for distinctness and counts (audit 2026-10-10) |
| `posets/umaxmls{n}.txt`, n = 3..9 | Posets on n elements with a unique maximal element; count equals A000112(n-1) | not loaded by any package | Unknown; validated for distinctness and counts (audit 2026-10-10) |

`trees17.g6`--`trees20.g6` account for about 41 MB of the 45 MB here; issue #8
tracks whether to keep them in the repository or download them on demand.
