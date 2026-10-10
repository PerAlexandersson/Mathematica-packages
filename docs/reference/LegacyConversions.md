---
title: LegacyConversions
parent: Reference
nav_order: 14
---

# LegacyConversions

Converters from the data of the legacy packages (GT patterns, tableaux, area lists, key indices) to the supported conventions.

Load with `` Needs["LegacyConversions`"] ``.

## Functions and symbols

### FromLegacyAreaList

FromLegacyAreaList\[a\] reverses a ChromaticFunctions area list ending in 0 to the 0-first convention used by CatalanObjects and UnicellularChromatics.

### FromLegacyEdges

FromLegacyEdges\[edges,n\] converts a legacy edge list to the supported vertex convention by relabelling every vertex v as n + 1 - v and reversing each ordered edge.

### FromLegacyGTPattern

FromLegacyGTPattern\[OldYoungTableaux\`GTPattern\[rows\]\] converts legacy top-to-bottom rows to a GTPatterns\`GTPattern with bottom-to-top rows.

### FromLegacyIndex

FromLegacyIndex\[family,index\] converts a legacy nonsymmetric-polynomial index: family "Key", "TKey", or "Lock" reverses index, while "Atom", "TAtom", "Schubert", "Slide", and "FundamentalSlide" leave it unchanged.

### FromLegacyKeyIndex

FromLegacyKeyIndex\[alpha\] reverses a MacdonaldPolynomials key, t-key, or lock index to the standard NonsymmetricPolynomials convention. Atom, t-atom, Schubert, and fundamental-slide indices are unchanged; use FromLegacyIndex for one explicit family.

### FromLegacyYoungTableau

FromLegacyYoungTableau\[OldYoungTableaux\`YoungTableau\[rows\]\] converts legacy skew markers OldYoungTableaux\`Private\`SKEW to None in a NewTableaux\`YoungTableau.

### ToLegacyAreaList

ToLegacyAreaList\[a\] reverses a 0-first area list to the ChromaticFunctions convention ending in 0.

### ToLegacyEdges

ToLegacyEdges\[edges,n\] applies the inverse of FromLegacyEdges; the vertex relabelling v -&gt; n + 1 - v is an involution.

### ToLegacyGTPattern

ToLegacyGTPattern\[GTPatterns\`GTPattern\[rows\]\] converts bottom-to-top rows to the legacy top-to-bottom representation.

### ToLegacyYoungTableau

ToLegacyYoungTableau\[NewTableaux\`YoungTableau\[rows\]\] converts None skew cells to OldYoungTableaux\`Private\`SKEW.

