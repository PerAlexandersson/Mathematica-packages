testRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[testRoot];

VerificationTest[
  Needs["Tex2WebUtilities`"],
  Null,
  TestID -> "Tex2WebUtilities-loads-cleanly"
]

(* GitHub issue #19: escape generated HTML text and attributes, and validate colours. *)
VerificationTest[
  Module[{bib, entry, html, unsafe},
    bib = "@article{safe, author={Smith, Sam}, title={A $GL_n$ & <script>alert(1)</script>}, year={2020}, url={https://example.org/a?x=1&y=2}}";
    entry = First[Quiet[CreateBibliography[bib]]];
    html = entry[["html"]];
    unsafe = First[Quiet[CreateBibliography[
        "@article{unsafe, author={Smith, Sam}, title={A title}, year={2020}, url={https://example.org/\" onmouseover=\"alert(1)}}"]]][["html"]];
    {
      StringContainsQ[html, "$GL_n$"],
      StringContainsQ[html, "&lt;script&gt;alert(1)&lt;/script&gt;"],
      StringContainsQ[html, "href=\"https://example.org/a?x=1&amp;y=2\""],
      StringFreeQ[unsafe, "onmouseover=\""],
      StringFreeQ[unsafe, "<script>"]
      }
    ],
  {True, True, True, True, True},
  TestID -> "Tex2WebUtilities-html-escapes-bibliography-output"
]

VerificationTest[
  Module[{valid, invalid},
    valid = StringReplace[
      "\\begin{tabular}*(rgb(1,0,0))x\\end{tabular}",
      TabularTableauToHTMLRule[]];
    invalid = StringReplace[
      "\\begin{tabular}*(red\" onclick=\"alert(1))x\\end{tabular}",
      TabularTableauToHTMLRule[]];
    {
      StringContainsQ[valid, "style=\"background-color: rgb(1,0,0)\""],
      StringFreeQ[valid, "onclick"],
      StringContainsQ[invalid, "&quot; onclick=&quot;"],
      StringFreeQ[invalid, " onclick=\""]
      }
    ],
  {True, True, True, True},
  TestID -> "Tex2WebUtilities-diagram-colour-validation"
]

(* GitHub issue #19: accept punctuation in keys and all ordinary BibTeX value forms,
   while leaving URL double hyphens untouched. *)
VerificationTest[
  Module[{entry, bib},
    bib = "@article{Smith:2020-test.v1, author=\"Smith, Sam\", title=\"A -- title\", year=2019, url={https://example.org/a--b}}";
    entry = First[Quiet[CreateBibliography[bib]]];
    {entry[["id"]], entry[["author"]], entry[["title"]], entry[["year"]], entry[["url"]]}
    ],
  {"Smith:2020-test.v1", {{"Sam", "Smith"}}, "A – title", "2019", "https://example.org/a--b"},
  TestID -> "Tex2WebUtilities-bibtex-parser-keys-values-and-url-dashes"
]

VerificationTest[
  First[Quiet[CreateBibliography[
      "@article{Doe2020, author={Doe, Jane}, title={A title}, year={2020}}"]]][["html"]],
  "<li class=\"citeLI\" id=\"Doe2020\"><span class=\"citeKey\">[Doe20]</span> <span class=\"citeAuthor\">Jane Doe</span>. <span class=\"citeTitle\">A title</span>.  ,  <span class=\"citeYear\">2020. </span></li>\n",
  TestID -> "Tex2WebUtilities-bibliography-existing-format"
]

(* GitHub issue #19: escaped TeX ampersands are cell content, not separators. *)
VerificationTest[
  Module[{tabular, tableau},
    tabular = StringReplace[
      "\\begin{tabular}a\\&b & c\\end{tabular}",
      TabularTableauToHTMLRule[]];
    tableau = StringReplace[
      "\\begin{ytableau}a\\&b & c\\end{ytableau}",
      YoungTableauToHTMLRule[]];
    {
      StringCount[tabular, "<td>"] == 2 && StringContainsQ[tabular, "a&amp;b"],
      StringCount[tableau, "<td>"] == 2 && StringContainsQ[tableau, "a&amp;b"]
      }
    ],
  {True, True},
  TestID -> "Tex2WebUtilities-table-cells-preserve-escaped-ampersands"
]

(* GitHub issue #19: an entry without an author must not abort the bibliography. *)
VerificationTest[
  Module[{entry},
    entry = First[Quiet[CreateBibliography[
        "@misc{noauthor, title={A title}, year=2020}"]]];
    {entry[["author"]], StringContainsQ[entry[["html"]], "A title"], entry[["key"]]}
    ],
  {{}, True, "noauthor"},
  TestID -> "Tex2WebUtilities-bibliography-missing-author"
]

(* GitHub issue #19: title case conversion must not modify math or protected words. *)
VerificationTest[
  Module[{html},
    html = First[Quiet[CreateBibliography[
        "@article{x, author={Smith, Sam}, title={An $GL_n$-module for {Macdonald}}, year={2020}}"]]][["html"]];
    {
      StringContainsQ[html, "$GL_n$-module for Macdonald"],
      StringFreeQ[html, "$gl_n$"],
      StringFreeQ[html, "MACDONALD"]
      }
    ],
  {True, True, True},
  TestID -> "Tex2WebUtilities-capitalization-preserves-math-and-braces"
]

(* GitHub issue #19: support common grouped and ungrouped TeX accent spellings. *)
VerificationTest[
  StringReplace[
    "Schr{\\\"o}der G\\\"{o}del \\\"o Erd{\\H o}s \\v{c} \\'{e} \\`{e} \\~{n} \\c{c} {\\={i}}",
    TeXToUTF8Rule[]],
  "Schröder Gödel ö Erdős č é è ñ ç ī",
  TestID -> "Tex2WebUtilities-tex-accents-convert-common-forms"
]

(* GitHub issue #19: the documented bibliography rule is defined, and tableau rules are a list. *)
VerificationTest[
  Length[DownValues[BibliographyHTMLRules]] > 0,
  True,
  TestID -> "Tex2WebUtilities-bibliography-html-rules-export"
]

VerificationTest[
  ListQ[YoungTableauToHTMLRule[]] && Length[YoungTableauToHTMLRule[]] == 5,
  True,
  TestID -> "Tex2WebUtilities-young-tableau-rules-return-list"
]

(* GitHub issue #19: memoized results are separated by the Function option. *)
VerificationTest[
  Module[{file = CreateTemporary[], first, second},
    Export[file, "abc", "Text"];
    first = MemoizedImport[file, "Function" -> (StringLength[#] &)];
    second = MemoizedImport[file, "Function" -> (ToUpperCase[#] &)];
    DeleteFile[file];
    {first, second}
    ],
  {3, "ABC"},
  TestID -> "Tex2WebUtilities-memoized-import-function-key"
]
