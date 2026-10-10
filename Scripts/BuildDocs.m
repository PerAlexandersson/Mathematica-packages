#!/usr/bin/env wolframscript
(* Generates the GitHub Pages site in docs/ (Jekyll, just-the-docs theme):
   - the guides (tutorial, conventions, migration, changelog) copied from the repository, with
     repository-relative links rewritten to site pages or to GitHub;
   - one reference page per supported package, generated from the usage messages, with
     background links to https://www.symmetricfunctions.com/ (Scripts/docs/symcat-links.wl).
   Usage: wolframscript -file Scripts/BuildDocs.m [outputDirectory]   (default: docs/)
   Tests/DocsTests.m checks that docs/ is up to date. *)

root = DirectoryName[DirectoryName[$InputFileName]];
out = If[Length[$ScriptCommandLine] >= 2, $ScriptCommandLine[[2]], FileNameJoin[{root, "docs"}]];
github = "https://github.com/PerAlexandersson/Mathematica-packages";
symcat = "https://www.symmetricfunctions.com/";

PacletDirectoryLoad[root];

(* Supported packages, in the order of the README table, and their descriptions. *)
readme = Import[FileNameJoin[{root, "README.md"}], "Text"];
packageRows = StringCases[readme,
	StartOfLine ~~ "| `" ~~ ctx : WordCharacter .. ~~ "`` | " ~~ desc : Shortest[__] ~~ " |" ~~ EndOfLine :>
		{ctx, desc}];
packages = Select[packageRows, FileExistsQ[FileNameJoin[{root, "Kernel", #[[1]] <> ".m"}]] &];
Quiet[Scan[Needs[#[[1]] <> "`"] &, packages], General::shdw];

links = Get[FileNameJoin[{root, "Scripts", "docs", "symcat-links.wl"}]];
(* Page names and titles of the site (from its built pages). *)
pageTitles = Association[Rule @@@ Import[FileNameJoin[{root, "Scripts", "docs", "symcat-pages.tsv"}], "TSV"]];
sitePages = Keys[pageTitles];
checkPage[p_String] := If[!MemberQ[sitePages, p],
	Print["BuildDocs: unknown symmetricfunctions.com page ", p]; Exit[1]];
Scan[checkPage, Union[Flatten[{Values[links["packages"]], links["symbols"][[All, 3]]}]]];
pageURL[p_String] := symcat <> p <> ".htm";

(* Markdown text from a usage string: escape Markdown syntax, keep line breaks. *)
mdEscape[s_String] := StringReplace[s, {"\\" -> "\\\\", "*" -> "\\*", "_" -> "\\_",
	"[" -> "\\[", "]" -> "\\]", "<" -> "&lt;", ">" -> "&gt;", "|" -> "\\|", "`" -> "\\`",
	"\n" -> "<br>\n"}];

symbolPage[pkg_String, name_String] := SelectFirst[links["symbols"],
	#[[1]] === pkg && StringMatchQ[name, #[[2]]] &, {None, None, None}][[3]];

usage[pkg_String, name_String] := With[{u = ToExpression[pkg <> "`" <> name, InputForm,
		Function[s, MessageName[s, "usage"], HoldAll]]}, If[StringQ[u], u, ""]];

frontMatter[assoc_Association] := "---\n" <> StringRiffle[
	KeyValueMap[#1 <> ": " <> ToString[#2] &, assoc], "\n"] <> "\n---\n\n";

writeFile[rel_String, text_String] := Module[{path = FileNameJoin[{out, rel}]},
	Quiet[CreateDirectory[DirectoryName[path], CreateIntermediateDirectories -> True]];
	Export[path, text, "Text", CharacterEncoding -> "UTF-8"]];

(* Guides copied from the repository: repository path -> {site path, title, nav order}. *)
guides = {
	{"TUTORIAL.md", "tutorial.md", "Tutorial", 2},
	{"CONVENTIONS.md", "conventions.md", "Conventions", 4},
	{"Legacy/MIGRATION.md", "migration.md", "Migrating from the legacy packages", 5},
	{"CHANGELOG.md", "changelog.md", "Changelog", 6}};
sitePath = Association[Join[Rule @@@ guides[[All, {1, 2}]], {"README.md" -> "index.md"}]];

(* Rewrite a relative link target in a file at repository path src. *)
resolve[src_String, target_String] := Module[{dir = FileNameDrop[src, -1], parts, path, anchor},
	{path, anchor} = Replace[StringSplit[target, "#", 2], {{p_} :> {p, ""}, {p_, a_} :> {p, "#" <> a}}];
	parts = Fold[Which[#2 === "..", Most[#1], #2 === "." || #2 === "", #1, True, Append[#1, #2]] &,
		If[dir === "", {}, FileNameSplit[dir]], StringSplit[path, "/"]];
	path = StringRiffle[parts, "/"];
	Which[
		KeyExistsQ[sitePath, path], StringReplace[sitePath[path], ".md" -> ".html"] <> anchor,
		DirectoryQ[FileNameJoin[{root, path}]], github <> "/tree/master/" <> path <> anchor,
		True, github <> "/blob/master/" <> path <> anchor]];
rewriteLinks[src_String, text_String] := StringReplace[text,
	"](" ~~ t : Except[")" | " "] .. ~~ ")" /;
		!StringStartsQ[t, "http" | "#" | "mailto:"] :> "](" <> resolve[src, t] <> ")"];

Scan[Function[g, Module[{text = Import[FileNameJoin[{root, g[[1]]}], "Text"]},
	(* The page title comes from the front matter; drop the first heading. *)
	text = StringReplace[text, StartOfString ~~ "# " ~~ Except["\n"] .. ~~ "\n" -> "", 1];
	writeFile[g[[2]], frontMatter[<|"title" -> g[[3]], "nav_order" -> g[[4]]|>] <>
		"# " <> g[[3]] <> "\n" <> rewriteLinks[g[[1]], text]]]], guides];

(* Reference pages. *)
refRows = {};
Do[Module[{pkg = p[[1]], desc = p[[2]], names, bg, text},
	names = Sort[Select[StringReplace[Names[pkg <> "`*"], pkg <> "`" -> ""],
		StringFreeQ[#, "$"] && usage[pkg, #] =!= "" &]];
	bg = Lookup[links["packages"], pkg, {}];
	text = frontMatter[<|"title" -> pkg, "parent" -> "Reference",
			"nav_order" -> First@FirstPosition[packages[[All, 1]], pkg]|>] <>
		"# " <> pkg <> "\n\n" <> rewriteLinks["README.md", desc] <> ".\n\n" <>
		"Load with `` Needs[\"" <> pkg <> "`\"] ``.\n\n" <>
		If[bg === {}, "", "Background on symmetricfunctions.com: " <>
			StringRiffle[("[" <> pageTitles[#] <> "](" <> pageURL[#] <> ")") & /@ bg, ", "] <> ".\n\n"] <>
		"## Functions and symbols\n\n" <>
		StringJoin[Function[n, Module[{pg = symbolPage[pkg, n]},
			"### " <> n <> "\n\n" <> mdEscape[usage[pkg, n]] <> "\n" <>
			If[pg === None, "", "\nBackground: [" <> pageTitles[pg] <> "](" <> pageURL[pg] <> ")\n"] <> "\n"]] /@ names];
	writeFile[FileNameJoin[{"reference", pkg <> ".md"}], text];
	AppendTo[refRows, "| [" <> pkg <> "](" <> pkg <> ".html) | " <> rewriteLinks["README.md", desc] <> " |"]],
	{p, packages}];

writeFile[FileNameJoin[{"reference", "index.md"}],
	frontMatter[<|"title" -> "Reference", "nav_order" -> 3, "has_children" -> "true"|>] <>
	"# Reference\n\nGenerated from the usage messages of each package. Each page links to the " <>
	"corresponding background pages on [symmetricfunctions.com](" <> symcat <> ").\n\n" <>
	"| Package | Contents |\n|---|---|\n" <> StringRiffle[refRows, "\n"] <> "\n"];

(* Home page: the README introduction and installation. *)
intro = StringTrim[StringSplit[readme, "\n## "][[1]]];
intro = StringReplace[intro, StartOfString ~~ "# " ~~ Except["\n"] .. ~~ "\n" -> ""];
install = First[StringCases[readme, "## Installation\n" ~~ s : Shortest[__] ~~ "\n## " :> s], ""];
writeFile["index.md", frontMatter[<|"title" -> "Home", "nav_order" -> 1|>] <>
	"# Mathematica packages for symmetric functions\n\n" <> rewriteLinks["README.md", StringTrim[intro]] <>
	"\n\n- [Tutorial](tutorial.html): a tour of the packages and how they fit together.\n" <>
	"- [Reference](reference/): every package and function, with background links to " <>
	"[symmetricfunctions.com](" <> symcat <> ").\n" <>
	"- [Conventions](conventions.html), [migrating from the legacy packages](migration.html), " <>
	"[changelog](changelog.html).\n" <>
	"- Source, issues and releases: [GitHub](" <> github <> ").\n\n" <>
	"## Installation\n" <> rewriteLinks["README.md", install] <> "\n"];

writeFile["_config.yml", StringRiffle[{
	"title: Mathematica packages",
	"description: Wolfram Language packages for symmetric functions and algebraic combinatorics",
	"remote_theme: just-the-docs/just-the-docs@v0.10.0",
	"plugins:", "  - jekyll-remote-theme",
	"search_enabled: true",
	"aux_links:", "  \"GitHub\": \"" <> github <> "\"", "  \"symmetricfunctions.com\": \"" <> symcat <> "\"",
	"footer_content: \"Generated by Scripts/BuildDocs.m from the repository. MIT license.\"",
	""}, "\n"]];

Print["BuildDocs: wrote ", Length[FileNames["*", out, Infinity]], " files to ", out];
