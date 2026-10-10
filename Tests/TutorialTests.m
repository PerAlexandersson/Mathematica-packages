(* Checks TUTORIAL.md: every ```wolfram code block is evaluated in order, statement by
   statement, in this kernel. A statement whose last line ends with (* => expected *) must
   evaluate to the expected value (SameQ, or a difference that simplifies to 0), and no
   statement may emit messages. Writing (* => ? *) prints the actual value instead (used
   when editing the tutorial; it fails the test). *)

testRoot = DirectoryName[DirectoryName[$InputFileName]];

(* SyntaxQ creates the symbols it parses; parse in a scratch context so that Global` stays
   empty and the tutorial's Needs calls do not report shadowing. *)
scratchSyntaxQ[code_String] := Block[{$Context = "TutorialScratch`", $ContextPath = {"System`"}},
	SyntaxQ[code]];

tutorialStatements[file_String] := Module[{text, blocks, statements = {}, buffer, expected},
	text = Import[file, "Text"];
	blocks = StringCases[text, "```wolfram\n" ~~ Shortest[b__] ~~ "\n```" :> b];
	Do[
		buffer = "";
		Do[
			buffer = If[buffer === "", line, buffer <> "\n" <> line];
			If[StringTrim[buffer] =!= "" && scratchSyntaxQ[StringReplace[buffer,
					"(*" ~~ Shortest[___] ~~ "*)" -> ""]],
				expected = StringCases[line, "(* => " ~~ e : Shortest[__] ~~ " *)" ~~ EndOfString :> e];
				AppendTo[statements, {buffer, If[expected === {}, None, First[expected]]}];
				buffer = ""],
			{line, StringSplit[block, "\n", All]}];
		If[StringTrim[buffer] =!= "", AppendTo[statements, {buffer, "UNFINISHED STATEMENT"}]],
		{block, blocks}];
	Quiet[Remove["TutorialScratch`*"]];
	statements];

sameResultQ[value_, expected_] := value === expected ||
	TrueQ[Quiet[Simplify[Together[value - expected]] === 0]];

runTutorial[file_String] := Module[{failures = {}, value, ok},
	PacletDirectoryLoad[testRoot];
	Do[
		(* The tutorial's placeholder path stands for this checkout. *)
		value = Block[{$MessageList = {}, r},
			r = ToExpression[StringReplace[st[[1]],
				"/path/to/Mathematica-packages" -> StringTrim[testRoot, "/" ~~ EndOfString]]];
			If[$MessageList === {}, r, Print["TUTORIAL MESSAGES in ", st[[1]], ": ", $MessageList]; $Failed["messages", 0]]];
		ok = Which[
			MatchQ[value, $Failed["messages", _]], False,
			st[[2]] === None, True,
			st[[2]] === "?", Print["TUTORIAL ? ", st[[1]], "\n   => ", InputForm[value]]; False,
			st[[2]] === "UNFINISHED STATEMENT", False,
			True, sameResultQ[value, ToExpression[st[[2]]]]];
		If[!ok, AppendTo[failures, {st[[1]], st[[2]], InputForm[value]}]],
		{st, tutorialStatements[file]}];
	Scan[Print["TUTORIAL MISMATCH: ", #[[1]], "\n   expected ", #[[2]], "\n   got      ", #[[3]]] &,
		failures];
	failures];

(* Run at top level: VerificationTest intercepts messages itself. *)
tutorialFailures = runTutorial[FileNameJoin[{testRoot, "TUTORIAL.md"}]];

VerificationTest[
	Length[tutorialFailures],
	0,
	TestID -> "Tutorial-code-blocks-evaluate-as-stated"
]
