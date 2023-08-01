(* ::Package:: *)

(* MathKernel -script file.m *)

Clear["Tex2WebUtilities`*"];
BeginPackage["Tex2WebUtilities`"];

TeXToUTF8Rule::usage = "List of string replace rules to convert latex syntax to utf8.";

BalancedBracketPattern::usage = "BalancedBracketPattern[left, right] returns a pattern that finds matching brackets. Default pair is {}. Note: this only verifies that the number of { and } in the string are the same.";

TabularTableauToHTMLRule::usage = "TabularTableauToHTMLRule[] returns two replacement rules 
where \begin{array} or \begin{tabular} matrices have been converted to html tables.";

(* This has color support. *)
YoungTableauToHTMLRule::usage = "YoungTableauToHTMLRule[] returns a rule where \begin{youngtab} matrices have been converted to html tables.";

(* This is deprecated? *)
BibliographyHTMLRules::usage = "BibliographyHTMLRules[.bblFile-as-string] returns the bibliographic data in .bbl file as list of rules.";


CreateBibliography::usage = "CreateBibliography[.bibFile-as-string] returns the bibliographic data in .bib file as list of associations.";

Doi2Bib;

MemoizedImport;


(***************** Switch to private context *********************************)
Begin["Private`"];

(* Add more from https://arxiv.org/edit-user/tex-accents.php and capital letters. *)

TeXToUTF8Rule[] := List[
"{\\'{}}" -> "\[CloseCurlyQuote]",
"{\\\"{A}}" -> "Ä",
"{\\\"{a}}" -> "ä",
"{\\'{A}}" -> "Á",
"{\\'{a}}" -> "á",
"{\\.{A}}" -> "Ȧ",
"{\\.{a}}" -> "ȧ",
"{\\={A}}" -> "Ā",
"{\\={a}}" -> "ā",
"{\\^{A}}" -> "Â",
"{\\^{a}}" -> "â",
"{\\`{A}}" -> "À",
"{\\`{a}}" -> "à",
"{\\k{A}}" -> "Ą",
"{\\k{a}}" -> "ą",
"{\\r{A}}" -> "Å",
"{\\r{a}}" -> "å",
"{\\u{A}}" -> "Ă",
"{\\u{a}}" -> "ă",
"{\\v{A}}" -> "Ǎ",
"{\\v{a}}" -> "ǎ",
"{\\~{A}}" -> "Ã",
"{\\~{a}}" -> "ã",
"{\\'{C}}" -> "Ć",
"{\\'{c}}" -> "ć",
"{\\.{C}}" -> "Ċ",
"{\\.{c}}" -> "ċ",
"{\\^{C}}" -> "Ĉ",
"{\\^{c}}" -> "ĉ",
"{\\c{C}}" -> "Ç",
"{\\c{c}}" -> "ç",
"{\\v{C}}" -> "Č",
"{\\v{c}}" -> "č",
"{\\v{D}}" -> "Ď",
"{\\v{d}}" -> "ď",
"{\\\"{E}}" -> "Ë",
"{\\\"{e}}" -> "ë",
"{\\'{E}}" -> "É",
"{\\'{e}}" -> "é",
"{\\.{E}}" -> "Ė",
"{\\.{e}}" -> "ė",
"{\\={E}}" -> "Ē",
"{\\={e}}" -> "ē",
"{\\^{E}}" -> "Ê",
"{\\^{e}}" -> "ê",
"{\\`{E}}" -> "È",
"{\\`{e}}" -> "è",
"{\\k{E}}" -> "Ę",
"{\\k{e}}" -> "ę",
"{\\u{E}}" -> "Ĕ",
"{\\u{e}}" -> "ĕ",
"{\\v{E}}" -> "Ě",
"{\\v{e}}" -> "ě",
"{\\.{G}}" -> "Ġ",
"{\\.{g}}" -> "ġ",
"{\\^{G}}" -> "Ĝ",
"{\\^{g}}" -> "ĝ",
"{\\c{G}}" -> "Ģ",
"{\\c{g}}" -> "ģ",
"{\\u{G}}" -> "Ğ",
"{\\u{g}}" -> "ğ",
"{\\v{G}}" -> "Ǧ",
"{\\v{g}}" -> "ǧ",
"{\\^{H}}" -> "Ĥ",
"{\\^{h}}" -> "ĥ",
"{\\v{H}}" -> "Ȟ",
"{\\v{h}}" -> "ȟ",
"{\\\"{I}}" -> "Ï",
"{\\\"{i}}" -> "ï",
"{\\'{I}}" -> "Í",
"{\\'{i}}" -> "í",
"{\\.{I}}" -> "İ",
"{\\={I}}" -> "Ī",
"{\\={I}}" -> "ī",
"{\\^{I}}" -> "Î",
"{\\^{i}}" -> "î", 
"{\\`{I}}" -> "Ì",
"{\\`{i}}" -> "ì",
"{\\k{I}}" -> "Į",
"{\\k{i}}" -> "į",
"{\\u{I}}" -> "Ĭ",
"{\\u{i}}" -> "ĭ",
"{\\v{I}}" -> "Ǐ",
"{\\v{i}}" -> "ǐ",
"{\\~{I}}" -> "Ĩ",
"{\\~{i}}" -> "ĩ",
"{\\^{J}}" -> "Ĵ",
"{\\^{j}}" -> "ĵ", 
"{\\c{K}}" -> "Ķ",
"{\\c{k}}" -> "ķ",
"{\\v{K}}" -> "Ǩ",
"{\\v{k}}" -> "ǩ",
"{\\'{L}}" -> "Ĺ",
"{\\'{l}}" -> "ĺ",
"{\\c{L}}" -> "Ļ",
"{\\c{l}}" -> "ļ",
"{\\v{L}}" -> "Ľ",
"{\\v{l}}" -> "ľ",
"{\\'{N}}" -> "Ń",
"{\\'{n}}" -> "ń",
"{\\c{N}}" -> "Ņ",
"{\\c{n}}" -> "ņ",
"{\\v{N}}" -> "Ň",
"{\\v{n}}" -> "ň",
"{\\~{N}}" -> "Ñ",
"{\\~{n}}" -> "ñ",
"{\\\"{o}}" -> "ö",
"{\\\"{O}}" -> "Ö",
"{\\'{O}}" -> "Ó",
"{\\'{o}}" -> "ó",
"{\\.{O}}" -> "Ȯ",
"{\\.{o}}" -> "ȯ",
"{\\={O}}" -> "Ō",
"{\\={o}}" -> "ō",
"{\\^{O}}" -> "Ô",
"{\\^{o}}" -> "ô",
"{\\`{O}}" -> "Ò",
"{\\`{o}}" -> "ò",
"{\\H{O}}" -> "Ő",
"{\\H{o}}" -> "ő",
"{\\k{O}}" -> "Ǫ",
"{\\k{o}}" -> "ǫ",
"{\\u{O}}" -> "Ŏ",
"{\\u{o}}" -> "ŏ", 
"{\\v{O}}" -> "Ǒ",
"{\\v{o}}" -> "ǒ",
"{\\~{O}}" -> "Õ",
"{\\~{o}}" -> "õ",
"{\\'{R}}" -> "Ŕ",
"{\\'{r}}" -> "ŕ",
"{\\c{R}}" -> "Ŗ",
"{\\c{r}}" -> "ŗ",
"{\\v{R}}" -> "Ř",
"{\\v{r}}" -> "ř",
"{\\'{S}}" -> "Ś",
"{\\'{s}}" -> "ś",
"{\\^{S}}" -> "Ŝ",  
"{\\^{s}}" -> "ŝ",
"{\\c{S}}" -> "Ş",  
"{\\c{s}}" -> "ş",
"{\\v{S}}" -> "Š",
"{\\v{s}}" -> "š",
"{\\c{T}}" -> "Ţ",
"{\\c{t}}" -> "ţ",
"{\\v{T}}" -> "Ť",
"{\\v{t}}" -> "ť", 
"{\\\"{U}}" -> "Ü",
"{\\\"{u}}" -> "ü",
"{\\'{U}}" -> "Ú",
"{\\'{u}}" -> "ú",
"{\\={U}}" -> "Ū",
"{\\={u}}" -> "ū", 
"{\\^{U}}" -> "Û",
"{\\^{u}}" -> "û", 
"{\\`{U}}" -> "Ù", 
"{\\`{u}}" -> "ù",
"{\\H{U}}" -> "Ű", 
"{\\H{u}}" -> "ű",
"{\\k{U}}" -> "Ų", 
"{\\k{u}}" -> "ų",
"{\\r{U}}" -> "Ů", 
"{\\r{u}}" -> "ů",
"{\\u{U}}" -> "Ŭ", 
"{\\u{u}}" -> "ŭ",
"{\\v{U}}" -> "Ǔ", 
"{\\v{u}}" -> "ǔ",
"{\\~{U}}" -> "Ũ", 
"{\\~{u}}" -> "ũ",
"{\\^{W}}" -> "Ŵ", 
"{\\^{w}}" -> "ŵ", 
"{\\\"{Y}}" -> "Ÿ",
"{\\\"{y}}" -> "ÿ",
"{\\'{Y}}" -> "Ý", 
"{\\'{y}}" -> "ý",
"{\\={Y}}" -> "Ȳ", 
"{\\={y}}" -> "ȳ",
"{\\^{Y}}" -> "Ŷ", 
"{\\^{y}}" -> "ŷ",
"{\\'{Z}}" -> "Ź", 
"{\\'{z}}" -> "ź",
"{\\.{Z}}" -> "Ż",
"{\\.{z}}" -> "ż",
"{\\v{Z}}" -> "Ž",
"{\\v{z}}" -> "ž",
"{\\aa}" -> "å",
"{\\AA}" -> "Å",
"{\\ae}" -> "æ",
"{\\AE}" -> "Æ",
"{\\DH}" -> "Ð",
"{\\dh}" -> "ð",
"{\\dj}" -> "đ",
"{\\DJ}" -> "Đ",
"{\\eth}" -> "ð",
"{\\ETH}" -> "Ð",
"{\\i}" -> "ı",
"{\\l}" -> "ł",
"{\\L}" -> "Ł",
"{\\ng}" -> "ŋ",
"{\\NG}" -> "Ŋ",
"{\\O}" -> "Ø",
"{\\o}" -> "ø",
"{\\oe}" -> "œ",
"{\\OE}" -> "Œ",
"{\\ss}" -> "ß",
"{\\th}" -> "þ",
"{\\TH}" -> "Þ",
"\\textasciitilde "->"~",
"{\\&}" -> "&"
];

UTF8ToTeXRule[] := Reverse /@ TeXToUTF8Rule[];


BalancedBracketPattern[left_: "{", right_: "}"] := Module[{f},
  f[m_String] := StringCount[m, left] - StringCount[m, right];
  (Shortest[mid___ /; f[mid] == 0])
];


(* Split string into characters, unless grouped by {..} *)
StringSplitBraces[str_String] := Module[{pieces},
		pieces = 
		DeleteCases[
		StringSplit[str, 
		s : Shortest["{" ~~ BalancedBracketPattern[] ~~ "}"] :> s], 
		""];
	Join @@ Table[
		If[StringStartsQ[p, "{"], {StringTake[p, {2, -2}]},
		StringSplit[p, ""]]
		, {p, pieces}]
];



(* For formatting a single entry in an array, ytableau, tabular, or similar. *)
Options[DiagramFormatEntry] = {
"Delimiter"->"$",
"Align"->""  (* lcr *)
};
DiagramFormatEntry[entIn_String, opts:OptionsPattern[]] := Module[
	{
		ent = entIn,
		delimiter = OptionValue["Delimiter"],
		align = OptionValue["Align"],
		color,
		classes = "",
		colorStyle = ""
	},
	
	(* Extract color directive *)
	color = StringCases[ent, Shortest["*(" ~~ col__ ~~ ")"] :> col];
	color = If[Length@color == 1,First@color, "" ];
	
	ent = StringReplace[ent, Shortest["*(" ~~ col__ ~~ ")"] :> ""];
	
	
	If[color != "",
		colorStyle = StringJoin[" style=\"background-color: ",color,"\" "];
	];
	
	(* No border class *)
	If[ !StringFreeQ[ent, "\\none"],
		ent = StringReplace[ent,"\\none"->""];
		classes = classes <> " none";
	];
	
	(* If there is entry alignment directive. *)
	If[StringMatchQ[align,"r"|"l"|"c"],
		classes = classes <> (" entryAlign-"<>align)
	];
	
	(* Empty string is replaced with html. *)
	If[StringTrim[ent] == "",
		ent = "&nbsp;";
		delimiter = "";
	];
	
	If[classes != "",
		classes = StringJoin[" class=\"",classes,"\" "];
	];
	
	StringJoin["<td",classes, colorStyle ,">",delimiter,ent,delimiter, "</td>"]
	
];


TabularTableauToHTMLRule[] := Module[
	{texTabularToHTML, out},
	
	(* Table rules must be on separate lines! *)
	 
	texTabularToHTML[s_String,type_:"tabular",spec_:""] := Module[
		{tab, lines, lineEntries, s2, specList = {} },
	
		(* This is empty list by default *)
		specList = Characters[spec]; 
		
		(* TeX does not end rule-lines with \\, 
		so we add these before split. *)
		s2 = StringReplace[s,
			{
				"\\toprule" -> "\\toprule\\\\",
				"\\midrule" -> "\\midrule\\\\",
				"\\bottomrule" -> "\\bottomrule\\\\",
				"\n"->""
				
			}];
		
		lines = StringSplit[s2, "\\\\"];
		
		If[ StringTrim@Last[lines] == "",
			lines = Most[lines]
		];
		
		tab = StringJoin @@ Table[
		
		
			Which[
				StringTrim@line === "\\toprule", 
					"<tr class=\"toprule\"><td colspan=\"100\"></td></tr>\n",
				StringTrim@line === "\\midrule", 
					"<tr class=\"midrule\"><td colspan=\"100\"></td></tr>\n",
				StringTrim@line === "\\bottomrule", 
					"<tr class=\"bottomrule\"><td colspan=\"100\"></td></tr>\n",
				StringTrim@line === "", "",
				
				True,
					(
					lineEntries = StringSplit[line, "&"];
					
					(* Add missing directives. *)
					specList = PadRight[specList, Length@lineEntries,"d"];
					
					StringJoin["<tr>",
					
						StringJoin @@ 
							MapThread[
								DiagramFormatEntry[ StringTrim[#1] , "Delimiter"->If[type == "array","$",""],"Align"->#2]&
								, {lineEntries,specList},1]
						
					, "</tr>\n"]
					)
			]
		, {line, lines}];
			
		
		StringJoin["<table class=\"", type, "\">", tab, "</table>"]
	];
	
	
	{
	Shortest["\\begin{tabular}{"~~spec:BalancedBracketPattern[] ~~"}" ~~ table__ ~~ "\\end{tabular}"] :> 
		texTabularToHTML[table, "tabular",spec],
	Shortest["\\begin{tabular}" ~~ table__ ~~ "\\end{tabular}"] :> 
		texTabularToHTML[table, "tabular"],
	Shortest["\\begin{array}{"~~spec:BalancedBracketPattern[] ~~"}" ~~ table__ ~~ "\\end{array}"] :> 
		texTabularToHTML[table, "array",spec],
	Shortest["\\begin{array}" ~~ table__ ~~ "\\end{array}"] :> 
		texTabularToHTML[table, "array"]
	}
	
];



(* Searches string and convert ytableau syntax to html *)

YoungTableauToHTMLRule[] := Module[{ytableaushortToHTML, youngtabToHTML},


	youngtabToHTML[s_String, del_:"$"] := Module[{lines,entries, maxWidth},
	
		lines = StringSplit[s, "\\\\"];
	
		If[StringTrim@Last[lines]=="",
			lines = Most[lines]
		];
	
		entries = StringSplit[#, "&"] & /@ lines;
		
		(* Pad *)
		maxWidth = Max[Length/@entries];
		entries = PadRight[#, maxWidth, "\\none"]& /@ entries;
		
		
		StringJoin[
			"<table class=\"ytab\">\n",
					
			StringJoin @@ Table[
			StringJoin["<tr>", 
			StringJoin @@ Table[
			DiagramFormatEntry[ StringTrim[e] ,"Delimiter"->del]
			, {e, line}], "</tr>\n"]
			, {line, entries}]
			,
			"</table>\n"
		]
	];
	
	ytableaushortToHTML[s_String, del_:"$"] := Module[{entries,maxWidth},
	
		entries = StringSplitBraces /@ StringSplit[s, ","];
		(* Pad *)
		maxWidth = Max[Length/@entries];
		entries = PadRight[#, maxWidth, "\\none"]& /@ entries;
		
		StringJoin[
			"<table class=\"ytab\">\n",
			
			StringJoin @@ Table[
				StringJoin["<tr>",
					StringJoin @@ Map[ DiagramFormatEntry[#,"Delimiter"->del]& ,line]
				, "</tr>\n"]
			, {line, entries}]
			,
			"</table>\n"]
	];
	
	(* Several rules. *)
	Sequence@@List[
		Shortest["\\" ~~ ("ytableaushort" | "young") ~~ "{" ~~ table:BalancedBracketPattern[] ~~ "}"] :> ytableaushortToHTML[table]
		,
		Shortest["\\" ~~ ("textytableaushort") ~~ "{" ~~ table:BalancedBracketPattern[] ~~ "}"] :> ytableaushortToHTML[table,""]
		,
		Shortest["\\begin{youngtab}"
			~~ table__ ~~
			"\\end{youngtab}"
		] :> youngtabToHTML[table]
		,
		Shortest["\\begin{ytableau}"
			~~ table__ ~~
			"\\end{ytableau}"
		] :> youngtabToHTML[table]
		,
		Shortest["\\begin{youngtabtext}"
			~~ table__ ~~
			"\\end{youngtabtext}"
		] :> youngtabToHTML[table,""]
	]
];


(***************** Bibliography functions below ******************************)


(* Process bibFile text and generate replacement rules. *)
(* TODO - HANDLE MATH? *)
(* IS THIS USED? NO? *)
BibliographyHTMLRules[textBlob_String] := Module[
  {str, citations, citationToDataList, citeDatatoHTML, cleanStuff, 
    out},
  
	str = textBlob;
	str = StringReplace[str, {
	"\\newblock" :> "===",  (* NOTE WE SEPARATE DATA TEMPORARILY USING === *)
	
	"\\end{thebibliography}" :> "",
	"{\\etalchar{+}}" :> "+",
	"{\\&}" -> "&",
	
	(* This replaces all LaTeX special chars with proper unicode. *)
	Sequence@@TeXToUTF8Rule[],
	
	("\\href" ~~ WhitespaceCharacter ..) :> "\\href",
	"\\phantom{x}" :> " ",
	"--" :> "\[Dash]",
	"~":>" ",
	"\\textasciitilde"~~(WhitespaceCharacter..) :> "~",
	"\n" :> ""
	}];
	
  
  (* Format url data, and journal *)
  str = StringReplace[str, {
      Shortest["\\url{" ~~ v__ ~~ "}"] :> "[url]" <> v <> "[/url]",
      Shortest["\\href{" ~~ v__ ~~ "}"] :> 
      "[href]" <> v <> "[/href]",
      Shortest["\\path{" ~~ v__ ~~ "}"] :> 
      "[path]" <> v <> "[/path]",
      Shortest["{\\em " ~~ v:BalancedBracketPattern[] ~~ "}"] :> 
      "<span class=\"journal\">" <> v <> "</span>",
      "{" ~~ v : LetterCharacter ~~ "}" :> v
      }];
  
  (* Split data into individual citations.*)
  
  citations = Rest@StringSplit[str, "\\bibitem"];
  
  citationToDataList[p_String] := Module[{dataList},
    
    (* Extract ref-tag, identifier and remaining data *)
    (* E.g. [AL] Alexandersson2018 Remaing. *)
          
    dataList = StringCases[p,
        Shortest["[" ~~ refTag : BalancedBracketPattern["[","]"] ~~ "]{"] ~~ 
          identifier : (WordCharacter | "." | "-") .. ~~ "}" ~~ 
          rest__ :> {refTag, identifier,
          StringReplace[rest, {"{" :> "", "}" :> ""}]
          }, 1][[1]];
 
    (* Split remaining *)
    dataList = {#1, #2, StringSplit[#3, "==="]} & @@ dataList;
    
    (* Tag, identifier, author(s), Title, rest *)
    {#1, #2, #3[[1]], #3[[2]], #3[[3 ;;]]} & @@ 
      dataList
    ];
  
  (* Given list of items (journal etc. and url, do some formatting *)
  cleanStuff[stuff_List] := Module[{s, out, cases, link = "", nonLink},    
    
   nonLink = Table[
      cases = StringCases[s, {
          Shortest["[href]" ~~ theURL__ ~~ "[/href]"] :> {"href", theURL},
          Shortest["[url]" ~~ theURL__ ~~ "[/url]"] :> {"url", theURL},          
          Shortest["[path]doi:" ~~ theURL__ ~~ "[/path]"] :> {"doi", "http://dx.doi.org/" <> theURL}
          }];
          
      out = If[Length@cases == 0,
        s,
        link = cases[[1, 2]];
        ""
        ];
      out
      , {s, stuff}];
      
    {nonLink, link}
    ];
  
  citeDatatoHTML[tag_, id_, auth_, title_, stuff_List] := 
    Module[{rest, link,html,toLinkRule},
    {rest, link} = cleanStuff[stuff];
   
     
    html = StringJoin[
        "<li class=\"citeLi\" id=\"", id, "\">",
        "<span class=\"citeID\">[", tag, "]</span> ",
        "<span class=\"citeAuthor\">",
        auth,
        "</span> ",
        "<span class=\"citeTitle\">",
        If[link == "", title,
        "<a class=\"citeLinkTitle\" href=\"" <> link <> "\">" <> 
          title <> "</a>"
        ]
        , "</span> ",
        StringJoin @@ rest
        , "</li>"];
        
        toLinkRule = (StringJoin["\\cite{", id, "}"] -> 
        StringJoin["<a class=\"cite\" href=\"#", id, "\">", tag, "</a>"]);
    
      {
        "Identifier" -> id,
        "Tag" -> tag,
        "HTML" -> html,
        "Author"-> auth,
        "Title" -> title,
        "CiteToLinkRule" -> toLinkRule
       }
    ];

  (* Process all pieces. *)
  
  out = (citeDatatoHTML @@ citationToDataList[#]) & /@ citations;
  BibliographyHTMLRules::info = "Gathered `1` citations.";
  Message[BibliographyHTMLRules::info, Length@citations];
  out
];







(***************** NEW Bibliography functions below ******************************)



(* Returns {{fname, lname}...} list *)

ParseAuthor[str_String] := Module[{authList, fnameLnameList},
   authList = StringTrim /@ StringSplit[StringReplace[str, WhitespaceCharacter .. -> " "],
      " and "];
   
   (* Replace spaces in "{van Helen}" so that we get van+Helen *) 
   authList = Map[
     StringReplace[#, 
       "{" ~~ lName__ ~~ "}" :> StringReplace[lName, " " :> "+"]] &
     , authList];
   
   fnameLnameList = Table[
     If[StringContainsQ[auth, ","],
      {StringJoin @@ (Riffle[StringTrim /@ (Rest@#), " "]), 
         First@#} &@StringSplit[auth, ","]
      ,
      {StringJoin @@ (Riffle[StringTrim /@ (Most@#), " "]), Last@#} &@
       StringSplit[auth, " "]
      ]
     , {auth, authList}];
   
   Map[StringReplace[#, "+" :> " "] &, fnameLnameList, {2}]
   ];

(* List of last names, and year --- generates a key. *)

AuthorBibKey[authors_List, year_: ""] := Module[{lastNamesFirstLetters, authKey},

  lastNamesFirstLetters[str_String] := StringJoin @@ (StringTake[#, 1] & /@ StringSplit[str, " "]);
    (* TODO - handle double last names! *)
    authKey = Which[
      Length@authors == 1 && StringLength@authors[[1]] >= 3,
        StringTake[authors[[1]], 3]
      ,
      Length@authors == 1 && StringLength@authors[[1]] <= 2,
        authors[[1]]
      ,
      True,
        StringJoin @@ (lastNamesFirstLetters /@ authors)
      ];
    authKey <> If[StringLength[year] >= 4, StringTake[year, {3, 4}], ""]
];

FixLaTeXCapitalization[Missing] := "";
FixLaTeXCapitalization[""] := "";
FixLaTeXCapitalization[title_String] := Module[{str},
   str = StringReplace[
     ToLowerCase[title]
     , Shortest["{" ~~ w : LetterCharacter .. ~~ "}"] :> ToUpperCase[w]
     ];
   
   (* Replace first letter with original first letter *)
   
   If[StringMatchQ[StringTake[title, 1], LetterCharacter],
    StringReplacePart[str, StringTake[title, {1, 1}], {1, 1}]
    ,
    str
    ]
   ];

RawBibTexEntries[bibData_String] := 
  Module[{fixValue, entries, splitEntry},
   
   entries = StringCases[
     StringReplace[bibData, Join[TeXToUTF8Rule[], {"--"->"–"}]]
     ,
     data : 
       Shortest[
        "@" ~~ type : LetterCharacter .. ~~ "{" ~~ 
         key : WordCharacter .. ~~ "," ~~ 
         body : BalancedBracketPattern[] ~~ "}"] :> {type, key, body}
     ];
   
   fixValue[str_String] := StringTrim@StringReplace[str, WhitespaceCharacter .. :> " "];
   
   splitEntry[more_String] := StringCases[more, {
      Shortest[
        StringExpression[
         key : (WordCharacter | "-" | "_") ..,
         WhitespaceCharacter ...,
         "=" ~~ WhitespaceCharacter ...,
         "{",
         value : BalancedBracketPattern[]
         , "}"
         ]
        ] :> (ToLowerCase[key] -> fixValue@value)
      ,
      "month" ~~ WhitespaceCharacter ... ~~ "=" ~~ 
        WhitespaceCharacter ... ~~
        value : WordCharacter .. :> ("month" -> value)
      }
     , IgnoreCase -> True
     ];
   
   (* TODO: Save raw bibtex entry as well, for debugging purposes? *)
   Association[
      Join[
        {"id" -> #2,
        "type" -> ToLowerCase@#1,
        "author" -> Missing,
        "year" -> ""},
       splitEntry[#3]]] & @@@ entries
   ];

(* Perhaps add url from doi and arxiv-id as well here *)

FixKeysAndAuthors[rawBibtexEntries_List] := 
  Module[{updatedEntries, dupEntries, authorsList, dupKeyEntries, 
    title, key, year, m},
   
   updatedEntries = Table[
     authorsList = ParseAuthor[dd[["author"]]];
     year = dd[["year"]];
     key = AuthorBibKey[Last /@ authorsList, year];
     Join[
      dd,
      Association[
       {"author" -> authorsList,
        "key" -> key
        }]
      ]
     ,
     {dd, rawBibtexEntries}];
   
   (* Now we have alphabetical ordering. *)
   
   updatedEntries = 
    SortBy[updatedEntries, {Last /@ #[["author"]] &, #[["year"]] &, #[["title"]] &}];
   
   (* Return all entries with key k. *)
   
   dupEntries[k_] := dupEntries[k] = Select[updatedEntries, #[["key"]] == k &];
   
   updatedEntries = Table[
     (* Select entries w same key *)
     
     dupKeyEntries = dupEntries@dd[["key"]];
     If[Length@dupKeyEntries > 1,
      m = Position[dupKeyEntries, dd][[1]];
      Join[dd, 
       Association[
        "key" -> (dd[["key"]] <> FromCharacterCode[96 + m])]]
      ,
      dd
      ]
     , {dd, updatedEntries}];
   updatedEntries
   ];


(* Given entry, determines an url. *)

BibTeXURL[entry_Association] := Module[{eprint, doi, url, theURL},
   eprint = Lookup[entry, "eprint", Missing];
   doi = Lookup[entry, "doi", Missing];
   url = Lookup[entry, "url", Missing];
   
   (* Priority for url rule. *)
   Which[
    doi=!=Missing, "http://dx.doi.org/" <> doi,
    eprint=!=Missing, "https://arxiv.org/abs/" <> eprint,
    url=!=Missing, url,
    True, Missing
    ]
];

AuthorsToString[authListIn_List] := Module[{authList},
   authList = (StringJoin @@ Riffle[#, " "]) & /@ authListIn;
   
   If[Length@authList == 1,
    authList[[1]]
    ,
    StringJoin[
     (StringJoin @@ Riffle[Most[authList], ", "])
     , " and ", Last@authList]
    ]
   ];

ToFullMonth[str_String] := Lookup[
   Association[
    {"jan" -> "January",
     "feb" -> "February",
     "mar" -> "March",
     "apr" -> "April",
     "may" -> "May",
     "jun" -> "June",
     "jul" -> "July",
     "aug" -> "August",
     "sep" -> "September",
     "oct" -> "October",
     "nov" -> "November",
     "dec" -> "December"}]
   , str, ""];

BibTeXEntryToHTML[entry_Association] := Module[{MakeSpanEntry,
    citeType, authors, title, year,note,
    journal, publisher, pages, eprint,
    month, volume, number, school, doi,series,
    edition, booktitle, institution, howpublished,
    mainData, citeId, citeKey
    },
   
   MakeSpanEntry[field_String] := With[
     {value = Lookup[entry, field, Missing]},
     MakeSpanEntry[field, value]];
   
   (* If value is missing, just return Missing. *)
   
   MakeSpanEntry[w_String, Missing] := "";
   MakeSpanEntry[field_String, value_String] :=
    StringJoin["<span class=\"cite", Capitalize[field], "\">", 
        StringReplace[value, Shortest["{" ~~ w : LetterCharacter .. ~~ "}"] :> w], (* Remove braces *)
     "</span>"];
   
    MakeSpanEntry["author", value_List] :=
    StringJoin["<span class=\"citeAuthor\">", AuthorsToString[value], 
     "</span>"];

	 (* TODO - Parse better -- *)
	MakeSpanEntry["edition", value_String] :=
    StringJoin["<span class=\"citeEdition\">", value, " edition,</span>"];
	 
   MakeSpanEntry["number", value_String] :=
    StringJoin["<span class=\"citeNumber\">(", value, ")</span>"];
   
   MakeSpanEntry["pages", value_String] :=
    StringJoin["<span class=\"citePages\">:", value, "</span>"];
   
   MakeSpanEntry["key", value_String] :=
    StringJoin["<span class=\"citeKey\">[", value, "]</span>"];
    
    MakeSpanEntry["series", value_String] :=
    StringJoin["<span class=\"citeSeries\">", value, ", </span>"];
    
    MakeSpanEntry["year", value_String] :=
    StringJoin["<span class=\"citeYear\">",value , ". </span>"];
   
	 
	MakeSpanEntry["note", value_String] :=StringJoin["<span class=\"citeNote\">", value, "</span>"];
   
   MakeSpanEntry["title", value_String] := With[{url = BibTeXURL[entry],capTitle = FixLaTeXCapitalization@value},
     If[url=!=Missing,
      StringJoin["<a class=\"citeLinkTitle\" href=\"", url, "\">",capTitle, "</a>"]
      ,
      StringJoin["<span class=\"citeTitle\">",capTitle, "</span>"]
      ]
     ];
   
   
   citeType = entry[["type"]];
   citeId = entry[["id"]];
   citeKey = MakeSpanEntry@"key";
   authors = MakeSpanEntry@"author";
   note = MakeSpanEntry@"note";
   title = MakeSpanEntry@"title";
   year = MakeSpanEntry@"year";
   month = ToFullMonth@Lookup[entry, "month", ""];
   volume = MakeSpanEntry@"volume";
   number = MakeSpanEntry@"number";
   journal = MakeSpanEntry@"journal";
   publisher = MakeSpanEntry@"publisher";
   pages = MakeSpanEntry@"pages";
   school = MakeSpanEntry@"school";
   series = MakeSpanEntry@"series";
   edition = MakeSpanEntry@"edition";
   booktitle = MakeSpanEntry@"booktitle";
   institution = MakeSpanEntry@"institution";
   howpublished = MakeSpanEntry@"howpublished";
   
   
   mainData = Which[
     citeType === "article",
     StringJoin[
      authors, ". ", title, ". ", journal, ", ", volume, number, 
      pages, ", ", month, " ", year, note],
     
     citeType === "book",
     StringJoin[
      authors, ". ", title, ". ", series , publisher, ", ", edition, month, " ", year, note],
     
     citeType === "incollection",
     StringJoin[
      authors, ". ", title, ". In ", booktitle, ". ", publisher, ", ",
       month, " ", year, note],
     
     citeType === "inproceedings",
     StringJoin[
      authors, ". ", title, ". ", booktitle, ". ", publisher, ", ", 
      volume, number, " ", month, " ", year, note],
     
     citeType === "mastersthesis" || citeType === "phdthesis",
     StringJoin[
      authors, ". ", title, ". ", school, ". ", month, " ", year, note],
     
     citeType === "techreport",
     StringJoin[
      authors, ". ", title, ". ", institution, ". ", volume, number," ", 
      month, " ", year, note],
     
     citeType === "misc" || True ,
     StringJoin[
      authors, ". ", title, ". ", howpublished, " ", year, note]
     ];
     mainData = StringReplace[mainData,{ ", ,"->", ", ". ,"->". " }];
   
   StringJoin[
    "<li class=\"citeLI\" id=\"",citeId,"\">",   citeKey, " ", mainData,
    "</li>\n"]
   
   ];

CreateBibliography[str_String] := Module[{citations},
	
	citations = FixKeysAndAuthors@RawBibTexEntries[str];
	
	CreateBibliography::info = "Gathered `1` citations.";
	 Message[CreateBibliography::info, Length@citations];
	
	Join[#,
	Association[
	"html" -> BibTeXEntryToHTML[#],
	"CiteToLinkRule" -> Rule[
		StringJoin["\\cite{", #[["id"]], "}"]
		,
		StringJoin["<a class=\"cite\" href=\"#", #[["id"]], 
		"\">", #[["key"]], "</a>"]
		]
	]] & /@ citations
];












(***************** Doi2BIB  ******************************)


Doi2Bib::usage = "Doi2Bib[url] or Doi2Bib[doi] returns the bib entry for the doi.";

Doi2Bib[doi_String] := Doi2Bib["https://doi.org/" <> doi] /; Not[StringStartsQ[doi, "http"]];
Doi2Bib[url_String] := Module[{request},
	request = HTTPRequest[URL[url],
		<|"Method" -> "GET",
		"Headers" -> {"Accept" -> "application/x-bibtex"},
		"ContentType" -> "application/x-bibtex"|>];
		
	URLRead[request, "Body"]
];


(***************** MemoizedImport  ******************************)

(* Util function for memoization of files. *)
(* Use func if the processed file should be stored.
Set FunctionName, as unique identifier for function, if several different functions 
are used on the same file name. This is to make memoization work correctly.
*)

MemoizedImport[file_String, type_, funcName_: None, 
   "MEMOIZED"] := {None, None};

Options[MemoizedImport] = {
	"Type" -> "Text", 
	"Function" -> (# &),
	"FunctionName" -> None,
	"Forced" -> False};
MemoizedImport::fileUpdated = "File `1` modified. Reprocessing.";
MemoizedImport[file_String, OptionsPattern[]] := Module[{fileDate,
    date, contents, type, fName, forced, func},
   
   {type, func, fName, forced} = OptionValue[
     MemoizedImport, {"Type", "Function", "FunctionName", "Forced"}];
   
   fileDate = FileDate[file];
   
   {date, contents} = MemoizedImport[file, type, fName, "MEMOIZED"];
   If[date === None || fileDate =!= date || forced,
    
    If[contents =!= None,
     Message[MemoizedImport::fileUpdated, file];
     ];
    
    contents = func@Import[file, type];
    MemoizedImport[file, type, fName, "MEMOIZED"] = {fileDate, 
      contents};
    
    ];
   MemoizedImport[file, type, fName, "MEMOIZED"][[2]]
];


End[(* End private *)];
EndPackage[];
