(* Mathematica Package *)

If[ $VersionNumber < 7,
	Print["FormLink needs Mathematica 7 or newer. Exiting the Kernel now."];
	Quit[]
]

Get@FileNameJoin[{DirectoryName[FindFile[$Input]], "Config.m"}];

(* Created by the Wolfram Workbench 20.08.2012 *)
BeginPackage["FormLink`"];

$DotPowerFix::usage =
"$DotPowerFix is  by default set to {a_ . b_^c_ :> (a.b)^c, (a_^b_).c_ :> (a.c)^b } \
and used in Form2M.";

$FormLinkVersion::usage =
"$FormLinkVersion gives the version number of FormLink.";

$FormLinkDir::usage =
"$FormLinkDir is the location of the FormLink installation."

FormStart::usage =
"FormStart[] or FormStart[formexelocation] starts Form in pipe mode."

FormStop::usage =
"FormStop[] uninstalls the link to FORM. FormStop[All] kills all FormLink processes.";

$FormLink::usage =
"$FormLink denotes the LinkObject of the FormLink MathLink executable."

FormInit::usage =
"FormInit[formfullpath] starts the MathLink function FormInit. This function is used inside FormStart.";

$FormSetup::usage =
"$FormSetup is a list of FORM settings, like TempDir, which dynamically produces a form.set file \
next to the binary which is then called automatically upon starting FORM through FormStart.";

FormSetup::usage =
"FormSetup is an option for FormStart, FormLink and RunForm. It is set by default to $FormSetup. \
$FormSetup can be modified to change TempDir, etc. .Currently a form.set file is written next to \
the FORM executable. ";

FormWrite::usage =
"FormWrite[str] sends a string to FORM."

RunForm::usage = "RunForm[script] runs script in FORM and writes the result to \"runform.frm\" and the log file \
to \"form.log\" in the directory Directory[]. RunForm[script, formfile] uses formfile instead of runform.frm. \
The first argument script can be a string or a list of strings. An optional third argument can be give to use a \
specific FORM executable, otherwise a FORM executable from $FormLinkDir/bin is used.";

ShowScript::usage =
"ShowScript[script] displays script in an Output Cell format which can be copied easily.";

FLReadString::usage =
"FLReadString[str] imports str as \"Text\" and translates it to Mathematica syntax by using $Form2M."

FormLink::formlinknotfound =
"The FormLink executable was not found here: `1`"

FormLink::loadfail =
"Failed to load the FormLink executable `1`.";

Form2M::usage =
"Form2M[string, replist] translates string by ToExpression[ StringReplace[string, replist], \
TraditionalForm]  to Mathematica."

FormLink::usage = "FormLink[\" form statements \"] runs the form statements in Form and \
returns the result to Mathematica. If only one Local assignment is present the return \
value is that result. If more Form Local variables are present, a list of results is returned.";

$FormOutputCellStyle::usage =
"$FormOutputCellStyle defines the Cell style used by RunForm. Default is \"Program\"."

$M2Form::usage =
"$M2Form is a list ob basic Mathematica -> Form function translations."

$Form2M::usage =
"$Form2M is a list ob basic Form -> Mathematica syntax changes."

$TForm::usage =
"$TForm can be set to an integer int, then tform -wint will be used instead of form."

Assign::usage =
"Assign is an option to FormLink. If set to True the Form Local variables are assigned in \
Mathematica, too."

ClearTemp::usage =
"ClearTemp[] clears the temporary directory as specified in $FormSet from xform* files. \
ClearTemp[dir] clears all temp files in dir."

Begin["`Private`"]
(* Implementation of the package *) (* ::Package:: *)

$FormLinkVersion = "1.0";

stringTrim2 =
If[ $VersionNumber >= 9,
	StringTrim,
	Function[s, StringTrim[
		StringReplace[StringTrim[s], {"\[IndentingNewLine]"~~x___ :> x, y___~~ "\[IndentingNewLine]" :> y}]]]
];

$FormLinkDir = DirectoryName[FindFile[$Input]];

If[ Global`FormLinkMessages === True,
	Print["FormLink " <> $FormLinkVersion <>" by Feng Feng and Rolf Mertig"]
];

getSystem[] :=
	getSystem[] =
		Switch[$SystemID,
			"Windows-x86-64",
				"windows",
			"Windows",
				"windows",
			"Linux-x86-64",
				"linux64",
			"Linux",
				"linux32",
			"MacOSX-x86-64",
				"macosx64",
			"MacOSX-x86",
				"macosx32"];

makeFormExe[formexein_String/; StringLength[formexein]>0] :=
	If[	FileExistsQ[formexein],
		formexein,
		Message[FormLink::formnotfound, formexein]
	];

makeFormExe[""] :=
	makeFormExe[];

makeFormExe[] :=
	FileNameJoin[{$FormLinkDir, "bin", getSystem[],
		If[	$TForm === False,
			"form",
			"tform"
		] <>
		If[ getSystem[]==="windows",
			".exe",
			""
		]}];

$FormSetup  =
	{ "TempDir" :> $TemporaryDirectory(* , "IncDir" -> "." *)
	(* IncDir does not work on Windows, gives a FORM error message     Setups: PATHVALUE not yet implemented *)};

ClearTemp[dir_String?DirectoryQ] :=
	Quiet[DeleteFile /@ Select[FileNames["xform*", dir], FileByteCount[#] === 0 &]];

ClearTemp[] :=
	DeleteFile /@ Select[FileNames["xform*", "TempDir" /. $FormSetup], FileByteCount[#] === 0 &];

Options[FormStart] = {
	FormSetup :> $FormSetup,
	Print -> False,
	Style -> {Darker@Darker@Orange, FontFamily -> "Courier" }
};

(*provide the ability to give a specific form executable (useful for testing, tform, parallelization via Mathematica ) *)
(* if no specific form executable is given (default), get the OS-dependent correct one *)

FormStart[formexe_String:"", OptionsPattern[]] :=
	Module[ {flinkfile, finit, print, fofi, formsetup,nb},
		ClearTemp[ "TempDir" /. OptionValue[FormSetup]];
		If[ $FrontEnd =!= Null,
			nb = EvaluationNotebook[]
		];

		print = Function[p,
			If[ p === False,
				Hold,
				p /. True -> Print
			]
		]@OptionValue[Print];

		(*Head[$FormLink]=!=LinkObject,*)
		(* this is not enough, since after FormStop[] the Head of $FormLink will still be LinkObject *)
		If[!ListQ[Quiet[LinkPatterns[$FormLink]]],

			If[$VersionNumber >= 10.3,

				If[getSystem[]==="macosx64",
					If[$VersionNumber >= 10.4,
						flinkfile = FileNameJoin[{$FormLinkDir, "bin", getSystem[], "FormLink"}],
						flinkfile = FileNameJoin[{$FormLinkDir, "bin", getSystem[], "FormLink103"}]

					],
					flinkfile = FileNameJoin[{$FormLinkDir, "bin", getSystem[], "FormLink"}]
				],

				flinkfile = FileNameJoin[{$FormLinkDir, "bin", getSystem[], "FormLinkLegacy"}]
			];


			flinkfile = flinkfile <> If[ getSystem[]==="windows",
										".exe",
										""
									];
			If[ FileExistsQ[flinkfile],

				(* careful: SetDirectory has to be also put before calling Install (on Linux ).
				doing SetDirectory only before FormInit does not work. *)
				SetDirectory[FileNameJoin[{$FormLinkDir, "bin", getSystem[]}]];
				formsetup = makeFormsetup[OptionValue[FormSetup]];
				(* if formsetup is different from what is in form.set, export it: *)
				Export["form.set", formsetup, "Text"] /; Import["form.set","Text"] =!= formsetup;

				(* 	If we don't get an answer after 5 secs, then most likely it is because the MathLink
					executable is not suitable ans Install got frozen.*)
				TimeConstrained[
					$FormLink = Install[ FileBaseName[FileNameTake[flinkfile]]<>
						If[	getSystem[]==="windows",
							".exe",
							""
						]
					],
				5];
				If[Head[$FormLink]=!=LinkObject,
					Message[FormLink::loadfail,flinkfile];
					Abort[]
				];
				ResetDirectory[],
				Message[FormLink::formlinknotfound, flinkfile]
			];
		];
		If[ IntegerQ[$TForm],
			finit = FormInit[ fofi = makeFormExe @ formexe, " -w " <> ToString[$TForm]];,
			finit = FormInit[ fofi = makeFormExe @ formexe]
		];
		If[ $Global`$FLDebug,
			If[ IntegerQ[finit],
				print["Form started in pipe modus by ", fofi]
			]
		];
		finit
	];

	(* kill old FormLink processes here *)
FormStop[] :=
	Uninstall[$FormLink];

FormStop[All] :=
	Quiet[  (* error messages here are not really relevant *)
		ClearTemp[];
		If[ Head[$FormLink]===LinkObject,
			Uninstall[$FormLink]
		];
		SetDirectory[FileNameJoin[{$FormLinkDir, "bin", getSystem[]}]];
		If[ # =!= {},
			DeleteFile/@#
		]&@FileNames["xform*.str" ];
		ResetDirectory[];
		Switch [
			$OperatingSystem,
			"Unix",
			Run["pkill -9 FormLink"],
			"Windows",
			Run["taskkill /im FormLink.exe /f"],
			"MacOSX",
			Run["killall FormLink"]
		]];

FormWrite["\[IndentingNewLine]"] :=
	FormWrite[""];

(* need to get rid of \[IndentingNewLine]'s ... *)
FormWrite[s_String /; StringMatchQ[s, __~~"\[IndentingNewLine]"~~__] ] :=
	FormWrite[StringReplace[s, "\[IndentingNewLine]" -> "\n"]];

FormWrite[s_List] :=
	Scan[FormWrite,s];


(* ::Section:: *)
(*Using FORM through files*)

Options[RunForm] = {
	Style -> {Darker@Darker@N[Orange], FontFamily -> "Courier" },
	FormSetup :> $FormSetup,
	Print -> True
};

RunForm[script_String, opts:OptionsPattern[]] :=
	RunForm[{script}, "runform.frm", opts];

RunForm[script_String, more__] :=
	RunForm[{script}, more];

RunForm[script_List/;MatchQ[script,{__String}], fn_String, formexe_String:"", OptionsPattern[]] :=
	Module[ {fs, logfile, formsetup, scriptj},
		fs = OpenWrite[fn];
		scriptj = StringJoin @@ script;
		WriteString[fs,#]&@
		StringReplace[
		StringJoin[stringTrim2 /@ StringSplit[ scriptj,";" -> ";\[Alpha]"]], "\[Alpha]" -> "\n"];
		If[ !StringMatchQ[stringTrim2[scriptj],"*.end*"],
			WriteString[fs, "\n.end"]
		];
		Close[fs];
		(* copied from FormLink *)
		SetDirectory[FileNameJoin[{$FormLinkDir, "bin", getSystem[]}]];
		formsetup = makeFormsetup[OptionValue[FormSetup]];
		(* if formsetup is different from what is in form.set, export it: *)
		Quiet[Export["form.set", formsetup, "Text"] /; Import["form.set","Text"] =!= formsetup];
		ResetDirectory[];
		Import["!" <> makeFormExe[formexe] <>" " <> fn <> " > form.log", "Text"];
		logfile = StringTrim@Import["form.log","Text"];
		If[ OptionValue[Print] =!= False,
			If[ $FrontEnd === Null,
				Print @ logfile,
				If[ OptionValue[Style] === None,
					CellPrint@Cell[TextData[logfile], $FormOutputCellStyle],
					CellPrint@Cell[TextData[logfile], $FormOutputCellStyle, OptionValue[Style]]
				]
			]
		];
		logfile
	];


(* RM: more general to use $Form2M here *)
(* RM20121113: this is better for larger strings, but be careful:
the first six empty characters NEED to be replaced, otherwise long integers which are
linebroken get converted wrong

RM 20121213:
UNLESS one would use the FORM option  in form.set:

on nospacesinnumbers;

which is not done by default (should it?)

*)
FLReadString[fn_String] :=
	StringReplace[Import[fn,"Text"], Prepend[$Form2M, "\n      "->""]];

(* comment RM: linebreak-conversion-trouble fixed by Feng in FromRead.c, however,
for RunForm we stil need this fix in toplevel Mathematica: *)
fixstr = Function[s,
	StringReplace[StringJoin[StringTrim[s]], $Form2M ]];

semik = Function[x,
	Replace[stringTrim2[x], s_String /; StringLength[s] > 0 :>
		If[	StringTake[s, -1] =!= ";",
			s <> ";",
			s
		]
	]
];

Form2M[$Failed,_] = $Failed;

(* needed for replacing [ ] in the right way; see also in Config.m of FormLink  the entries of
	FormLink`$Form2M
*)
holdidentityrep = {
	(h_Symbol[Identity][x__]) :> (ToExpression[StringReplace[ToString[h], "Hold" -> ""]][
	x]) /; (StringLength[StringReplace[ToString[h], "Hold" -> ""]] > 0),
	Hold[Identity][x_] :> x(*, Hold[Identity][x_,y__] :> Hold[x,y]*)
};

Form2M[frm_String, _:$Form2M] :=
	(Replace[ToExpression[ "("<>fixstr[frm]<>")",
		TraditionalForm],$Failed -> frm]) /. holdidentityrep /. $DotPowerFix;


(* generate the form.set file contents *)
makeFormsetup[formset_List] :=
	Module[ {sfix},
		If[ $OperatingSystem === "Windows",  (* need to do this since Form does not recognize DOS-style path names *)
			sfix = Function[fn,StringReplace[fn, {Shortest[s_]~~":" :> ("/cygdrive/"~~s), "\\"->"/", " " -> "\\ "}]],
			sfix = Identity
		];
		StringReplace[ExportString[StringJoin[Riffle[sfix/@#," "]]& /@ (List @@@ formset),"Lines"], "\r\n"->"\n"]
	];

(* e.g.: Replace -> { "XX" -> "y"} *)
Options[FormLink] = {
	Assign -> False,
	Form2M -> Form2M,
	FormSetup :> $FormSetup,
	Replace :> $Form2M,
	Print -> True,
	Style -> {Darker@Darker@N[Orange], FontFamily -> "Courier"}
};

toString2[s_,m___] :=
	ToString[s,m, PageWidth -> $FormPageWidth];

FormLink[expr_ /; !MatchQ[expr, _String | {__String}], opts:OptionsPattern[]] :=
	FormLink[ {
		"Symbols " <> Apply[StringJoin, Riffle[toString2/@Variables[expr],","]],
		"Local FormLinkExpr = " <> StringReplace[toString2[expr, InputForm], Join[$M2Form, { "["->"(","]" -> ")"} ]],
		".sort"}, opts
	];

FormLink[formstatements_String, opts:OptionsPattern[]] :=
	If[ !StringFreeQ[formstatements, "\r\n"],
		FormLink[StringReplace[formstatements, "\r\n"->"\n"], opts],
		FormLink[(StringSplit[formstatements,";"]) /. "":> Sequence[], opts]
	];


FormLink[fsli:{__String}, OptionsPattern[]] :=
	Module[ {fs, res, locvars, locals, formstatement, loc, timestart, print, globloc, totaltimestart,
		assignfun, f2m, cprint},
		totaltimestart = AbsoluteTime[];
		print = Function[p, If[ p === False,
								Hold,
								p /. True -> Print
							]]@OptionValue[Print];
		cprint = Function[p, If[ p === False,
								Hold,
								p /. True -> CellPrint
							]]@OptionValue[Print];
		(* start Form if it has not been done yet *)
		If[ !ListQ[Quiet[LinkPatterns[$FormLink]]],
			FormStart[ FormSetup -> OptionValue[FormSetup], Print -> OptionValue[Print] ]
		];
		fs = semik /@ DeleteCases[stringTrim2 /@ fsli, "" | ".end" | ".end;"];
		(* extract all Local = statements *)
		locals = Select[fs, StringMatchQ[#, "L*=*"]&];
		(* get all Local variables *)
		locvars = StringReplace[#, "L"~~Shortest[___] ~~ " " ~~ Shortest[var__] ~~ "=" ~~__ :> stringTrim2[var]]& /@ locals;
		(* print the Form statements *)
		assignfun = If[ OptionValue[Assign] =!= True,
						(#2)&,
						Function[{lc, rhs}, Clear @@ {globloc = "Global`" <> lc };
											Set @@ {Symbol@globloc, rhs};
											rhs]
					];
		timestart = AbsoluteTime[];
		(* RM 20121120: this *should* work, but it does not, on Windows:
		formsetup = ("#:"<> StringJoin[Riffle[sfix/@#," "]])& /@ (List @@@ opformset);
		fs = Join[formsetup, fs];
		*)
		(* since this is exactly the input modulo .end there is no need except for debugging to print it again *)
		If[ Global`$FLDebug,
			If[ Head[$FrontEnd] === System`FrontEndObject,
			(* CellPrint here enables easy copy and paste in the FrontEnd ... *)
				cprint@Cell[TextData[ExportString[fs, "Text"]], "Output", OptionValue[Style]],
				print@ExportString[fs, "Text"]
			];
		];

		(* send all Form statements to Form *)
		If[ Global`$FLDebug,
			Global`FS = fs
		];
		If[ Global`$FLDebug,
			print["starting FormWrite loop"]
		];
		Do[  If[ Global`$FLDebug,
				print["piping to FormWrite ", formstatement]
			];
			FormWrite[formstatement], { formstatement, fs }];
		res =
		Table[
				FormWrite[{".sort",
							"#call put(\"(%E)\", " <> loc <> ")",
							".sort" ,
							"#fromexternal" } ];
				If[ Global`$FLDebug,
					print["before FormPrompt"]
				];
				FormLink`FormPrompt[];
				If[ Global`$FLDebug,
					print["before FormRead"]
				];
				FormLink`FormRead[],
		{loc, locvars }
		];
		print["FORM and FormRead finished, time needed before translating to Mathematica: ",
			Round[10000 ( AbsoluteTime[] - timestart )]/10000., " sec"];
		(* clean up *)
		SetDirectory[FileNameJoin[{$FormLinkDir, "bin", getSystem[]}]];
		If[ # =!= {},
			DeleteFile/@#
		]&@FileNames["xform*.str" ];
		ResetDirectory[];
		(* uninstall the link *)
		Uninstall[$FormLink];
		f2m = OptionValue[Form2M];
		If[ f2m =!= False,
			If[ Global`$FLDebug,
				print["start to translate the string back to Mathematica using the function ", f2m];
			];
			If[ f2m === Form2M,
				res = f2m[#, OptionValue[Replace] ]& /@ res,
				res = f2m /@ res
			]
		];
		print["Translation done. Total wall clock time needed: ", Round[ 10000 ( AbsoluteTime[] - totaltimestart ) ] / 10000. ," sec"];
		MapThread[assignfun, {locvars, res}] /. {one_} :> one
	];


Options[ShowScript] = {
	Style -> {Darker@Darker@Orange, FontFamily -> "Courier" }
};

ShowScript[script_List, OptionsPattern[]] :=
	If[ $FrontEnd =!= Null,
		CellPrint@Cell[TextData[ExportString[script, "Text"]], "Output", OptionValue[Style]]
	];

(* via FeynCalc $FrontEnd should not be changed, but it might be. So, set it back to the default here *)
(*If[ !MemberQ[$Packages, "FeynCalc`"],
	If[ $FrontEnd =!= Null,
		SetOptions[#, "CommonDefaultFormatTypes" ->
		{"Input" -> StandardForm, "InputInline" -> StandardForm, "Output" -> StandardForm,
		"OutputInline" -> StandardForm,
		"Text" -> TextForm, "TextInline" -> TraditionalForm}
		]& /@ {$FrontEnd, $FrontEndSession}
	]
];*)

End[]

EndPackage[]
