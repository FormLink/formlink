(* This is a Mathematica only installer for FormLink / FeynCalcFormLink, and FeynCalc if needed *)

If[ $VersionNumber < 7,
	Print["FormLink does need at least Mathematica 7. Exiting now. "];
	Quit[]
];

(* check for a setting in the Mathematica preferences *)
If[ ("AllowInternetUse" /. SystemInformation["Network"]) === False,
	Print["You have configured Mathematica not to access the internet. Too bad.
	Please check the \"Allow Mathematica to use the Internet\" box in the
	Help \[FilledRightTriangle] Internet Connectivity dialog. Exiting now." ];
	Quit[]
];

$FormLinkZipFile = "http://www.feyncalc.org/formlink/formlink.zip";
(*$FormLinkZipFile = "http://192.168.1.17/formlink.zip";*)

If[ !ValueQ[$installdirectory],
	$installdirectory = FileNameJoin[{$UserBaseDirectory,"Applications"}]
];



(* Executing

Import["http://www.feyncalc.org/formlink/FormLinkInstall.m"]
	will automatically download the latest version of FormLink and
	unzip formlink.zip to
	FileNameJoin[{$UserBaseDirectory, "Applications"}]

(  $installdirectory = "mydir";
Import["http://www.feyncalc.org/formlink/FormLinkInstall.m"]
)
	installs FormLink to "mydir".

*)

BeginPackage["Unzip`",{"JLink`"}]
(* Exported symbols added here with SymbolName::usage *)


CopyRemote::usage = "CopyRemote[url, localfilename] copies a file from
an http location to localfilename."

Unzip::usage = "Unzip[file] unzips file."

Verbose::usage = "Verbose is an option to Unzip."

URLFileByteSize::usage = "gives the remote file size in Byte."

Begin["`Private`"]

InstallJava[];


Options[Unzip]  =  {Verbose -> True};

Unzip[zipfilein_String?FileExistsQ, dir_: Directory[], OptionsPattern[]] :=
	JavaBlock[
	Module[ {enum, saveEntry, startdir, zf, buf, zipfile, comments, targets, target, len, dirs},
		zipfile = If[ DirectoryName[zipfilein] === "",
					FileNameJoin[{Directory[],zipfilein}],
					zipfilein
				];
		buf = JavaNew["[B", 8192]; (* ] *)
		If[ startdir =!= dir,
			If[ !DirectoryQ[dir],
				mkdirs[dir]
			];
			SetDirectory[dir]
		];
		saveEntry[zipfi_, zipentry_] :=
			JavaBlock[
			Block[ {bos, fi, fos, numRead, stream, outStream, fromcharcode, topdirle},
				fi = zipentry[getName[]];
				If[ zipentry[isDirectory[]],
					mkdirs[FileNameJoin[{dir, fi}]],
					stream = JavaNew["java.io.BufferedInputStream",
					zipfi[getInputStream[zipentry]]];
					outStream =
					JavaNew["java.io.BufferedOutputStream",
					JavaNew["java.io.FileOutputStream", FileNameJoin[{dir, fi}]]];
					While[(numRead = stream[read[buf]]) > 0, outStream[write[buf, 0, numRead]]];
					stream[close[]];
					outStream[close[]];
				]
			]];
		zf = JavaNew["java.util.zip.ZipFile", zipfile];
		len = zf[size[]];
		enum = zf[entries[]];
		comments = OptionValue[Verbose] /. Options[Unzip];
		targets = Table[enum[nextElement[]],{len}];
		dirs = Function[x, If[ !DirectoryQ[x],
							CreateDirectory[x, CreateIntermediateDirectories -> True]
						]] /@ (Union[DirectoryName[#[getName[]]]& /@ targets]/."":>Sequence[]);
		Do[
			If[ comments,
				Print[StringJoin["extracting: ", FileNameJoin[{dir, StringReplace[target[getName[]], "/" -> $PathnameSeparator]}]]]
			];
			saveEntry[zf, target],
		{target, targets}
		];
		zf @ close[];
		dir
	]];



(*
Example usage:
CopyRemote["http://www.mertig.com/mathdepot/buttons/ButtonTools.nb",
ToFileName[{$UserAddOnsDirectory,"SystemFiles","FrontEnd","Palettes"},
"ButtonTools.nb"]]
*)

(* You need JLink 2.0 or higher.
this code is based on the GetRemote example in the JLink
documentation *)

Options[CopyRemote] = {ProxyHost :> None, ProxyPort :> None};

CopyRemote[url_String /; StringMatchQ[url, "http://*.*", IgnoreCase-> True],
localfile_:Automatic, opts___?OptionQ] :=
	(
	Needs["JLink`"];
	JLink`JavaBlock[
		Module[ {u, stream, numRead, outFile, buf, prxyHost, prxyPort},
			{prxyHost, prxyPort} = {ProxyHost, ProxyPort} /.
			Flatten[{opts}] /. Options[CopyRemote];
			JLink`InstallJava[];
			If[ StringQ[prxyHost],
				(* Set properties to force use of proxy. *)
				JLink`SetInternetProxy[prxyHost, prxyPort]
			];
			u = JLink`JavaNew["java.net.URL", url];
			(* This is where the error will show up if the URL is not valid.
			A Java exception will be thrown during openStream, which
			causes the method to return $Failed.
			*)
			stream = u@openStream[];
			If[ stream === $Failed,
				Return[$Failed]
			];
			buf = JLink`JavaNew["[B", 8192];
			If[ StringQ[localfile],
				outFile = OpenWrite[localfile, DOSTextFormat -> False],
				outFile = OpenTemporary[DOSTextFormat->False];
			];
			While[(numRead = stream@read[buf]) > 0,
			WriteString[outFile, FromCharacterCode[If[ # < 0,
														#+256,
														#
													]& /@ Take[JLink`Val[buf], numRead]]]
			];
			stream@close[];
			Close[outFile]
		(* Close returns the filename *)
		]
	] );

URLFileByteSize[link_String] :=
	URLFileByteSize[link] =
	Module[ {url, urlcon, len},
		url = JavaNew["java.net.URL", link];
		urlcon = url@openConnection[];
		len = urlcon@getContentLength[];
		urlcon@getInputStream[]@close[];
		len
	];

(*
URLFileByteSize@"http://www.feyncalc.org/formlink/formlink.zip"
*)

End[]

EndPackage[]

If[ !DirectoryQ[$installdirectory],
	CreateDirectory[$installdirectory]
];
Module[ {flziplocal, flfilesize},
	flziplocal = FileNameJoin[{ $installdirectory, FileNameTake @ $FormLinkZipFile}];
	(* get rid of previous download *)
	If[ FileExistsQ[flziplocal],
		DeleteFile@flziplocal
	];
	flfilesize =  Unzip`URLFileByteSize[$FormLinkZipFile];
	If[ Head[$FrontEnd]===System`FrontEndObject,
		PrintTemporary @  (* this way it does not get saved which is good *)
		Dynamic@Row[{"Downloading ", Round[flfilesize/1024.^2]," MB from ",
		If[ StringQ[Setting@#],
			#,
			" "
		] &@$FormLinkZipFile, " ",
		ProgressIndicator[
		Quiet[If[ ! NumberQ[#],
				0,
				#
			] &@(Refresh[FileByteCount[flziplocal],
			UpdateInterval -> .01]/flfilesize)]],
		" ", If[ ! NumberQ[Setting@#],
				0,
				#
			] &@
		Refresh[FileByteCount[flziplocal]/1024.^2, UpdateInterval -> .02],
		" MByte"
		}],
		Print["Downloading ", $FormLinkZipFile,"   please wait "]
	];
	CopyRemote[$FormLinkZipFile, flziplocal];
	Print["Downloading done, installing now to ", Style[$installdirectory, FontWeight -> "Bold"]];

	(*
	TODO: take this out eventually later
	*)
	Quiet[DeleteDirectory[FileNameJoin[{$installdirectory,"FormLink"}], DeleteContents->True]];
	Quiet[DeleteDirectory[FileNameJoin[{$installdirectory,"FeynCalcFormLink"}], DeleteContents->True]];
	Unzip[flziplocal, $installdirectory, Verbose -> False];
	If[ $OperatingSystem=!="Windows",
		Do[Run["chmod +x " <> FileNameJoin[{$installdirectory,"FormLink","bin",os,#}]]& /@ {"form","tform","FormLink"},{os,{"linux64","macosx64","linux32"}}]
	];
	Print["Installation of FormLink and FeynCalcFormLink done."];
	Print["Please notice that FORM 4.0 executables originating from http://www.nikhef.nl/~form/maindir/binaries/binaries.html have been installed too, and
that you are bound to agree to the license terms as explained here http://www.nikhef.nl/~form/license/license.html before using FORM.
Most prominently you should  refer to the FORM publication (J.A.M.Vermaseren \"New features of FORM\" math-ph/0010025) when you use (Par)(T)FORM in scientific publications."];

	(* check if FeynCalc is installed. If not, install it *)
	If[ (FindFile["FeynCalc`"] === $Failed)  &&
		(FindFile["FeynCalc`"] === $Failed), (* for FC 9 *)

		(* something in Phi,only M9, need to fix in a new version *)
		Off[Optional::opdef];
		Print["FeynCalc is needed for FeynCalcFormLink (not for FormLink alone though). "];
		(*
		Block[{Print},
		*)
		(* this prevents the display of two progress indicators*)
		Global`$FCProgressDisplay = True;
		Import["http://www.feyncalc.org/install.m"];
	(*
		];
		*)
	];
	If[ $FrontEnd =!= Null,
		CellPrint[Cell["Loading FormLink and FeynCalcFormLink","Subsection"]];
		Print["Loading FeynCalcFormLink and FormLink by: \n <<FeynCalcFormLink`"];
		CellPrint[Cell["Get[\"FeynCalcFormLink`\"]","Input"]],
		Print["Loading FeynCalcFormLink and FormLink by: \n <<FeynCalcFormLink`"];
	];

	(* otherwise, under Linux only, Kernel only a d$::shdw message appears ...*)
	Remove[d];
	Get["FeynCalcFormLink`"];
	Print["The basic functions are FormLink and FeynCalcFormLink."];
	Print["running now two simple examples:  \n"];
	CellPrint[Cell[
		(* there is something really weird with newlines under linux, therefore: *)
	ExportString[{
	"FormLink[ \"    AutoDeclare vector p;",
	"               Local T = g_(0, p1,p2,p3,p4,p5,p6);",
	"               trace4 0;",
	"          \"   (*, Form2M -> Identity *) (* <-- uncommenting returns a string *)",
	"        ];"
	},"Lines"]
	, "Input"]
	];
	Print @
	ToExpression["FormLink"][
	" AutoDeclare vector p;
Local T = g_(0, p1,p2,p3,p4,p5,p6);
trace4 0;
"  (*, FormLink`Form2M -> Identity *)
		];

	(* this is a function to switch on TraditionalForm Output, useful for FeynCalc typesetting *)
	(*FeynCalcFormLink`SetTF;*)
	(*ToExpression["SetTF"];*)
	If[ $FrontEnd =!= Null,
		CellPrint[Cell[
			"FeynCalcFormLink[ DiracTrace[GA[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Alpha]]] ]","Input"]];
		Print @  TraditionalForm @
		ExpressionCell[
		ToExpression["FeynCalcFormLink"][ ToExpression["DiracTrace"][
			ToExpression["GA"][\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Alpha]]]],
		DefaultFormatType -> TraditionalForm],
		ToExpression["FI"];
		Print["FeynCalcFormLink[ DiracTrace[GA[mu,nu,rho,si,tau,al] ] ] = ",
			ToExpression["FeynCalcFormLink"][
				ToExpression["DiracTrace"][
					ToExpression["GA"][mu,nu,rho,si,tau,al] ] ]
		]
	];
(* SetSF would set the Default Format Output Form back to StandardForm  for $FrontEnd and $FrontEndSession *)
	(* SetSF; *)
	];
