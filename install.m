(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: install															*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 2012-2016 Feng Feng
	Copyright (C) 2012-2016 Rolf Mertig
	Copyright (C) 2015-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Installs FormLink *)

(* ------------------------------------------------------------------------ *)

InstallFormLink::notcomp =
"Your Mathematica version is too old. FormLink requires at least Mathematica 8. Installation aborted!";

InstallFormLink::failed =
"Download of `1` failed. Installation aborted!";

AutoOverwriteFormLinkDirectory::usage="AutoOverwriteFormLinkDirectory is an option of InstallFormLink. If \
set to True, the existing FormLink directory will be deleted without any further notice. The default
value None means that the user will be asked by a dialog. False means that the directory will be overwritten.";

FormLinkDevelopmentVersionLink::usage="FormLinkDevelopmentVersionLink is an option of InstallFormLink. It specifies the url \
to the main repository of FormLink. This repository is used to install the development version of FormLink.";

CygwinDLLLink::usage="CygwinDLLLink is an option of InstallFormLink. It specifies the url \
to cygwin1.dll.";

FormLinkStableVersionLink::usage="FormLinkStableVersionLink is an option of InstallFormLink. It specifies the url \
to the latest stable release of FormLink.";

InstallFormLinkDevelopmentVersion::usage="InstallFormLinkDevelopmentVersion is an option of InstallFormLink. If \
set to True, the installer will download the latest development version of FormLink from the git repository. \
Otherwise it will install the latest stable version.";

InstallFormLinkTo::usage="InstallFormLinkTo is an option of InstallFormLink. It specifies, the full path \
to the directory where FormLink will be installed.";

InstallFORMTo::usage="InstallFORMTo is an option of InstallFORM. It specifies, the full path \
to the directory where FORM binaries will be installed.";

If[  $VersionNumber == 8,
(*To use FetchURL in MMA8 we need to load URLTools first *)
Needs["Utilities`URLTools`"];
];

Options[InstallFormLink]={
	AutoInstallFORM->None,
	AutoOverwriteFormLinkDirectory->None,
	FormLinkDevelopmentVersionLink->"https://github.com/FormLink/formlink/archive/master.zip",
	(*for the moment there is no stable branch!*)
	FormLinkStableVersionLink->"",
	InstallFormLinkDevelopmentVersion->True,
	InstallFormLinkTo->FileNameJoin[{$UserBaseDirectory, "Applications","FormLink"}]
};

Options[InstallFORM]={
	FORMLink->"https://github.com/FormLink/form-binaries/archive/master.zip",
	CygwinDLLLink->"https://raw.githubusercontent.com/FormLink/cygwin-files/master/cygwin1.dll",
	InstallFORMTo->FileNameJoin[{$UserBaseDirectory, "Applications","FormLink","bin"}]
};

InstallFORM[OptionsPattern[]]:=
	Module[{tmpzip,zip,FCGetUrl,unzipDir,packageDir,packageName,
		linux32Path, linux64Path, osxPath, windowsPath,
		linux32FormPath, linux32TformPath,
		linux64FormPath, linux64TformPath,
		osxFormPath, osxTformPath,
		windowsFormPath, windowsTformPath, massDelete, cygwinDLLPath},
		(* Install FORM	*)

		packageDir=OptionValue[InstallFORMTo];
		zip = OptionValue[FORMLink];
		packageName = "FORM";

		linux32Path = FileNameJoin[{packageDir,"linux32"}];
		linux64Path = FileNameJoin[{packageDir,"linux64"}];
		osxPath 	= FileNameJoin[{packageDir,"macosx64"}];
		windowsPath = FileNameJoin[{packageDir,"windows"}];

		linux32FormPath = FileNameJoin[{linux32Path,"form"}];
		linux32TformPath = FileNameJoin[{linux32Path,"tform"}];

		linux64FormPath = FileNameJoin[{linux64Path,"form"}];
		linux64TformPath = FileNameJoin[{linux64Path,"tform"}];

		osxFormPath = FileNameJoin[{osxPath,"form"}];
		osxTformPath = FileNameJoin[{osxPath,"tform"}];

		windowsFormPath = FileNameJoin[{windowsPath,"form.exe"}];
		windowsTformPath = FileNameJoin[{windowsPath,"tform.exe"}];
		cygwinDLLPath = FileNameJoin[{windowsPath,"cygwin1.dll"}];

		If[$VersionNumber == 8,
			(*To use FetchURL in MMA8 we need to load URLTools first *)
			FCGetUrl[x_]:= Utilities`URLTools`FetchURL[x],
			FCGetUrl[x_]:= URLSave[x,CreateTemporary[]]
		];

		WriteString["stdout", "Downloading FORM binaries from ", zip," ..."];
		tmpzip=FCGetUrl[zip];
		unzipDir= tmpzip<>".dir";
		WriteString["stdout", "done! \n"];

		(* Extract to the content	*)
		WriteString["stdout", "FORM binaries zip file was saved to ", tmpzip,".\n"];
		WriteString["stdout", "Extracting FORM binaries zip file to ", packageDir, " ..."];
		ExtractArchive[tmpzip, unzipDir];
		WriteString["stdout", "done! \n"];

		(* Move the files to the final destination	*)
		massDelete = Function[ path,
			If[FileExistsQ[path],
				DeleteFile[path]
			]
		];
		massDelete/@ {linux32FormPath, linux32TformPath, linux64FormPath,
			linux64TformPath, osxFormPath, osxTformPath,
			windowsFormPath, windowsTformPath, cygwinDLLPath};

		WriteString["stdout", "Copying FORM 32-bit Linux binaries to ", linux32Path, " ..."];

		CopyFile[FileNameJoin[{unzipDir,"form-binaries-master","linux32","form"}],linux32FormPath];
		CopyFile[FileNameJoin[{unzipDir,"form-binaries-master","linux32","tform"}],linux32TformPath];
		WriteString["stdout", "done! \n"];

		WriteString["stdout", "Copying FORM 64-bit Linux binaries to ", linux64Path, " ..."];
		CopyFile[FileNameJoin[{unzipDir,"form-binaries-master","linux64","form"}],linux64FormPath];
		CopyFile[FileNameJoin[{unzipDir,"form-binaries-master","linux64","tform"}],linux64TformPath];
		WriteString["stdout", "done! \n"];

		WriteString["stdout", "Copying FORM OS X binaries to ", osxPath, " ..."];
		CopyFile[FileNameJoin[{unzipDir,"form-binaries-master","macosx64","form"}],osxFormPath];
		CopyFile[FileNameJoin[{unzipDir,"form-binaries-master","macosx64","tform"}],osxTformPath];
		WriteString["stdout", "done! \n"];

		WriteString["stdout", "Copying FORM Windows binaries to ", windowsPath, " ..."];
		CopyFile[FileNameJoin[{unzipDir,"form-binaries-master","windows","form.exe"}],windowsFormPath];
		CopyFile[FileNameJoin[{unzipDir,"form-binaries-master","windows","tform.exe"}],windowsTformPath];
		WriteString["stdout", "done! \n"];

		(* Delete the downloaded file	*)
		Quiet@DeleteFile[tmpzip];

		(* Delete the extracted archive *)
		Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];

		zip = OptionValue[CygwinDLLLink];
		WriteString["stdout", "Downloading cygwin1.dll from ", zip," ..."];
		tmpzip=FCGetUrl[zip];
		WriteString["stdout", "done! \n"];

		WriteString["stdout", "Copying cygwin1.dll to ", cygwinDLLPath, " ..."];
		CopyFile[tmpzip,cygwinDLLPath];
		WriteString["stdout", "done! \n"];

		(* Delete the downloaded file	*)
		Quiet@DeleteFile[tmpzip];

		(* Mark binaries as executable *)
		If[$OperatingSystem === "Unix" || $OperatingSystem === "MacOSX",
			Run["chmod +x "<>linux32FormPath];
			Run["chmod +x "<>linux32TformPath];
			Run["chmod +x "<>linux64FormPath];
			Run["chmod +x "<>linux64TformPath];
			Run["chmod +x "<>osxFormPath];
			Run["chmod +x "<>osxTformPath]
		];


	];

InstallFormLink[OptionsPattern[]]:=
	Module[{	unzipDir, tmpzip, gitzip, packageName, packageDir,
				strFORM,FCGetUrl,
				strOverwriteFCdit, formInstalled, zipDir},

	If[OptionValue[InstallFormLinkDevelopmentVersion],
		gitzip = OptionValue[FormLinkDevelopmentVersionLink];
		zipDir = "formlink-master",
		gitzip = OptionValue[FormLinkStableVersionLink];
		zipDir = "formlink-stable"
	];
	formInstalled=False;

	packageName = "FormLink";
	packageDir = OptionValue[InstallFormLinkTo];

strFORM="Do you want to install compiled binaries of FORM from "<> OptionValue[InstallFORM,FORMLink] <> "? FORM is a \
very fast computer algebra system for high energy physics calculations developed by Vermaseren (J.A.M.Vermaseren \
\"New features of FORM\", math-ph/0010025) . FormLink will not work without FORM. Before you proceed, \
please look at the license of FORM (http://www.nikhef.nl/~form/license/license.html) \
and make sure that you understand it and that you agree with it. Furtherhmore, all the existing FORM binaries in \
your FormLink directory will be overwritten.";

strOverwriteFCdit="Looks like FormLink is already installed. Do you want to replace the content \
of " <> packageDir <> " with the downloaded version of FormLink? If you are using any custom configuration \
files or add-ons that are located in that directory, please backup them in advance.";

	If[$VersionNumber < 8,
		Message[InstallFormLink::notcomp];
		Abort[]
	];

	If[$VersionNumber == 8,
		(*To use FetchURL in MMA8 we need to load URLTools first *)
		FCGetUrl[x_]:= Utilities`URLTools`FetchURL[x],
		FCGetUrl[x_]:= URLSave[x,CreateTemporary[]]
	];


	(* If the package directory already exists, ask the user about overwriting *)
	If[ DirectoryQ[packageDir],

		If[ OptionValue[AutoOverwriteFormLinkDirectory],

			Quiet@DeleteDirectory[packageDir, DeleteContents -> True],

			Null,
			If[ ChoiceDialog[strOverwriteFCdit,{"Yes, overwrite the " <> packageName <>" directory"->True,
				"No! I need to do a backup first."->False}],
				Quiet@DeleteDirectory[packageDir, DeleteContents -> True],
				Abort[]
			]
		]
	];

	(* Download FormLink tarball	*)
	WriteString["stdout", "Downloading FormLink from ", gitzip," ..."];
	tmpzip=FCGetUrl[gitzip];
	unzipDir= tmpzip<>".dir";
	WriteString["stdout", "done! \n"];

	(* Extract to the content	*)
	WriteString["stdout", "FormLink zip file was saved to ", tmpzip,".\n"];
	WriteString["stdout", "Extracting FormLink zip file to ", unzipDir, " ..."];
	ExtractArchive[tmpzip, unzipDir];
	WriteString["stdout", "done! \n"];

	(* Delete the downloaded file	*)
	Quiet@DeleteFile[tmpzip];

	(* Move the files to the final destination	*)
	WriteString["stdout", "Copying "<>packageName<>" to ", packageDir, " ..."];
	Print[FileNameJoin[{unzipDir,zipDir}]];
	CopyDirectory[FileNameJoin[{unzipDir,zipDir}],packageDir];
	WriteString["stdout", "done! \n"];
	(* Delete the extracted archive *)
	Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];

	(* Mark binaries as executable *)
	If[$OperatingSystem === "Unix" || $OperatingSystem === "MacOSX",
		Run["chmod +x "<>FileNameJoin[{$UserBaseDirectory, "Applications","FormLink","bin","linux32","FormLink"}]];
		Run["chmod +x "<>FileNameJoin[{$UserBaseDirectory, "Applications","FormLink","bin","linux32","FormLinkLegacy"}]];
		Run["chmod +x "<>FileNameJoin[{$UserBaseDirectory, "Applications","FormLink","bin","linux64","FormLink"}]];
		Run["chmod +x "<>FileNameJoin[{$UserBaseDirectory, "Applications","FormLink","bin","linux64","FormLinkLegacy"}]];
		Run["chmod +x "<>FileNameJoin[{$UserBaseDirectory, "Applications","FormLink","bin","macosx64","FormLink"}]];
		Run["chmod +x "<>FileNameJoin[{$UserBaseDirectory, "Applications","FormLink","bin","macosx64","FormLinkLegacy"}]]
	];

	If[ OptionValue[AutoInstallFORM],

		formInstalled=True;
		InstallFORM[],
		Null,
		If[ ChoiceDialog[strFORM],
			formInstalled=True;
			InstallFORM[]
		]
	];


	WriteString["stdout","\nInstallation complete! To load FormLink, restart Mathematica \
and evaluate \n\n <<FormLink` \n\nIf you chose not to install FORM binaries, make sure that \
you first compile FORM for your system!"];



];
