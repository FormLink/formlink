:Begin: 
:Function: FormInit
:Pattern:	FormLink`FormInit[formfullpath_String, exargs_String:""]
:Arguments:	{formfullpath, exargs}
:ArgumentTypes: {Manual}
:ReturnType: Integer
:End:

:Begin:
:Function: FormWrite
:Pattern:	FormLink`FormWrite[script_String /; StringFreeQ[script, "\[IndentingNewLine]"]]
:Arguments:	{script}
:ArgumentTypes: {Manual}
:ReturnType: Integer
:End:

:Begin:
:Function: FormPrompt
:Pattern:	FormLink`FormPrompt[]
:Arguments:	{}
:ArgumentTypes: {}
:ReturnType: Integer
:End:

:Begin:
:Function: FormRead
:Pattern:	FormLink`FormRead[]
:Arguments:	{}
:ArgumentTypes: {}
:ReturnType: Manual
:End: