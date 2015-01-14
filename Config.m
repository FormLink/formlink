(* Mathematica package *)


(* this setting determines how long (at most) the FORM lines will be *)
FormLink`$FormPageWidth = 153;

FormLink`$FormOutputCellStyle = "Program";

(*
do not do this: screws up the predictive interface in Mathematica 9.0.0 ...
SetOptions[ToString, PageWidth -> 153 ];
*)


If[ Head[$FrontEnd] === System`FrontEndObject,
    SetOptions[$FrontEndSession, EvaluationCompletionAction -> {"ScrollToOutput"}];
    SetOptions[$FrontEndSession, MessageOptions -> {"InsufficientVersionWarning" -> False}];
];

(* another V9 thing: *)
Off[StringMatchQ::strse];
(* for running without a FrontEnd : *)
Off[FrontEndObject::notavail];

FormLink`$M2Form = {
          "BernoulliB[" -> "bernoulli_(",
          "Binomial[" -> "binom_(",
          "Cos[" -> "cos_(", 
          "Factorial[" -> "fac_(",
          "GCD[" -> "gcd_(",
          (*
          "KroneckerDelta" -> "delta_",
          *)
          "Log[" -> "ln_(",
          "Max[" -> "max_(",
          "Min[" -> "min_(",
          "Mod[" -> "mod_(",
          "Pi" -> "pi_",
          "PolyLog[2," -> "li2_(",
          (* 
          "PolyLog[3," -> "lin_(3,",
          *)
          "Sqrt[" -> "sqrt_(", 
          "Sign[" -> "sign_(", 
          "Sin[" -> "sin_(", 
          "Sum["-> "sum_(", 
          "Tan[" -> "tan_(",
          (* this is strictly speaking, ..., dangerous, but for now probably enough *)
          "I" -> "i_"
          };
          
FormLink`$Form2M = Join[ 
     	{"bernoulli_" -> "BernoulliB", "binom_" -> "Binomial", 
           "cos_" -> "Cos", "fac_" -> "Factorial", "gcd_" -> "GCD", 
           "ln_" -> "Log", "max_" -> "Max", "min_" -> "Min", "mod_" -> "Mod", 
           "pi_" -> "Pi", "li2_" -> "PolyLog2,", "sqrt_" -> "Sqrt", 
           "sign_" -> "Sign", "sin_" -> "Sin", "sum_" -> "Sum", "tan_" -> "Tan",
           "i_" -> "I"},
	                      {
                            "e_" -> "I*$LeviCivitaSign*Eps", (*By Feng*)
	                        "d_" -> "Pair", 
                            (*
	                        "d_" -> "KroneckerDelta", 
	                        *)
	                        (*"i_" -> "I",*) 
	                      	"\\" -> "","gi_"->"DiracGamma",(*"*"->"." , *)" "->"", 
	                      	"_"->""
	                      	(* for brackets : *)
	                      ,	"[" -> "Hold[Identity]["
	                      	}
	                      	];
	                      	

(* this is of course only correct in the context of FORM,i.e., . is always scalar product multiplication and not general noncommuative multiplication*)
FormLink`$DotPowerFix = {a_ . b_^c_ :> (a.b)^c, (a_^b_).c_ :> (a.c)^b } ;

FormLink`$TForm = False;
