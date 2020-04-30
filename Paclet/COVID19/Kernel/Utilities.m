(* Wolfram Language Package *)

BeginPackage["COVID19`"]

COVID19`$DataTimestamps
Begin["`Private`"] 

If[!AssociationQ[COVID19`$DataTimestamps],
	COVID19`$DataTimestamps=<||>
]

updateProgress[id_String, expr_]:=(
progressStatus[id]=expr;
)/;TrueQ[inprogress[id]]

updateProgress[id_String, expr_]:=(
Block[{$ContextPath,$Context},
	Needs["GeneralUtilities`"]
];
progressStatus[id]=expr;
inprogress[id]=True;
progressCell[id]=PrintTemporary[GeneralUtilities`ProgressPanel@Dynamic[progressStatus[id]]]
)

endProgress[id_String]:=(
inprogress[id]=False;
Quiet[NotebookDelete[progressCell[id]]]
)

End[] 

EndPackage[]