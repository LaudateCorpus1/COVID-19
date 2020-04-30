(* Wolfram Language Package *)

BeginPackage["COVID19`"]
Begin["`Private`"] 

MetroAreaCounties["STL"]:=With[{res=Interpreter["USCounty"][{"St. Louis County", "St. Louis City", 
   "St. Charles County, MO", "Jefferson County, MO", 
   "Franklin County, MO", "St. Clair County, IL", 
   "Madison County, IL", "Bond County, IL", "Calhoun County, IL", 
   "Clinton County, IL", "Macoupin COunty, IL", "Monroe County, IL", 
   "Lincoln County, MO", "Warren County, MO"}]},
   If[FreeQ[res,_Failure],
   	MetroAreaCounties["STL"]=res,
   	res
   ]
]

End[] 
EndPackage[]