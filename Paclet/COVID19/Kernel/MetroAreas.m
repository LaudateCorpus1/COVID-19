(* Wolfram Language Package *)

BeginPackage["COVID19`"]
Begin["`Private`"] 

MetroAreaCounties["STL"]:=With[{res=Interpreter["USCounty"][{"St. Louis County", "St. Louis City", 
	"St. Clair County, IL", 
   "St. Charles County, MO", 
   "Madison County, IL", "Jefferson County, MO", 
   "Franklin County, MO", 
   "Clinton County, IL", "Macoupin COunty, IL", "Monroe County, IL", 
   "Lincoln County, MO", "Warren County, MO", "Bond County, IL", "Calhoun County, IL"}]},
   If[FreeQ[res,_Failure],
   	MetroAreaCounties["STL"]=res,
   	res
   ]
]


MetroAreaCounties["Destin"]:={Entity["AdministrativeDivision", {"WaltonCounty", "Florida", 
   "UnitedStates"}], 
 Entity["AdministrativeDivision", {"OkaloosaCounty", "Florida", 
   "UnitedStates"}], 
 Entity["AdministrativeDivision", {"SantaRosaCounty", "Florida", 
   "UnitedStates"}], 
 Entity["AdministrativeDivision", {"WashingtonCounty", "Florida", 
   "UnitedStates"}], 
 Entity["AdministrativeDivision", {"BayCounty", "Florida", 
   "UnitedStates"}], 
 Entity["AdministrativeDivision", {"EscambiaCounty", "Florida", 
   "UnitedStates"}], 
 Entity["AdministrativeDivision", {"HolmesCounty", "Florida", 
   "UnitedStates"}], 
 Entity["AdministrativeDivision", {"CovingtonCounty", "Alabama", 
   "UnitedStates"}], 
 Entity["AdministrativeDivision", {"EscambiaCounty", "Alabama", 
   "UnitedStates"}]};
   
End[] 
EndPackage[]