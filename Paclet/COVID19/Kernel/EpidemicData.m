(* Wolfram Language Package *)

BeginPackage["COVID19`"]
COVID19`EpidemicData
Begin["`Private`"] 

COVID19`EpidemicData[args___]:=epidemicData[args]

epidemicData[type_,"New"]:=(
ResourceRemove[ResourceObject["Epidemic Data for Novel Coronavirus COVID-19"]];
With[{data=ResourceData["Epidemic Data for Novel Coronavirus COVID-19", type]},
	If[!FailureQ[data],
		COVID19`$DataTimestamps[{"Epidemic",type}]=Now;
		epidemicData[type]=data
	]
])

epidemicData[type_]:=epidemicData[type,"New"]

	
End[] 
EndPackage[]