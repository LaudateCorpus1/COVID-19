(* Wolfram Language Package *)

BeginPackage["COVID19`"]
COVID19`COVIDTrackingData
Begin["`Private`"] 

COVID19`COVIDTrackingData[args___]:=covidtrackingdata[args]

covidtrackingdata[type_,"New"]:=With[{data=ResourceFunction["COVIDTrackingData"][type]},
	If[!FailureQ[data],
		COVID19`$DataTimestamps[{"COVIDTracking",type}]=Now;
		covidtrackingdata[type]=data
	]
]

covidtrackingdata[type_]:=covidtrackingdata[type,"New"]


End[] 
EndPackage[]