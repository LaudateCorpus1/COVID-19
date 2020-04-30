(* Wolfram Language Package *)

BeginPackage["COVID19`"]
COVID19`NYTimesData
Begin["`Private`"] 

COVID19`NYTimesData[args___]:=nytimesdata[args]

nytimesdata[type_,"New"]:=With[{data=ResourceFunction["NYTimesCOVID19Data"][type]},
	If[!FailureQ[data],
		COVID19`$DataTimestamps[{"NYTimes",type}]=Now;
		nytimesdata[type]=data
	]
]

nytimesdata[type_]:=nytimesdata[type,"New"]


End[] 
EndPackage[]