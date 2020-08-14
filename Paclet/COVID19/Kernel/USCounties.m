(* Wolfram Language Package *)

BeginPackage["COVID19`"]

COVID19`GroupCounties

Begin["`Private`"] 

COVID19`GroupCounties[args___]:=Catch[groupCounties[args]]
  	
  	
groupCounties[f_]:=Module[
	{timeseries,mintime,maxtime, cases, deaths},
	
	updateProgress[$covidprogessid, "Getting Data From NY Times"];
	timeseries = If[TrueQ[OptionValue["UpdateData"]],
		COVID19`NYTimesData["USCountiesTimeSeries","New"],
		COVID19`NYTimesData["USCountiesTimeSeries"]
	];
	
	KeySelect[f,timeseries]
]


End[] 
EndPackage[]