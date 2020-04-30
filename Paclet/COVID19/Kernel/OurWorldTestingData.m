(* Wolfram Language Package *)

BeginPackage["COVID19`"]
COVID19`COVIDTrackingData
Begin["`Private`"] 

COVID19`OurWorldInDataTestingData[args___]:=ourWorldInDataTestingData[args]

ourWorldInDataTestingData["New"]:=With[{data=getOurWorldData[]},
	If[!FailureQ[data],
		COVID19`$DataTimestamps["OurWorldInData"]=Now;
		ourWorldInDataTestingData[]=data
	]
]

ourWorldInDataTestingData[]:=ourWorldInDataTestingData["New"]


getOurWorldData[]:=With[
	{resp=URLRead["https://drive.google.com/u/0/uc?id=1ieHV7KQhr-hOg5KqGoVz4ETPSc9tx-zc&export=download"]},
	parseOurWorldData@Import[resp, "Dataset", "HeaderLines" -> 1]
	
]

parseOurWorldData[dslist_]:=First[dslist][DeleteDuplicates][
	Select[StringLength[#Entity] > 1 &], 
		{"Entity" -> (Interpreter["Country"][First[StringSplit[#, " - "]]] &),"Source URL"->URL}][
		All,
		KeySortBy[#/.{"Entity"->1,"Date"->2,"Cumulative total"->3,
			"Daily change in cumulative total"->4,
			"Cumulative total per million"->5,"Daily change in cumulative total per million"->6}&]
		]

	
End[] 
EndPackage[]