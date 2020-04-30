(* Wolfram Language Package *)

BeginPackage["COVID19`"]


COVID19`StateTestingPlots
Begin["`Private`"] 

Clear[COVID19`StateTestingPlots,stateTestingPlots];  	
Options[COVID19`StateTestingPlots]= Options[stateTestingPlots]={"UpdateData"->False,"SmoothingDays"->7,"TotalTestsScale"->100};
 
 
COVID19`StateTestingPlots[args___]:=Catch[stateTestingPlots[args]]

stateTestingPlots[state_Entity,OptionsPattern[]] := With[{statedata=If[TrueQ[OptionValue["UpdateData"]],
	COVID19`COVIDTrackingData["StatesDaily","New"],
	COVID19`COVIDTrackingData["StatesDaily"]
][GroupBy["State"]][state][stateDataToTimeSeries]},
Global`sd=statedata;
	{
DateListPlot[{
	If[
		TrueQ[OptionValue["TotalTestsScale"]>1],
     	Callout[#,"x "<>ToString[OptionValue["TotalTestsScale"]],Top]&,
     	Identity][
     statedata[(#Positive + #Negative)/OptionValue["TotalTestsScale"] & ]], 
     statedata[All, Differences][All, MovingAverage[# , 
     	Quantity[OptionValue["SmoothingDays"], "Days"]] & ][((#Positive + #Negative) &)]
		},
     PlotRange -> {{"March 28", Automatic}, Full}, ImageSize -> 400,PlotStyle->{Orange,Blue},
     PlotLabel -> "COVID-19 Test Count "<>CommonName[state]]
     ,
DateListPlot[{
     statedata[#Positive/(#Positive + #Negative) & ], 
     statedata[All, Differences][All, MovingAverage[# , 
     	Quantity[OptionValue["SmoothingDays"], "Days"]] & ][(#Positive/(#Positive + #Negative) &)]
		},
     PlotRange -> {{"March 28", Automatic}, {0.05, 0.15}}, ImageSize -> 400,PlotStyle->{Orange,Blue},
     PlotLabel -> "COVID-19 Positive Testing Rate in "<>CommonName[state]],
     
 SwatchLegend[{Orange,Blue},{"Cumulative",ToString[OptionValue["SmoothingDays"]]<>" Day Average"}]
}
]

stateTestingPlots[state_String,opts:OptionsPattern[]]:=stateTestingPlots[Interpreter["USState"][state],opts]

stateTestingPlots[___]:=$Failed
stateDataToTimeSeries=(AssociationMap[
      Function[key, 
       TimeSeries[Lookup[#1, key], {Lookup[#1, "Date"]}]], 
             {"Positive", "Negative", 
       "Death"}]&)
	
End[] 
EndPackage[]