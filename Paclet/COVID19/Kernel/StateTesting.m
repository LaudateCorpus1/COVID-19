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
	{
DateListPlot[{
	If[
		TrueQ[OptionValue["TotalTestsScale"]>1],
     	Callout[#,"x "<>ToString[OptionValue["TotalTestsScale"]],Top]&,
     	Identity][
     statedata[(#Positive + #Negative)/OptionValue["TotalTestsScale"] & ]], 
     statedata[{"Positive","Negative"}, Differences][All, MovingAverage[# , 
     	Quantity[OptionValue["SmoothingDays"], "Days"]] & ][((#Positive + #Negative) &)]
		},
		stateTestingPlotOptions[state,"Count"], ImageSize -> 400,PlotStyle->{Orange,Blue}
     ]
     ,
DateListPlot[{
     statedata[#Positive/(#Positive + #Negative) & ], 
     statedata[{"Positive","Negative"}, Differences][All, MovingAverage[# , 
     	Quantity[OptionValue["SmoothingDays"], "Days"]] & ][(#Positive/(#Positive + #Negative) &)]
		},
		stateTestingPlotOptions[state,"Rate"], ImageSize -> 400,PlotStyle->{Orange,Blue}
     ],
   DateListPlot[{
     statedata["HospitalizedCurrently"],
     MovingAverage[statedata["HospitalizedCurrently"] , 
     	Quantity[OptionValue["SmoothingDays"], "Days"]]
		},
		stateTestingPlotOptions[state,"Hospitalized"], ImageSize -> 400,PlotStyle->{Purple,Blue}
     ],
   DateListPlot[{
     statedata["OnVentilatorCurrently"],
     MovingAverage[statedata["OnVentilatorCurrently"] , 
     	Quantity[OptionValue["SmoothingDays"], "Days"]]
		},
		stateTestingPlotOptions[state,"On Ventilator"], ImageSize -> 400,PlotStyle->{Purple,Blue}
     ]    
     
     ,
     
 SwatchLegend[{Orange,Blue,Purple},{"Cumulative",ToString[OptionValue["SmoothingDays"]]<>" Day Average","Daily"}]
}
]

stateTestingPlots[state_String,opts:OptionsPattern[]]:=stateTestingPlots[Interpreter["USState"][state],opts]

stateTestingPlots[___]:=$Failed
stateDataToTimeSeries=(AssociationMap[
      Function[key, 
       TimeSeries[Lookup[#1, key], {Lookup[#1, "Date"]}]], 
             {"Positive", "Negative", 
       "Death","HospitalizedCurrently","OnVentilatorCurrently"}]&)
	
	
stateTestingPlotOptions[Entity["AdministrativeDivision", {"Missouri", "UnitedStates"}],"Count"]:=Sequence@@{
     PlotRange -> {{"March 28", Automatic}, Automatic},
     PlotLabel->Grid[{{"COVID-19 Test Count in Missouri"},
     	{Style["Virus and Antibody tests were combined before May 22",10]}},Aligment->Center],
     Epilog->{Dashed,Line[{{{2020,05,22},0},{{2020,05,22},10^10}}],Opacity[.3],Red,Rectangle[{{2020,05,22},0},{{2020,05,30},10^10}]}
     	
}
	
stateTestingPlotOptions[Entity["AdministrativeDivision", {"Missouri", "UnitedStates"}],"Rate"]:=Sequence@@{
     PlotRange -> {{"March 28", Automatic}, Automatic},
     PlotLabel->Grid[{{"COVID-19 Positive Testing Rate in Missouri"},
     	{Style["Virus and Antibody tests were combined before May 22",10]}},Aligment->Center],
     Epilog->{Dashed,Line[{{{2020,05,22},0},{{2020,05,22},1}}],Opacity[.3],Red,Rectangle[{{2020,05,22},0},{{2020,05,30},1}]}
}


stateTestingPlotOptions[state_,"Count"]:=Sequence@@{
     PlotRange -> {{"March 28", Automatic}, Automatic},
     PlotLabel->"COVID-19 Test Count in "<>StringReplace[CommonName[state],", United States"->""]}
     
     
stateTestingPlotOptions[state_,"Rate"]:=Sequence@@{
     PlotRange -> {{"March 28", Automatic}, Automatic},
     PlotLabel->"COVID-19 Positive Testing Rate in "<>StringReplace[CommonName[state],", United States"->""]}
     
	
stateTestingPlotOptions[state_,str_]:=Sequence@@{
     PlotRange -> {{"March 28", Automatic}, Automatic},
     PlotLabel->"COVID-19 "<>str<>" in "<>StringReplace[CommonName[state],", United States"->""]}
	
     
End[] 
EndPackage[]