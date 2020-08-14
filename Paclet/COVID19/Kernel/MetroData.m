(* Wolfram Language Package *)

BeginPackage["COVID19`"]
COVID19`MetroData
COVID19`DeployMetroData
Begin["`Private`"] 
Clear[COVID19`MetroData,metroData];
COVID19`MetroData[args___]:=Catch[metroData[args]]

Options[COVID19`MetroData]=Options[metroData]={"UpdateData"->False,"SmoothingDays"->7,"MinimumCases"->100,"MinimumDeaths"->30,"MetroName"->""};

Options[COVID19`DeployMetroData]=Join[Options[COVID19`MetroData],Options[CloudDeploy]]

metroData[counties:{_Entity..},state_:None,opts:OptionsPattern[]]:=Module[
	{timeseries,mintime,maxtime, cases, deaths},
	
	timeseries = metroDataTimeSeries[counties, opts];
	mintime = timeseries[Min, All, #["FirstDate"] &];
	maxtime = timeseries[Max, All, #["LastDate"] &];
	timeseries = Prepend[timeseries, "Metro Area Total" -> Normal[timeseries[Total]]];
	
	updateProgress[$covidprogessid, "Formatting Time Series"];
	cases = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Cases, #2 > OptionValue["MinimumCases"] &] &][Select[#["PathLength"] > (OptionValue["SmoothingDays"]+1) &]];
	deaths = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Deaths, #2 > OptionValue["MinimumDeaths"] &] &][Select[#["PathLength"] > (OptionValue["SmoothingDays"]+1) &]];
	updateProgress[$covidprogessid, "Creating Timeline Grid"];
	{metroPlotGrid[
		counties,
		cases,
		deaths,
		{mintime,maxtime},
		OptionValue["MetroName"]<>"Metro Area COVID-19 Timelines",
		opts
	],
	If[Head[state]===Entity,
	updateProgress[$covidprogessid, "Creating State Testing Plots"];
		Grid[{
			{Style["State Testing Data from covidtracking.com",24]},
			{Row@COVID19`StateTestingPlots[state,Sequence@@FilterRules[{opts},Options[COVID19`StateTestingPlots]]]}}],
		Nothing
	]
	,
	updateProgress[$covidprogessid, "Creating Data Table"];
	Labeled[tableData[timeseries[All, {"Cases", "Deaths"}]],DateString[maxtime, "Date"], Top],
	
	If[counties===MetroAreaCounties["STL"],
	updateProgress[$covidprogessid, "Creating Regional Hospitalization Plots"];
		Echo@Grid[{
			{Style[Row[{"Metro Area Hospital Data from ",Hyperlink["SLU OpenGIS","https://github.com/slu-openGIS/MO_HEALTH_Covid_Tracking"]}],24]},
			{Row@prenerSTLHospitalData[]}}],
		Nothing
	]
	}
]

metroData[area_String, rest___]:=Block[{$covidprogessid=CreateUUID[]},
	updateProgress[$covidprogessid, "Finding Counties"];
	metroData[MetroAreaCounties[area],rest]
]


metroData[___]:=$Failed

metroDataTimeSeries[counties_,OptionsPattern[metroData]]:=Module[
	{timeseries,mintime,maxtime},
	updateProgress[$covidprogessid, "Getting Data From NY Times"];
	timeseries = If[TrueQ[OptionValue["UpdateData"]],
		COVID19`NYTimesData["USCountiesTimeSeries","New"],
		COVID19`NYTimesData["USCountiesTimeSeries"]
	];
	
	updateProgress[$covidprogessid, "Processing data"];
	timeseries = KeyTake[timeseries[All, KeyDrop["State"]], counties];
	If[!TrueQ[Length[timeseries]>0],Throw[Failure["nocountydata",<|"Message"->"No Counties matched your spec."|>]]];
	mintime = timeseries[Min, All, #["FirstDate"] &];
	updateProgress[$covidprogessid, "Totalling Metro Area"];
	timeseries[All, All, TimeSeries[TimeSeriesInsert[#, {mintime, 0}]["DatePath"], 
 			ResamplingMethod -> {"Interpolation", InterpolationOrder -> 0}] &]
]


stylemap[counties_]:= (stylemap[counties]=Prepend[MapIndexed[# -> ColorData[3][(1+#2[[1]])/.{3->18}] &,counties],$entitytotallabel->Directive[Thickness[.005],Black]  ]);

optsfunc[counties_,data_,start_] := {PlotRange -> {{start, Automatic}, Full},
   ImageSize -> 400, PlotLegends -> None, 
   FrameTicks -> {{Automatic, All}, {Automatic, False}}, 
   PlotStyle -> Values@KeyTake[stylemap[counties], Normal@Keys[data]], 
   GridLines -> {None, Automatic}};
   
timelineTitle[title_,date_]:=Style[Column[{title, "From NY Times data on " <> DateString[date, "DateShort"]}, Alignment -> Center], 24] 
 
columnLabels[minc_,mind_]:=Style[#, 18, Italic] & /@ {
		"Cases (> "<>ToString[minc]<>")", "Deaths (> " <> ToString[mind] <> ")"}

$CovidEntityType="USCounty";
Clear[$entitytotallabel];
$entitytotallabel:="Metro Area Total"/;$CovidEntityType==="USCounty"
$entitytotallabel:="Combined Total"

countyLegend[counties_, cases_, deaths_]:=SwatchLegend[Lookup[stylemap[counties], #], #] &@
    ResourceFunction["SortLike"][Normal[Union[Keys[cases], Keys[deaths]]], 
     Prepend[counties, $entitytotallabel]]
     
metroPlotGrid[counties_,cases_,deaths_,{mintime_,maxtime_},
	title_:"Metro Area COVID-19 Timelines",opts:OptionsPattern[metroData]]:=With[
	{smoothing=OptionValue["SmoothingDays"],
	alignedcases = cases[All, ResourceFunction["TimeSeriesAlign"][#, 0] &][All, tozerotime],
	aligneddeaths = deaths[All, ResourceFunction["TimeSeriesAlign"][#, 0] &][All, tozerotime],
	casesopts=optsfunc[counties,cases, mintime+Quantity[OptionValue["SmoothingDays"],"Days"]],
	deathsopts=optsfunc[counties,deaths,mintime+Quantity[OptionValue["SmoothingDays"],"Days"]]},
Grid[{
	{Style[timelineTitle[title,maxtime], 24], SpanFromLeft, ""},
	Append[columnLabels[OptionValue["MinimumCases"],OptionValue["MinimumDeaths"]], SpanFromAbove],
  	Append[cumulativePlots[{cases,casesopts},{deaths,deathsopts}],countyLegend[counties, cases, deaths]],
	Append[perCapitaPlots[{cases,casesopts},{deaths,deathsopts},counties],SpanFromAbove],
  	Append[differencesPlots[{cases,casesopts},{deaths,deathsopts}, smoothing],SpanFromAbove],
  	Append[differencesPerCapitaPlots[{cases,casesopts},{deaths,deathsopts}, smoothing,counties],SpanFromAbove],
  	Append[ratioPlots[{cases,casesopts},{deaths,deathsopts}, smoothing],SpanFromAbove],
  	Append[movingRatioPlots[{cases,casesopts},{deaths,deathsopts}, smoothing],SpanFromAbove],
  	Append[alignedGrowthPlots[{alignedcases,casesopts},{aligneddeaths,deathsopts},{OptionValue["MinimumCases"],OptionValue["MinimumDeaths"]}],SpanFromAbove]
  	
  }]
]


tableData[ds_]:=With[{locs=Cases[Normal@Keys[ds],_Entity]},
	Dataset@(Association@KeyValueMap[#1->tabledata[QuantityMagnitude[population[locs,#1]],#2]&,Normal[ds]])
]

tabledata[pop_,data_Association]:=tabledata[pop,#]&/@data

tabledata[pop_,ts_] := With[{path = Normal[ts]},
  Association @@ {
    "Total (Per 1000 pop)" ->
    	Row[{path[[-1, 2]], 
       " (", Round[1000*path[[-1, 2]]/pop,.001], ")"}],
    "New Today (Yesterday)" -> 
     Row[{(path[[-1, 2]] - path[[-2, 2]]), 
       " (", (path[[-2, 2]] - path[[-3, 2]]), ")"}],
    "New This Week (Last Week)" -> 
     Row[{path[[-1, 2]] - path[[-8, 2]], 
       " (", (path[[-8, 2]] - path[[-15, 2]]), ")"}]
    }
  ]

Options[COVID19`DeployMetroData]=Join[Options[COVID19`MetroData],Options[CloudDeploy]]

DeployMetroData[area_,location_, opts:OptionsPattern[]]:=(Quiet[DeleteObject[CloudObject[location]];
	With[{nb=metroDataNotebook[area,opts]},
		CloudDeploy[nb, location, Sequence@@FilterRules[{opts},Options[CloudDeploy]],
		 Permissions -> "Public"]
		]
	
])

metroDataNotebook[{metro_String,state_},opts___]:=With[{res=COVID19`MetroData[metro,state, Sequence@@FilterRules[{opts},Options[COVID19`MetroData]]]},
		Notebook[Flatten@{Cell["Metro Area Timelines", "Section"],
		   Cell[BoxData[ToBoxes[res[[1]]]], "Output"], 
		   If[Length[res]>3,
		   	{Cell["Metro Area Hospital Data", "Section"],
			   Cell[BoxData[ToBoxes[res[[4]]]], "Output"]},
			   Nothing
		   	
		   ],
		   Cell["State Testing Timelines", "Section"],
		   Cell[BoxData[ToBoxes[res[[2]]]], "Output"],
		   Cell["Metro Area Data", "Section"],
		   Cell[BoxData[ToBoxes[res[[3]]]], "Output"]
		  
		   }, "ClickToCopyEnabled" -> 
		   False]
		]

metroDataNotebook[metro_String,opts___]:=With[{res=COVID19`MetroData[area, Sequence@@FilterRules[{opts},Options[COVID19`MetroData]]]},
		Notebook[{Cell["Timelines", "Section"],
		   Cell[BoxData[ToBoxes[res[[1]]]], "Output"],
		   Cell["Data", "Section"],
		   Cell[BoxData[ToBoxes[res[[2]]]], "Output"]
		   }, "ClickToCopyEnabled" -> 
		   False]
		]
End[] 
EndPackage[]