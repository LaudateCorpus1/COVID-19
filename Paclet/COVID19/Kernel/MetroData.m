(* Wolfram Language Package *)

BeginPackage["COVID19`"]

COVID19`MetroData
COVID19`DeployMetroData
Begin["`Private`"] 

COVID19`MetroData[args___]:=Catch[metroData[args]]

Options[COVID19`MetroData]=Options[metroData]={"UpdateData"->False,"SmoothingDays"->7,"MinimumCases"->50,"MinimumDeaths"->10,"MetroName"->""};

Options[COVID19`DeployMetroData]=Join[Options[COVID19`MetroData],Options[CloudDeploy]]

metroData[counties:{_Entity..},opts:OptionsPattern[]]:=Module[
	{timeseries, totalpop,mintime,maxtime, cases, deaths},
	
	updateProgress[$covidprogessid, "Getting Data From NY Times"];
	timeseries = If[TrueQ[OptionValue["UpdateData"]],
		COVID19`NYTimesData["USCountiesTimeSeries","New"],
		COVID19`NYTimesData["USCountiesTimeSeries"]
	];
	
	updateProgress[$covidprogessid, "Processing data"];
	timeseries = KeyTake[timeseries[All, KeyDrop["State"]], counties];
	If[!TrueQ[Length[timeseries]>0],Throw[Failure["nocountydata",<|"Message"->"No Counties matched your spec."|>]]];
	mintime = timeseries[Min, All, #["FirstDate"] &];
	maxtime = timeseries[Max, All, #["LastDate"] &];
	updateProgress[$covidprogessid, "Totalling Metro Area"];
	timeseries = timeseries[All, All, TimeSeries[TimeSeriesInsert[#, {mintime, 0}]["DatePath"], 
 			ResamplingMethod -> {"Interpolation", InterpolationOrder -> 0}] &];
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
	]
	,
	updateProgress[$covidprogessid, "Creating Data Table"];
	Labeled[tableData[timeseries[All, {"Cases", "Deaths"}]],DateString[maxtime, "Date"], Top]
	}
]

metroData[area_, opts:OptionsPattern[]]:=Block[{$covidprogessid=CreateUUID[]},
	updateProgress[$covidprogessid, "Finding Counties"];
	metroData[MetroAreaCounties[area],opts]
]


metroData[___]:=$Failed

stylemap[counties_]:= (stylemap[counties]=MapIndexed[# -> ColorData[1][#2[[1]]] &,Prepend[counties, $entitytotallabel]]);

optsfunc[counties_,data_,start_] := {PlotRange -> {{start, Automatic}, Full},
   ImageSize -> 400, PlotLegends -> None, 
   FrameTicks -> {{Automatic, All}, {Automatic, False}}, 
   PlotStyle -> Values@KeyTake[stylemap[counties], Normal@Keys[data]], 
   GridLines -> {None, Automatic}};
   
timelineTitle[title_,date_]:=Style[Column[{title, "From NY Times data on " <> DateString[date, "DateShort"]}, Alignment -> Center], 24] 
 
columnLabels[minc_,mind_]:=Style[#, 18, Italic] & /@ {
		"Cases (> "<>ToString[minc]<>")", "Deaths (> " <> ToString[mind] <> ")"}

$CovidEntityType="USCounty";
$entitytotallabel:="Combined Total"/;$CovidEntityType==="Country"
$entitytotallabel:="Metro Area Total"

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
  	Append[ratioPlots[{cases,casesopts},{deaths,deathsopts}, smoothing],SpanFromAbove],
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
	With[{res=COVID19`MetroData[area, Sequence@@FilterRules[{opts},Options[COVID19`MetroData]]]},
		CloudDeploy[Notebook[{Cell["Timelines", "Section"],
		   Cell[BoxData[ToBoxes[res[[1]]]], "Output"],
		   Cell["Data", "Section"],
		   Cell[BoxData[ToBoxes[res[[2]]]], "Output"]
		   }, "ClickToCopyEnabled" -> 
		   False], location, Sequence@@FilterRules[{opts},Options[CloudDeploy]],
		 Permissions -> "Public"]
		]
])

End[] 
EndPackage[]