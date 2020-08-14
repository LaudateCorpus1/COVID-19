(* Wolfram Language Package *)

BeginPackage["COVID19`"]

COVID19`StateData
Begin["`Private`"] 
Clear[COVID19`StateData,stateData];
COVID19`StateData[args___]:=Catch[stateData[args]]

Options[COVID19`StateData]=Options[stateData]={"UpdateData"->False,"SmoothingDays"->7,"MinimumCases"->100,"MinimumDeaths"->30,"PlotName"->"State"};

stateData[counties:{_Entity..},state_:None,opts:OptionsPattern[]]:=Module[
	{timeseries,mintime,maxtime, cases, deaths},
	
	timeseries = stateDataTimeSeries[counties, opts];
	mintime = timeseries[Min, All, #["FirstDate"] &];
	maxtime = timeseries[Max, All, #["LastDate"] &];
	timeseries = Prepend[timeseries, "All States Total" -> Normal[timeseries[Total]]];
	
	updateProgress[$covidprogessid, "Formatting Time Series"];
	cases = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Cases, #2 > OptionValue["MinimumCases"] &] &][Select[#["PathLength"] > (OptionValue["SmoothingDays"]+1) &]];
	deaths = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Deaths, #2 > OptionValue["MinimumDeaths"] &] &][Select[#["PathLength"] > (OptionValue["SmoothingDays"]+1) &]];
	updateProgress[$covidprogessid, "Creating Timeline Grid"];
	{metroPlotGrid[
		counties,
		cases,
		deaths,
		{mintime,maxtime},
		OptionValue["PlotName"]<>" COVID-19 Timelines",
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
	Labeled[tableData[timeseries[All, {"Cases", "Deaths"}]],DateString[maxtime, "Date"], Top]
	}
]

stateData[area_String, rest___]:=Block[{$covidprogessid=CreateUUID[]},
	updateProgress[$covidprogessid, "Finding States"];
	stateData[interpretStates[area],rest]
]


stateData[___]:=$Failed

stateDataTimeSeries[counties_,OptionsPattern[stateData]]:=Module[
	{timeseries,mintime},
	updateProgress[$covidprogessid, "Getting Data From COVIDTracking"];
	timeseries = If[TrueQ[OptionValue["UpdateData"]],
		COVID19`COVIDTrackingData["StatesDaily","New"],
		COVID19`COVIDTrackingData["StatesDaily"]
	];
	timeseries=timeseries[GroupBy["State"], KeyDrop["State"]][All, Transpose /* (With[{d = #"Date"}, 
     TimeSeries[#, {d},ResamplingMethod -> {"Interpolation", InterpolationOrder -> 0}] & /@ KeyDrop[#, "Date"]] &)][
     All,KeyMap[#/.{"Positive"->"Cases","Death"->"Deaths"}&]];
	updateProgress[$covidprogessid, "Processing data"];
	timeseries = KeyTake[timeseries[All, KeyDrop["State"]], counties];
	If[!TrueQ[Length[timeseries]>0],Throw[Failure["nocountydata",<|"Message"->"No Counties matched your spec."|>]]];
	mintime = timeseries[Min, All, #["FirstDate"] &];
	updateProgress[$covidprogessid, "Totalling Metro Area"];
	timeseries[All, All, TimeSeries[TimeSeriesInsert[#, {mintime, 0}]["DatePath"], 
 			ResamplingMethod -> {"Interpolation", InterpolationOrder -> 0}] &]
]

interpretStates[l_List]:=Interpreter["USState"][l]
interpretStates[expr_]:=interpretStates[{expr}]


End[] 
EndPackage[]