(* Wolfram Language Package *)

BeginPackage["COVID19`"]

COVID19`DailyPeakPlot

COVID19`CountryPlotsAndData
Begin["`Private`"] 

COVID19`CountryPlotsAndData[args___]:=Block[{$covidprogessid=CreateUUID[],
	$CovidEntityType="Country"},
	updateProgress[$covidprogessid, "Finding Countries"];
	Catch[countryData[args]]
]

Options[COVID19`CountryPlotsAndData]=Options[countryData]={"UpdateData"->False,"SmoothingDays"->7,"MinimumCases"->50,"MinimumDeaths"->10,"Name"->""};

countryData[countries:{_Entity..}|All|Automatic,opts:OptionsPattern[]]:=Module[
	{timeseries,mintime,maxtime, cases, deaths,countrylist},
	timeseries=countryTimelines[countries,opts];
	countrylist=Normal@Keys[timeseries];
	mintime = timeseries[Min, All, #["FirstDate"] &];
	maxtime = timeseries[Max, All, #["LastDate"] &];
	updateProgress[$covidprogessid, "Totalling Countries"];
	timeseries = timeseries[All, All, TimeSeries[TimeSeriesInsert[#, {mintime, 0}]["DatePath"], 
 			ResamplingMethod -> {"Interpolation", InterpolationOrder -> 0}] &];
	timeseries = Prepend[timeseries, "Combined Total" -> Normal[timeseries[Total]]];
	
	updateProgress[$covidprogessid, "Formatting Time Series"];
	cases = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Cases, #2 > OptionValue["MinimumCases"] &] &][Select[#["PathLength"] > OptionValue["SmoothingDays"] &]];
	deaths = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Deaths, #2 > OptionValue["MinimumDeaths"] &] &][Select[#["PathLength"] > OptionValue["SmoothingDays"] &]];
	updateProgress[$covidprogessid, "Creating Timeline Grid"];
	{metroPlotGrid[
		countrylist,
		cases,
		deaths,
		{mintime,maxtime},
		OptionValue["Name"]<>"COVID-19 Timelines",
		opts
	]
	,
	updateProgress[$covidprogessid, "Creating Data Table"];
	Labeled[tableData[timeseries[All, {"Cases", "Deaths"}]],DateString[maxtime, "Date"], Top]
	}
]

countryData[area_, opts:OptionsPattern[]]:=Block[{$covidprogessid=CreateUUID[]},
	updateProgress[$covidprogessid, "Finding Counties"];
	countryData[ChooseCounties[area],opts]
]


countryData[___]:=$Failed


Options[COVID19`DailyPeakPlot]=Join[Options[COVID19`CountryPlotsAndData],{"MaximumPostPeakRatio"->.8,"DeathValueMagnification"->5}]


COVID19`DailyPeakPlot[args___]:=Block[{$covidprogessid=CreateUUID[],
	$CovidEntityType="Country"},
	updateProgress[$covidprogessid, "Retrieving Data"];
	Catch[dailyPeakPlot[args]]
]

dailyPeakPlot[countries:{_Entity..}|All|Automatic,opts:OptionsPattern[COVID19`DailyPeakPlot]]:=Module[
	{diffs,pastpeak,maxdifftimes,p1,p2},
	diffs=countryTimelines[countries,opts][All, All, (MovingAverage[#, Quantity[OptionValue["SmoothingDays"], "Days"]] &) /* Differences];
	pastpeak = diffs[Select[#Deaths["LastValue"]/Max[#Deaths] < OptionValue["MaximumPostPeakRatio"] &]];
	maxdifftimes = pastpeak[All, All, TakeLargestBy[#["Path"], Last, 1][[1]] &];
	p1 = DateListPlot[
     Labeled[#[[2]], Style[N@#[[1, 2]], Darker@Blue], 
      DateObject@#[[1, 1]]], PlotRange -> Full, 
     PlotRangePadding -> Scaled[.1]] & /@ Merge[{Normal@maxdifftimes[All, "Cases"],
     Normal@pastpeak[All, "Cases"]}, Identity];
	p2 = DateListPlot[
     Labeled[OptionValue["DeathValueMagnification"]*#[[2]], Style[N@#[[1, 2]], Darker@Green], 
      DateObject@#[[1, 1]]], PlotRange -> Full, 
     PlotStyle -> Green] & /@ Merge[{Normal@maxdifftimes[All, "Deaths"],Normal@pastpeak[All, "Deaths"]}, Identity];
	Merge[{p1, p2, Normal@maxdifftimes[All, UnitConvert[Quantity[#Deaths[[1]] - #Cases[[1]], "Seconds"], "Days"] &]}, Labeled[Show[Most[#]], Last[#], Right] &]
]

countryTimelines[countries_,opts:OptionsPattern[COVID19`DailyPeakPlot]]:=Block[{timeseries},
	
	updateProgress[$covidprogessid, "Getting Data From Wolfram Data Repository"];
	timeseries = If[TrueQ[OptionValue["UpdateData"]],
		COVID19`EpidemicData["WorldCountries","New"],
		COVID19`EpidemicData["WorldCountries"]
	];
	
	updateProgress[$covidprogessid, "Processing data"];
	
	timeseries = timeseries[Association, #Country -> 
    Association@@{
    	"Cases"->ResourceFunction["TimeSeriesSelect"][#ConfirmedCases, #2 > OptionValue["MinimumCases"] &],
    	"Deaths"->ResourceFunction["TimeSeriesSelect"][#Deaths, #2 > OptionValue["MinimumDeaths"] &]
    }&];
	Which[ListQ[countries],
		timeseries = KeyTake[timeseries, countries];
		If[!TrueQ[Length[timeseries]>0],Throw[Failure["nocountydata",<|"Message"->"No Countries matched your spec."|>]]],
		countries===Automatic,
		timeseries=timeseries[TakeLargestBy[Max[#Cases]&,9]];
	];
	timeseries
]

End[] 
EndPackage[]