(* Wolfram Language Package *)

BeginPackage["COVID19`"]

COVID19`USCountyGrowthPlot

Begin["`Private`"] 

COVID19`USCountyGrowthPlot[args___]:=Block[{$covidprogessid=CreateUUID[]},
	updateProgress[$covidprogessid, "Choosing Counties"];
	Catch[usCountyGrowthPlot[args]]
]

Options[COVID19`USCountyGrowthPlot]=Options[usCountyGrowthPlot]={"UpdateData"->False,"SmoothingDays"->7,"MinimumCases"->50,"MinimumDeaths"->10,"MetroName"->""};

Clear[usCountyGrowthPlot];

usCountyGrowthPlot[opts:OptionsPattern[]]:=usCountyGrowthPlot[Automatic,opts]

usCountyGrowthPlot[counties:{_Entity...}|Automatic,opts:OptionsPattern[]]:=(
	updateProgress[$covidprogessid, "Creating Plot"];
	With[{data=getCountyTimeLines[counties,opts]},
		Block[{$maxTrendLineHeight=2.*Log[2, data[Max, Max]]},
		DateListLogPlot[data, opts,countygrowthplotopts[data[1, #["LastTime"] - #["FirstTime"] &]*.7]]
		]
	]
	)


usCountyGrowthPlot[l_List,opts:OptionsPattern[]]:=With[{cs=Interpreter["USCounty"][l]},
	usCountyGrowthPlot[Cases[cs,_Entity,{1}],opts]
]
usCountyGrowthPlot[expr_,opts:OptionsPattern[]]:=usCountyGrowthPlot[{expr},opts]/;!ListQ[expr]

usCountyGrowthPlot[___]:=$Failed

countygrowthplotopts[days_]:=Sequence@@{FrameTicks->{Automatic, Automatic},PlotRange->{Automatic,{5,Automatic}},
	PlotLegends->None,PlotLabels->Placed[Automatic,Right,StringDelete[CommonName[#],", United States"]&],
	Epilog->{Dashed,makeline[{"Deaths",days},#]&/@{3,5,10,20}},ImageSize->1400};

getCountyTimeLines[counties_, OptionsPattern[usCountyGrowthPlot]]:=Module[{timeseries},
	
	updateProgress[$covidprogessid, "Getting NY Times Data"];
	timeseries = If[TrueQ[OptionValue["UpdateData"]],
		COVID19`NYTimesData["USCountiesTimeSeries","New"],
		COVID19`NYTimesData["USCountiesTimeSeries"]
	];
	updateProgress[$covidprogessid, "Processing Data"];
	timeseries[All,
		ResourceFunction["TimeSeriesSelect"][#Deaths,#2>OptionValue["MinimumDeaths"]&]&
		][selectCountyFunc[counties,OptionValue["SmoothingDays"]]/*ResourceFunction["TimeSeriesAlign"]][All,tozerotime]
]

selectCountyFunc[Automatic, smoothing_]:=(TakeLargestBy[#["PathLength"]&,UpTo[20]]/*Select[#["PathLength"]>smoothing&])

selectCountyFunc[l_List,_]:=KeyTake[l]
selectCountyFunc[___]:=Throw[$Failed]





growthPlotWithTrendLines[data_, type_, opts___]:=
		Block[{$maxTrendLineHeight=Log[2, data[Max, Max]]},
			ListLogPlot[data, opts, Joined -> True, 
				Epilog -> {Dashed, makeline[{type,data[1, #["LastTime"] - #["FirstTime"] &]*.9}, #] & /@ {3, 5, 10,20}}]
		]

tozerotime[ts_] := TimeSeries[With[{first = #[[1, 1]]},
     MapAt[(# - first)/86400 &, #, {All, 1}]] &[ts["Path"]]]
     
tozerodays[ts_] := TimeSeries[With[{first = #[[1, 1]]},
     MapAt[(# - first) &, #, {All, 1}]] &[ts["Path"]]]
     
$maxTrendLineHeight=7;

linedays[{"Deaths",n_}] = n;
linedays["Deaths"] = 12;
linedays[_] = 20;
startlinevalue["Deaths"|{"Deaths",_}] := 2.4;
startlinevalue[_] := 4;

makeline[t_, a_] := With[{sl = startlinevalue[t], ld = linedays[t]},
  N@{Darker[ColorData["TemperatureMap"][3/4 + 4/(12+a)], .2],
    Line[{{0, sl}, {ld, sl + (ld/a)}}],
    Text["Doubles every " <> ToString[a] <> " days", 
     textlocation[sl, ld, a, $maxTrendLineHeight]]
    }]


textlocation[sl_, ld_, a_,maxheight_] := If[2^(ld/a) < maxheight, {ld, sl + (ld/a)},
  With[{d = 
     First[\[FormalX] /. 
       NSolve[2^(\[FormalX]/a) == maxheight && \[FormalX] > 
          0, \[FormalX]]]},
   {d - 2, sl + N@Log[2, maxheight]}
   ]
  ]
End[] 
EndPackage[]