(* Wolfram Language Package *)

BeginPackage["COVID19`"]

Begin["`Private`"] 

cumulativePlots[{cases_,casesopts_},{deaths_,deathsopts_}]:={
	updateProgress[$covidprogessid, "Cumulative Plots"];
	DateListLogPlot[cases, casesopts, PlotLabel -> "Cumulative (Log)"],
   	DateListLogPlot[deaths, deathsopts, PlotLabel -> "Cumulative (Log)"]
  	}
  	
Clear[population]

population[counties_,Key[k_]]:=population[counties,k]

population[counties_,ent_Entity] := ent["Population"]
population[counties_,"Metro Area Total"|"Combined Total"|$entitytotallabel] := (
	updateProgress[$covidprogessid, "Finding Populations"];
	Total[#["Population"]& /@ counties])

perCapitaPlots[{cases_,casesopts_},{deaths_,deathsopts_},counties_]:={
	updateProgress[$covidprogessid, "Per Capita Plots"];
	DateListPlot[cases[MapIndexed[1000*#1/QuantityMagnitude[population[counties,#2[[1]]]] &]], casesopts, PlotLabel -> "Cumulative per 1000 pop"],
   	DateListPlot[deaths[MapIndexed[1000*#1/QuantityMagnitude[population[counties,#2[[1]]]] &]],deathsopts, PlotLabel -> "Cumulative per 1000 pop"]
  	}
   
differencesPlots[{cases_,casesopts_},{deaths_,deathsopts_}, smoothing_]:={
	updateProgress[$covidprogessid, "Differences Plots"];
	DateListPlot[cases[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /* Differences], 
		casesopts, PlotLabel ->"New Cases (" <> ToString[smoothing] <> " day average)"],
   	DateListPlot[deaths[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /* Differences], 
   		deathsopts, PlotLabel ->"New Deaths (" <> ToString[smoothing] <> " day average)"]
  	}

differencesPerCapitaPlots[{cases_,casesopts_},{deaths_,deathsopts_}, smoothing_,counties_]:={
	updateProgress[$covidprogessid, "Differences Plots"];
	DateListPlot[cases[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /* Differences][MapIndexed[1000*#1/QuantityMagnitude[population[counties,#2[[1]]]] &]], 
		casesopts, PlotLabel ->"New Cases per 1000 pop (" <> ToString[smoothing] <> " day average)"],
   	DateListPlot[deaths[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /* Differences][MapIndexed[1000*#1/QuantityMagnitude[population[counties,#2[[1]]]] &]], 
   		deathsopts, PlotLabel ->"New Deaths per 1000 pop (" <> ToString[smoothing] <> " day average)"]
  	}
  	 	
ratioPlots[{cases_,casesopts_},{deaths_,deathsopts_}, smoothing_]:={
	updateProgress[$covidprogessid, "Ratio Plots"];
	DateListPlot[cases[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /*Ratios], casesopts, 
    PlotLabel -> "Growth Ratio (" <> ToString[smoothing] <> " day average)"],
   	DateListPlot[deaths[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /* Ratios], deathsopts, 
    PlotLabel -> "Growth Ratio (" <> ToString[smoothing] <> " day average)"]
  	}
 
  	 	
movingRatioPlots[{cases_,casesopts_},{deaths_,deathsopts_}, smoothing_]:={
	updateProgress[$covidprogessid, "Moving Ratio Plots"];
	DateListPlot[cases[Select[Length[#["Path"]]>smoothing*4&], 
		
		MovingAverage[MovingMap[(#[[-1]] - #[[1]])/(#[[-2]]- #[[1]])&, 
   		#, Quantity[3*smoothing, "Days"]], Quantity[smoothing, "Days"]]&], casesopts, 
    	PlotLabel -> "Moving Growth Ratio (" <> ToString[3*smoothing] <> " day window, "<> ToString[smoothing]<>" day average)"],
    DateListPlot[deaths[Select[Length[#["Path"]]>smoothing*4&], 
		MovingAverage[MovingMap[(#[[-1]] - #[[1]])/(#[[-2]]- #[[1]])&, 
   		#, Quantity[3*smoothing, "Days"]], Quantity[smoothing, "Days"]]&], deathsopts, 
    	PlotLabel -> "Moving Growth Ratio (" <> ToString[3*smoothing] <> " day window, "<> ToString[smoothing]<>" day average)"]
    
  	}
  		
  		 		
alignedGrowthPlots[{alignedcases_,casesopts_},{aligneddeaths_,deathsopts_}, {mincases_,mindeaths_}]:={
	updateProgress[$covidprogessid, "Aligned Growth Plots"];
	growthPlotWithTrendLines[alignedcases,"Cases", Sequence @@ Normal@KeyDrop[casesopts, PlotRange], 
   		PlotLabel -> "Aligned Cumulative Plot (Log vs Days since "<>ResourceFunction["OrdinalNumberString"][mincases]<>" case)"],
   	growthPlotWithTrendLines[aligneddeaths,"Deaths", Sequence @@ Normal@KeyDrop[deathsopts, PlotRange], 
    	PlotLabel -> "Aligned Cumulative Plot (Log vs Days since "<>ResourceFunction["OrdinalNumberString"][mindeaths]<>" death)"]
  	}

  	
End[] 
EndPackage[]