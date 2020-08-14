(* Wolfram Language Package *)

BeginPackage["COVID19`"]
Begin["`Private`"] 


prenerSTLHospitalData[]:=With[
	{raw=Import["https://raw.githubusercontent.com/slu-openGIS/MO_HEALTH_Covid_Tracking/master/data/metro/stl_hospital.csv", "Dataset", HeaderLines -> 1]},
	If[Head[raw]===Dataset,
		stlHospitalMaps[stlHospitalData[raw]]
		,
		$Failed
	]
	
]

stlHospitalData[raw_]:=(raw[All, {"report_date" -> DateObject}] /. "NA" -> Missing[])[AssociationMap[
  Function[key, 
   TimeSeries[
     Lookup[#, key], {Lookup[#, "report_date"]}] &], {"new_in_pt", 
   "in_pt", "icu", "vent", "discharge"}]]
stlHospitalMaps[ds_]:=Normal@ds[
  KeyValueMap[
   DateListPlot[MovingAverage[#2, Quantity[7, "Days"]], 
     PlotRange -> All, GridLines->{None,Automatic},PlotLabel -> prenerKeyName[#1],ImageSize->300] &]]
   
prenerKeyName["new_in_pt"]:="New Inpatients"
prenerKeyName["in_pt"]:="Current Inpatients"
prenerKeyName["vent"]:="On Ventilator"
prenerKeyName["discharge"]:="Total Discharged"
prenerKeyName[expr_]:=ToUpperCase[expr]
End[] 
EndPackage[]