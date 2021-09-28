# Is cycling infrastructure in London safe and equitable? Evidence from the Cycling Infrastructure Database.

LINK TO PAPER

The below files show the data cleaning process for each of the 9 CID datasets.  The data cleansed files can be found in the data subfolder.   

[data_clean_CID_asl.R](data_clean_CID_asl.R) - data cleaning file for Advanced Stop Lines  

[data_clean_CID_crossings.R](data_clean_CID_crossings.R) - data cleaning file for cycle crossings  

[data_clean_CID_cyclelanestracks.R](data_clean_CID_cyclelanetracks.R) - data cleaning file for cycle lanes and tracks  

[data_clean_CID_parking.R](data_clean_CID_parking.R) - data cleaning file for cycle parking  

[data_clean_CID_restrictedpoints.R](data_clean_CID_restrictedpoints.R) - data cleaning file for restricted points  

[data_clean_CID_restrictedroutes.R](data_clean_CID_restrictedroutes.R) - data cleaning file for restricted routes  

[data_clean_CID_signage.R](data_clean_CID_signage.R) - data cleaning file for signage  

[data_clean_CID_signals.R](data_clean_CID_signals.R) - data cleaning file for cycle signals  

[data_clean_CID_trafficcalming.R](data_clean_CID_trafficcalming.R) - data cleaning file for traffic calming

  
This code get the estimated about of commuting cycling through each borough using the Propensity to Cycle Tool (based on the 2011 Census):

[get_pct_km_cycled.R](get_pct_km_cycled.R)  
  
  
This code generates the map of the safety-related infrastructure in the CID:
[visualise_all_assets.R](visualise_all_assets.R) 
![Maps of safety-related infrastructure in the CID.](all_assets_map_with_legend_v2.jpeg)

