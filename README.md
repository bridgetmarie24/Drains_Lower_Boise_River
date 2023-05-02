# Analysis of Drain Flows in the Lower Boise River Basin

## Summary

The goals of the scripts in this repository are to 1) quantify changes in annual flow volumes from 1987 to 2020 for the 15 main drains in the Lower Boise River Basin and 2) understand the impacts of urbanization, climate, and canal flows on the drain flows through time. 

## Steps to running drain analysis: 

Python scripts are data preprocessing while R scripts are mainly modeling and figure outputs. You will need to run the dataretrieval.R script to get data for flow values before running the drain_flows.ipynb script. 

## *Python*
1) **drain_flows.ipynb**: Sums irrigation season flows based on start and end dates from diversion analysis
2) The following 2 scripts can be run in any order:
 - **climate_data_extract.ipynb**: Calculates annual zonal stats for each drain watershed from Daymet and SSEBop
 - **landcover_calculations.ipynb**: Calculates annual percent for each land use from LCMAP data. 
3) **Compile_data.ipynb**: Merges flow, climate, and land use annual stats together *** cannot use direct output from this in R. It merges Mason Creek and Mason Drain wrong. This was manually corrected. 

## *R*
1) **dataretrieval.R**: This gets all the USGS flow data for the drains. You will need to do this before you sum the flows in the first Python script. 
2) **drain_preprocessing.R**: This script standardizes predictor variables, add canal flow values for each drain each year, and does Mann Kendall tests. It also checks for correlations between variables. The output from here can go into the mixed_model_borah script. 
3) **mixed_model_borah.R**: This runs the Generalized Linear Mixed Effects Model. 
4) **figures.R**: Creates figures from GLMM

## Contact Information

For questions or comments regarding this analysis, please email Bridget Bittmann at bridgetbittmann@u.boisestate.edu.
