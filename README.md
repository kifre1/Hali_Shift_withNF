---
title: "Hali_Shift_withNF"
author: "Kiyomi Ferguson "
date: "2025/04/08"
output: html_document
---

# Hali_Shift_withNF
Repository for Atlantic Halibut shift analysis about the Hague line, before and during a warming period, using Core Areas
<br> 

## Data Preparation  
Data from multiple RV surveys, and environmental covariates are combined into a single dataframe (Halibut_Catch_Covariates_Scaled_*date*.csv: Al11 includes the flemmish cap (NAFO 3M), Al14 does not...removed due do very sparse data challenging model fitting process, this can be re-visited late but because NF is not focus here, ok) 
- **1.1DataPrep_MergeSurveyData.R**: combine survey data   
  - **all_unique_towsAl4.rds**: a combined dataframe for each unique tow (longitude, latitude, trawl_id, season, year, survey, date, swept) 
  - **all_raw_halibut_catchAl4.rds**: a combined dataframe for Atlantic Halibut survey data at each unique tow 
  - **all_raw_halibut_catch_formattedAl4.rds**: reformatted the survey catch data, suited to VAST input requirements 
<br> 

- **1.2DataPrep_addBNAMcovariates.R**: BNAM surface and bottom temperature folders each contain annual folders with monthly temperature .mat files.  They are to be stacked into a single raster stack and then the respective values assigned to the survey catch data. 
  - *process_mat_to_raster_stack()*- This function processes individual .mat files and converts the data into a raster stack 
  - *process_all_mat_files()*- This function applies process_mat_to_raster_stack() to all .mat files in a  folder, and combines them into a single large raster stack. 
  - **all_raw_halibut_catch_with_covariates_Al4.csv**: Surface Temperature, Bottom Temperature, and Depth are extracted from the raster stacks and assigned to all_raw_halibut_catch_formattedAl4.rds based on the date and location. 
  -**Halibut_Catch_Covariates_Scaled_Al14.csv**: at April 14, 2025.  
    Covariate, removed outliers, scaled and to have a mean of 0 and standard deviation of 1. 
<br> 

## Run Model and Diagnostics  
- **2.1FitFullMod.R**: This is the original SDM based on "A species distribution modeling workflow to project changes in marine  species occurrence in the northwest Atlantic" - Gulf of Maine Research Institute: Integrated Systems Ecology  Laboratory   
  - https://gulfofmaine.github.io/sdm_workflow/docs/index.html#preface 
  - **Formula**: ~ season + bs(Depth, degree = 2, intercept = FALSE) + bs(Bottom temp, degree = 2, intercept = FALSE) + st(Surface temp, degree = 2, intercept = FALSE) 
  - **Data**NEFSC, DFO Maritimes, and DFO Newfoundland RV surveys combined using catchability formula (survey is treated as a factor )
    - Years 1990- 2023, Spring, Summer, Fall 
    - Strata: whole area, USA, Canada, and the 12 core areas, this allows the model to calculate some shift analysis statistics at each of these levels 
  - **Output**: *Null_mod_fit.rds, EnvOnly_mod_fit.rds, Sp_mod_fit.rds, SpST_mod_fit.rds, vast_samp_dat.csv*  
<br> 

- **2.1DiagnosticsValidataion.R**: Model diagnostics, evaluation and validation statistics  
  1. Default plots: For diagnostic purposes, model plotting function writs plots folder,but we need to do pull these functions apart later so that we can get at the indexed data 
  2. Co-variate effects and plots: plot the co-variate effects and then look at the shape/strength of the response curves for each linear predictor
  3. Deviance & AIC...how well does the model fit the data 
  4. Parameter Estimates: Pulled from the SD report and t test (Estimate / SE) to see which parameters are more likely to be statistically significant 
  5. AUC: measure model strength at distinguish between the positive and negative classes 
  6. Taylor diagram
  7. Plot random effects (Omega, Epsilon, R, P)...spatial
<br> 

## Prepare model output for Shift analysis  
Model output are huge .rds files that contain everything (input, estimates, indices, diagnostics...) so they need to be reorganized  
  - **3.1Data_prep.R**:  Prepare data for use in the development of shift indicator data.  Grouped by All, Nation, and Core Area
    - step 1: get and plot generated stratified abundance and standard error estimates  
    - step 2: get the abundance estimates per grid location and compare to Step 1  
    - step 3, Add season, Year, and the area (km2) of the Stratum, and save data
    - **Output:* AbundanceEstimates_GridCentriods_All.csv,AbundanceEstimates_GridCentriods_Reg.csv, AbundanceEstimates_GridCentriods_CA.csv 
<br>

  - **3.2Binned_density_plot.R**:   
    - text  
<br>

  - **3.3Regional_Proportions.R**: Preparing Estimated Abundance data and plotting the timeseries to compare the abundance trends of National and Core Area stratum. Using the generated indexed abundance data from get_vast_index_timeseries() becasue the standard errors are not available at scale of grid location 
    - STEP 1: get annual regional proportions (as percentages)
      - 1.1 general data prep
      - 1.2 Add the spatial area of the stratum to the df by joining the values generated in 3.1Data_prep.R by stratum name
      - 1.3 annual regional proportional abundance and density for each season and stratum grouping (National, Core Area)
        - proportion (abundance): total annual estimate (within stratum)/ total annual estimate across study area  
        - relative density: same as above but standardized by square area
        - *Output*: proportions_and_density_CA.csv, proportions_and_density_Regional.csv
    - STEP 2: Compare these data using means before vs during warming timeframes (1990-2005, 2006-2023) 
      - Calculate the mean proportion for each Region, season, and timeframe as well as the difference and percent change
      - *Output*: Reg_Proportions_TF.csv,  Reg_ProportionalDensity_TF.csv,CA_Proportions_TF.csv, CA_ProportionalDensity_TF.csv
    - STEP3: Plotting
      - plot 1: canada vs USA trends of estimated proportion of abundance and relative density across the time serie
      - plot 2: basic trends for each CA across the time series with the percent change noted in the legend
      - Plot 3: Plot 2 for relative density
      - Plot 4: Gain/loss maps  
    - STEP4: slopes, estimate LM
      - Fit a linear model predicting proportion as a function of Year, within each region/period groupimg


<br> 

## Shift Analysis 
For each shift indicator, create data, fit LM, and plot 
<br> 

###  Trends in Abundance  
**3.2Plot_Abundance_trends.R**: Using Regionally and per Core Area abundance and standard error estimates 

### Effective Area Occupied 
### Centre of Gravity 
### Distance from Hague 
### Range Edge 
### Deepening 

## Supplemental 
**Sup1install_INLA.R** 
**Sup2DataPlotst.R** 
- Plot 1: Map of Core areas 
- Plot 2: A look at the distribution of RV Survey data 

**Sup3TMB_Error_fix.R**

