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
- **1.1DataPrep_CombineSurveyData.R**: combine survey data   
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
      - **Output**: abundance_ind_Region.csv, abundance_ind_CA.csv
    - step 2: get the abundance estimates per grid location and compare to Step 1    
      - **Output**: Mod_Pred_Abundance_grid_Locs.csv 
    - step 3, Add season, Year, and the area (km2) of the Stratum, and save data
    - **Output:** AbundanceEstimates_GridCentriods_All.csv,AbundanceEstimates_GridCentriods_Reg.csv, AbundanceEstimates_GridCentriods_CA.csv 
<br>

  - **3.2Binned_density_plot.R**: *vast_fit_plot_spatial_kf_binned_new()*function to interpolate and map the predicted density (log+1) of grid centroids estimates,  over a regular grid. Plot two bins: before and after accelerated warming period (2005)  
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
      - *Output*: Reg_Proportions_TF.csv,  Reg_ProportionalDensity_TF.csv, CA_Proportions_TF.csv, CA_ProportionalDensity_TF.csv
    - STEP3: Plotting
      - plot 1: canada vs USA trends of estimated proportion of abundance and relative density across the time serie
      - plot 2: basic trends for each CA across the time series with the percent change noted in the legend
      - Plot 3: Plot 2 for relative density
      - Plot 4: Gain/loss maps  
    - STEP4: slopes, estimate LM
      - Fit a linear model predicting proportion as a function of Year, within each region/period grouping
      -Completed and plotted for Abundance, Proportion, and relative density 
<br> 

## Shift Analysis 
For each shift indicator, create data, fit LM, and plot 
<br> 

###  Trends in Abundance  
**4.1 Plot_Abundance_trends.R**: Using stratified abundance and standard error estimates (regional and per core area)
  - Plots estimated abundance time series 
  - Plots change in slope bevore v after accelerated warming period 
<br> 

### Centre of Gravity 
**5.1Centre_of Gravity.R**  
  - Uses the Abundance Estimates Grid Centriods data to calculate the  mean/median/Q5/Q95, longitude and latitide,  weighted by Abundance, for each year/season/grouping   
  - Plots and maps temporal trends in COG  
  - **Output:** seasonal_centroid_data_CA.csv, seasonal_centroid_data_regional.csv, seasonal_centroid_data.csv  
<br>  
 
**5.2Fit_LM_COG_PlotSlopes** 
  - Uses seasonal_centroid_data data, adds a field for period (before vs after accelerated warming)  
  - Perform lm on each group (region and CA) and extract coefficients, and filter on Year  
  - Plots: Rate of change in Centre of Gravity per year, by Time Period and Core Area or region   
  - *Output:* COGSlopeCI_Regional.csv, COGSlopeCI_CoreAreas.csv  
<br> 

### Distance from Hague 
**6.1Distance_From_Hague.R**: Calculate Distance from a fixed point on the hague line to the Centre of gravity (Mean, Median, Q5 and Q95) for timeserie and grouping using seasonal_centroid_data  
  1. Hague line and centriod data are turned to spatial points. A subset of the Hague line is also made to prevent Browns, Sable, CB, and Gully from estimating across land 
  2. *find_nearest_point():* function identifies the nearest hague_point to each centroid in the timeseries and creates a df that has a "closest" point for each year/season/grouping 
  3. *calculate_distances():* function created df by calculating the distance between each "closest" point and the corresponding centriod 
  4. **Output** for Mean, Median, Q5 and Q95 (by year/season/grouping) are joined into one dataframe: dist_hague_all_seasonal.csv, dist_hague_Reg_seasonal.csv, dist_hague_CA_seasonal.csv  

<br> 
**6.2Fit_LM_Distance_From_Hague_PlotSlopes.R**:  
 - Statistics that fall in the US (regional and CA) are transformed to have negative values (hague being zero) 
 -**Output:** DistHagCASpringTransformedforFig.csv  
 - Plot Distance from Hague line (km) per core area  
 - Perform lm on each group and extract coefficients, and plot 
   - Plot1: Core Areas DFH timeseries plots
   - Plot2: Core areas DFH rate of change (lm)
   - Plot : regional DFH rate of change (lm)
   - Plot : regional DFH timeseries plots
   - Plot : aggregated Regional DFH

# Perform lm on each group and extract coefficients----

### Range Edge 
### Deepening 
### Effective Area Occupied 

## Supplemental 
**Sup1install_INLA.R** 
**Sup2DataPlotst.R** 
- Plot 1: Map of Core areas 
- Plot 2: A look at the distribution of RV Survey data 

**Sup3TMB_Error_fix.R**

