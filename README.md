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
  - **all_raw_halibut_catch_formattedAl4.rds**: reformated the survey catch data, suited to VAST input requirements 
<br> 

- **1.2DataPrep_addBNAMcovariates.R**: BNAM surface and bottom temperature folders each contain annual folders with monthly temperature .mat files.  They are to be stacked into a single raster stack and then the respective values assigned to the survey catch data. 
  - *process_mat_to_raster_stack()*- This function processes individual .mat files and converts the data into a raster stack 
  - *process_all_mat_files()*- This function applies process_mat_to_raster_stack() to all .mat files in a  folder, and combines them into a single large raster stack. 
  - **all_raw_halibut_catch_with_covariates_Al4.csv**: Surface Temperature, Bottom Temperature, and Depth are extracted from the raster stacks and assigned to all_raw_halibut_catch_formattedAl4.rds based on the date and location. 
<br> 

## Run Model and Diagnostics  
- **2.1Fit_Model_March27.R**: This is the original SDM based on "A species distribution modeling workflow to project changes in marine  species occurrence in the northwest Atlantic" - Gulf of Maine Research Institute: Integrated Systems Ecology  Laboratory   
  - https://gulfofmaine.github.io/sdm_workflow/docs/index.html#preface 
  - **Formula**: ~ season + bs(Depth, degree = 2, intercept = FALSE) + bs(Bottom temp, degree = 2, intercept = FALSE) + st(Surface temp, degree = 2, intercept = FALSE) 
  - **Data**NEFSC, DFM Maritimes, and DFO Newfoundland RV surveys, combined using catchability formula (survey is treated as a factor )
    - Years 1990- 2023, Spring, Summer, Fall 
    - Strata: whole area, USA, Canada, and the 12 core areas, this allows the model to calculate some shift analysis statistics at each of these levels 
  - **Output**: *Null_mod_fit.rds, EnvOnly_mod_fit.rds, Sp_mod_fit.rds, SpST_mod_fit.rds, vast_samp_dat.csv*
<br> 
- **2.1Diagnlostis_ Validataion.R**: Model diagnostics, evaluation and validation statistics
  1. Default plots: For diagnostic purposes, model plotting function writs plots folder,but we need to do pull these functions apart later so that we can get at the indexed data 
  2. Covariate effects and plots: plot the covariate effects and then look at the shape/strength of the response curves for each linear predictor
  3. Deviance & AIC...how well does the model fit the data 
  4. Parameter Estimates: Pulled from the SD report and t test (Estimate / SE) to see which parameters are more likely to be statistically significant 
  5. AUC: measure model strength at distinguish between the positive and negative classes 
  6. Taylor diagram
  7. Plot random effects (Omega, Epsilon, R, P)...spatial
<br> 

## Prepare model output for Shift analysis
Model output are huge .rds files that contain everything (input, estimates, indices, diagnostics...) so they need to be reorganized  
- **3.1Data_prep.R**: 
- **3.2Binned_density_plot.R**: 
- **3.3Regional_Proportions.R**: 
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

