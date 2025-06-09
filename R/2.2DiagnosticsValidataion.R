
#####
# Model diagnostics, evaluation and validation statistics, mostly based on: https://github.com/aallyn/TargetsSDM/blob/main/Vignette.Rmd
#####

# So d_i would be predicted density at that observation as an example. 
#Then for the mesh and extrapolation grid, you will have the same points per year and 
#season because the mesh and grid are constant. Those are indexed as “g” 
#As an example, d_gct is the predicted density at a given grid location 
#(g) for a given class (c) at a given time (t). 
R.version
packageVersion("TMB")
packageVersion("INLA")
packageVersion("VAST")

####
library(here)
library(VAST)
library(splines)
library(tidyverse)
library(lubridate)
library(sf)
library(raster)
library(googledrive)
library(ggforce)
library(patchwork)
library(DHARMa)
library(forecast)
library(tibble)

# Andrew's Functions, I cloned his directory here: C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/
source(here::here("R/VAST_functions/dfo_functions.R"))
source(here::here("R/VAST_functions/nmfs_functions.R"))
source(here::here("R/VAST_functions/combo_functions.R"))
source(here::here("R/VAST_functions/enhance_r_funcs.R"))
source(here::here("R/VAST_functions/vast_functions.R"))
source(here::here("R/VAST_functions/covariate_functions.R"))
source(here::here("R/VAST_functions/project.fit_model_aja.R"))
source(here::here("R/VAST_functions/DHARMa utilities.R"))
source(here::here("R/VAST_functions/SDM_PredValidation_Functions.R"))#PresenceAbsence library is gone so this doesn't work anymore- 
source(here::here("R/VAST_functions/vast_function_edits.R"))
source(here::here("R/VAST_functions/vast_plotting_functions.R"))

#####
# Model diagnostics, evaluation and validation statistics
#1. Default plots
#2. Covariate effects (+plots)
#3. Deviance* & AIC
#4. Parameter Estimates
#5. AUC
#6. Taylor 
#7. Plot random effects (Omega, Epsilon, R, P)
#####

#reopen each model output...we are going to compare them 
#files are too large for GIT so having to read them in from my comp
Hali_SpSt<- readRDS( here::here("2025-04-23/Halibut_BC/SpST_mod_fit.rds")) 
Hali_Null<- readRDS( here::here("2025-04-23/Halibut_BC/Null_mod_fit.rds")) 
Hali_Env<- readRDS( here::here("2025-04-23/Halibut_BC/EnvOnly_mod_fit.rds")) 
Hali_Sp<- readRDS( here::here("2025-04-23/Halibut_BC/Sp_mod_fit.rds"))  
vast_sample_data<- read.csv(here::here("2025-04-23/Output/vast_samp_dat.csv"), header=T)#written mid model-setup 


Hali_SpSt$Report$jnll
Hali_SpSt$Report$deviance #is NaN
print(Hali_SpSt$parameter_estimates$objective)
print(Hali_SpSt$parameter_estimates)

#I can compute deviance from the reported objective using:
#deviance = 2 * fit$parameter_estimates$objective
# but first double check that the model converged and that the NaN is coming from the report stage, not the optimization itself
#check convergence
Hali_SpSt$parameter_estimates$Convergence_check # "There is no evidence that the model is not converged"
print(Hali_SpSt$parameter_estimates$diagnostics)#Gradient should be < 0.001, no NAS or inf in MLE
Hali_SpSt$parameter_estimates$SD# not null
Report <- Hali_SpSt$tmb_list$Obj$report()
Report$deviance#If this is NAN the problem is not with optimization, but rather in how VAST is reporting outputs.


#1. Default plots----
#model plotting function writs plots to your folder,
#but we need to do pull these functions apart later so that we can get at the indexed data for the shift analysis
setwd(here::here("2025-04-23/Output/Plot/SpSt"))
plot(Hali_SpSt)
setwd(here::here("2025-04-23/Output/Plot/Sp"))
plot(Hali_Sp)
setwd(here::here("2025-04-23/Output/Plot/Env"))
plot(Hali_Env)
setwd(here::here("2025-04-23/Output/Plot/Null"))
plot(Hali_Null)


#2. Covariate effects (+plots)----
#plot the covariate effects and then look at the shape/strength of the response curves
#season is in the model as a fixed effect.
#columns are the covariates and then the rows are the linear predictors: 
#X1 is presence/absence- probability of positive catch, 
#X2 abundance...catch, given a positive X1
out_dir<- here::here("2025-04-23/Output/Plot/ValidationData/")

#2.0. get the covariate effects data
cov_effs_Env<- get_vast_covariate_effects(vast_fit = Hali_Env, params_plot = c("Depth_sc", "SST_monthly_sc", "BT_monthly_sc"), params_plot_levels = 100, effects_pad_values = c(1), nice_category_names = "Halibut_Env", out_dir = out_dir)
cov_effs_Sp<- get_vast_covariate_effects(vast_fit = Hali_Sp, params_plot = c("Depth_sc", "SST_monthly_sc", "BT_monthly_sc"), params_plot_levels = 100, effects_pad_values = c(1), nice_category_names = "Halibut_Sp", out_dir = out_dir)
cov_effs_SpSt<- get_vast_covariate_effects(vast_fit = Hali_SpSt, params_plot = c("Depth_sc", "SST_monthly_sc", "BT_monthly_sc"), params_plot_levels = 100, effects_pad_values = c(1), nice_category_names = "Halibut_SpSt", out_dir = out_dir)

#Because we scaled the covariates, these would be plotted more meaningfully after unscaling
#unscaling function:
unscale <- function(scaled_x, original_mean, original_sd) {
  (scaled_x * original_sd) + original_mean
}

#2.1.get the data to populate: bring in the data that were used in the model and re-scale the raw covariate columns  
OGData <- read.csv(here::here("Data/Derived/Halibut_Catch_Covariates_Scaled_Al14.csv"))
Depth_scaled_obj <- scale(OGData$Depth)
BT_monthly_scaled_obj <- scale(OGData$BT_monthly)
SST_monthly_scaled_obj <- scale(OGData$SST_monthly)

#2.2.so that I can get the centre (Mean) and the scale (SD)
Depth_mean <- attr(Depth_scaled_obj, "scaled:center")
Depth_sd <- attr(Depth_scaled_obj, "scaled:scale")
BT_monthly_mean <- attr(BT_monthly_scaled_obj, "scaled:center")
BT_monthly_sd <- attr(BT_monthly_scaled_obj, "scaled:scale")
SST_monthly_mean <- attr(SST_monthly_scaled_obj, "scaled:center")
SST_monthly_sd <- attr(SST_monthly_scaled_obj, "scaled:scale")

#2.3.Put them into vectors
means <- c(
  Depth = Depth_mean,
  SST_monthly = SST_monthly_mean,
  BT_monthly = BT_monthly_mean
)
sds <- c(
  Depth = Depth_sd,
  SST_monthly = SST_monthly_sd,
  BT_monthly = BT_monthly_sd
)

#2.4. use the unscale function on the output from get_vast_covariate_effects()
cov_effs_Env <- cov_effs_Env %>%
  mutate(Real_Value = case_when(
      Covariate == "Depth_sc" ~ unscale(Value, means["Depth"], sds["Depth"]),
      Covariate == "SST_monthly_sc" ~ unscale(Value, means["SST_monthly"], sds["SST_monthly"]),
      Covariate == "BT_monthly_sc" ~ unscale(Value, means["BT_monthly"], sds["BT_monthly"]),
      TRUE ~ Value
    ),
    Covariate = str_replace(Covariate, "_sc", "")  # Remove "_sc" from Covariate names so that they will plot properly 
  )

cov_effs_Sp <- cov_effs_Sp %>%
  mutate(Real_Value = case_when(
      Covariate == "Depth_sc" ~ unscale(Value, means["Depth"], sds["Depth"]),
      Covariate == "SST_monthly_sc" ~ unscale(Value, means["SST_monthly"], sds["SST_monthly"]),
      Covariate == "BT_monthly_sc" ~ unscale(Value, means["BT_monthly"], sds["BT_monthly"]),
      TRUE ~ Value
    ),
    Covariate = str_replace(Covariate, "_sc", "")  # Remove "_sc" from Covariate names
  )

cov_effs_SpSt <- cov_effs_SpSt %>%
  mutate(Real_Value = case_when(
    Covariate == "Depth_sc" ~ unscale(Value, means["Depth"], sds["Depth"]),
    Covariate == "SST_monthly_sc" ~ unscale(Value, means["SST_monthly"], sds["SST_monthly"]),
    Covariate == "BT_monthly_sc" ~ unscale(Value, means["BT_monthly"], sds["BT_monthly"]),
    TRUE ~ Value
  ),
  Covariate = str_replace(Covariate, "_sc", "")  # Remove "_sc" from Covariate names
  )
#2.5. make sure that this worked ok (check that the values are in a similar range to the original) 
# Depth
range(cov_effs_SpSt$Real_Value[cov_effs_SpSt$Covariate == "Depth"])
range(Hali_SpSt$effects$covariate_data_full$Depth)
# SST_monthly
range(cov_effs_SpSt$Real_Value[cov_effs_SpSt$Covariate == "SST_monthly"])
range(Hali_SpSt$effects$covariate_data_full$SST_monthly)
# BT_monthly
range(cov_effs_SpSt$Real_Value[cov_effs_SpSt$Covariate == "BT_monthly"])
range(Hali_SpSt$effects$covariate_data_full$BT_monthly)
#good 

str(Hali_SpSt$covariate_data)
str(cov_effs_SpSt)

#2.6 Tables: Real_Value at which each covariate has its max effect
#Probability of presence 
cov_effs_SpSt %>%
  filter(Lin_pred == "X1") %>%
  mutate(prob = plogis(fit)) %>%
  group_by(Covariate) %>%
  slice_max(prob, n = 1)
#    fit  se    lower upper Lin_pred  Covariate   Value   Real_Value   prob
#1  3.34 0.534  2.29  4.39   X1       BT_monthly   0.8    6.34        0.966
#2  3.42 0.552  2.33  4.50   X1       Depth        1.73   576.        0.968
#3  3.61 0.603  2.43  4.79   X1       SST_monthly  3.04   25.9        0.974

#99% of max 
cov_effs_SpSt %>%
  filter(Lin_pred == "X1") %>%
  mutate(prob = plogis(fit)) %>%
  group_by(Covariate) %>%
  mutate(prob_pct = prob / max(prob)) %>%
  filter(prob_pct >= 0.99) %>%
  summarise(
    min_val = min(Real_Value),
    max_val = max(Real_Value),
    max_prob = max(prob)
  )
#Covariate      min_val max_val max_prob
#1 BT_monthly   4.11    8.37    0.966
#2 Depth        252.    900.    0.968
#3 SST_monthly  13.1    25.9    0.974

#positive catch rate
cov_effs_SpSt %>%
  filter(Lin_pred == "X2") %>%
  mutate(prob = plogis(fit)) %>%
  group_by(Covariate) %>%
  slice_max(prob, n = 1)
#    fit     se   lower upper  Lin_pred Covariate    Value  Real_Value  prob
#1  -0.427 0.390 -1.19  0.337   X2       BT_monthly   0.426    5.23    0.395
#2  1.81   1.15  -0.454 4.07    X2       Depth        8.53     2014.   0.859
#3  0.286  0.350 -0.400 0.972   X2       SST_monthly -1.61    -1.61    0.571
cov_effs_SpSt %>%
  filter(Lin_pred == "X2") %>%
  mutate(prob = plogis(fit)) %>%
  group_by(Covariate) %>%
  mutate(prob_pct = prob / max(prob)) %>%
  filter(prob_pct >= 0.99) %>%
  summarise(
    min_val = min(Real_Value),
    max_val = max(Real_Value),
    max_prob = max(prob)
  )
#Covariate   min_val max_val max_prob
#1 BT_monthly  3.18    7.45    0.395
#2 Depth       1995.   2014.   0.859
#3 SST_monthly -1.61   -1.38   0.571
#remove depth outliers?
cov_effs_SpSt_clipped <- cov_effs_SpSt %>%
  filter(Covariate != "Depth" |
           (Covariate == "Depth" & Real_Value >= 20 & Real_Value <= 1300))

cov_effs_SpSt_clipped %>%
  filter(Lin_pred == "X2") %>%
  mutate(prob = plogis(fit)) %>%
  group_by(Covariate) %>%
  mutate(prob_pct = prob / max(prob)) %>%
  filter(prob_pct >= 0.95) %>%
  summarise(
    min_val = min(Real_Value),
    max_val = max(Real_Value),
    max_prob = max(prob)
  )



#2.6. Plot Real instead of scaled values(altered original function) 
source(here::here("R/VAST_functions/kf_vast_function_edits.R"))
plot_vast_covariate_effects_kf(vast_covariate_effects = cov_effs_Env, vast_fit = Hali_Env, nice_category_names = "Halibut_Env", out_dir = out_dir)
plot_vast_covariate_effects_kf2(vast_covariate_effects = cov_effs_Sp, vast_fit = Hali_Sp, nice_category_names = "Halibut_Sp", out_dir = out_dir)
plot_vast_covariate_effects_kf(vast_covariate_effects = cov_effs_SpSt, vast_fit = Hali_SpSt, nice_category_names = "Halibut_SpSt", out_dir = out_dir)


#3. AIC----
#TMBAIC for comparing models 
# if you get a "TMBconfig" error, run Sup3TMB_Error_Fix.R first
#skip deviance and instead do the "new" marginal AIC calculation 
#(https://rdrr.io/github/James-Thorson/VAST/src/R/TMBAIC.R) 
#https://github.com/kifre1/Hali_Shift_withNF/blob/main/R/Sup5ReducedModel.R 

TMBAIC=function(opt, p=2, n=Inf){
  k = length(opt[["par"]])
  if( all(c("par","objective") %in% names(opt)) ) negloglike = opt[["objective"]]
  if( all(c("par","value") %in% names(opt)) ) negloglike = opt[["value"]]
  Return = p*k + 2*negloglike + 2*k*(k+1)/(n-k-1)
  return( Return )
}

fit_opt_Null <- nlminb(Hali_Null$tmb_list$Obj$par, Hali_Null$tmb_list$Obj$fn, Hali_Null$tmb_list$Obj$gr)
fit_opt_Env <- nlminb(Hali_Env$tmb_list$Obj$par, Hali_Env$tmb_list$Obj$fn, Hali_Env$tmb_list$Obj$gr)
fit_opt_Sp <- nlminb(Hali_Sp$tmb_list$Obj$par, Hali_Sp$tmb_list$Obj$fn, Hali_Sp$tmb_list$Obj$gr)
fit_opt_SpSt <- nlminb(Hali_SpSt$tmb_list$Obj$par, Hali_SpSt$tmb_list$Obj$fn, Hali_SpSt$tmb_list$Obj$gr)

#AIC-  we want our model to effectively explain the data while avoiding overfitting.
# Lower AIC, indicates a relatively better balance between goodness of fit and model complexity 
TMBAIC(fit_opt_Null)#31481.99
TMBAIC(fit_opt_Env)#29035.9
TMBAIC(fit_opt_Sp)#24793.84
TMBAIC(fit_opt_SpSt)# 24135.08

# 4. Parameter Estimates----
# For the fixed effect parameters, we can get their MLE as well as standard errors.
# Parameter estimates and standard errors..omega is spatial, epsilon is spatio-temporal
Hali_SpSt$parameter_estimates$SD
#From the sd report, I did a t test (Estimate / SE) to see which parameters are more likely to be statistically significant (higher vals)  
#Also calculated magnitude of the standard errors (% difference) to see which parameters had better precision (lower vals) ...related 

#these ones seemed statistically significant
#Omega explains how much spatial correlation exists between observations.  Higher estimates indicate that there is greater spatial dependence
L_omega1_z #probability of presence,
L_omega2_z#positive catch rates,
#Epsilon explains spatio-temporal variability: how both space and time interact in the model .
L_epsilon1_z#probability of presence,
L_epsilon2_z#positive catch rates

Beta_mean1_c#spatial structure of fixed effects
Beta_rho2_f #temporal correlation in fixed effects, each year’s pattern is correlated with the previous

#LogKappa reflects the range of correlation or spatial smoothness 
logkappa1<- -4.59410320
logkappa2<- -2.97457159
kappa1<- exp(logkappa1)#0.01
kappa2<- exp(logkappa2)#0.05
range1 <- sqrt(8) / kappa1 #279.7298, Small Kappa suggests a large spatial range, observations remain spatially correlated up to 280 units (km).
range2 <- sqrt(8) / kappa2 #55.38409, Larger Kappa, correlation decays faster for this parameter at around 55 units (km) 


# 5. AUC----
# Get the observation and prediction dataframe (to the survey points not the grid)
#the function defaults to renaming the predicted variable Biomass but we did this on abundance so just keep this in mind

out_dir<- here::here("2025-04-23/Output/Plot/ValidationData/")

obs_pred0<- vast_get_point_preds(vast_fit = Hali_SpSt, use_PredTF_only = FALSE, nice_category_names = "Atlantic_halibut_SpSt", out_dir = out_dir)
obs_pred1<- vast_get_point_preds(vast_fit = Hali_Sp, use_PredTF_only = FALSE, nice_category_names = "Atlantic_halibut_Sp", out_dir = out_dir)
obs_pred2<- vast_get_point_preds(vast_fit = Hali_Env, use_PredTF_only = FALSE, nice_category_names = "Atlantic_halibut_Env", out_dir = out_dir)
obs_pred3<- vast_get_point_preds(vast_fit = Hali_Null, use_PredTF_only = FALSE, nice_category_names = "Atlantic_halibut_Null", out_dir = out_dir)

hist(obs_pred0$Year)

# AUC near 1: A high AUC value (close to 1) indicates that the model is strong at distinguish between the positive and negative classes. 
# a high true positive rate (sensitivity) and a low false positive rate (1 - specificity).
auc0<- AUC(y_pred = obs_pred0$Predicted_ProbPresence, y_true = obs_pred0$Presence)
auc1<- AUC(y_pred = obs_pred1$Predicted_ProbPresence, y_true = obs_pred1$Presence)
auc2<- AUC(y_pred = obs_pred2$Predicted_ProbPresence, y_true = obs_pred2$Presence)
auc3<- AUC(y_pred = obs_pred3$Predicted_ProbPresence, y_true = obs_pred3$Presence)

auc0#0.8788853 (SpSt)
auc1#0.8740018 (Sp)
auc2#0.7891228 (Env)
auc3#0.687837(Null)

#6. Taylor Diagram*----
#*to look at predictive skill in Abundance and Presence...not overly useful if we are not comparing models anymore
names(obs_pred0)[names(obs_pred0) == "Biomass"] <- "Abundance" #lets get rid of this confusion
names(obs_pred0)[names(obs_pred0) == "Predicted_Biomass"] <- "Predicted_Abundance"
taylor_diag0<- taylor_diagram_func(dat = obs_pred0, obs = "Abundance", mod = "Predicted_Abundance",fill.cols = "orange", color.cols = "orange",shape= 16,  out.file = paste0(out_dir, "/Atlantic_halibut_TaylorDiagram_abun.jpg"))
taylor_diag0<- taylor_diagram_func(dat = obs_pred0, obs = "Presence", mod = "Predicted_ProbPresence",fill.cols = "orange", color.cols = "orange",shape= 16,  out.file = paste0(out_dir, "/Atlantic_halibut_TaylorDiagram_pres.jpg"))

taylor_diag0
head(obs_pred0)

#7. Plot random effects (Omega, Epsilon, R, P)----
#Along with the fixed effect parameters, we might also have some random effects -- 
#especially if we have turned on the spatial (omega) or spatio-temporal (epsilon) variability model components. -yes
#visualize these random effect surfaces.
#data/info needed:
library(sf)
region_shape <- st_read(here::here("R/Shapefiles/IndexShapefiles", "Full_RegionAl3.shp"))
crs(region_shape)
land_use<-st_read(here::here("Data/land_shapefile/", "ne_50m_land.shp"))
crs(land_use)
#tidy_mod_data<- read.csv(here::here("R/data/tidy_data_for_SDM.csv"))
vast_sample_data<- read.csv(here::here("2025-04-23/Output/vast_samp_dat.csv"), header=T)
xlim_use <- c(-73, -48)
ylim_use <- c(39.355, 48)
pred_label<-"test"
#spatial variability (omega)
#vast_omega1_plot<- vast_fit_plot_variable(vast_fit = Hali_SpSt,manual_pred_df=NULL, spatial_var = "Omega1_gc", nice_category_names = "Atlantic_halibut", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = NULL, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9")
#vast_omega2_plot<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL, spatial_var = "Omega2_gc", nice_category_names = "Atlantic_halibut", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = NULL, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9")
#vast_omega1_plot + vast_omega2_plot
#spatio-temporal variability (epsilon) -- one layer for each time step for each model stage
#vast_epsilon1_plot<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL, spatial_var = "Epsilon1_gct", nice_category_names = "Atlantic_halibut",pred_label="_", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = NULL, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 10, panel_rows = 11)
#vast_epsilon2_plot<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL, spatial_var = "Epsilon2_gct", nice_category_names = "Atlantic_halibut",pred_label="_", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = NULL, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 10, panel_rows = 11)
#vast_epsilon1_plot+vast_epsilon2_plot

#Plot by season---at the moment this is not working but i feel like i am close
selected_years_Spring <- as.character(seq(0, 101, by = 3))
selected_years_Summer <- as.character(seq(1, 101, by = 3))
selected_years_Fall <- as.character(seq(2, 101, by = 3))
summary(selected_years_Fall)
#all_times = as.character(unique(vast_sample_data$Year))
#pt <- all_times[all_times %in% selected_years_Spring]
vast_epsilon1_Spring<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "Epsilon1_gct", nice_category_names = "Spring_Atlantic_halibut",pred_label="Spring", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Spring, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 7, panel_rows = 5)
vast_epsilon2_Spring<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "Epsilon2_gct", nice_category_names = "Spring_Atlantic_halibut",pred_label="Spring", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Spring, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 7, panel_rows = 5)
vast_omega1_Spring<- vast_fit_plot_spatial_omega(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "Omega1_gc", nice_category_names = "testSpring_Atlantic_halibut",pred_label="Spring", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Spring, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9")
vast_omega2_Spring<- vast_fit_plot_spatial_omega(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "Omega2_gc", nice_category_names = "Spring_Atlantic_halibut",pred_label="Spring", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Spring, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9")
vast_P1_Spring<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "P1_gct", nice_category_names = "Spring_Atlantic_halibut",pred_label="Spring", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Spring, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 7, panel_rows = 5)
vast_P2_Spring<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "P2_gct", nice_category_names = "Spring_Atlantic_halibut",pred_label="Spring", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Spring, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 7, panel_rows = 5)
vast_R1_Spring<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "R1_gct", nice_category_names = "Spring_Atlantic_halibut",pred_label="Spring", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Spring, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 7, panel_rows = 5)
vast_R2_Spring<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "R2_gct", nice_category_names = "Spring_Atlantic_halibut",pred_label="Spring", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Spring, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 7, panel_rows = 5)

#vast_epsilon1_Summer<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "Epsilon1_gct", nice_category_names = "Summer_Atlantic_halibut",pred_label="Summer", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Summer, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 7, panel_rows = 5
#vast_epsilon1_Fall<- vast_fit_plot_spatial(vast_fit = Hali_SpSt,manual_pred_df=NULL,spatial_var = "Epsilon1_gct", nice_category_names = "Fall_Atlantic_halibut",pred_label="Fall", mask = region_shape, all_times = as.character(unique(vast_sample_data$Year)), plot_times = selected_years_Fall, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", panel_cols = 7, panel_rows = 5)


#i am looking for the grid...i want to assign the area of each cell to prediciton point
#a_i is km^2 units...looks like it is based on the survey that it is from...what is this doing/ is this for the catchability function
halidf<-Hali_SpSt$data_frame

halidf$a_i <- as.numeric(halidf$a_i)
ggplot(halidf, aes(x = Lon_i, y = Lat_i, color = a_i)) +
  geom_point() +
  labs(
    title = "Scatter plot of x and y colored by z",
    x = "lon",
    y = "lat",
    color = "km^2"
  ) +
  theme_minimal() 

#END



#8. extra from TargetsSDM----
#this combines all the models and calculates:
# DHARMA residuals, deviance explained, root mean square error, and a combined Taylor Diagram
# this code takes a very long time and is quite finicky. I prefer the above methods,
# but this can technically be more handsoff if that is what you are looking for. 
date_dir<- here::here("2024-10-04/Halibut_BC/")
# Get a list of fitted models and read them in...,"SpST" removed for now 
mod_fit_names <- c("Null", "EnvOnly", "Sp", "SpST")
all_fits <- vector("list", length(mod_fit_names))
mod_comp_res <- tibble("Model_Name" = mod_fit_names, "Fitted_Mod" = list(length(mod_fit_names)))
for (i in seq_along(mod_fit_names)) {
  mod_temp <- readRDS(paste0(date_dir,"/", mod_fit_names[i], "_mod_fit.rds"))
  mod_comp_res$Fitted_Mod[[i]] <- mod_temp
}


# DHARMA residuals
# Model Diagnostics from A. Gruss. Then using pwalk as we aren't saving anything here and instead writing out the results to a folder
mod_comp_res %>%
  pwalk(.,
        .l = list(n_samples = 1000, fit = .$Fitted_Mod, response_units = list(NULL), out_dir = list(date_dir), out_file = .$Model_Name),
        .f = plot_DHARMa_res)
#plot_DHARMa_res
# Deviance explained
deviance_explained_func <- function(null_mod_deviance, new_mod) {
  out <- round((1 - (new_mod$Report$deviance / null_mod_deviance)) * 100, 2)
  return(out)
}

mod_comp_res <- mod_comp_res %>%
  mutate(.,
         "Null_Mod_Deviance" = mod_comp_res$Fitted_Mod[[1]]$Report$deviance,
         "Deviance_Exp" = map2_dbl(Null_Mod_Deviance, Fitted_Mod, deviance_explained_func)
  )

summary(mod_comp_res)
# Predictions for validation
mod_comp_res <- mod_comp_res %>%
  mutate(., "Preds" = pmap(list(vast_fit = Fitted_Mod, use_PredTF_only = list(TRUE), nice_category_names = Model_Name, out_dir = date_dir), vast_get_point_preds))
# Other measures of prediction skill?
cor_func<- function(df) {
  cor.out<- cor(df$Abundance, df$Predicted_Abundance, method = "spearman")
  return(cor.out)
}

rmse_func<- function(df){
  rmse.out<- round(accuracy(df$Predicted_Abundance, df$Abundance)[,'RMSE'], 2)
  return(rmse.out)
}
#this one failed
mod_comp_res<- mod_comp_res %>%
  mutate(., "SpearmanCorrelation" = map_dbl(Preds, cor_func), 
         "RMSE" = map_dbl(Preds, rmse_func))

# Get nice season_year names
dates <- mod_comp_res$Fitted_Mod[[1]]$covariate_data %>%
  dplyr::select(., Year, Year_Cov, Season) %>%
  distinct() %>%
  mutate("Year_Season" = paste(Year_Cov, Season, sep = "_")) %>%
  arrange(Year)

# Taylor Diagram
td_dat <- mod_comp_res %>%
  dplyr::select(., Model_Name, Preds) %>%
  unnest(cols = c(Preds))
td_dat$Model_Name <- factor(td_dat$Model_Name, levels = mod_comp_res$Model_Name)

td_dat <- td_dat %>%
  left_join(., dates)
#saveRDS(object = mod_comp_res, file = here::here("2024-10-04/Output/mod_comp_res.rds"))
write.csv(td_dat)
#loading the functions that might have been missed
# Functions
source("C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/dfo_functions.R")
source("C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/nmfs_functions.R")
source("C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/combo_functions.R")
source("C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/enhance_r_funcs.R")
source("C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/vast_functions.R")
source("C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/covariate_functions.R")
source("C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/project.fit_model_aja.R")

td_plot_pres <- taylor_diagram_func(dat = td_dat, obs = "Presence", mod = "Predicted_ProbPresence", group = "Model_Name", color.cols = rep("black", 4), fill.cols = c("light gray", "#1b9e77", "#d95f02", "#7570b3"), shapes = rep(21, 4), alpha = 0.6, out.file = paste0(date_dir, "TaylorDiagram_Presence.jpg"))
td_plot_pres

td_plot_bio <- taylor_diagram_func(dat = td_dat, obs = "Biomass", mod = "Predicted_Biomass", group = "Model_Name", color.cols = rep("black", 4), fill.cols = c("light gray", "#1b9e77", "#d95f02", "#7570b3"), shapes = rep(21, 4), alpha = 0.6, out.file = paste0(date_dir, "TaylorDiagram_Biomass.jpg"))

td_plot_bio


ggplot(data = extrap_grid, aes(color = Region), alpha = 0.5) +
  geom_sf() +
  facet_wrap(~Region)
