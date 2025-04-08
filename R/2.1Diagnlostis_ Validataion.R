# Covariate effects: object 'prior.weights' not found
#Deviance explained is full of NAs
#Canada was missed in the index= found error 

#####
# Model diagnostics, evaluation and validation statistics, mostly based on: https://github.com/aallyn/TargetsSDM/blob/main/Vignette.Rmd
#####
R.version # several of the diagnostic functions only work with version 4.2.2


library(VAST)

# So d_i would be predicted density at that observation as an example. 
#Then for the mesh and extrapolation grid, you will have the same points per year and 
#season because the mesh and grid are constant. Those are indexed as “g” 
#As an example, d_gct is the predicted density at a given grid location 
#(g) for a given class (c) at a given time (t). 

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
#3. Deviance & AIC
#4. Parameter Estimates
#5. AUC
#6. Taylor 
#7. Plot random effects (Omega, Epsilon, R, P)
#####
#reopen each model output...we are going to compare them 
#files are too large for GIT so having to read them in from my comp
Hali_SpSt<- readRDS( here::here("2025-04-04/Halibut_BC/SpST_mod_fit.rds")) 
Hali_Null<- readRDS( here::here("2025-04-04/Halibut_BC/Null_mod_fit.rds")) 
Hali_Env<- readRDS( here::here("2025-04-04/Halibut_BC/EnvOnly_mod_fit.rds")) 
Hali_Sp<- readRDS( here::here("2025-04-04/Halibut_BC/Sp_mod_fit.rds"))  
vast_sample_data<- read.csv(here::here("2025-04-04/Output/vast_samp_dat.csv"), header=T)#written mid model-setup 
Hali_SpSt$setting
#1. Default plots
#model plotting function writs plots to your folder,
#but we need to do pull these functions apart later so that we can get at the indexed data for the shift analysis
setwd(here::here("2025-04-04/Output/Plot/SpSt"))
plot(Hali_SpSt)
setwd(here::here("2025-04-04/Output/Plot/Sp"))
plot(Hali_Sp)
setwd(here::here("2025-04-04/Output/Plot/Env"))
plot(Hali_Env)
setwd(here::here("2025-04-04/Output/Plot/Null"))
plot(Hali_Null)



#2. Covariate effects (+plots)#object 'prior.weights' not found
#plot the covariate effects and then look at the shape/strength of the response curves
#season is in the model as a fixed effect, it does not come up here as a covatiate....
out_dir<- here::here("2025-04-04/Output/Plot/Validation/")
graphics.off() 
dev.new()
cov_effs<- get_vast_covariate_effects(vast_fit = Hali_SpSt, params_plot = c("Depth", "SST_monthly", "BT_monthly"), params_plot_levels = 100, effects_pad_values = c(1), nice_category_names = "Halibut_SpSt", out_dir = out_dir)
cov_effs<- get_vast_covariate_effects(vast_fit = Hali_Sp, params_plot = c("Depth", "SST_monthly", "BT_monthly"), params_plot_levels = 100, effects_pad_values = c(1), nice_category_names = "Halibut_Sp", out_dir = out_dir)
cov_effs<- get_vast_covariate_effects(vast_fit = Hali_Env, params_plot = c("Depth", "SST_monthly", "BT_monthly"), params_plot_levels = 100, effects_pad_values = c(1), nice_category_names = "Halibut_Env", out_dir = out_dir)


#columns are the covariates and then the rows are the linear predictors... X1 is the density covariate, would X2 be abundance
#they seem to have the same formula soo I'm not too sure 
plot_vast_covariate_effects(vast_covariate_effects = cov_effs, vast_fit = Hali_SpSt, nice_category_names = "Halibut_SpSt", out_dir = out_dir)
plot_vast_covariate_effects(vast_covariate_effects = cov_effs, vast_fit = Hali_Sp, nice_category_names = "Halibut_Sp", out_dir = out_dir)
plot_vast_covariate_effects(vast_covariate_effects = cov_effs, vast_fit = Hali_Env, nice_category_names = "Halibut_Env", out_dir = out_dir)
plot_vast_covariate_effects(vast_covariate_effects = cov_effs, vast_fit = Hali_Null, nice_category_names = "Halibut_Null", out_dir = out_dir)

#3. Deviance & AIC--- full of NAs 
#how well does the model fit the data, compare observed vs. predicted values
#Get the deviance 
Hali_SpSt$parameter_estimates$diagnostics
Hali_SpSt$Report$Index_ctl

Hali_Null$Report$deviance
Hali_Env$Report$deviance
Hali_Sp$Report$deviance
Hali_SpSt$Report$deviance
#calculate the percent deviance explained 
#0- the model explains no of the variablity, 1 model explains all of the variability 
(Hali_Null$Report$deviance-Hali_Env$Report$deviance)/Hali_Null$Report$deviance #0.1159818
(Hali_Null$Report$deviance-Hali_Sp$Report$deviance)/Hali_Null$Report$deviance #0.3009554
(Hali_Null$Report$deviance-Hali_SpSt$Report$deviance)/Hali_Null$Report$deviance #0.3359598

str(Hali_Null$Report)
#AIC-  we want our model to effectively explain the data while avoiding overfitting.
# Lower AIC, indicates a relatively better balance between goodness of fit and model complexity 

as.numeric(Hali_Null$parameter_estimates$AIC)#31812.25
as.numeric(Hali_Env$parameter_estimates$AIC)#30553.27
as.numeric(Hali_Sp$parameter_estimates$AIC)#25243.85
as.numeric(Hali_SpSt$parameter_estimates$AIC)#24346.6

# 4. Parameter Estimates-- leave for now
# For the fixed effect parameters, we can get their MLE as well as standard errors.
# Parameter estimates and standard errors..omega is spatial, epsilon is spatio-temporal
Hali_SpSt$parameter_estimates$SD
#From the sd report, I did a t test (Estimate / SE) to see which parameters are more likely to be statistically significant (higher vals)  
#Also calculated magnitude of the standard errors (% difference) to see which parameters had better precision (lower vals) ...related 

#these ones seemed statistically significant 
#Omega explains how much spatial correlation exists between observations.  Higher estimates indicate that there is greater spatial dependence
#Epsilon explains spatio-temporal variability: how both space and time interact in the model .
#LogKappa reflects the range of correlation or spatial smoothness 
logkappa1<- -4.52184
logkappa2<- -2.95032
kappa1<- exp(logkappa1)#0.01
kappa2<- exp(logkappa2)#0.052
range1 <- sqrt(8) / kappa1 #260.2287, Small Kappa suggests a large spatial range, observations remain spatially correlated up to 260 units.
range2 <- sqrt(8) / kappa2 #54.17623, Larger Kappa, correlation decays faster for this parameter at around 54 units 
#sigma
logSigma<- -0.65942
exp(logSigma) # 0.5171512
gamma1_cp


# 5. AUC
# Get the observation and prediction dataframe (to the survey points not the grid)
#the function defaults to renaming the predicted variable Biomass but we did this on abundance so just keep this in mind

out_dir<- here::here("2025-04-04/Output/Plot/Validation/")

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

auc0#0.8805 (SpSt)
auc1#0.8734 (Sp)
auc2#0.7408 (Env)
auc3#0.6958 (Null)

#6. Taylor Diagram
#to look at predictive skill in Abundance and Presence...not overly useful if we are not comparing models anymore
names(obs_pred0)[names(obs_pred0) == "Biomass"] <- "Abundance" #lets get rid of this confusion
names(obs_pred0)[names(obs_pred0) == "Predicted_Biomass"] <- "Predicted_Abundance"
taylor_diag0<- taylor_diagram_func(dat = obs_pred0, obs = "Abundance", mod = "Predicted_Abundance",fill.cols = "orange", color.cols = "orange",shape= 16,  out.file = paste0(out_dir, "/Atlantic_halibut_TaylorDiagram_abun.jpg"))
taylor_diag0<- taylor_diagram_func(dat = obs_pred0, obs = "Presence", mod = "Predicted_ProbPresence",fill.cols = "orange", color.cols = "orange",shape= 16,  out.file = paste0(out_dir, "/Atlantic_halibut_TaylorDiagram_pres.jpg"))

taylor_diag0
head(obs_pred0)

#7. Plot random effects (Omega, Epsilon, R, P)
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
vast_sample_data<- read.csv(here::here("2025-04-04/Output/vast_samp_dat.csv"), header=T)
xlim_use <- c(-78.5, -45)
ylim_use <- c(35, 50.5)
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
selected_years_Spring <- as.character(seq(0, 104, by = 3))
selected_years_Summer <- as.character(seq(1, 104, by = 3))
selected_years_Fall <- as.character(seq(2, 104, by = 3))
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



#######8. extra from TargetsSDM----
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
