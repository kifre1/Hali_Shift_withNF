#####
## Halibut use case
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

# Use edited functions?
use_edited_funs<- TRUE
if (use_edited_funs) {
    source(here::here("R/vast_function_edits.R"))
    assignInNamespace("match_strata_fn", match_strata_fn, "FishStatsUtils")
    assignInNamespace("Prepare_User_Extrapolation_Data_Fn", Prepare_User_Extrapolation_Data_Fn, "FishStatsUtils")
}

devtools::source_url("https://raw.github.com/aallyn/TargetsSDM/main/R/vast_functions.R")
devtools::source_url("https://raw.github.com/aallyn/TargetsSDM/main/R/SDM_PredValidation_Functions.R")
source(here("R/DHARMA utilities.R"))
source(here("R/vast_function_edits.R"))

# New SF nonsense
sf_use_s2(FALSE)

sys_env<- Sys.getenv() 
root_dir<- paste0(here::here(), "/")

######
# Global use case options
strata_use <- data.frame("STRATA" = c("All", "NMFS", "DFO")) # We may want to adjust this to include more areas
cell_size <- 25000
n_x_use <- 400
fine_scale_use<- TRUE
bias_correct_use <- TRUE
use_anisotropy_use <- TRUE
fit_year_min<- 1985
fit_year_max<- 2014 # Could adjust this if we want
covariates <- c("Depth", "BT_seasonal")
hab_formula <- ~ Season + bs(Depth, degree = 2, intercept = FALSE) + bs(BT_seasonal, degree = 2, intercept = FALSE) # We may want to adjust this to have more covariates
smooth_hab_covs<- 2
catch_formula <- ~ factor(Survey)

# Start work -- first run or not? If we have already fit the model, change first_run to FALSE and set date_dir where fitted model object was saved. Otherwise, have first_run = TRUE and set date_dir to NULL. Working with the fitted model object starts around line 231 (likely a better way to link this in the code...)
first_run <- FALSE
#date_dir<- NULL
date_dir<- here::here("2023-08-02/Halibut_BC")

if(first_run){
    # Create directory folder
    Date<- Sys.Date()
    date_dir<- paste0(root_dir, Date, "/Halibut_BC", "/")
    dir.create(date_dir, recursive = TRUE)
    
    # Download data from Google drive
    Data = read.csv(here::here("data", "halibut_all_data.csv"))

    # Prep and processing to accomodate the seasonal model
    data_temp<- Data %>%
        mutate(., "VAST_SEASON" = case_when(
            SURVEY == "DFO" & SEASON == "SPRING" ~ "SPRING",
            SURVEY == "NMFS" & SEASON == "SPRING" ~ "SPRING",
            SURVEY == "DFO" & SEASON == "SUMMER" ~ "SUMMER",
            SURVEY == "NMFS" & SEASON == "FALL" ~ "FALL",
            SURVEY == "DFO" & SEASON == "FALL" ~ NA_character_ # Not consistent enough to include
        )) %>%
    drop_na(VAST_SEASON)
    
    # Set of years and seasons- all year/season combinations that exist
    all_years <- seq(from = min(data_temp$EST_YEAR), to = max(data_temp$EST_YEAR), by = 1)
    all_seasons<- c("SPRING", "SUMMER", "FALL")
    yearseason_set <- expand.grid("SEASON" = all_seasons, "EST_YEAR" = all_years)
    all_yearseason_levels <- apply(yearseason_set[, 2:1], MARGIN = 1, FUN = paste, collapse = "_")

    # Similar process, but for the observations- takes out the combos that were actually sampled
    yearseason_i <- apply(data_temp[, c("EST_YEAR", "VAST_SEASON")], MARGIN = 1, FUN = paste, collapse = "_")
    yearseason_i <- factor(yearseason_i, levels = all_yearseason_levels)

    # Add the year_season factor column to our sampling_data data set
    data_temp$VAST_YEAR_SEASON <- yearseason_i
    data_temp$VAST_SEASON <- factor(data_temp$VAST_SEASON, levels = all_seasons)
    
    # VAST year
    data_temp$VAST_YEAR_COV <- data_temp$EST_YEAR

    # Using PredTF for model validation? 
    data_temp$PredTF <- ifelse(data_temp$EST_YEAR <= fit_year_max, FALSE, TRUE)

    # Some quick organization
    data_temp <- data_temp %>%
        dplyr::select("ID", "DATE", "EST_YEAR", "SEASON", "SURVEY", "SVVESSEL", "DECDEG_BEGLAT", "DECDEG_BEGLON", "NMFS_SVSPP", "DFO_SPEC", "PRESENCE", "BIOMASS", "ABUNDANCE", "PredTF", "VAST_YEAR_COV", "VAST_SEASON", "VAST_YEAR_SEASON", {{ covariates }})
    
    # Make dummy data for all year_seasons to estimate gaps in sampling if needed
    dummy_data <- data.frame("ID" = sample(data_temp$ID, size = 1), "DATE" = sample(data_temp$DATE, size = 1), "EST_YEAR" = yearseason_set[, "EST_YEAR"], "SEASON" = yearseason_set[, "SEASON"], "SURVEY" = "NMFS", "SVVESSEL" = "DUMMY", "DECDEG_BEGLAT" = mean(data_temp$DECDEG_BEGLAT, na.rm = TRUE), "DECDEG_BEGLON" = mean(data_temp$DECDEG_BEGLON, na.rm = TRUE), "NMFS_SVSPP" = "NMFS", "DFO_SPEC" = "DUMMY", "PRESENCE" = 1, "BIOMASS" = 1, "ABUNDANCE" = 1, "PredTF" = TRUE, "VAST_YEAR_COV" = yearseason_set[, "EST_YEAR"], "VAST_SEASON" = yearseason_set[, "SEASON"], "VAST_YEAR_SEASON" = all_yearseason_levels, "Depth" = mean(data_temp$Depth), "BT_seasonal" = mean(data_temp$BT_seasonal))

    # Combine them
    full_data <- rbind(data_temp, dummy_data) %>%
        arrange(EST_YEAR)

    # Now, generate the sample and covariate datasets
    vast_samp_dat <- data.frame(
        "Year" = as.numeric(full_data$VAST_YEAR_SEASON) - 1,
        "Lat" = full_data$DECDEG_BEGLAT,
        "Lon" = full_data$DECDEG_BEGLON,
        "Biomass" = full_data$BIOMASS,
        "Swept" = ifelse(full_data$SURVEY == "NMFS", 0.0384, 0.0404),
        "Pred_TF" = full_data$PredTF
    )
    
    # Select columns we want from the "full" vast_seasonal_data dataset
    vast_cov_dat <- data.frame(
        "Year" = as.numeric(full_data$VAST_YEAR_SEASON) - 1,
        "Year_Cov" = full_data$VAST_YEAR_COV,
        "Season" = full_data$VAST_SEASON,
        "Depth" = full_data$Depth,
        "BT_seasonal" = full_data$BT_seasonal,
        "Lat" = full_data$DECDEG_BEGLAT,
        "Lon" = full_data$DECDEG_BEGLON
    )
#for CATCHAbility formula it separates out which survey it is from
    vast_catch_dat <- data.frame(
        "Year" = as.numeric(full_data$VAST_YEAR_SEASON) - 1,
        "Year_Cov" = full_data$VAST_YEAR_COV,
        "Season" = full_data$VAST_SEASON,
        "Lat" = full_data$DECDEG_BEGLAT,
        "Lon" = full_data$DECDEG_BEGLON,
        "Survey" = factor(full_data$SURVEY, levels = c("NMFS", "DFO"))
    )
    
    #####
    # VAST stuff start
    ## Extrapolation grid
    shp_folder_id = drive_get("CRSBND_Halibut/data/region_shapefile")
    
    # Download each of the files in folder
    shp_files<- drive_ls(shp_folder_id)
    for (i in seq_along(shp_files$id)) {
        drive_download(shp_files$id[i], overwrite = TRUE)
    }
    
    # Read in region shapefile
    region_wgs84 <- st_read(here::here("", "full_survey_region.shp"))
    
    # Get UTM zone
    lon <- sum(st_bbox(region_wgs84)[c(1, 3)]) / 2
    utm_zone <- floor((lon + 180) / 6) + 1
    
    # Transform to the UTM zone
    crs_utm <- st_crs(paste0("+proj=utm +zone=", utm_zone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
    region_utm <- st_transform(region_wgs84, crs = crs_utm)
    
    # Make extrapolation grid with sf
    region_grid <- st_make_grid(region_utm, cellsize = cell_size, what = "centers")
    region_raster <- raster(crs = st_crs(region_grid), vals = 0, resolution = cell_size, ext = extent(st_bbox(region_grid))) %>%
        rasterize(as(region_grid, "Spatial"), .)
    crs(region_raster) <- st_crs(region_grid)$input
    region_raster<- projectRaster(region_raster, crs = 4326)
    region_raster[]<- NA
    region_sf <- st_as_sf(region_grid, crs = crs_utm)

    # Now get only the points that fall within the shape polygon
    points_keep <- data.frame("pt_row" = seq(from = 1, to = nrow(region_sf), by = 1), "in_out" = st_intersects(region_sf, region_utm, sparse = FALSE))
    region_sf <- region_sf %>%
        mutate(., "in_poly" = st_intersects(region_sf, region_utm, sparse = FALSE)) %>%
        filter(., in_poly == TRUE)
    
    # Convert back to WGS84 lon/lat, as that is what VAST expects.
    extrap_grid <- region_sf %>%
        st_transform(., crs = 4326)

    # Plot it
    plot(extrap_grid)
    
    # Adding in the a strata/region component for stratified abundance. This could add an interesting piece to the results emphasizing how projections can help assess changes across management zones.
    shp_folder_id = drive_get("CRSBND_Halibut/data/index_shapefiles")
    
    # Download each of the files specified in zones_use in folder
    shp_files<- drive_ls(shp_folder_id)
    for (i in seq_along(shp_files$id)) {
        drive_download(shp_files$id[i], overwrite = TRUE)
    }
#ADD OTHER shapefiles here
    all_shp <- st_read(here::here("", "All.shp"))
    nmfs_shp <- st_read(here::here("", "NMFS.shp"))
    dfo_shp<- st_read(here::here("", "DFO.shp"))

    index_shapes <- bind_rows(all_shp, nmfs_shp, dfo_shp)

    # Add this information to the extrapolation grid
    extrap_grid <- extrap_grid %>%
      st_join(., index_shapes, join = st_within) %>%
      mutate(.,
        "Lon" = as.numeric(st_coordinates(.)[, 1]),
        "Lat" = as.numeric(st_coordinates(.)[, 2])
      ) 
      
    # Plot check
     ggplot(data = extrap_grid, aes(color = Region), alpha = 0.5) +
         geom_sf() +
         facet_wrap(~Region)
    
  
    # Convert to dataframe that VAST expects
    extrap_df<- extrap_grid %>%
      st_drop_geometry() %>%
          dplyr::select(., Lon, Lat, Region) %>%
          mutate(.,
              Area_km2 = ((cell_size / 1000)^2),
              STRATA = factor(Region, levels = index_shapes$Region, labels = index_shapes$Region)
          )
    
    ## VAST settings
    # Null intercept only model- this way we can compare deviance explained relative to some of the other variables
    null_sets <- make_settings(
        n_x = n_x_use,
        Region = "User",
        strata.limits = strata_use,
        purpose = "index2",
        FieldConfig = c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = 0, "Epsilon2" = 0),
        RhoConfig = c("Beta1" = 3, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0),
        bias.correct = FALSE,
        use_anisotropy = FALSE
    )

    # Environment only model - same as above, but would include X1/X2 formula
    env_only_sets <- null_sets

    # Spatial# this one brings in habitat covaiates
    env_sp_sets <- env_only_sets
    env_sp_sets$FieldConfig <- c("Omega1" = 1, "Epsilon1" = 0, "Omega2" = 1, "Epsilon2" = 0)
    env_sp_sets$RhoConfig <- c("Beta1" = 2, "Beta2" = 2, "Epsilon1" = 0, "Epsilon2" = 0)
    env_sp_sets$bias.correct<- TRUE
    env_sp_sets$use_anisotropy<- TRUE
    
    # Spatio-temporal #this one turns on spatial variability through the omegas and epsilon
    env_sp_st_sets <- env_sp_sets
    env_sp_st_sets$FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1)
    env_sp_st_sets$RhoConfig <- c("Beta1" = 2, "Beta2" = 2, "Epsilon1" = 4, "Epsilon2" = 4) # Eps1 rho error
    # env_sp_st_sets$RhoConfig <- c("Beta1" = 4, "Beta2" = 4, "Epsilon1" = 1, "Epsilon2" = 4) # Beta errors
    # env_sp_st_sets$RhoConfig <- c("Beta1" = 2, "Beta2" = 2, "Epsilon1" = 1, "Epsilon2" = 4)

    # Bundle together in a list that we can loop through
    settings_all<- list("Null" = null_sets, "EnvOnly" = env_only_sets, "Sp" = env_sp_sets, "SpST" = env_sp_st_sets)

    ## VAST extrapolation info
    vast_extrap <- make_extrapolation_info(Region = null_sets$Region, projargs = NA, zone = null_sets$zone, strata.limits = strata_use, input_grid = extrap_df)
    
    ## VAST spatial info
    vast_spatial<- make_spatial_info(n_x = null_sets$n_x, Lon_i = vast_samp_dat[, "Lon"], Lat_i = vast_samp_dat[, "Lat"], Extrapolation_List = vast_extrap, grid_size_km = null_sets$grid_size_km, fine_scale = fine_scale_use)

    ## VAST formula info
    hab_formula_all<- list("Null" = ~0, "EnvOnly" = hab_formula, "Sp" = hab_formula, "SpST" = hab_formula)
    catch_formula_all<- list("Null" = ~0, "EnvOnly" = catch_formula, "Sp" = catch_formula, "SpST" = catch_formula)

    ## Adjust influence of different parameters
    X1config_cp_use <- matrix(c(2, rep(3, nlevels(vast_cov_dat$Season) - 1), rep(rep(1, 2), smooth_hab_covs)), nrow = 1)
    X2config_cp_use <- X1config_cp_use

    # All param info
    X1config_cp_all<- list("Null" = NULL, "EnvOnly" = X1config_cp_use, "Sp" = X1config_cp_use, "SpST" = X1config_cp_use)
    X2config_cp_all<- X1config_cp_all

    # All contrast info
    Xcontrasts_all<- list("Null" = NULL, "EnvOnly" = list(Season = contrasts(vast_cov_dat$Season, contrasts = FALSE)), "Sp" = list(Season = contrasts(vast_cov_dat$Season, contrasts = FALSE)), "SpST" = list(Season = contrasts(vast_cov_dat$Season, contrasts = FALSE)))

    ## VAST model fittings- here is looks over the 3  models that we set up
    for (i in seq_along(settings_all)) {

        ## Build base model
        fit_orig <- fit_model(
            "working_dir" = date_dir,
            "settings" = settings_all[[i]],
            "spatial_list" = vast_spatial,
            "extrapolation_list" = vast_extrap,
            "input_grid" = extrap_df,
            "Lat_i" = vast_samp_dat[, "Lat"],
            "Lon_i" = vast_samp_dat[, "Lon"],
            "t_i" = vast_samp_dat[, "Year"],
            "b_i" = as_units(vast_samp_dat[, "Biomass"], "kg"),
            "a_i" = as_units(vast_samp_dat[, "Swept"], "km2"),
            "X1config_cp" = X1config_cp_all[[i]],
            "X2config_cp" = X2config_cp_all[[i]],
            "covariate_data" = vast_cov_dat,
            "X1_formula" = hab_formula_all[[i]],
            "X2_formula" = hab_formula_all[[i]],
            "catchability_data" = vast_catch_dat,
            "Q1_formula" = catch_formula_all[[i]],
            "Q2_formula" = catch_formula_all[[i]], 
            "X_contrasts" = Xcontrasts_all[[i]],
            "run_model" = FALSE,
            "PredTF_i" = vast_samp_dat[, "Pred_TF"]
        )

        # Mapping adjustments for all models but null
        if(names(settings_all)[i] == "Null"){
            # Fit model and save it
            fit = fit_model(
                "working_dir" = date_dir,
                "settings" = settings_all[[i]],
                "spatial_list" = vast_spatial,
                "extrapolation_list" = vast_extrap,
                "input_grid" = extrap_df,
                "Lat_i" = vast_samp_dat[, "Lat"],
                "Lon_i" = vast_samp_dat[, "Lon"],
                "t_i" = vast_samp_dat[, "Year"],
                "b_i" = as_units(vast_samp_dat[, "Biomass"], "kg"),
                "a_i" = as_units(vast_samp_dat[, "Swept"], "km2"),
                "X1config_cp" = X1config_cp_all[[i]],
                "X2config_cp" = X2config_cp_all[[i]],
                "covariate_data" = vast_cov_dat,
                "X1_formula" = hab_formula_all[[i]],
                "X2_formula" = hab_formula_all[[i]],
                "catchability_data" = vast_catch_dat,
                "Q1_formula" = catch_formula_all[[i]],
                "Q2_formula" = catch_formula_all[[i]],
                "X_contrasts" = Xcontrasts_all[[i]],
                "newtonsteps" = 1,
                "run_model" = TRUE,
                "PredTF_i" = vast_samp_dat[, "Pred_TF"]
            )
            
            saveRDS(object = fit, file = paste0(date_dir, names(settings_all)[i], "_mod_fit.rds"))
        } else {
            # Make adjustments
            # Adjust mapping for log_sigmaXi and fitting final model -- pool variance for all seasons and then set year's to NA
            Map_adjust = fit_orig$tmb_list$Map
            
            # Pool variances for each term to a single value
            Map_adjust$log_sigmaXi1_cp = factor(c(
            rep(as.numeric(Map_adjust$log_sigmaXi1_cp[1]), nlevels(vast_cov_dat$Season)), rep(NA, smooth_hab_covs*2)))
            
            Map_adjust$log_sigmaXi2_cp = factor(c(
            rep(as.numeric(Map_adjust$log_sigmaXi2_cp[1]), nlevels(vast_cov_dat$Season)), rep(NA, smooth_hab_covs*2)))
            
            # Fit final model with new mapping
            fit = fit_model(
                "working_dir" = date_dir,
                "settings" = settings_all[[i]],
                "spatial_list" = vast_spatial,
                "extrapolation_list" = vast_extrap,
                "input_grid" = extrap_df,
                "Lat_i" = vast_samp_dat[, "Lat"],
                "Lon_i" = vast_samp_dat[, "Lon"],
                "t_i" = vast_samp_dat[, "Year"],
                "b_i" = as_units(vast_samp_dat[, "Biomass"], "kg"),
                "a_i" = as_units(vast_samp_dat[, "Swept"], "km2"),
                "X1config_cp" = X1config_cp_all[[i]],
                "X2config_cp" = X2config_cp_all[[i]],
                "covariate_data" = vast_cov_dat,
                "X1_formula" = hab_formula_all[[i]],
                "X2_formula" = hab_formula_all[[i]],
                "catchability_data" = vast_catch_dat,
                "Q1_formula" = catch_formula_all[[i]],
                "Q2_formula" = catch_formula_all[[i]],
                "X_contrasts" = Xcontrasts_all[[i]],
                "newtonsteps" = 1,
                "run_model" = TRUE,
                "PredTF_i" = vast_samp_dat[, "Pred_TF"],
                "Map" = Map_adjust
            )
            
            saveRDS(object = fit, file = paste0(date_dir, names(settings_all)[i], "_mod_fit.rds"))
        }
    }
} 

#####
# Model diagnostics, evaluation and validation statistics
#####
Hali_Null<- readRDS( here::here("2023-08-02/Halibut_BCNull_mod_fit.rds")) 
Hali_Env<- readRDS( here::here("2023-08-02/Halibut_BCEnvOnly_mod_fit.rds")) 
Hali_Sp<- readRDS( here::here("2023-08-02/Halibut_BCSp_mod_fit.rds")) 

reload_model(Hali_Null)
reload_model(Hali_Env)
reload_model(Hali_Sp)

#filename_Null <- file.choose(here::here("2023-08-02/Halibut_BC/Null_mod_fit.rds"))
#Null_mod_fit <- readRDS(filename_Null)
date_dir<- here::here("2023-08-02/Halibut_BC/")
# Get a list of fitted models and read them in...,"SpST" removed for now 
mod_fit_names <- c("Null", "EnvOnly", "Sp")
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

# Model predictions for validation
mod_comp_res <- mod_comp_res %>%
    mutate(., "Preds" = pmap(list(vast_fit = Fitted_Mod, use_PredTF_only = list(TRUE), nice_category_names = Model_Name, out_dir = date_dir), vast_get_point_preds))
# Other measures of prediction skill?
cor_func<- function(df) {
    cor.out<- cor(df$Biomass, df$Predicted_Biomass, method = "spearman")
    return(cor.out)
}

rmse_func<- function(df){
    rmse.out<- round(accuracy(df$Predicted_Biomass, df$Biomass)[,'RMSE'], 2)
    return(rmse.out)
}

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

#####
## VAST model predictions?
#####
#Percent deviance explained
str(Hali_Null)
Hali_Env$Report$deviance/Hali_Null$Report$deviance
Hali_Sp$Report$deviance/Hali_Null$Report$deviance
get_vast_covariate_effects(Hali_Sp)
plot(Hali_Null)
plot(Hali_Env)
plot_trawl_dat_summs(Hali_Env)
plot_results
#pull out some data frames looking for the predictions
Env_Extrap<-Hali_Env[["extrapolation_list"]]
Env_Data_Extrap<-Env_Extrap[["Data_Extrap"]]
Env_DF<-Hali_Env[["data_frame"]]

