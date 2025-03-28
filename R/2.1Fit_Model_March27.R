#REworking the original model to include NFdata
#to do list
#Plot MA and ME_NH and NEFSC data
#Get NF 0s
#Clip US to bnam
#rerun BNAM_Data_Prep- running in Halibut_Paper1.Rproj
#finish making full_model_region.shp

#change index shape files
#change the way "swept" is used
#check that ID format is ok

####this is the SMD, the first step in the analysis 
#R.version 4.4.1
#library(devtools)
#library(remotes)
# Install package
#install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")
#required_packages <- c("Matrix", "methods", "stats", "graphics", "grDevices", "utils", "tools",
#                       "sp", "rgdal", "rgeos", "raster", "splancs", "maptools", "mgcv", "ggplot2", "fields")

#install_if_missing <- function(pkg) {
#  if (!requireNamespace(pkg, quietly = TRUE)) {
#    install.packages(pkg)
#  }
#}

#sapply(required_packages, install_if_missing)
#install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"))

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
library(INLA)

aa<- TRUE #!!!Kiyomi set to FALSE!!!
# Use edited functions?
use_edited_funs<- TRUE
if (use_edited_funs) {
  if(aa){
    source(here::here("R/VAST_functions/vast_function_edits.R")) ## AA had to edit this for it to work for me
  } else {
    source(here::here("R/VAST_functions/vast_function_edits.R"))
  }
  assignInNamespace("match_strata_fn", match_strata_fn, "FishStatsUtils")
  assignInNamespace("Prepare_User_Extrapolation_Data_Fn", Prepare_User_Extrapolation_Data_Fn, "FishStatsUtils")
}

devtools::source_url("https://raw.github.com/aallyn/TargetsSDM/main/R/vast_functions.R")
devtools::source_url("https://raw.github.com/aallyn/TargetsSDM/main/R/SDM_PredValidation_Functions.R")

if(aa){
  source(here("R/VAST_functions/DHARMA utilities.R"))
  source(here("R/VAST_functions/vast_function_edits.R")) 
} else {
  source(here("R/VAST_functions/DHARMA utilities.R"))
  source(here("R/VAST_functions/vast_function_edits.R"))
}

# New SF nonsense
sf_use_s2(FALSE)

sys_env<- Sys.getenv() 
root_dir<- paste0(here::here(), "/")

######
# Global use case options
#seasons_run<- c("SPRING") #!!!Kiyomi: adding this part to try to make it a bit more flexible and reduce chance of error if you do end up running more than just the spring. It comes into play with data filtering, setting up observation "times", etc.
strata_use <- data.frame("STRATA" = c("All", "NMFS", "DFO", "Sable",  "Gully", "BOF",  "Browns", "CapeCod", "Nantucket", "EGOM", "Georges",  "CB4Vn")) # We may want to adjust this to include more areas
cell_size <- 25000 #!!!Kiyomi: here is where you could adjust the resolution of the extrpolation grid. Right now, this generates a grid that is roughly equivalent to the NOAA OISST data (25 km X 25 km)!!! 
n_x_use <- 400  
fine_scale_use<- TRUE
bias_correct_use <- TRUE
use_anisotropy_use <- TRUE
fit_year_min<- 1985
fit_year_max<- 2014 #!!!Kiyomi: this piece is just for the model validation side of things, so it will basically go in and filter out any data after 2014 and use those as hold out observations. We then internally get "predictions" for those points and can then look at the predictive skill of the model by comparing the model predictions to the held out observations. It doesn't influence the "projection" part (i.e., projecting out to 2100)
covariates <- c("Depth", "BT_seasonal", "SST_seasonal")
hab_formula <- ~Season + bs(Depth, degree = 2, intercept = FALSE) + bs(BT_seasonal, degree = 2, intercept = FALSE)+ bs(SST_seasonal, degree = 2, intercept = FALSE) #repeat with left our variables and degrree=3
#degree denotes now widdly the model can be , runnin in a single curve makes sense 
smooth_hab_covs<- 2
catch_formula <- ~ factor(Survey)

# Start work -- first run or not? If we have already fit the model, change first_run to FALSE and set date_dir where fitted model object was saved. Otherwise, have first_run = TRUE and set date_dir to NULL. Working with the fitted model object starts around line 231 (likely a better way to link this in the code...)
first_run <- TRUE
date_dir<- NULL
#date_dir<- here::here("2023-08-02/Halibut_BC")

if(first_run){
  # Create directory folder
  Date<- Sys.Date()
  date_dir<- paste0(root_dir, Date, "/Halibut_BC", "/")
  dir.create(date_dir, recursive = TRUE)
  
  # Load data
  Data = read.csv(here::here("R/data/halibut_all_data.csv"))
  
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
  #%>%
  #  dplyr::filter(., VAST_SEASON %in% seasons_run)
  #here we take only spting 
  
  
  # Set of years and seasons- all year/season combinations that exist. This isn't too crucial here, though would allow prediction to unsampled "springs" if necessary
  all_years <- seq(from = min(data_temp$EST_YEAR), to = max(data_temp$EST_YEAR), by = 1)
  all_seasons<- c("SPRING", "SUMMER", "FALL")
  yearseason_set <- expand.grid("SEASON" = all_seasons, "EST_YEAR" = all_years)
  all_yearseason_levels <- apply(yearseason_set[, 2:1], MARGIN = 1, FUN = paste, collapse = "_")
  # yearseason_set <- expand.grid("SEASON" = seasons_run, "EST_YEAR" = all_years)
  # all_yearseason_levels <- apply(yearseason_set[, 2:1], MARGIN = 1, FUN = paste, collapse = "_")
  
  # Similar process, but for the observations- takes out the combos that were actually sampled
  yearseason_i <- apply(data_temp[, c("EST_YEAR", "VAST_SEASON")], MARGIN = 1, FUN = paste, collapse = "_")
  yearseason_i <- factor(yearseason_i, levels = all_yearseason_levels)
  
  # Add the year_season factor column to our sampling_data data set
  data_temp$VAST_YEAR_SEASON <- yearseason_i
  #data_temp$VAST_SEASON <- factor(data_temp$VAST_SEASON, levels = seasons_run)
  data_temp$VAST_SEASON <- factor(data_temp$VAST_SEASON, levels = all_seasons)
  
  
  # VAST year
  data_temp$VAST_YEAR_COV <- data_temp$EST_YEAR
  
  # Using PredTF for model validation? we have data through to 2019, so observations 2015- onwayd will have a true value and will not be used to predict parameters, but it will forecast to these years to see how well the monitor predicts (holds our 5 year for predictions)
  #inserting project model piece (after the model fitting)
  data_temp$PredTF <- ifelse(data_temp$EST_YEAR <= fit_year_max, FALSE, TRUE)
  
  # Some quick organization
  data_temp <- data_temp %>%
    dplyr::select("ID", "DATE", "EST_YEAR", "SEASON", "SURVEY", "SVVESSEL", "DECDEG_BEGLAT", "DECDEG_BEGLON", "NMFS_SVSPP", "DFO_SPEC", "PRESENCE", "BIOMASS", "ABUNDANCE", "PredTF", "VAST_YEAR_COV", "VAST_SEASON", "VAST_YEAR_SEASON", {{ covariates }})
  
  # Make dummy data for all year_seasons to estimate gaps in sampling if needed
  dummy_data <- data.frame("ID" = sample(data_temp$ID, size = 1), "DATE" = sample(data_temp$DATE, size = 1), "EST_YEAR" = yearseason_set[, "EST_YEAR"], "SEASON" = yearseason_set[, "SEASON"], "SURVEY" = "NMFS", "SVVESSEL" = "DUMMY", "DECDEG_BEGLAT" = mean(data_temp$DECDEG_BEGLAT, na.rm = TRUE), "DECDEG_BEGLON" = mean(data_temp$DECDEG_BEGLON, na.rm = TRUE), "NMFS_SVSPP" = "NMFS", "DFO_SPEC" = "DUMMY", "PRESENCE" = 1, "BIOMASS" = 1, "ABUNDANCE" = 1, "PredTF" = TRUE, "VAST_YEAR_COV" = yearseason_set[, "EST_YEAR"], "VAST_SEASON" = yearseason_set[, "SEASON"], "VAST_YEAR_SEASON" = all_yearseason_levels, "Depth" = mean(data_temp$Depth), "BT_seasonal" = mean(data_temp$BT_seasonal), "SST_seasonal" = mean(data_temp$SST_seasonal))
  
  # Combine them
  full_data <- rbind(data_temp, dummy_data) %>%
    arrange(EST_YEAR)
  
  # Now, generate the sample and covariate datasets
  vast_samp_dat <- data.frame(
    "Year" = as.numeric(full_data$VAST_YEAR_SEASON) - 1,
    "Lat" = full_data$DECDEG_BEGLAT,
    "Lon" = full_data$DECDEG_BEGLON,
    "Abundance" = full_data$ABUNDANCE,
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
    "SST_seasonal" = full_data$SST_seasonal,
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
  
  #shp_folder_id = drive_get(here::here("R/data/region_shapefile"))
  
  # Download each of the files in folder
  #shp_files<- drive_ls(shp_folder_id)
  #for (i in seq_along(shp_files$id)) {
  #  drive_download(shp_files$id[i], overwrite = TRUE)
  #}
  
  # Read in region shapefile
  region_wgs84 <- st_read(here::here( "R/data/region_shapefile/full_survey_region.shp"))
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
  region_raster<- projectRaster(region_raster, crs = 4326)# i have exported this raster becasue i think i need it to calculate area occupied down the road
  #filepath <- "C:/Users/FergusonK/Documents/Halibut/Shift_Analysis/Shift_Analysis/R/data/RegionRaster/region_raster.tif"
  #writeRaster(region_raster, filepath, format = "GTiff", overwrite = TRUE)
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
  
  # Add survey regions and core areas 
  #shp_folder_id = drive_get("R/data/index_shapefiles/")
  
  # Download each of the files specified in zones_use in folder
  #shp_files<- drive_ls(shp_folder_id)
  #for (i in seq_along(shp_files$id)) {
  #  drive_download(shp_files$id[i], overwrite = TRUE)
  #}
  #ADD OTHER shapefiles here
  all_shp <- st_read(here::here("R/data/index_shapefiles/All.shp"))
  nmfs_shp <- st_read(here::here("R/data/index_shapefiles/NMFS.shp"))
  dfo_shp<- st_read(here::here("R/data/index_shapefiles/DFO.shp"))
  BB_shp<- st_read(here::here("R/data/index_shapefiles/Browns2.shp"))
  BOF_shp<- st_read(here::here("R/data/index_shapefiles/BOF2.shp"))
  CC_shp<- st_read(here::here("R/data/index_shapefiles/CapeCod2.shp"))
  EGOM_shp<- st_read(here::here("R/data/index_shapefiles/EGOM2.shp"))
  GB_shp<- st_read(here::here("R/data/index_shapefiles/Georges.shp"))
  Gully_shp<- st_read(here::here("R/data/index_shapefiles/Gully2.shp"))
  CB4Vn_shp<- st_read(here::here("R/data/index_shapefiles/CB4Vn.shp"))
  Nant_shp<- st_read(here::here("R/data/index_shapefiles/Nantucket2.shp"))
  Sable_shp<- st_read(here::here("R/data/index_shapefiles/Sable2.shp"))
  
  index_shapes <- bind_rows(all_shp, nmfs_shp, dfo_shp, BB_shp,BOF_shp, CC_shp, EGOM_shp, GB_shp, Gully_shp, CB4Vn_shp, Nant_shp, Sable_shp )
  
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
  ## every year gets a value and 
  null_sets <- make_settings(
    n_x = n_x_use,
    Region = "User",
    strata.limits = strata_use,
    purpose = "index2",
    FieldConfig = c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = 0, "Epsilon2" = 0),
    # RhoConfig = c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0),
    RhoConfig = c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0),
    bias.correct = FALSE,
    use_anisotropy = FALSE,
    ObsModel = c(7,0)
  )
  
  # Environment only model - same as above, but would include habitat covariates specified by X1/X2 formula
  env_only_sets <- null_sets
  
  # Spatial# this one brings in habitat covaiates and spatial variability through omegas
  env_sp_sets <- env_only_sets
  env_sp_sets$FieldConfig <- c("Omega1" = 1, "Epsilon1" = 0, "Omega2" = 1, "Epsilon2" = 0)
  #env_sp_sets$RhoConfig <- c("Beta1" = 4, "Beta2" = 4, "Epsilon1" = 0, "Epsilon2" = 0) #!!!Kiyomi: In the ideal case, we would be able to estimate the autoressive structure among yearly intercepts (betas) as an AR1 process, so trying that. If there are errors about these terms, just let me know and we can revert back to trying something else. 
  env_sp_sets$RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0) #!!!Kiyomi: In the ideal case, we would be able to estimate the autoressive structure among yearly intercepts (betas) as an AR1 process, so trying that. If there are errors about these terms, just let me know and we can revert back to trying something else. 
  env_sp_sets$bias.correct<- TRUE
  env_sp_sets$use_anisotropy<- TRUE
  
  # Spatio-temporal #this one turns on spatial and spatio-temporal variability through the omegas and epsilon
  env_sp_st_sets <- env_sp_sets
  env_sp_st_sets$FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1)
  #env_sp_st_sets$RhoConfig <- c("Beta1" = 4, "Beta2" = 4, "Epsilon1" = 4, "Epsilon2" = 4) #!!!Kiyomi: Same comment as above
  env_sp_st_sets$RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 2, "Epsilon2" = 4) #!!!Kiyomi: Same comment as above
  
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
  #!!!Kiyomi: edited this part. So, before, we basically had to tell it to estimate "Season" as a spatially-varying effect. Now, because season isn't there, we can just delete that part. I left the old ones commented out so you can compare if you'd like.
  # X1config_cp_use <- matrix(c(2, rep(3, nlevels(vast_cov_dat$Season) - 1), rep(rep(1, 2), smooth_hab_covs)), nrow = 1)
  # X1config_cp_use <- matrix(c(2, rep(rep(1, 2), smooth_hab_covs)), nrow = 1)
  # X2config_cp_use <- X1config_cp_use
  
  # All param info
  X1config_cp_all<- list("Null" = NULL, "EnvOnly" = NULL, "Sp" = NULL, "SpST" = NULL)
  X2config_cp_all<- X1config_cp_all
  
  # All contrast info
  #!!!Kiyomi: similar to the Xconfig stuff, we can also adjust the Season part here as that isn't going to be an issue for us. 
  # Xcontrasts_all<- list("Null" = NULL, "EnvOnly" = list(Season = contrasts(vast_cov_dat$Season, contrasts = FALSE)), "Sp" = list(Season = contrasts(vast_cov_dat$Season, contrasts = FALSE)), "SpST" = list(Season = contrasts(vast_cov_dat$Season, contrasts = FALSE)))
  Xcontrasts_all<- list("Null" = NULL, "EnvOnly" = NULL, "Sp" = NULL, "SpST" = NULL)
  
  ## VAST model fittings- here is looks over the 3  models that we set up
  for (i in seq_along(settings_all)) {
    
    ## Build base model. Because we don't have "Season" issues, this should actually work for all of them.
    fit_orig <- fit_model(
      "working_dir" = date_dir,
      "settings" = settings_all[[i]],
      "spatial_list" = vast_spatial,
      "extrapolation_list" = vast_extrap,
      "input_grid" = extrap_df,
      "Lat_i" = vast_samp_dat[, "Lat"],
      "Lon_i" = vast_samp_dat[, "Lon"],
      "t_i" = vast_samp_dat[, "Year"],
      "b_i" = as_units(vast_samp_dat[, "Abundance"], "count"),
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
    
    # If that all went okay..
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
      "b_i" = as_units(vast_samp_dat[, "Abundance"], "count"),
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
  }
} 

#lest save some extra output so that we have it 
saveRDS(object = env_sp_st_sets, file = here::here("2024-10-04/Output/env_sp_st_sets.rds"))
saveRDS(object = vast_extrap, file = here::here("2024-10-04/Output/vast_extrap.rds"))
write.csv(extrap_df,(here::here("2024-10-04/Output/extrap_grid_df.csv")), row.names = FALSE)
write.csv(vast_samp_dat,(here::here("2024-10-04/Output/vast_samp_dat.csv")), row.names = FALSE)
st_write(region_grid, (here::here("2024-10-04/Output/region_grid.shp")))
writeRaster(region_raster, here::here("2024-10-04/Output", "region_raster.tif"), format="GTiff", overwrite=TRUE)

#####Bring the models back in for standard plotting
Hali_SpSt<- readRDS( here::here("2024-10-04/Halibut_BC/SpST_mod_fit.rds")) 
Hali_Null<- readRDS( here::here("2024-10-04/Halibut_BC/Null_mod_fit.rds")) 
Hali_Env<- readRDS( here::here("2024-10-04/Halibut_BC/EnvOnly_mod_fit.rds")) 
Hali_Sp<- readRDS( here::here("2024-10-04/Halibut_BC/Sp_mod_fit.rds")) 
str(Hali_SpSt)

#model plotting function writs plots to your folder, but we need to do these individually so that we can get at the data
plot(Hali_Null)
plot(Hali_Env)
plot(Hali_Sp)
plot(Hali_SpSt)

