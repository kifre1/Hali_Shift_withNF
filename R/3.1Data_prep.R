#Data prep
#step 1: get and plot  stratified abundance and standard error estimates
#step 2: get the abundance estimates per grid location 
#Step 3, Add season, Year, Region, Core Area (note count/km2 is the z value )
# plots abundance trends per indexed regions (Core areas) 
# save PredictionData_for_ShiftAnalysis.csv
#Step 4: Mapping the predicted density from knot locations, smoothed over a regular grid. 
#After: map the core areas (same as the generic template for mapping)

# Load package
library(VAST)
library(stringr)
library(patchwork)
library(tidyr)
library(dplyr)
library(ggplot2)
library(raster)
library(gstat)
library(rasterize)
library(here)
library(splines)
library(tidyverse)
library(lubridate)
library(sf)
library(googledrive)
library(ggforce)
library(patchwork)
library(DHARMa)
library(forecast)
library(sp)
library(RColorBrewer)
library(tibble)

devtools::source_url("https://raw.github.com/aallyn/TargetsSDM/main/R/vast_functions.R")
source(here::here("R/VAST_functions/kf_vast_function_edits.R"))
# Andrew's Functions, I cloned his directory here: C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/
source(here::here("R/VAST_functions/dfo_functions.R"))
source(here::here("R/VAST_functions/nmfs_functions.R"))
source(here::here("R/VAST_functions/combo_functions.R"))
source(here::here("R/VAST_functions/enhance_r_funcs.R"))
#source(here::here("R/VAST_functions/vast_functions.R"))
source(here::here("R/VAST_functions/covariate_functions.R"))
source(here::here("R/VAST_functions/project.fit_model_aja.R"))
source(here::here("R/VAST_functions/DHARMa utilities.R"))
source(here::here("R/VAST_functions/SDM_PredValidation_Functions.R"))#PresenceAbsence library is gone so this doesn't work anymore- 
source(here::here("R/VAST_functions/vast_function_edits.R"))
source(here::here("R/VAST_functions/vast_plotting_functions.R"))

fit<- readRDS( here::here("2025-04-11/Halibut_BC/EnvOnly_mod_fit.rds")) 

out_dir <- here::here("2025-04-11/")
# Step 1: get and plot  stratified abundance and standard error estimates
#Question: are the index estimates summed predicted abundance values calculated per strata...no standardization (ieper km2)
  #plot(fit) # Internal VAST one
all_times<-unique(fit$data_frame$t_i)
abundance_ind<- get_vast_index_timeseries(vast_fit = fit, nice_category_names = "Halibut", index_scale = c("raw"), all_times = all_times, out_dir = here::here("2025-04-11_3"))
unique(abundance_ind$Index_Region)
abundance_ind_Region<-subset(abundance_ind, Index_Region == "All"| Index_Region == "USA"| Index_Region == "Canada")
abundance_ind_CA <- subset(abundance_ind, !(Index_Region %in% c("All", "USA", "Canada")))

ggplot(abundance_ind_Region, aes(x = Time, y = Index_Estimate, color = Index_Region)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(
    x = "Time",
    y = "Index Estimate",
    color = "Index Region"
    #title = "Index Estimate Over Time by Region"
  )
ggplot(abundance_ind_CA, aes(x = Time, y = Index_Estimate, color = Index_Region)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(
    x = "Time",
    y = "Index Estimate",
    color = "Index Region"
    #title = "Index Estimate Over Time by Region"
  )
#plot just to check 
#aa_ind_plot<- plot_vast_index_timeseries(index_res_df= abundance_ind, index_scale = "raw", nice_category_names = "Halibut", nice_xlab = "Year", nice_ylab = "Index_Estimate", paneling = "none", color_pal = NULL,out_dir = here::here("2025-04-04/Output/Index_Abundance"))
#lets add season and Year to these data and subset the core areas from the region so that we can plot them properly 
years_Spring <- as.numeric(seq(0, 104, by = 3))
years_Summer <- as.numeric(seq(1, 104, by = 3))
years_Fall <- as.numeric(seq(2, 104, by = 3))
abundance_ind$Season <- ifelse(abundance_ind$Year %in% years_Fall, "Fall",
                                   ifelse(abundance_ind$Year %in% years_Spring, "Spring",
                                          ifelse(abundance_ind$Year %in% years_Summer, "Summer", NA)))

# Group the data into threes and reassign values starting at 1985
increment_value <- 3
#group Time intervals by year
abundance_ind <- abundance_ind %>%
  mutate(YearGroup = (Time %/% increment_value) + 1)
head(abundance_ind)
#Assign actual years to these groups
abundance_ind <- abundance_ind %>%
  mutate(Year = (YearGroup + 1984))
head(abundance_ind)

#plot_vast_index_timeseries_seasonal(index_res_df = abundance_ind_CA, index_scale = "raw", nice_category_names = "CA_Atlantic_halibut_Spring", nice_xlab = "Year", nice_ylab= "Abundance index", paneling = "none", color_pal = c("#1f77b4","#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#17becf", "#8c564b","#e377c2", "yellow3"), out_dir = out_dir)
#Core Areas,spring, abundance time series
#plot_vast_index_timeseries_subest_season(index_res_df = abundance_ind_CA, index_scale = "raw", nice_category_names = "CA_Atlantic_halibut_Spring", nice_xlab = "Year", nice_ylab= "Abundance index", paneling = "none", color_pal = c("#1f77b4","#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#17becf", "#8c564b","#e377c2", "yellow3"), out_dir = out_dir, subsetseason="Spring")

#save these data for calculating slope from regression
write.csv(abundance_ind_Region, (here::here("2025-04-04/Output/abundance_ind_Region.csv")))
write.csv(abundance_ind_CA, (here::here("2025-04-04/Output/abundance_ind_CA.csv")))
#step 2: get the abundance estimates per grid location 
# Now,  stuff to get back those indices. Importantly, we are not going to be able to get the same standard errors as those are going to be calculated internally by TMB and will also include bias correction if turned on. There is an option I believe for getting the standard errors of each density prediction but it is painfully slow (see discussion here: https://github.com/James-Thorson-NOAA/VAST/issues/333).
pred_array <- fit$Report$D_gct
str(pred_array) # grid cells (1583), categories (1), time steps (105)
spat_data <- fit$extrapolation_list
str(spat_data)

loc_g <- spat_data$Data_Extrap[which(spat_data$Data_Extrap[, "Include"] > 0), c("Lon", "Lat")]
loc_g$Site <- 1:3391
# Just making sure we are keeping only the ones we want
g_ind_keep<- which(spat_data$Data_Extrap[, "Include"] > 0) # For this example, its all of em
g_ind_keep
area_g<- as.numeric(spat_data$Area_km2_x[g_ind_keep])

for (tI in 1:dim(pred_array)[3]) {
  data_df <- data.frame(loc_g, time = all_times[tI], z = pred_array[, 1, tI], area = area_g) # Gets tI time step
  
  if(tI == 1){
    pred_df_out<- data_df
  } else {
    pred_df_out<- bind_rows(pred_df_out, data_df)
  }
  
  print(paste0("Time ", tI, " is done!"))
}

str(pred_df_out)
write.csv(pred_df_out, (here::here("2025-04-04/Output/Mod_Pred_grid_Loc.csv")))

# Now, presumably we should be able to multiple these values, summarize em by year and get close to the index we had above 
#note, this is because we are just plotting the overall index of abundance for the entire region 
ind_from_dgct<- pred_df_out |>
  mutate("Index_g" = as.numeric(z) * area) |>
  group_by(time) |>
  summarize_at("Index_g", sum)

#subset original data for only the entire region 
ai_all<-subset(abundance_ind, abundance_ind$Index_Region=="All")
original<- ggplot(data = ai_all, aes(x = Time, y = Index_Estimate))+
  geom_point()+
  geom_line()+
  theme_bw()
original+geom_point(data = ind_from_dgct, aes(x = time, y = Index_g), color = "blue" )+
  geom_line(data = ind_from_dgct, aes(x = time, y = Index_g), color = "blue" )+
  theme_bw()

# the trend is similar but the values are not..,.does this matter, are they technically different values?
#add the strata in and see how it plots...i feel like the area value may be the issue ...it is becasue of the bias correction parameters and andrew says that this is justifiable 

#Step 3, Add season, Year, Region, Core Area (note count/km2 is the z value )


raster_data<-read.csv(here::here("2025-04-04/Output/Mod_Pred_grid_Loc.csv"))

# we are only using abundance data so they were pulled in 03, we ignore a couple values:
#C_iz comes into play if we have multiple categories in the model (sizes, sex, species, etc)
#V_i comes into play if we have data from multiple vessels and we want to then look at vessel effects as a random effect (for example, if you had fishery-dependent data from a bunch of different fishing vessels, you might use this to soak up the variability among vessels).

#SEASON
summary(raster_data)#timesteps go from 0-104
SPRING<- c(0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99,102)
SUMMER<- c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79,82,85,88,91,94,97,100,103)
FALL<- c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89,92,95,98,101,104)
# if t_i matches, assign the season
#create empty column 
SpSt_DF<-raster_data
head(SpSt_DF)
SpSt_DF$Season<-NA
SpSt_DF$Season <- ifelse(SpSt_DF$time %in% SPRING, "Spring", SpSt_DF$Season)
SpSt_DF$Season <- ifelse(SpSt_DF$time %in% SUMMER, "Summer", SpSt_DF$Season)
SpSt_DF$Season <- ifelse(SpSt_DF$time %in% FALL, "Fall", SpSt_DF$Season)
unique(SpSt_DF$Season)

#SURVEY 
#add spatial polygons dataframe (sf) for DFO and NMFS
nmfs_shp <- st_read(here::here("", "R/data/index_shapefiles/NMFS.shp"))
dfo_shp<- st_read(here::here("", "R/data/index_shapefiles/DFO.shp"))
st_crs(nmfs_shp)
st_crs(dfo_shp)
#join dfo and nmfs polygons, Region is the identifier
SurveyRegion<- rbind(nmfs_shp, dfo_shp)
SurveyRegion_sf <- SurveyRegion %>%
  group_by(Region) %>%
  summarize(geometry = st_union(geometry))
plot(SurveyRegion_sf)

# Create a spatial points dataframe (sf) from the dataframe, and match the crs or the polygons
#add unique id
names(SpSt_DF)
SpSt_DF$point_id <- seq.int(nrow(SpSt_DF))
SpSt_points <- st_as_sf(SpSt_DF, coords = c("Lon", "Lat"), crs = 4326)
# Perform a spatial join to find which points are within the polygon
SurveyJoin <- st_join(SpSt_points, SurveyRegion_sf, join = st_within)
# Check for and remove duplicate rows based on the point_id
SurveyJoin <- SurveyJoin[!duplicated(SurveyJoin$point_id), ]
# Create a new column in the original dataframe based on the spatial join
SpSt_DF$survey <- ifelse(!is.na(SurveyJoin$Region), SurveyJoin$Region, NA)

head(SpSt_DF)

#CORE AREA (and respective km2)
#Individual core areas were drawn in ArcGIS,
#Grouped core area shapefiles are created at : CoreArea_shapefiles
CoreAreas <- st_read(here::here("R/data/CoreAreas/CoreAreas_Oc15.shp"))

names(SpSt_DF)
# Create a spatial points dataframe (sf) from the dataframe, and match the crs or the polygons
SpSt_points <- st_as_sf(SpSt_DF, coords = c("Lon", "Lat"), crs = 4326)

# Perform a spatial join to find which points are within the polygon
AreaJoin <- st_join(SpSt_points, CoreAreas, join = st_within)
# Check for and remove duplicate rows based on the point_id
AreaJoin <- AreaJoin[!duplicated(AreaJoin$point_id), ]
# Create a new column in the original dataframe based on the spatial join
#create a column for core areas in the new data
SpSt_DF$Core_Area<-NA
SpSt_DF$Core_Area <- ifelse(!is.na(AreaJoin$Region), AreaJoin$Region, NA)
unique(SpSt_DF$Core_Area)
head(SpSt_DF)

#CORE AREA KM2
library(s2)
# Calculate the areas of polygons in square meters
areas_m2 <- s2_area(CoreAreas)
# Convert areas from square meters to square kilometers
areas_km2 <- areas_m2 / 1e6  # 1 square kilometer = 1,000,000 square meters
# Add the areas as a new column to the dataframe
CoreAreas$CA_Area_km2 <- areas_km2
# Print the resulting dataframe with areas in square kilometers
print(CoreAreas)
CoreAreas_km2 <- st_drop_geometry(CoreAreas)
#write.csv(CoreAreas_km2, (here::here("R/data/CoreAreas/CoreAreas_km2.csv")), row.names = FALSE)
names(CoreAreas_km2)[names(CoreAreas_km2) == "Region"] <- "Core_Area"
SpSt_DF <- merge(SpSt_DF, CoreAreas_km2, by = "Core_Area", all.x = TRUE)

#REGION KM2
# Calculate the areas of polygons in square meters
areas_m2 <- s2_area(SurveyRegion_sf)
# Convert areas from square meters to square kilometers
areas_km2 <- areas_m2 / 1e6  # 1 square kilometer = 1,000,000 square meters
# Add the areas as a new column to the dataframe
SurveyRegion_sf$Reg_Area_km2 <- areas_km2
# Print the resulting dataframe with areas in square kilometers
print(SurveyRegion_sf)
SurveyRegion_km2 <- st_drop_geometry(SurveyRegion_sf)
names(SurveyRegion_km2)[names(SurveyRegion_km2) == "Region"] <- "survey"
SpSt_DF <- merge(SpSt_DF, SurveyRegion_km2, by = "survey", all.x = TRUE)

# YEAR (better than working with t_i)
# 1985 & 2014 first and last years of the model, t_i runs from 0 to 104
#t_i needs to be grouped into 3s and then assigned years starting in 1985
# Group the data into threes and reassign values starting at 1985
increment_value <- 3

#group Time intervals by year
SpSt_DF <- SpSt_DF %>%
  mutate(YearGroup = (time %/% increment_value) + 1)
head(SpSt_DF)
#Assign actual years to these groups
SpSt_DF <- SpSt_DF %>%
  mutate(Year = (YearGroup + 1984))
head(SpSt_DF)
SpSt_DF <- subset(SpSt_DF, select = -X)

#we are changing CB4Vn to CapeBreton
SpSt_DF$Core_Area[SpSt_DF$Core_Area == "CB4Vn"] <- "CapeBreton"
write.csv(SpSt_DF, (here::here("2025-04-04/Output/PredictionData_for_ShiftAnalysis.csv")))

#validate: is the $CA_Area_km2 value similar to group_by Core_Area, sum $area
library(scales)

#exploratory plotting to compare the indexed data from the model to the data aggregatede from the prediction grid
#pulled from row 97
# Now we can plot per indexed regions 
#subset to only include data with CA !NA
SpSt_DF_CA<- SpSt_DF[!is.na(SpSt_DF$Core_Area), ]

ind_from_dgct2<- SpSt_DF_CA |>
  mutate("Index_g" = as.numeric(z) * area) |>
  group_by(time, Core_Area) |>
  summarize_at("Index_g", sum)

#subset original data for only the entire region 
library(viridis) # "viridis", "plasma", "magma", "inferno", and "cividis."

original2<- ggplot(data = abundance_ind_CA, aes(x = Time, y = Index_Estimate, color = Index_Region))+
  geom_point()+
  scale_y_continuous(labels = comma) +
geom_line()+
  geom_errorbar(data = abundance_ind_CA, aes(x = Time, ymin = (Index_Estimate - Index_SD), ymax = (Index_Estimate + Index_SD), color = Index_Region, group = Index_Region), alpha = 0.65) +
  labs(title="Indexed Data")+
  theme_bw()+
  theme(legend.position="none")


new1<-ggplot(data = ind_from_dgct2, aes(x = time, y = Index_g, color = Core_Area))+
  geom_point()+
  scale_y_continuous(labels = comma) +
  geom_line()+
  labs(title="sum(Estimate*Area)")+
  theme_bw()+
 theme(legend.position="none")
  
original2+new1

#instead of using the grid area, lets try calculating based on the area of the Core area shapefiles 
ind_from_dgct4 <- SpSt_DF_CA %>%
  group_by(time, Core_Area) %>%
  summarize(
    mean_z = mean(as.numeric(z), na.rm = TRUE),   # Calculate the average z for each group
    CA_Area_km2 = first(CA_Area_km2),            # Assuming CA_Area_km2 is constant within Core_Area
    total_area = sum(area, na.rm = TRUE)          # Summing up the area of grid cells in the group
  ) %>%
  mutate(Index_g = mean_z * CA_Area_km2)          # Multiply average z by Core_Area's total area

print(ind_from_dgct4)

# Plotting
new2 <- ggplot(data = ind_from_dgct4, aes(x = time, y = Index_g, color = Core_Area)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  geom_line() +
  labs(title = "Average z * CoreAreaKm2") +
  theme_bw()
#this way the values are much lower...the area calculated on the polygons are much smaller than the sum of the grid cells. problem? 
# i struggle with the regular sized grid, but there is no way around this
#to standardize this to account for the differences i CA sizes, we could also divide the count by the total number of grid cells
#normalization by number of units: average value per grid cell across 
ind_from_dgct_std <- SpSt_DF_CA |>
  mutate(Index_g = as.numeric(z) * area) |>
  group_by(time, Core_Area) |>
  summarize(Index_g = sum(Index_g),
            Index_g_std = sum(Index_g) / n()) |>
  ungroup()


new4<-ggplot(data = ind_from_dgct_std, aes(x = time, y = Index_g_std, color = Core_Area))+
  geom_point()+
  scale_y_continuous(labels = comma) +
  geom_line()+
  labs(title="(sum(Estimate*Area))/n")+
  theme_bw()
original2+new4

## ok...so the trends are very similar and the values are different

#go to 03_1_Plot_Abundance_trends

