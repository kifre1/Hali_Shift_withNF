#Data prep
#step 1: get and plot generated stratified abundance and standard error estimates
#step 2: get the abundance estimates per grid location and compare to Step 1
#Step 3, Add season, Year, and the area (km2) of the Stratum, and save data


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

fit<- readRDS( here::here("2025-04-23/Halibut_BC/SpSt_mod_fit.rds")) 

out_dir <- here::here("2025-04-23/Output/IndexAbundance/")
# Step 1: get and plot  stratified abundance and standard error estimates
all_times<-unique(fit$data_frame$t_i)
#function gets total abundance for each time step and index region
abundance_ind<- get_vast_index_timeseries(vast_fit = fit, nice_category_names = "Halibut", index_scale = c("raw"), all_times = all_times, out_dir = here::here("2025-04-23"))
unique(abundance_ind$Index_Region)
#lets add season and Year to these data and subset the core areas from the region so that we can plot them properly 
years_Spring <- as.numeric(seq(0, 101, by = 3))
years_Summer <- as.numeric(seq(1, 101, by = 3))
years_Fall <- as.numeric(seq(2, 101, by = 3))
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
  mutate(Year = (YearGroup + 1989))
head(abundance_ind)

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
unique(abundance_ind$Time)

#save these data for calculating slope from regression
write.csv(abundance_ind_Region, (here::here("2025-04-23/Output/IndexAbundance/abundance_ind_Region.csv")))
write.csv(abundance_ind_CA, (here::here("2025-04-23/Output/IndexAbundance/abundance_ind_CA.csv")))

#step 2: get the density estimates per grid location 
# Importantly, we are not going to be able to get the same standard errors as those are going to be calculated internally by TMB and will also include bias correction if turned on. 
# There is an option I believe for getting the standard errors of each density prediction but it is painfully slow adn i have not been successful (see discussion here: https://github.com/James-Thorson-NOAA/VAST/issues/333).
#C_iz comes into play if we have multiple categories in the model (sizes, sex, species, etc)
#V_i comes into play if we have data from multiple vessels and we want to then look at vessel effects as a random effect (for example, if you had fishery-dependent data from a bunch of different fishing vessels, you might use this to soak up the variability among vessels).

#Density Estimates
str(fit$Report)
pred_array <- fit$Report$D_gct #D_gct multiplies the two components (encounter model X positive density)
str(pred_array) #this gives the density (count/biomass per km²) for each: site (3247 grid centriod), category (1, halibut), and time step (102)
#this can be multiplied by the cell area to get counts, however it gets  lower counts 
#because the extrapolation grid is equal area,area weights used for the predictions correspond to a smaller scale (observation level)
#cell area is technically not the same multiplier used in the model 

#Instead of the equally spaced prediction grid, 
#VAST aggregates over a smaller number of unique area-weighted locations 
#(usually integration points or centroids of spatial knots or prediction grid cells).
#Index_gctl already includes area-weighted contribution it is ok that it has strayed from the regular grid
#it is a spatially representative subset that makes the model run better
#Index_gctl multiplies D_gct by the site and area weight from the model 
pred_array_ind <- fit$Report$Index_gctl 
str(pred_array_ind)#the indexed prediction array has total abundance for each category (halibut) time step(102), grid location(3247) and index region(15)

#Turn it into a df
pred_df_ind <- as.data.frame.table(pred_array_ind, responseName = "Abundance") %>%
  rename(Site = Site, Category = Category, Time = Time, Stratum = Stratum) %>%
  mutate(
    Site = as.factor(Site),#so that they repeat instead of becoming a unique id
    Category = as.integer(Category),
    Time = as.numeric(Time),
    Stratum = as.character(Stratum)
  )

pred_df_ind <- pred_df_ind %>%
  mutate(Site = as.integer(as.character(Site)))#convert it back to int so that it can be joined with the spatial data
summary(pred_df_ind)#$Stratum  is generic (1:15)--replace these with their actual names (accessed in get_vast_index_timeseries())



#Create a named lookup for associated stratum 
stratum_map <- data.frame(
  Stratum = paste0("Stratum_", 1:length(unique(abundance_ind$Index_Region))),
  Region_Name = unique(abundance_ind$Index_Region)
)

# Join will use stratum_map to replace the stratum names...make sure to use dplyr
pred_df_ind <- pred_df_ind %>%
  dplyr::left_join(stratum_map, by = "Stratum") %>%
  dplyr::select(-Stratum) %>%
  dplyr::rename(Stratum = Region_Name)
str(pred_df_ind)

# Grid location
#we need to add the coordinate information to this for spatial analysis
spat_data <- fit$extrapolation_list
str(fit$extrapolation_list)

Loc_g <- data.frame(
  Site = 1:3247,
  Lon = fit$extrapolation_list$Data_Extrap$Lon,
  Lat = fit$extrapolation_list$Data_Extrap$Lat#,
  #Area_km2 = fit$extrapolation_list$Data_Extrap$Area_km2
)
str(Loc_g)
str(pred_df_ind)
  # convert factor to integer

# Perform the left join
pred_df_ind <- pred_df_ind %>%
  left_join(Loc_g, by = "Site")
head(pred_df_ind)

str(pred_df_ind)#Abundance (count) per: Site(grid centriod), Category, Time,  Stratum, Lon, Lat
write.csv(pred_df_ind, (here::here("2025-04-23/Output/IndexAbundance/Mod_Pred_Abundance_grid_Locs.csv")))


#Summarize these counts for indexed totals 
Pred_df_all<-subset(pred_df_ind, Stratum == "All")#start with just "all" and check against the 
ind_from_dgcl <- Pred_df_all %>%
  group_by(Time) %>%
  summarize(Abundance = sum(Abundance, na.rm = TRUE))
str(ind_from_dgcl)

ggplot(ind_from_dgcl, aes(x = Time, y = Abundance)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(
    x = "Time",
    y = "Abundance Estimate",
    title = "Index_gctl Estimates"
  )

#Compare to check data generated by get_vast_index_timeseries()
ai_all<-subset(abundance_ind, Index_Region == "All")

ggplot(ai_all, aes(x = Time, y = Index_Estimate)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(
    x = "Time",
    y = "Abundance Estimate",
    title = "Generated Index Estimates"
  )

#Step 3, Add season, Year, subset based on use in indexing and save the data for shift analysis 
raster_data<-read.csv(here::here("2025-04-23/Output/IndexAbundance/Mod_Pred_Abundance_grid_Locs.csv"))
summary(raster_data)
raster_data_copy<-raster_data
raster_data<-raster_data_copy

#SEASON
summary(raster_data)#timesteps go from 0-104
SPRING<- c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79,82,85,88,91,94,97,100)
SUMMER<- c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89,92,95,98,101)
FALL<- c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99,102)
# Check if the sequences are increasing by 3
all(diff(SPRING) == 3)
all(diff(SUMMER) == 3)
all(diff(FALL) == 3)
# if Time matches, assign the season
raster_data$Season<-NA#create empty column 
head(raster_data)

raster_data$Season <- ifelse(raster_data$Time %in% SPRING, "Spring", raster_data$Season)
raster_data$Season <- ifelse(raster_data$Time %in% SUMMER, "Summer", raster_data$Season)
raster_data$Season <- ifelse(raster_data$Time %in% FALL, "Fall", raster_data$Season)
unique(raster_data$Season)

# YEAR
# 1990 & 2023 first and last years of the model, Time runs from 1 to 102
#Time needs to be grouped into 3s and then assigned years starting in 1990
raster_data$Time<-raster_data$Time-1 #change range to 0-101
increment_value <- 3
#group Time intervals by year
raster_data <- raster_data %>%
  mutate(YearGroup = (Time %/% increment_value) )
#test<-raster_data %>%
#  group_by(YearGroup) %>%
#  summarise(Unique_Times = list(unique(Time)))
summary(raster_data)
#Assign actual years to these groups
range(raster_data$YearGroup)
raster_data <- raster_data %>%
  mutate(Year = (YearGroup + 1990))
summary(raster_data)
str(raster_data)


raster_data <- subset(raster_data, select = -X)
unique(raster_data$Stratum)
str(raster_data)

#CORE AREA (and respective km2)
#Individual core areas were drawn in ArcGIS,
CoreAreas <- st_read(here::here("R/Shapefiles/CoreAreas/CoreAreas_Al14.shp"))
plot(CoreAreas)
names(raster_data)

#CORE AREA KM2
library(s2)
areas_km2 <- s2_area(CoreAreas)/ 1e6# Calculate the areas of polygons in km2
CoreAreas$Area_km2 <- areas_km2# Add the areas as a new column to the dataframe
print(CoreAreas)
CoreAreas_km2 <- st_drop_geometry(CoreAreas)
names(CoreAreas_km2)[names(CoreAreas_km2) == "Region"] <- "Stratum"
raster_data <- merge(raster_data, CoreAreas_km2, by = "Stratum", all.x = TRUE)
summary(raster_data)
Core_Area_data <- raster_data[!is.na(raster_data$Area_km2), ]#subset CA data only 
unique(Core_Area_data$Stratum)#check
#Core_Area_data <- Core_Area_data[ , !(names(Core_Area_data) %in% "X.1")]
summary(Core_Area_data)
str(Core_Area_data)

#REGION KM2
Regional_data<-subset(raster_data, raster_data$Stratum =="Canada" | raster_data$Stratum =="USA")
Regional_data <- Regional_data[ , !(names(Regional_data) %in% "Area_km2")]
Regional_data <- Regional_data[ , !(names(Regional_data) %in% "X.1")]
All_data<-subset(raster_data, raster_data$Stratum =="All")
All_data <- All_data[ , !(names(All_data) %in% "Area_km2")]
All_data <- All_data[ , !(names(All_data) %in% "X.1")]

Canada <- st_read(here::here("R/Shapefiles/IndexShapefiles/Canada_RegionAl14.shp"))
USA<- st_read(here::here("R/Shapefiles/IndexShapefiles/USA_RegionAl3.shp"))

Canada_km2 <- s2_area(Canada)/ 1e6
Canada$Area_km2 <- Canada_km2
print(Canada)
Canada_km2 <- st_drop_geometry(Canada)
names(Canada_km2)[names(Canada_km2) == "Region"] <- "Stratum"

USA_km2 <- s2_area(USA)/ 1e6
USA$Area_km2 <- USA_km2
print(USA)
USA_km2 <- st_drop_geometry(USA)
names(USA_km2)[names(USA_km2) == "Region"] <- "Stratum"

Regions_combined <- rbind(Canada_km2, USA_km2)
Regional_data <- merge(Regional_data, Regions_combined, by = "Stratum", all.x = TRUE)
summary(Regional_data)
str(Regional_data)
unique(Regional_data$Stratum)

#CLEAN
#remove sites that were not used in the index calculation 
#create dfs of used sites for each stratum grouping
fit<- readRDS( here::here("2025-04-23/Halibut_BC/SpSt_mod_fit.rds")) 
used_sites_by_stratum <- drop_units(fit$extrapolation_list$a_el) > 0
# Convert to data frame for easier manipulation
used_df <- as.data.frame(used_sites_by_stratum)
used_df$Site <- 1:nrow(used_df)
library(tidyverse)
library(tidyr)
library(dplyr)
# 1. All
used_all <- used_df[, c(1, 16)]
used_all <- used_all[rowSums(used_all[1]) > 0, ]
used_all <- used_all %>%
  pivot_longer(cols = c(1), names_to = "Stratum", values_to = "Used") %>%
  filter(Used) %>%
  dplyr::select(Site, Stratum)
str(used_all)
# 2. Canada USA
used_Reg <- used_df[, c(2:3, 16)]
used_Reg <- used_Reg[rowSums(used_Reg[1:2]) > 0, ]
used_Reg <- used_Reg %>%
  pivot_longer(cols = c(1:2), names_to = "Stratum", values_to = "Used") %>%
  filter(Used) %>%
  dplyr::select(Site, Stratum)
str(used_Reg)
used_Reg %>%
  count(Stratum)
# 3. Core areas
used_CA <- used_df[, c(4:16)]
used_CA <- used_CA[rowSums(used_CA[1:12]) > 0, ]
used_CA <- used_CA %>%
  pivot_longer(cols = c(1:12), names_to = "Stratum", values_to = "Used") %>%
  filter(Used) %>%
  dplyr::select(Site, Stratum)
str(used_CA)
used_CA %>%
  count(Stratum)

filtered_All <- All_data %>%
  semi_join(used_all, by = c("Site", "Stratum"))
filtered_Reg <- Regional_data %>%
  semi_join(used_Reg, by = c("Site", "Stratum"))
filtered_CA <- Core_Area_data %>%
  semi_join(used_CA, by = c("Site", "Stratum"))

write.csv(filtered_Reg, (here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_Reg_May20.csv")))
write.csv(filtered_All, (here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_All_May20.csv")))
write.csv(filtered_CA, (here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_CA_May20.csv")))

ggplot(filtered_CA, aes(x = Lon, y = Lat, color = Stratum)) +
  geom_point(size = 2) +
  theme_minimal() 
