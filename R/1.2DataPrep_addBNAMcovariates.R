
library(R.matlab)
library(raster)
library(sp)
library(lubridate)

# Function to process individual .mat files----
process_mat_to_raster_stack <- function(mat_file, info_file, variable_name) {
  # Load .mat files
  mat_data <- readMat(mat_file, fixNames = TRUE)
  data_info <- readMat(info_file, fixNames = TRUE)
  
  # Extract lat/lon from data_info
  mat_data$nav.lon <- data_info$nav.lon
  mat_data$nav.lat <- data_info$nav.lat
  mat_data$mask <- data_info$land.mask  # Ensure correct variable name
  
  # Extract and flatten lat/lon
  BNAM_lat <- as.vector(mat_data$nav.lat[1:801, 1:401])
  BNAM_lon <- as.vector(mat_data$nav.lon[1:801, 1:401])
  
  # Check if the specified variable exists in the .mat file
  if (!variable_name %in% names(mat_data)) {
    stop(paste("Variable", variable_name, "not found in", mat_file))
  }
  
  # Extract and flatten each month's matrix
  month_list <- lapply(1:12, function(i) as.vector(mat_data[[variable_name]][i, 1:801, 1:401]))
  
  # Convert to data frame
  df <- data.frame(
    Latitude = BNAM_lat,
    Longitude = BNAM_lon,
    setNames(data.frame(month_list), sprintf("X%d.%02d", mat_data$yr, 1:12))
  )
  
  coordinates(df) <- ~Longitude + Latitude
  rast <- raster(ncol = 800, nrow = 400)
  extent(rast) <- extent(df)
  
  # Rasterize each month and store in a list
  raster_List <- setNames(
    lapply(names(df), function(name) {
      rasterize(df, rast, df[[name]], fun = mean)
    }), 
    names(df)
  )
  
  # Function to fill NA values
  fill.na <- function(x, i = 5) {
    if (is.na(x)[i]) {
      return(round(mean(x, na.rm = TRUE), 5))
    } else {
      return(round(x[i], 5))
    }
  }
  
  # Apply focal filter to fill NAs
  raster_List <- lapply(raster_List, function(r) focal(r, w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE))
  
  # Stack the raster list
  raster_stack <- stack(raster_List)
  
  return(raster_stack)
}

# Process all .mat files in a folder
process_all_mat_files <- function(mat_folder, info_file, variable_name) {
  # Get list of .mat files
  mat_files <- list.files(mat_folder, pattern = "\\.mat$", full.names = TRUE)
  
  # Apply function to all files and combine into a large stack
  all_raster_stacks <- lapply(mat_files, function(file) process_mat_to_raster_stack(file, info_file, variable_name))
  
  # Merge all raster stacks into a single giant stack
  final_raster_stack <- stack(all_raster_stacks)
  
  return(final_raster_stack)
}

#Make rasters----
# Surface Temperature
mat_folder <- "C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSsfce-20250319T175426Z-001/TSsfce/"
info_file <- "C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/bnam_grid_info.mat"
# Select the variable you want to process
variable_to_process <- "Tsfce"  
# Run function on all .mat files
SurfaceTemperature_stack <- process_all_mat_files(mat_folder, info_file, variable_to_process)
S_layer_names <- names(SurfaceTemperature_stack)

# Bottom Temperature
mat_folder <- "C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSbtm-20250319T175520Z-001/TSbtm/"
info_file <- "C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/bnam_grid_info.mat"
# Select the variable you want to process
variable_to_process <- "Tbtm"  
# Run function on all .mat files
BottomTemperature_stack <- process_all_mat_files(mat_folder, info_file, variable_to_process)
B_layer_names <- names(SurfaceTemperature_stack)

#Depth
data_info = readMat('C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/bnam_grid_info.mat', fixNames = TRUE)
#isolate variables
Depth <- (data_info$Bathy.depth) 
Depth_lat <-(data_info$nav.lat)  
Depth_lon <-(data_info$nav.lon) 
Depth_lat<-Depth_lat[1:801, 1:401]
Depth_lon<-Depth_lon[1:801, 1:401]
Depth<-Depth[1:801, 1:401]
#Convert matrices to lists and join into DF
Depth_lat<-as.list(Depth_lat)
Depth_lon<-as.list(Depth_lon)
Depth<-as.list(Depth)
df_Depth<- do.call(rbind.data.frame, Map('c',Depth_lat, Depth_lon, Depth))     
colnames(df_Depth)[1] <- "Latitude"
colnames(df_Depth)[2] <- "Longitude"
colnames(df_Depth)[3] <- "Depth"
#make spatial points
coordinates(df_Depth)=~Longitude+Latitude
rast <- raster(ncol = 800, nrow = 400)
extent(rast) <- extent(df_Depth)
#rasterize and fill gaps  
Depth_raster<-rasterize(df_Depth, rast, df_Depth$Depth, fun = mean)
fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return(round(mean(x, na.rm=TRUE),5) )
  } else {
    return(round(x[i],5)) 
  }
}
Depth_raster<- focal(Depth_raster, w = matrix(1,3,3), fun = fill.na, 
           pad = TRUE, na.rm = FALSE )
plot(Depth_raster)



#----

#join raster data to survey points----
#SurfaceTemperature_stack, BottomTemperature_stack, Depth_raster
catch_data <- read_rds(here("Data/Derived/all_raw_halibut_catch_formattedAl4.rds"))


head(catch_data)
st_crs(SurfaceTemperature_stack)

#isolate year and month
catch_data$DATE <- as.Date(catch_data$DATE)
catch_data <- catch_data |> 
  mutate(YEAR = year(DATE),
         MONTH = sprintf("%02d", month(DATE)))  # Ensures month is always 2 digits

# Create the LAYER_NAME that matches the format in the raster stack
catch_data <- catch_data |> 
  mutate(LAYER_NAME = paste0("X", YEAR, ".", MONTH))

# Create a SpatialPoints object for catch_data
coordinates(catch_data) <- ~DECDEG_BEGLON + DECDEG_BEGLAT
proj4string(catch_data) <- CRS(proj4string(BottomTemperature_stack))


# SURFACE ----
#empty vector to store the extracted temperature values
SurfaceT_extracted_values <- numeric(nrow(catch_data))

# Loop through each row of the catch_data to extract the correct layer and value

for (i in 1:nrow(catch_data)) {
  # Extract the layer name from the LAYER_NAME column
  layer_name <- catch_data$LAYER_NAME[i]
  
  # Check if the layer exists in the raster stack
  if (layer_name %in% names(SurfaceTemperature_stack)) {
    # Extract the correct layer from the raster stack
    raster_layer <- SurfaceTemperature_stack[[layer_name]]
    
    # Extract the value from the specified layer for the corresponding lat/lon
    SurfaceT_extracted_values[i] <-  raster::extract(raster_layer, catch_data[i, , drop = FALSE])
  } else {
    # If the layer does not exist, assign NA
    SurfaceT_extracted_values[i] <- NA
  }
}
catch_data$SST_monthly <- SurfaceT_extracted_values
catch_df <- as.data.frame(catch_data)
write.csv(catch_df, here("Data/Derived/all_raw_halibut_catch_with_stAl4.csv"))

#BOTTOM----
# empty vector to store the extracted temperature values
BottomT_extracted_values <- numeric(nrow(catch_data))

# Loop through each row of the catch_data to extract the correct layer and value

for (i in 1:nrow(catch_data)) {
  # Extract the layer name from the LAYER_NAME column
  layer_name <- catch_data$LAYER_NAME[i]
  
  # Check if the layer exists in the raster stack
  if (layer_name %in% names(BottomTemperature_stack)) {
    # Extract the correct layer from the raster stack
    raster_layer <- BottomTemperature_stack[[layer_name]]
    
    # Extract the value from the specified layer for the corresponding lat/lon
    BottomT_extracted_values[i] <-  raster::extract(raster_layer, catch_data[i, , drop = FALSE])
  } else {
    # If the layer does not exist, assign NA
    BottomT_extracted_values[i] <- NA
  }
}
catch_data$BT_monthly <- BottomT_extracted_values

catch_df <- as.data.frame(catch_data)
write.csv(catch_df, here("Data/Derived/all_raw_halibut_catch_with_st_BtAl14.csv"))

# DEPTH----
depth_extracted_values <- numeric(nrow(catch_data))
# Loop through each row of the catch_data to extract depth values
for (i in 1:nrow(catch_data)) {
  # Extract the value from the Depth_dims_raster_layer for the corresponding lat/lon
  depth_extracted_values[i] <-  raster::extract(Depth_raster, catch_data[i, , drop = FALSE])
}
catch_data$Depth_value <- depth_extracted_values
catch_df <- as.data.frame(catch_data)
names(catch_df)

#remove where 0 (too close to land), and NA (too far south)
catch_df2 <- catch_df %>%
  filter(!(SST_monthly == 0 & BT_monthly == 0 & Depth_value == 0) &
           !(is.na(SST_monthly) & is.na(BT_monthly) & is.na(Depth_value)))

catch_df2 %>%
  group_by(SURVEY) %>%
  summarize(
    min_year = min(YEAR),
    max_year = max(YEAR),
    .groups = "drop"
  )

# Plot the survey locations
plot(catch_df2$DECDEG_BEGLON, catch_df2$DECDEG_BEGLAT, 
     col = as.factor(catch_df2$SURVEY),  # Convert SURVEY to a color factor
     pch = 46, cex = 1, 
     xlab = "Longitude", ylab = "Latitude",
     main = "Survey Locations")

# Add a legend separately
legend("topright", legend = unique(catch_df2$SURVEY), 
       col = as.factor(unique(catch_df2$SURVEY)), pch = 20)

catch_df2 %>%
  group_by(SURVEY) %>%
  summarize(
    min_year = min(YEAR),
    max_year = max(YEAR),
    .groups = "drop"
  )
#removing ME_NH,and MA because unlilke the other 3, 
#they are near shore surveys, cannot be included without a coprable 
#representation in Canadian waters, also ME_NH is only 2000-2023,
#a temporal unbalance larger than I am comforable with 

catch_df2 %>%
  group_by(SURVEY, SEASON) %>%
  summarize(count = n(), .groups = "drop")
catch_df2 %>%
  group_by(SURVEY, Swept) %>%
  summarize(count = n(), .groups = "drop")
names(catch_df2)
catch_df2 <- catch_df2 %>%
  rename(Depth = Depth_value)

write.csv(catch_df2, here("Data/Derived/all_raw_halibut_catch_with_covariates_Al14.csv"))
library(ggplot2)
library(dplyr)

Data = read.csv(here::here("Data/Derived/all_raw_halibut_catch_with_covariates_Al14.csv"))
 #identidy & remove covariate outliers
ggplot(Data, aes(x = Depth)) + geom_histogram()
ggplot(Data, aes(x = BT_monthly)) + geom_histogram()
ggplot(Data, aes(x = SST_monthly)) + geom_histogram()#normal distribution
range(Data$Depth)#9.42973 2014.53257
summary(Data$Depth)
range(Data$BT_monthly)#-1.46245 20.49377
summary(Data$BT_monthly)
range(Data$SST_monthly)#-1.63401 25.90371

Data %>%
  filter(Depth > 1300 ) %>%
  count()#n186
Data %>%
  filter(Depth < 20 ) %>%
  count()#n13
Data %>%
  filter(BT_monthly > 17) %>%
  count()#n22

Data_clean <- Data %>%
  filter(Depth < 1300 | Depth >20 ,  BT_monthly < 17) # =57216-57194

Data_scaled <- Data_clean %>%
  mutate(
    Depth_sc = scale(Depth)[,1],
    BT_monthly_sc = scale(BT_monthly)[,1],
    SST_monthly_sc = scale(SST_monthly)[,1]
  )
write.csv(Data_scaled, here("Data/Derived/Halibut_Catch_Covariates_Scaled_Al14.csv"))

#END----




# Plot to make sure things look ok----
plot(Depth_raster, main = "", col = terrain.colors(20))
# Overlay catch data points (assuming catch_data has 'longitude' and 'latitude' columns)
points(catch_df$DECDEG_BEGLON, catch_df$DECDEG_BEGLAT, col = "black", pch = 46, cex = 1)

catch_data_zero <- catch_df %>%
  filter( Depth_value == 0)#n=194,all very close to land
catch_data_NA  <- catch_df %>% filter(is.na(Depth_value))#n=3093, all in southern range beyone BNAM
#write this to shorten the shapefile 
write.csv(catch_data_NA, here("Data/Derived/CatchCLip.csv"))

plot(catch_data_NA)
#plot survey data----
All_region <- st_read(here("Data/TempShapefiles/full_survey_region_simple.shp"))
crs <- st_crs(All_region)
plot(All_region)
#All
land <- st_read(here("Data/Mapping_shapefiles/poly_NAD83.shp"))
land <- st_transform (land, crs)
land <- st_make_valid(land)

#plot zeros
catch_data_zero_sf <- catch_data_NA |> 
  st_as_sf(coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"), crs = crs)  
zero_bbox <- st_bbox(catch_data_zero_sf)
zero_bbox_poly <- st_as_sfc(st_bbox(zero_bbox, crs = st_crs(land)))
zero_land <- st_intersection(land, zero_bbox_poly)

ggplot() +
  
  geom_sf(data = zero_land, fill = "gray80", color = "black") +
  geom_sf(data = catch_data_zero_sf, aes(color = SURVEY)) 

plot(catch_data_zero_sf)
