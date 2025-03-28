
#BNAM .mat to .nc

library(R.matlab)
library(terra)
library(raster)
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
#Monthly Temperature
# Read the .mat file and extract the data
mat_data = readMat('C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSsfce-20250319T175426Z-001/TSsfce/TSsfce_1990.mat', fixNames = TRUE) # load file
array_data <- mat_data$Tsfce
mat_data2 = readMat('C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSbtm-20250319T175520Z-001/TSbtm/TSbtm_1990.mat', fixNames = TRUE) # load file
array_data2 <- mat_data2$TSbtm
#-----
#test and get lat lon----
# Check dimensions
str(mat_data)
dims <- dim(array_data)
print(paste("Matrix dimensions:", paste(dims, collapse=" x ")))
# Create an empty raster stack
Temperature_stack <- stack()
#loop through layers
for (i in 1:dims[1]) {  # layers are on the first dimension
  #create raster
  raster_layer <- raster(array_data[i,,])
  #Flip on y so that they plot properly, stack
  raster_layer <- flip(raster_layer, direction = "y")
  Temperature_stack <- addLayer(Temperature_stack, raster_layer)
}
#test
plot(Temperature_stack[[3]], main = "March Surface Temperature", col = terrain.colors(20))

#there doesn't seem to be any lat/long data in this df so pulling from an older one that i have 
#Latitude
Tbtm2055 = readMat('C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/for_kiyomi_02.mat', fixNames = TRUE)
Lat_data <- Tbtm2055$nav.lat
# Check dimensions
str(Tbtm2055)
Lat_dims <- dim(Lat_data)
print(paste("Matrix dimensions:", paste(Lat_dims, collapse=" x ")))
#create Lat raster
  Lat_dims_raster_layer <- raster(Lat_data[,])
  #Flip on y so that they plot properly, stack
  Lat_dims_raster_layer <- flip(Lat_dims_raster_layer, direction = "y")
  
plot(Lat_dims_raster_layer, main = "Latitude", col = terrain.colors(20))

#Longitude
Lon_data <- Tbtm2055$nav.lon
# Check dimensions
str(Tbtm2055)
Lon_dims <- dim(Lon_data)
print(paste("Matrix dimensions:", paste(Lon_dims, collapse=" x ")))
#create Lat raster
Lon_dims_raster_layer <- raster(Lon_data[,])
#Flip on y so that they plot properly, stack
Lon_dims_raster_layer <- flip(Lon_dims_raster_layer, direction = "y")

plot(Lon_dims_raster_layer, main = "Longitude", col = terrain.colors(20))

#Depth
Depth_data <- Tbtm2055$Bathy.depth
# Check dimensions
str(Tbtm2055)
Depth_dims <- dim(Depth_data)
print(paste("Matrix dimensions:", paste(Depth_dims, collapse=" x ")))
#create Lat raster
Depth_dims_raster_layer <- raster(Depth_data[,])
#Flip on y so that they plot properly, stack
Depth_dims_raster_layer <- flip(Depth_dims_raster_layer, direction = "y")

plot(Depth_dims_raster_layer, main = "Depth", col = terrain.colors(20))
#---------
#---------
#Surface temperature----
#loop and function
# Set the path to the folder containing your .mat files
folder_path <- "C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSsfce-20250319T175426Z-001/TSsfce/"

# List all .mat files in the folder
mat_files <- list.files(folder_path, pattern = "\\.mat$", full.names = TRUE)

# Initialize an empty raster stack
Temperature_stack <- stack()

# Loop through each .mat file
for (file in mat_files) {
  # Extract year from filename (assuming the year is the last 4 characters before the .mat extension)
  year <- substr(basename(file), nchar(basename(file)) - 7, nchar(basename(file)) - 4)
  # Read the .mat file
  mat_data <- readMat(file, fixNames = TRUE)
  array_data <- mat_data$Tsfce
  
  # Check dimensions of the array (rows, columns, layers)
  dims <- dim(array_data)
  
  # Loop through layers (months)
  for (i in 1:dims[1]) {  # Assuming layers are on the first dimension
    # Create a raster for each layer
    raster_layer <- raster(array_data[i,,])
    
    # Flip y-axis to correct orientation
    raster_layer <- flip(raster_layer, direction = "y")
    
    # Set layer name based on year and month
    layer_name <- paste(year, sprintf("%02d", i), sep = "-")
    names(raster_layer) <- layer_name
    
    # Add layer to the stack
    Temperature_stack <- addLayer(Temperature_stack, raster_layer)
  }
}
layer_names <- names(Temperature_stack)
print(layer_names)
plot(Temperature_stack[[1]], main = "March Surface Temperature", col = terrain.colors(20))

# Set the correct longitude and latitude values to the raster stack
x_coords <- as.vector(Lon_dims_raster_layer)
y_coords <- as.vector(Lat_dims_raster_layer)

# Assign the coordinates
extent(Temperature_stack) <- extent(min(x_coords), max(x_coords), min(y_coords), max(y_coords))
crs(Temperature_stack) <- "+proj=longlat +datum=WGS84"

# Plot to verify it shows correct lat/lon coordinates
plot(Temperature_stack[[24]], main = "Temperature with Correct Coordinates")
#---------
#Bottom Temperature----
# Set the path to the folder containing your .mat files
folder_path <- "C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSbtm-20250319T175520Z-001/TSbtm/"

# List all .mat files in the folder
mat_files <- list.files(folder_path, pattern = "\\.mat$", full.names = TRUE)

# Initialize an empty raster stack
B_Temperature_stack <- stack()

# Loop through each .mat file
for (file in mat_files) {
  # Extract year from filename (assuming the year is the last 4 characters before the .mat extension)
  year <- substr(basename(file), nchar(basename(file)) - 7, nchar(basename(file)) - 4)
  
  # Read the .mat file
  mat_data <- readMat(file, fixNames = TRUE)
  array_data <- mat_data$Tbtm
  
  # Check dimensions of the array (rows, columns, layers)
  dims <- dim(array_data)
  
  # Loop through layers (months)
  for (i in 1:dims[1]) {  # Assuming layers are on the first dimension
    # Create a raster for each layer
    raster_layer <- raster(array_data[i,,])
    
    # Flip y-axis to correct orientation
    raster_layer <- flip(raster_layer, direction = "y")
    
    # Set layer name based on year and month
    layer_name <- paste(year, sprintf("%02d", i), sep = "-")
    names(raster_layer) <- layer_name
    
    # Add layer to the stack
    B_Temperature_stack <- addLayer(B_Temperature_stack, raster_layer)
  }
}
layer_names <- names(B_Temperature_stack)
print(layer_names)
plot(B_Temperature_stack[[1]], main = "March Bottom Temperature", col = terrain.colors(20))

# Set the correct longitude and latitude values to the raster stack
x_coords <- as.vector(Lon_dims_raster_layer)
y_coords <- as.vector(Lat_dims_raster_layer)

# Assign the coordinates
extent(B_Temperature_stack) <- extent(min(x_coords), max(x_coords), min(y_coords), max(y_coords))
crs(B_Temperature_stack) <- "+proj=longlat +datum=WGS84"

# Plot to verify it shows correct lat/lon coordinates
plot(B_Temperature_stack[[1]], main = "Bottom Temperature with Correct Coordinates")
#--------
#--------
#Depth:  Reproject the depth layer Assign the coordinates----
extent(Depth_dims_raster_layer) <- extent(min(x_coords), max(x_coords), min(y_coords), max(y_coords))
crs(Depth_dims_raster_layer) <- "+proj=longlat +datum=WGS84"

# Plot to verify it shows correct lat/lon coordinates
plot(Depth_dims_raster_layer, main = "Depth with Correct Coordinates")
#--------
#--------
#pull our new data (Depth_dims_raster_layer, B_Temperature_stack ,Temperature_stack, to the survey data----
catch_data <- read_rds("Data/Derived/all_raw_halibut_catch_formatted.rds") 
head(catch_data)

#ATTACH Environmentla covariates#ATTACHSURVEY Environmentla covariates----
"Depth"         
"SST_seasonal"  
"BT_seasonal"  
library(dplyr)
library(lubridate)
library(terra)

crs(B_Temperature_stack)

library(sp)
library(raster)
library(dplyr)

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
proj4string(catch_data) <- CRS(proj4string(B_Temperature_stack))

# empty vector to store the extracted temperature values
BT_extracted_values <- numeric(nrow(catch_data))
T_extracted_values <- numeric(nrow(catch_data))

# Loop through each row of the catch_data to extract the correct layer and value
#BT
for (i in 1:nrow(catch_data)) {
  # Extract the layer name from the LAYER_NAME column
  layer_name <- catch_data$LAYER_NAME[i]
  
  # Check if the layer exists in the raster stack
  if (layer_name %in% names(B_Temperature_stack)) {
    # Extract the correct layer from the raster stack
    raster_layer <- B_Temperature_stack[[layer_name]]
    
    # Extract the value from the specified layer for the corresponding lat/lon
    BT_extracted_values[i] <-  raster::extract(raster_layer, catch_data[i, , drop = FALSE])
  } else {
    # If the layer does not exist, assign NA
    BT_extracted_values[i] <- NA
  }
}
#surface temp
for (i in 1:nrow(catch_data)) {
  # Extract the layer name from the LAYER_NAME column
  layer_name <- catch_data$LAYER_NAME[i]
  
  # Check if the layer exists in the raster stack
  if (layer_name %in% names(Temperature_stack)) {
    # Extract the correct layer from the raster stack
    raster_layer <- Temperature_stack[[layer_name]]
    
    # Extract the value from the specified layer for the corresponding lat/lon
    T_extracted_values[i] <-  raster::extract(raster_layer, catch_data[i, , drop = FALSE])
  } else {
    # If the layer does not exist, assign NA
    T_extracted_values[i] <- NA
  }
}

# Add the extracted temperature values to catch_data
catch_data$BT_monthly <- BT_extracted_values
catch_data$T_monthly <- T_extracted_values

# Depth, add values
depth_extracted_values <- numeric(nrow(catch_data))
# Loop through each row of the catch_data to extract depth values
for (i in 1:nrow(catch_data)) {
  # Extract the value from the Depth_dims_raster_layer for the corresponding lat/lon
  depth_extracted_values[i] <-  raster::extract(Depth_dims_raster_layer, catch_data[i, , drop = FALSE])
}
catch_data$Depth_value <- depth_extracted_values
catch_df <- as.data.frame(catch_data)
catch_df$DECDEG_BEGLON <- coordinates(catch_data)[, 1]  # Extract longitude
catch_df$DECDEG_BEGLAT <- coordinates(catch_data)[, 2]  # Extract latitude
 
write_rds(catch_df, here("Data/Derived/all_raw_halibut_catch_with_covariates.rds"), compress = "gz")
#-------

#The data now have 
catch_data <- read_rds("Data/Derived/all_raw_halibut_catch_with_covariates.rds") 
#we do not have surface temperature files beyond 2021, BUT we do have 2022/2023 for bottom temperature 
catch_data <- catch_data %>%
  filter(EST_YEAR < 2022)
write_rds(catch_data, here("Data/Derived/all_raw_halibut_catch_with_covariates_1990to2021.rds"), compress = "gz")
catch_data <- read_rds("Data/Derived/all_raw_halibut_catch_with_covariates_1990to2021.rds") 

str(catch_data)
catch_data %>%
  count(SURVEY, PRESENCE)
catch_data %>%
  group_by(SURVEY, SEASON) %>%
  summarize(count = n(), .groups = "drop")
catch_data %>%
  group_by(SURVEY, Swept) %>%
  summarize(count = n(), .groups = "drop")
hist(catch_data$Swept)

# From here, the data represent 1990-2021, this solves the NAs from missing year/months
#now we still have locations where all covariates are 0
catch_data_zero <- catch_data %>%
  filter(BT_monthly == 0, T_monthly == 0, Depth_value == 0)#n=6345

library(here)
#plot survey data----
All_region <- st_read(here:here("Data/TempShapefiles/full_survey_region_simple.shp"))
crs <- st_crs(All_region)
plot(All_region)
#All
land <- st_read(here:here("Data/Mapping_shapefiles/poly_NAD83.shp"))
land <- st_transform (land, crs)
land <- st_make_valid(land)

#plot zeros
catch_data_zero_sf <- catch_data_zero |> 
  st_as_sf(coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"), crs = crs)  
zero_bbox <- st_bbox(catch_data_zero_sf)
zero_bbox_poly <- st_as_sfc(st_bbox(zero_bbox, crs = st_crs(land)))
zero_land <- st_intersection(land, zero_bbox_poly)

ggplot() +
  geom_sf(data = catch_data_zero_sf, aes(color = SURVEY)) + 
  geom_sf(data = zero_land, fill = "gray80", color = "black") 

#all
catch_data_sf <- catch_data |> 
  st_as_sf(coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"), crs = crs)  
all_bbox <- st_bbox(catch_data_sf)
all_bbox_poly <- st_as_sfc(st_bbox(all_bbox, crs = st_crs(land)))
all_land <- st_intersection(land, all_bbox_poly)

ggplot() +
  geom_sf(data = catch_data_sf, aes(color = SURVEY)) + 
  geom_sf(data = all_land, fill = "gray80", color = "black") 

#US
US_Data<-subset(catch_data, SURVEY=="MA"|SURVEY=="ME_NH"|SURVEY=="NEFSC")
US_Data_sf <- US_Data |> 
  st_as_sf(coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"), crs = crs)  # Use same CRS as land_sf
#for plotting
US_bbox <- st_bbox(US_Data_sf)
US_bbox_poly <- st_as_sfc(st_bbox(US_bbox, crs = st_crs(land)))
US_land <- st_intersection(land, US_bbox_poly)

ggplot() +
  geom_sf(data = US_Data_sf, aes(color = SURVEY)) + 
  facet_wrap(~SURVEY) +  
  geom_sf(data = US_land, fill = "gray80", color = "black") 


