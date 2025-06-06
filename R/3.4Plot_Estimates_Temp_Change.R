#Plotting estimates vs temperature figure 
#STEP 1: Plotting abundance estimates and difference
  #Before, 1990-2005, During(2006-2023), mean sqrt(Abun.) rasters calculated in 3.2
  #diff_rast_spring.tif: difference(After-Before) Avg.Count rasters calculated in 3.2, sqrt transformation here for plotting 
  #percent_change_rast_spring.tif: percent change (((After-Before) / Before) * 100), (Avg.Count)
#STEP2: TEMPERATURE...prepare temperature and temperature change rasters from BNAM .mat files
  #mean_bottom_temperature_Before_annual.tif: Mean annual BT 1990-2005
  #mean_bottom_temperature_During_annual.tif: Mean annual BT 2006-2023
  #diff_Btemp_rast_annual.tif: (Mean annual BT 2006-2023)-(Mean annual BT 1990-2005)
#STEP3: plot temperature and temperature change

#STEP 1 Plotting abundance estimates and difference---
library(terra)
library(here)
library(sf)
library(ggplot2)

#read in the rasters(from 3.2Binned_Density_Plot.R)
out_dir <- here::here("2025-04-23/Output/GridPlot")#for output

region_shape <- st_read(here::here("R/Shapefiles/IndexShapefiles/Full_RegionAl14.shp"))#for clipping interpolation
region_shape <- st_make_valid(region_shape)

r1 <- rast(here::here("2025-04-23/Output/GridPlot/AtlanticHalibut_Index_gctl_sqrt_Spring_Index_gctl_bin1.tif"))
r2 <- rast(here::here("2025-04-23/Output/GridPlot/AtlanticHalibut_Index_gctl_sqrt_Spring_Index_gctl_bin2.tif"))
r1<- mask(r1, region_shape)
r2<- mask(r2, region_shape)
plot(r1)
plot(r2)


#turn the rasters to a df for ggplot
#load data from Sup2
#Before, 1990-2005
rast_lims<-c(0,80)

R1_df <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)  # Include coordinates
names(R1_df)[3] <- "EstimatedAbundance"

BeforePlot<- ggplot() +
  geom_raster(data = R1_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  scale_fill_viridis_c()+
 # scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(a) Mean Estimated Abundance, 1990-2005", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  theme(
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1)  # anchor legend to top-left of its box
  )

#During, 2006-2023
R2_df <- as.data.frame(r2, xy = TRUE, na.rm = TRUE)  # Include coordinates
names(R2_df)[3] <- "EstimatedAbundance"

DuringPlot<- ggplot() +
  geom_raster(data = R2_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  scale_fill_viridis_c()+
  # scale_fill_gradientn(colors = c("darkblue", "deepskyblue1","darkorange",   "red"), na.value = "transparent",limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="Estimated Abundance, 2006-2023", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  theme(
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # rmoves minor grid lines
    legend.position = "inside",
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1)  # anchor legend to top-left of its box
  )

#Difference
#to get the difference and percent change we will difference the raw data 
r1 <- rast(here::here("2025-04-23/Output/GridPlot/AtlanticHalibut_Index_gctl_raw_Spring_Index_gctl_bin1.tif"))
r2 <- rast(here::here("2025-04-23/Output/GridPlot/AtlanticHalibut_Index_gctl_raw_Spring_Index_gctl_bin2.tif"))

max(r1)

r1<- mask(r1, region_shape)
r2<- mask(r2, region_shape)
plot(r1)
plot(r2)
#Create rasters for difference and % change
diff_rast <- r2 - r1
percent_change_rast <- ((r2 - r1) / r1) * 100
percent_change_rast[!is.finite(percent_change_rast)] <- NA
plot(percent_change_rast)
plot(diff_rast)
max(diff_rast)

diff_rast_df <- as.data.frame(diff_rast, xy = TRUE, na.rm = TRUE)  # Include coordinates
names(diff_rast_df)[3] <- "difference"

library(scales)
#set up how the colour gradient will be defined
min_val  <- min(diff_rast_df$difference, na.rm = TRUE)
max_val <- max(diff_rast_df$difference, na.rm = TRUE)
values <- rescale(c(min_val,-30,  -0.01, 0, 0.01, 500, max_val))
range(diff_rast_df$difference)#min:-446.4199 max: 5781.7812
#because the positive range is so much larger we will transform these so that the negative range shows up
#diff_rast_df$difference_trans <- sign(diff_rast_df$difference) * sqrt(abs(diff_rast_df$difference))#because the 
#min_val  <- min(diff_rast_df$difference_trans, na.rm = TRUE)
#max_val <- max(diff_rast_df$difference_trans, na.rm = TRUE)
#values <- rescale(c(min_val,  -0.01, 0, 0.01, 28, max_val))
#check out the colours in vik
scico::scico(n = 10, palette = "vik")
#"#001260" "#023E7D" "#1D6E9C" "#71A7C4" "#C9DDE7" "#EACEBE" "#D29773" "#BD6432" "#8B2706" "#590007"

DifferencePlot<-ggplot() +
  geom_raster(data = diff_rast_df, aes(x = x, y = y, fill = difference)) +
  scale_fill_gradientn(
    colours = c("#001260","#1D6E9C",  "#C9DDE7", "white", "#EACEBE", "#D29773",  "#8B2706"), # duplicate colors for sharp edges
    values = values,
    limits = c(min_val, max_val),
    oob = scales::squish
  ) +
  #coord_fixed() +
  #scale_fill_scico(palette = "vik")+
  #scale_fill_viridis_c()+
  # scale_fill_gradientn(colors = c("darkblue", "deepskyblue1",   "red"), na.value = "transparent") +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(b) Change in Estimated Abundance (2006-2023)", x = NULL, y = NULL, fill = "Avg.Count")+
  theme(
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1)  # anchor legend to top-left of its box
  )
DifferencePlot
DifferencePlot+BeforePlot
#Percent Change
percent_change_df <- as.data.frame(percent_change_rast, xy = TRUE, na.rm = TRUE)  # Include coordinates
names(percent_change_df)[3] <- "PChange"

ChangePlot<-ggplot() +
  geom_raster(data = percent_change_df, aes(x = x, y = y, fill = PChange)) +
  scale_fill_viridis_c()+
 # scale_fill_gradientn(colors = c("darkblue", "deepskyblue1",  "red"), na.value = "transparent") +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  labs(title="Percent Change", x = NULL, y = NULL)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1)  # anchor legend to top-left of its box
  )

BeforePlot+DifferencePlot

#save the rasters for difference and change
writeRaster(diff_rast, filename = paste0(out_dir, "/diff_rast_spring.tif"), overwrite = TRUE)
writeRaster(percent_change_rast, filename = paste0(out_dir, "/percent_change_rast_spring.tif"), overwrite = TRUE)


#STEP2: TEMPERATURE...prepare temperature and temperature change rasters from BNAM .mat files ----
#make a spring 1990-2005 and 2006-2023 layer----

library(R.matlab)
library(raster)
library(sp)
library(lubridate)

mat_test <- readMat("C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSsfce-20250319T175426Z-001/TSsfce/TSsfce_1990.mat", fixNames = TRUE)

# Function to process individual .mat files----
process_mat_to_raster_stack <- function(mat_file, info_file, yearrange, variable_name) {
  # Load .mat files
  mat_data <- readMat(mat_file, fixNames = TRUE)
  data_info <- readMat(info_file, fixNames = TRUE)
  
  # Extract lat/lon and mask
  mat_data$nav.lon <- data_info$nav.lon
  mat_data$nav.lat <- data_info$nav.lat
  mat_data$mask <- data_info$land.mask
  
  # Get the year
  year <- as.integer(mat_data$yr)
  
  # Only proceed if year is in desired range
  if (!(year %in% yearrange)) return(NULL)
  
  # Flatten lat/lon
  BNAM_lat <- as.vector(mat_data$nav.lat[1:801, 1:401])
  BNAM_lon <- as.vector(mat_data$nav.lon[1:801, 1:401])
  
  # Only extract months 4(April, 5 (May), 6 (June)
  selected_months <- 1:12 #set for whole year
  month_list <- lapply(selected_months, function(i) {
    as.vector(mat_data[[variable_name]][i, 1:801, 1:401])
  })
  
  # Create column names like "X1990.05", etc.
  col_names <- sprintf("X%d.%02d", year, selected_months)
  
  # Create data frame
  df <- data.frame(
    Latitude = BNAM_lat,
    Longitude = BNAM_lon,
    setNames(data.frame(month_list), col_names)
  )
  
  coordinates(df) <- ~Longitude + Latitude
  rast <- raster(ncol = 800, nrow = 400)
  extent(rast) <- extent(df)
  
  # Rasterize each selected month
  raster_List <- setNames(
    lapply(col_names, function(name) {
      rasterize(df, rast, df[[name]], fun = mean)
    }),
    col_names
  )
  
  # Focal fill function
  fill.na <- function(x, i = 5) {
    if (is.na(x)[i]) {
      return(round(mean(x, na.rm = TRUE), 5))
    } else {
      return(round(x[i], 5))
    }
  }
  
  # Fill NAs with focal
  raster_List <- lapply(raster_List, function(r) focal(r, w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE))
  
  # Stack and return
  raster_stack <- stack(raster_List)
  return(raster_stack)
}
# Process all .mat files in a folder
process_all_mat_files <- function(mat_folder, info_file, variable_name) {
  # Get list of .mat files
  mat_files <- list.files(mat_folder, pattern = "\\.mat$", full.names = TRUE)
  
  # Apply function to all files and combine into a large stack
  #all_raster_stacks <- lapply(mat_files, function(file) process_mat_to_raster_stack(file, info_file, yearrange, variable_name))
  
  # Merge all raster stacks into a single giant stack
  #final_raster_stack <- stack(all_raster_stacks)
  all_raster_stacks <- lapply(mat_files, function(file) {
    process_mat_to_raster_stack(file, info_file, yearrange, variable_name)
  })
  
  # Remove NULLs (files skipped due to year not in range)
  all_raster_stacks <- Filter(Negate(is.null), all_raster_stacks)
  
  # Combine only valid RasterStacks
  final_raster_stack <- stack(all_raster_stacks)
  return(final_raster_stack)
}
#----
#Make rasters----
# Surface Temperature
#mat_folder <- "C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSsfce-20250319T175426Z-001/TSsfce/"  #Surface
mat_folder <- "C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSbtm-20250319T175520Z-001/TSbtm/"  #bottom
info_file <- "C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/bnam_grid_info.mat"
# Select the variable you want to process
#variable_to_process <- "Tsfce"  #surface
variable_to_process <- "Tbtm"  #bottom

# Run function on all .mat files
yearrange<- 1990:2005
Before_T_Stack <- process_all_mat_files(mat_folder, info_file, variable_to_process)
yearrange<- 2006:2023
After_T_Stack <- process_all_mat_files(mat_folder, info_file, variable_to_process)

names(Before_T_Stack)
names(After_T_Stack)

mean_Braster <- calc(Before_T_Stack, fun = mean, na.rm = TRUE)
mean_Araster <- calc(After_T_Stack, fun = mean, na.rm = TRUE)
plot(mean_Braster)
plot(mean_Araster)
diff_temp_rast <- mean_Araster - mean_Braster
plot(diff_temp_rast)

out_dir <- here::here("2025-04-23/Output/GridPlot")
#annual mean Bottom 
writeRaster(mean_Braster, filename = paste0(out_dir, "/mean_bottom_temperature_Before_annual.tif"), overwrite = TRUE)
writeRaster(mean_Araster, filename = paste0(out_dir, "/mean_bottom_temperature_During_annual.tif"), overwrite = TRUE)
writeRaster(diff_temp_rast, filename = paste0(out_dir, "/diff_Btemp_rast_annual.tif"), overwrite = TRUE)
#annual mean surface
#writeRaster(mean_Braster, filename = paste0(out_dir, "/mean_temperature_Before_annual.tif"), overwrite = TRUE)
#writeRaster(mean_Araster, filename = paste0(out_dir, "/mean_temperature_During_annual.tif"), overwrite = TRUE)
#writeRaster(diff_temp_rast, filename = paste0(out_dir, "/diff_temp_rast_annual.tif"), overwrite = TRUE)
#spring mean surface
#writeRaster(mean_Braster, filename = paste0(out_dir, "/mean_surface_temperature_Before_Spring.tif"), overwrite = TRUE)
#writeRaster(mean_Araster, filename = paste0(out_dir, "/mean_surface_temperature_During_Spring.tif"), overwrite = TRUE)
#writeRaster(diff_temp_rast, filename = paste0(out_dir, "/diff_Stemp_rast_Spring.tif"), overwrite = TRUE)

#STEP3 plot temperature and temperature change----
library(scico)
library(ggplot2)
BT1_r <- rast(here::here("2025-04-23/Output/GridPlot/mean_bottom_temperature_Before_annual.tif"))
BT2_r <- rast(here::here("2025-04-23/Output/GridPlot/mean_bottom_temperature_During_annual.tif"))
BTchange_r <- rast(here::here("2025-04-23/Output/GridPlot/diff_Btemp_rast_annual.tif"))
BT1_r_df <- as.data.frame(BT1_r, xy = TRUE, na.rm = TRUE)  # Include coordinates
names(BT1_r_df)[3] <- "Temperature"
BT2_r_df <- as.data.frame(BT2_r, xy = TRUE, na.rm = TRUE)  # Include coordinates
names(BT2_r_df)[3] <- "Temperature"
BTchange_r_df <- as.data.frame(BTchange_r, xy = TRUE, na.rm = TRUE)  # Include coordinates
names(BTchange_r_df)[3] <- "Temperature"

#we want 0 to be the midpoint
range(BT1_r_df)
hist(BT1_r_df$Temperature)
min_BT  <- min(BT1_r_df$Temperature, na.rm = TRUE)
max_BT <- max(BT1_r_df$Temperature, na.rm = TRUE)
#colours <- c("blue", "white", "darkred")
#values_BT <- rescale(c(min_BT,  -0.01, 0, 0.01, max_BT))
#values_BT <- rescale(c(min_BT,  -0.01, 0, 0.01,  5,9, max_BT)) 
#colours <- c("blue", "white", "yellow", "orange", "darkred")

tempBefore<- ggplot() +
  geom_raster(data = BT1_r_df, aes(x = x, y = y, fill = Temperature)) +
  #scale_fill_gradientn(colours = rev(rainbow(7)))+
   scale_fill_gradientn(
    colours = c("purple", "blue", "cyan", "green", "yellow", "orange", "red"),     # Reversed rainbow   # Reversed rainbow
    values = scales::rescale(c(min_BT, 0, 2, 4, 6, 8 , max_BT)),
    limits = c(min_BT, max_BT),
    oob = scales::squish
  )+
  #scale_fill_scico(palette = "vik", name = "Temp (°C)")+
  #scale_fill_gradientn(colors = c("blue1", "red"), na.value = "transparent") +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(c) Mean Bottom Temp. (1990-2005)", x = NULL, y = NULL, fill = "°C")+
  theme(
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1)  # anchor legend to top-left of its box

    )
tempBefore

tempAfter<- ggplot() +
  geom_raster(data = BT2_r_df, aes(x = x, y = y, fill = Temperature)) +
  scale_fill_scico(palette = "vik", name = "Temp (°C)")+
  #scale_fill_gradientn(colors = c("blue1", "red"), na.value = "transparent") +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(c) Mean Bottom Temp. 2006-2023", x = NULL, y = NULL, fill = "°C")+
  theme(
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1)  # anchor legend to top-left of its box
  )

max_change <- max(abs(BTchange_r_df$Temperature), na.rm = TRUE)
min_change <- max(abs(BTchange_r_df$Temperature), na.rm = TRUE)

tempchange<-ggplot() +
  geom_raster(data = BTchange_r_df, aes(x = x, y = y, fill = Temperature)) +
  scale_fill_scico(
    palette = "vik",
    name = "(°C)",
    limits = c(-min_change, max_change), 
    oob = scales::squish
  )+
  #scale_fill_scico(palette = "vik", name = "°C")+
  #scale_fill_gradientn(colors = c("blue1", "red"), na.value = "transparent") +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(d) Change in Mean BTemp. (2006-2023)", x = NULL, y = NULL, fill = "°C")+
  theme(
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1)  # anchor legend to top-left of its box
  )

library(gridExtra)
grid.arrange(tempBefore, tempchange, ncol = 2)

#FIGURE 2:
grid.arrange(BeforePlot,DifferencePlot,tempBefore,tempchange, ncol = 2)

GridPlot_BTemp