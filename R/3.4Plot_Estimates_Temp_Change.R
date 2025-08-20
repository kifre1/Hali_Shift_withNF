#Plotting estimates vs temperature figure 
#STEP 1: Plotting abundance estimates and difference
  #(PANEL A): Before, 1990-2005 , mean sqrt(Abun.) rasters calculated in 3.2
  #During(2006-2023), mean sqrt(Abun.) rasters calculated in 3.2
  #(PANEL B):diff_rast_spring.tif: difference(After-Before) Avg.Count rasters calculated in 3.2, 
  #percent_change_rast_spring.tif: percent change (((After-Before) / Before) * 100), (Avg.Count)
#STEP2: TEMPERATURE...prepare temperature and temperature change rasters from BNAM .mat files
  #mean_bottom_temperature_Before_annual.tif: Mean annual BT 1990-2005
  #mean_bottom_temperature_During_annual.tif: Mean annual BT 2006-2023
  #diff_Btemp_rast_annual.tif: (Mean annual BT 2006-2023)-(Mean annual BT 1990-2005)
#STEP3: plot temperature and temperature change
  #(PANEL C):TempBefore, 
  #TempAfter, 
  #(PANEL D): TempChange (TempAfter-TempBefore)
  #FIGURE 2: BeforePlot,DifferencePlot,tempBefore,tempchange
#STEP 4: Appendix figure3, seasonal comparison of estimates 
#save as GridPlot_BTemp

#STEP 5:Plots for shinyApp start around line 792


#STEP 1 Plotting abundance estimates and difference---

library(ggplot2)
library(sf)
library(dplyr) 
library(scales)
library(terra)
library(here)
#read in the rasters(from 3.2Binned_Density_Plot.R)
out_dir <- here::here("2025-04-23/Output/GridPlot")#for output

region_shape <- st_read(here::here("R/Shapefiles/IndexShapefiles/Full_RegionAl14.shp"))#for clipping interpolation
region_shape <- st_make_valid(region_shape)

r1 <- rast(here::here("2025-04-23/Output/GridPlot/SeasonalSQRT/AtlanticHalibut_Index_gctl_sqrt_Spring_Index_gctl_bin1.tif"))
r2 <- rast(here::here("2025-04-23/Output/GridPlot/SeasonalSQRT/AtlanticHalibut_Index_gctl_sqrt_Spring_Index_gctl_bin2.tif"))
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

#(PANEL A)
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
  labs(title=NULL, x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  theme(
    plot.margin = unit(c(23,3,0,2), "pt"),
    #plot.margin=margin(20,3,3,3),
    axis.text = element_text(family = "serif",size=10),
    plot.title=element_text(size=16,family="serif"),
    legend.title = element_text(size = 10,family="serif"), 
    legend.text = element_text(size = 10,family="serif"),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.key.height   = unit(.4, "cm"),
    legend.key.width = unit(.5, "cm"),
    legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2),
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1),# anchor legend to top-left of its box
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
r1 <- rast(here::here("2025-04-23/Output/GridPlot/EstimateDifference/AtlanticHalibut_Index_gctl_raw_Spring_Index_gctl_bin1.tif"))
r2 <- rast(here::here("2025-04-23/Output/GridPlot/EstimateDifference/AtlanticHalibut_Index_gctl_raw_Spring_Index_gctl_bin2.tif"))

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

#set up how the colour gradient will be defined
min_val  <- min(diff_rast_df$difference, na.rm = TRUE)
max_val <- max(diff_rast_df$difference, na.rm = TRUE)
values <- scales::rescale(c(min_val,-30,  -0.01, 0, 0.01, 500, max_val))
range(diff_rast_df$difference)#min:-446.4199 max: 5781.7812

scico::scico(n = 10, palette = "vik")
#"#001260" "#023E7D" "#1D6E9C" "#71A7C4" "#C9DDE7" "#EACEBE" "#D29773" "#BD6432" "#8B2706" "#590007"

#(PANEL B)
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
  labs(title=NULL, x = NULL, y = NULL, fill = "Avg.\nCount")+
  theme(
    plot.margin = unit(c(23,3,0,2), "pt"),
  #plot.margin=margin(20,3,3,3),
    axis.text = element_text(family = "serif",size=10),
    plot.title=element_text(size=16,family="serif"),
    legend.title = element_text(size = 10,family="serif"), 
    legend.text = element_text(size = 10,family="serif"),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.key.height   = unit(.4, "cm"),
    legend.key.width = unit(.5, "cm"),
    legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2),
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1),# anchor legend to top-left of its box
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
    plot.margin = grid::unit(c(1, 1,1, 1)),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2),
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1)  # anchor legend to top-left of its box
  )

BeforePlot+DifferencePlot

#save the rasters for difference and change
writeRaster(diff_rast, filename = paste0(out_dir, "/EstimateDifference/diff_rast_spring.tif"), overwrite = TRUE)
writeRaster(percent_change_rast, filename = paste0(out_dir, "/EstimateDifference/percent_change_rast_spring.tif"), overwrite = TRUE)


#STEP2: TEMPERATURE...prepare temperature and temperature change rasters from BNAM .mat files ----
#make a spring 1990-2005 and 2006-2023 layer----

library(R.matlab)
library(raster)
library(sp)
library(lubridate)

mat_test <- readMat("C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSsfce-20250319T175426Z-001/TSsfce/TSsfce_1990.mat", fixNames = TRUE)

#SKIP FOR PLOTTING ONLY  Function to process individual .mat files----
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

#Make rasters
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
writeRaster(mean_Braster, filename = paste0(out_dir, "/BTChange/mean_bottom_temperature_Before_annual.tif"), overwrite = TRUE)
writeRaster(mean_Araster, filename = paste0(out_dir, "/BTChange/mean_bottom_temperature_During_annual.tif"), overwrite = TRUE)
writeRaster(diff_temp_rast, filename = paste0(out_dir, "/BTChange/diff_Btemp_rast_annual.tif"), overwrite = TRUE)
#annual mean surface
#writeRaster(mean_Braster, filename = paste0(out_dir, "/STChange/mean_temperature_Before_annual.tif"), overwrite = TRUE)
#writeRaster(mean_Araster, filename = paste0(out_dir, "/STChange/mean_temperature_During_annual.tif"), overwrite = TRUE)
#writeRaster(diff_temp_rast, filename = paste0(out_dir, "/STChange/diff_Stemp_rast_annual.tif"), overwrite = TRUE)
#spring mean surface
#writeRaster(mean_Braster, filename = paste0(out_dir, "/STChange/mean_surface_temperature_Before_Spring.tif"), overwrite = TRUE)
#writeRaster(mean_Araster, filename = paste0(out_dir, "/STChange/mean_surface_temperature_During_Spring.tif"), overwrite = TRUE)
#writeRaster(diff_temp_rast, filename = paste0(out_dir, "/STChange/diff_Stemp_rast_Spring.tif"), overwrite = TRUE)

#STEP3 plot temperature and temperature change----
library(scico)

BT1_r <- rast(here::here("2025-04-23/Output/GridPlot/BTChange/mean_bottom_temperature_Before_annual.tif"))
BT2_r <- rast(here::here("2025-04-23/Output/GridPlot/BTChange/mean_bottom_temperature_During_annual.tif"))
BTchange_r <- rast(here::here("2025-04-23/Output/GridPlot/BTChange/diff_Btemp_rast_annual.tif"))
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
sf::sf_use_s2(FALSE)  # needed for clean cropping sometimes

#(PANEL C)
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
  #scale_x_continuous(expand = c(0, 0)) +  # Remove x-axis expansion
  #scale_y_continuous(expand = c(0, 0)) +  # Remove y-axis expansion
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title=NULL, x = NULL, y = NULL, fill = "°C")+
  theme(
    plot.margin = unit(c(23,3,0,2), "pt"),
    #plot.margin=margin(20,3,3,3),
    axis.text = element_text(family = "serif",size=10),
    plot.title=element_text(size=16,family="serif"),
    legend.title = element_text(size = 10,family="serif"), 
    legend.text = element_text(size = 10,family="serif"),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.key.height   = unit(.4, "cm"),
    legend.key.width = unit(.5, "cm"),
    legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2),
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1),# anchor legend to top-left of its box
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

#(PANEL D)
tempchange<-ggplot() +
  geom_raster(data = BTchange_r_df, aes(x = x, y = y, fill = Temperature)) +
  scale_fill_scico(
    palette = "vik",
    name = "(°C)",
    limits = c(-min_change, max_change), 
    oob = scales::squish
  )+
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title=NULL, x = NULL, y = NULL, fill = "°C")+
  theme(
    plot.margin = unit(c(23,3,0,2), "pt"),
    #plot.margin=margin(20,3,3,3),
    axis.text = element_text(family = "serif",size=10),
    plot.title=element_text(size=16,family="serif"),
    legend.title = element_text(size = 10,family="serif"), 
    legend.text = element_text(size = 10,family="serif"),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.key.height   = unit(.4, "cm"),
    legend.key.width = unit(.5, "cm"),
    legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2),
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1),# anchor legend to top-left of its box
  )

library(gridExtra)
library(cowplot)
grid.arrange(tempBefore, tempchange, ncol = 2)

#FIGURE 2:

Figure2<-plot_grid(BeforePlot,DifferencePlot,tempBefore,tempchange, 
                   nrow = 2,ncol=2, labels = c("(a)", "(b)", "(c)", "(d)"),
                   align = "v", axis = "lr",rel_heights = c(.5,.5),
                   label_x      = c(0, 0, 0, 0),  # move labels slightly right
                   label_y      = c(0.995, 0.995, 0.995, 0.995),  # move labels down a bit
                   #hjust        = 0,              # left-aligned relative to label_x
                   #vjust        = .9,               # top-aligned relative to label_y
                   panel_spacing = unit(0, "lines"))

#save as GridPlot_BTemp
ggsave(
  filename = "Fig2_change_Maps.png",
  plot = Figure2,          # optional if it's the last plot
  path =  here::here("2025-04-23/FinalPlots"),
  width = 6.35,
  height = 5.1,
  dpi = 300
)


#STEP 4: Appendix figure3, seasonal comparison of estimates 

library(ggplot2)
library(sf)
library(dplyr) 
library(scales)
library(terra)
library(here)
#read in the rasters(from 3.2Binned_Density_Plot.R)
out_dir <- here::here("2025-04-23/Output/GridPlot")#for output

region_shape <- st_read(here::here("R/Shapefiles/IndexShapefiles/Full_RegionAl14.shp"))#for clipping interpolation
region_shape <- st_make_valid(region_shape)

Spring1 <- rast(here::here("2025-04-23/Output/GridPlot/SeasonalSQRT/AtlanticHalibut_Index_gctl_sqrt_Spring_Index_gctl_bin1.tif"))
Spring2 <- rast(here::here("2025-04-23/Output/GridPlot/SeasonalSQRT/AtlanticHalibut_Index_gctl_sqrt_Spring_Index_gctl_bin2.tif"))
Summer1 <- rast(here::here("2025-04-23/Output/GridPlot/SeasonalSQRT/AtlanticHalibut_Index_gctl_sqrt_Summer_Index_gctl_bin1.tif"))
Summer2 <- rast(here::here("2025-04-23/Output/GridPlot/SeasonalSQRT/AtlanticHalibut_Index_gctl_sqrt_Summer_Index_gctl_bin2.tif"))
Fall1 <- rast(here::here("2025-04-23/Output/GridPlot/SeasonalSQRT/AtlanticHalibut_Index_gctl_sqrt_Fall_Index_gctl_bin1.tif"))
Fall2 <- rast(here::here("2025-04-23/Output/GridPlot/SeasonalSQRT/AtlanticHalibut_Index_gctl_sqrt_Fall_Index_gctl_bin2.tif"))

#function to mask the raster with the study are shapefile, and turn it into a df for plotting 
process_estimate_raster <- function(raster_obj) {
  masked <- mask(raster_obj, region_shape)
  plot(masked)
  df <- as.data.frame(masked, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "EstimatedAbundance"
  
  return(df)
}

Spring1_df <- process_estimate_raster(Spring1)
range(Spring1_df$EstimatedAbundance)
Spring2_df <- process_estimate_raster(Spring2)
range(Spring2_df$EstimatedAbundance)
Summer1_df <- process_estimate_raster(Summer1)
range(Summer1_df$EstimatedAbundance)
Summer2_df <- process_estimate_raster(Summer2)
range(Summer2_df$EstimatedAbundance)
Fall1_df <- process_estimate_raster(Fall1)
range(Fall1_df$EstimatedAbundance)
Fall2_df <- process_estimate_raster(Fall2)
range(Fall2_df$EstimatedAbundance)

#to fix the legend scale
rast_lims<-c(0,110)

#Spring 1990-2005
Spring1_plot<-ggplot() +
  geom_raster(data = Spring1_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  scale_fill_viridis_c(limits = rast_lims)+
  # scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(a)", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  annotate(
    "text",
    x = -Inf, y = 51.7,
   label = " Spring: 1990-2005",
   hjust = 0, vjust = 1,
   size = 4,
   family = "serif"
  ) +
  theme(
    plot.margin = grid::unit(c(.05, .1, .05,.1), "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "none",
    axis.text.x = element_text(family = "serif",size=10),
    axis.text.y = element_text( family = "serif",size=10)
  )


#Spring 2006-2023
Spring2_plot<-ggplot() +
  geom_raster(data = Spring2_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  scale_fill_viridis_c(limits = rast_lims)+
  # scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(b)", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  annotate(
    "text",
    x = -Inf, y = 51.7,
    label = " Spring: 2006-2023",
    hjust = 0, vjust = 1,
    size = 4,
    family = "serif"
  ) +
  theme(
    plot.margin = grid::unit(c(.05, .1, .05,.1), "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "none",
    axis.text.x = element_text( family = "serif",size=10),
    axis.text.y = element_text( family = "serif",size=10)
  )
#Summer 1990-2005
Summer1_plot<-ggplot() +
  geom_raster(data = Summer1_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  scale_fill_viridis_c(limits = rast_lims)+
  # scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(c)", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  annotate(
    "text",
    x = -Inf, y = 51.7,
    label = " Summer: 1990-2005",
    hjust = 0, vjust = 1,
    size = 4,
    family = "serif"
  ) +
  theme(
    plot.margin = grid::unit(c(.05, .1, .05,.1), "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "none",
    axis.text.x = element_text(family = "serif",size=10),
    axis.text.y = element_text( family = "serif",size=10)
  )

#Summer 2006-2023
Summer2_plot<-ggplot() +
  geom_raster(data = Summer2_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  scale_fill_viridis_c(limits = rast_lims)+
  # scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(d)", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  annotate(
    "text",
    x = -Inf, y = 51.7,
    label = " Summer: 2006-2023",
    hjust = 0, vjust = 1,
    size = 4,
    family = "serif"
  ) +
  theme(
    plot.margin = grid::unit(c(.05, .1, .05,.1), "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "none",
    axis.text.x = element_text(family = "serif",size=10),
    axis.text.y = element_text( family = "serif",size=10)
  )

#Fall 1990-2005
Fall1_plot<-ggplot() +
  geom_raster(data = Fall1_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  scale_fill_viridis_c(limits = rast_lims)+
  # scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(e)", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  annotate(
    "text",
    x = -Inf, y = 51.7,
    label = " Fall: 1990-2005",
    hjust = 0, vjust = 1,
    size = 4,
    family = "serif"
  ) +
  theme(
    plot.margin = grid::unit(c(.05, .1, .05,.1), "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "none",
    axis.text.x = element_text( family = "serif",size=10),
    axis.text.y = element_text( family = "serif",size=10)
  )

#Fall 2006-2023
Fall2_plot<-ggplot() +
  geom_raster(data = Fall2_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  scale_fill_viridis_c(limits = rast_lims)+
  # scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(f)", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  annotate(
    "text",
    x = -Inf, y = 51.7,
    label = " Fall: 2006-2023",
    hjust = 0, vjust = 1,
    size = 4,
    family = "serif"
  ) +
  theme(
    plot.margin = grid::unit(c(.05, .1, .05,.1), "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "none",
    axis.text.x = element_text( family = "serif",size=10),
    axis.text.y = element_text(family = "serif",size=10)
  )
library(gridExtra)
library(ggpubr)
grid.arrange(Spring1_plot, Spring2_plot, Summer1_plot, Summer2_plot, Fall1_plot, Fall2_plot, ncol = 2, padding = unit(0, "line"))

#put the legend outside the plot
get_plot_legend<-ggplot() +
  geom_raster(data = Spring1_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  scale_fill_viridis_c(limits = rast_lims)+
  # scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="(a) Spring, 1990-2005", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  theme(
    plot.margin = grid::unit(c(.1, .1, .1,.1), "cm"),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 10,family="serif"), 
    legend.text = element_text(size = 10,family="serif"),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "inside",
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1)  # anchor legend to top-left of its box
  )

legend <- get_legend(
  get_plot_legend + theme_bw()+
    theme(
      text = element_text(family = "serif", size = 10),       # applies globally
      legend.text = element_text(family = "serif", size = 10),
      legend.title = element_text(family = "serif", size = 10)
    )
)


SeasonalPlot<-grid.arrange(
  grobs = list(Spring1_plot, Spring2_plot, Summer1_plot, Summer2_plot, Fall1_plot, Fall2_plot, legend),
  layout_matrix = rbind(
    c(1, 2, 7),
    c(3, 4, NA),
    c(5, 6, NA)
  ),
  padding = unit(0, "line"),
  widths = c(1, 1, 0.2)  # adjust the third column width as needed
)

ggsave(
  filename = "SeasonalEstimates.png",
  plot = SeasonalPlot,          # optional if it's the last plot
  path =  here::here("2025-04-23/FinalPlots"),
  width = 6.5,
  height = 7.3,
  dpi = 300
)

##FOR SHINY APP
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
gradient_colors1 <- rev(colorRampPalette(brewer.pal(9, "YlGnBu"), bias = 0.)(100))
#max_value<-max(R1_df$EstimatedAbundance)
max_value<-max(R2_df$EstimatedAbundance)
ShinyAbdMap<- ggplot() +
  geom_raster(data = R1_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  #scale_fill_viridis_c()+
  scale_color_gradientn(colors = gradient_colors1,
                        limits = c(0, max_value),   # ensure 0 = blue
                        oob = scales::squish  )+      # clamp values outside range
  scale_fill_gradientn(colors = gradient_colors1)+
  #scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="black", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
    labs(title="Abundance 1990-2005", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  theme(
    plot.margin = unit(c(23,3,0,2), "pt"),
    #plot.margin=margin(20,3,3,3),
    axis.text = element_text(family = "serif",size=10),
    plot.title=element_text(size=12,family="serif"),
    legend.title = element_text(size = 10,family="serif"), 
    legend.text = element_text(size = 10,family="serif"),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "bottom",
    legend.key.height   = unit(.4, "cm"),
    legend.key.width = unit(.5, "cm"),
    legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2),
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1),# anchor legend to top-left of its box
  )
ShinyAbdMap
ShinyAbdMap2<- ggplot() +
  geom_raster(data = R2_df, aes(x = x, y = y, fill = EstimatedAbundance)) +
  #scale_fill_viridis_c()+
  scale_color_gradientn(colors = gradient_colors1,
                        limits = c(0, max_value),   # ensure 0 = blue
                        oob = scales::squish  )+      # clamp values outside range
  scale_fill_gradientn(colors = gradient_colors1)+
  #scale_fill_gradientn(colors = c("darkblue","deepskyblue1","darkorange",   "red"), na.value = "transparent", limits = rast_lims) +
  coord_sf() +
  geom_sf(data = NAFO, color="black", fill = NA) +
  geom_sf(data = Hague, color="black", size = 2) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 2) +
  geom_sf(data = land, fill="cornsilk") +
  xlim(-73, -48) + ylim(39, 52)+
  theme_bw()+
  labs(title="Abundance 2006-2023", x = NULL, y = NULL, fill = "sqrt\n(Abun.)")+
  theme(
    plot.margin = unit(c(23,3,0,2), "pt"),
    #plot.margin=margin(20,3,3,3),
    axis.text = element_text(family = "serif",size=10),
    axis.text.y = element_blank(),
    plot.title=element_text(size=12,family="serif"),
    legend.title = element_text(size = 10,family="serif"), 
    legend.text = element_text(size = 10,family="serif"),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),  # removes major grid lines
    panel.grid.minor = element_blank(),   # removes minor grid lines
    legend.position = "bottom",
    legend.key.height   = unit(.4, "cm"),
    legend.key.width = unit(.5, "cm"),
    legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2),
    legend.position.inside = c(0.01, 0.99),         # (x, y) coordinates inside plot
    legend.justification.inside = c(0, 1),# anchor legend to top-left of its box
  )

ShinyAbdMap2
library(grid)

g1 <- ggplotGrob(ShinyAbdMap)
g2 <- ggplotGrob(ShinyAbdMap2)
g1$widths <- g2$widths <- unit.pmax(g1$widths, g2$widths)

ShinyAbdMap12 <- grid.arrange(
  grobs = list(g1, g2),
  layout_matrix = rbind(c(1, 2)),
  nrow = 1,
  padding = unit(0, "line"),
  widths = c(1, 1)
)

ShinyAbdMap12<-grid.arrange(
  grobs = list(ShinyAbdMap, ShinyAbdMap2),
  layout_matrix = rbind(c(1, 2)),
  nrow=1,
    padding = unit(0, "line")
 ) # adjust the third column width as needed


ggsave(
  filename = "ShinyAbdMap12.png",
  plot = ShinyAbdMap12,          # optional if it's the last plot
  path =  here::here("NancBranchDataScript/ShinyApp/www/"),
  width = 8,
  height = 4,
  dpi = 300
)

