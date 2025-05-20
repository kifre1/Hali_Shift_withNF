
#Step 1: Add depth data  (AbundanceEstimates_GridCentriods_All.csv)
  ##adds Depth data (using a raster from BNAM) to the abundance estimates at grid centriods data (AbundanceEstimates_GridCentriods_.csv) 
#Step 2: Calculate Deepening (Seasonal_Deepening_All.csv)
  #Group data by year and season (*and Stratum) and calculate the mean, median, Q5, and Q95 depth, 
  #weighted by estimated abundance values, then add a field for period (before vs after accelerated warming)
#Step3: Perform lm on each Stratum grouping, extract coefficients, and filter on Year (Deepening_Slope_All.csv)

#Step 1 add depth data----
#1.1 Create Depth raster: get BNAM data code from script 1.2
library(R.matlab)
library(raster)
library(sp)
library(lubridate)
library(sf)
library(dplyr)

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

#1.2 Join depth data to Estimates data
SDM_data_All<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_All.csv"))
SDM_data_Reg<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_Reg.csv"))
SDM_data_CA<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_CA.csv"))

# They need to be spatial 
st_crs(Depth_raster)
coordinates(SDM_data_All) <- ~Lon + Lat
coordinates(SDM_data_Reg) <- ~Lon + Lat
coordinates(SDM_data_CA) <- ~Lon + Lat
proj4string(SDM_data_All) <- CRS(proj4string(Depth_raster))
proj4string(SDM_data_Reg) <- CRS(proj4string(Depth_raster))
proj4string(SDM_data_CA) <- CRS(proj4string(Depth_raster))



process_catch_data <- function(catch_data, Depth_raster, Cleaned_Data) {
  # Initialize a vector to store depth values
  depth_extracted_values <- numeric(nrow(catch_data))
  
  # Extract depth values for each row
  for (i in 1:nrow(catch_data)) {
    depth_extracted_values[i] <- raster::extract(Depth_raster, catch_data[i, , drop = FALSE])
  }
  
  # Append extracted depth values to the data
  catch_data$Depth_value <- depth_extracted_values
  
  # Convert to data frame
  catch_df <- as.data.frame(catch_data)
  
    Cleaned_Data = catch_df
}

#Save these: 
SDM_data_All_Depth <- process_catch_data( SDM_data_All,  Depth_raster, Cleaned_Data_All)
#write.csv(SDM_data_All_Depth,(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_All_wDepth.csv")), row.names = FALSE)
SDM_data_Reg_Depth <- process_catch_data( SDM_data_Reg,  Depth_raster, Cleaned_Data_Reg)
#write.csv(SDM_data_Reg_Depth,(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_Reg_wDepth.csv")), row.names = FALSE)
SDM_data_CA_Depth <- process_catch_data( SDM_data_CA,  Depth_raster, Cleaned_Data_CA)
#write.csv(SDM_data_CA_Depth,(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_CA_wDepth.csv")), row.names = FALSE)

#Step 2: Calculate Deepening (same for COG)----
library(dplyr)
library(ggtext)
library(spatstat)
library(tidyverse)

data_All<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_All_wDepth.csv"))
data_Reg<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_Reg_wDepth.csv"))
data_CA<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_CA_wDepth.csv"))
unique(data_All$Stratum)
unique(data_Reg$Stratum)
unique(data_CA$Stratum)

# Group data by year and season (*and Stratum)
#calculate the mean,median, Q5, and Q95 depth, weighted by estimated abundance values 

#All
Depth_data_All <- data_All %>%
  group_by(Year, Season) %>%
  summarize(
    Depth_Mean = weighted.mean(Depth_value, w = Abundance),
    Depth_Median = weighted.median(Depth_value, w = Abundance),
    Depth_Q5 = weighted.quantile(Depth_value, w = Abundance, probs = 0.05),
    Depth_Q95 = weighted.quantile(Depth_value, w = Abundance, probs = 0.95),  
  )
#Regional
Depth_data_Reg <- data_Reg %>%
  group_by(Year, Season, Stratum) %>%
  summarize(
    Depth_Mean = weighted.mean(Depth_value, w = Abundance),
    Depth_Median = weighted.median(Depth_value, w = Abundance),
    Depth_Q5 = weighted.quantile(Depth_value, w = Abundance, probs = 0.05), 
    Depth_Q95 = weighted.quantile(Depth_value, w = Abundance, probs = 0.95), 
  )
#Core Areas
Depth_data_CA <- data_CA %>%
  group_by(Year, Season, Stratum) %>%
  summarize(
    Depth_Mean = weighted.mean(Depth_value, w = Abundance),
    Depth_Median = weighted.median(Depth_value, w = Abundance),
    Depth_Q5 = weighted.quantile(Depth_value, w = Abundance, probs = 0.05),
    Depth_Q95 = weighted.quantile(Depth_value, w = Abundance, probs = 0.95),
  )

#add a field for period (before vs after accelerated warming) 

#All
Depth_data_All$Period<-NULL
Depth_data_All$Period[Depth_data_All$Year<2006]<-"Before Warming"
Depth_data_All$Period[Depth_data_All$Year>2005]<-"During Warming"
str(Depth_data_All)
#By Region
Depth_data_Reg$Period<-NULL
Depth_data_Reg$Period[Depth_data_Reg$Year<2006]<-"Before Warming"
Depth_data_Reg$Period[Depth_data_Reg$Year>2005]<-"During Warming"
str(Depth_data_Reg)
#by Core Area
Depth_data_CA$Period<-NULL
Depth_data_CA$Period[Depth_data_CA$Year<2006]<-"Before Warming"
Depth_data_CA$Period[Depth_data_CA$Year>2005]<-"During Warming"
str(Depth_data_CA$Stratum)
Depth_data_CA$Stratum<-factor(Depth_data_CA$Stratum,levels=c("EGOM","BOF","CapeBreton","HaliChan",
                                                             "CapeCod","Browns","Gully","GrandBanks",
                                                             "Nantucket","Georges","Sable","GBTail"))
str(centroid_data)

write.csv(Depth_data_CA,(here::here("2025-04-23/Output/Shift_Indicators/Seasonal_Deepening_CA.csv")), row.names = FALSE)
write.csv(Depth_data_Reg,(here::here("2025-04-23/Output/Shift_Indicators/Seasonal_Deepening_Reg.csv")), row.names = FALSE)
write.csv(Depth_data_All, (here::here("2025-04-23/Output/Shift_Indicators/Seasonal_Deepening_All.csv")), row.names = FALSE)

#Step3: Perform lm on each Stratum grouping, extract coefficients, and filter on year----
library(dplyr)
library(broom)
library(ggplot2)
library(patchwork)
library(grid)  # For unit() function
D_data_All<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/Seasonal_Deepening_All.csv"))
D_data_Reg<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/Seasonal_Deepening_Reg.csv"))
D_data_CA<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/Seasonal_Deepening_CA.csv"))

#all
Deepening_coefficients_All <- D_data_All %>%
  group_by(Period, Season) %>%
  do({
    model <- lm(Depth_Mean ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
summary(Deepening_coefficients_All)
filtered_All <- Deepening_coefficients_All %>%
  filter(term == "Year")  #  to isolate the effects of year and plot slopes and CIs

#Regional
Deepening_coefficients_Reg <- D_data_Reg %>%
  group_by(Period,Stratum, Season) %>%
  do({
    model <- lm(Depth_Mean ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
summary(Deepening_coefficients_Reg)
filtered_Reg <- Deepening_coefficients_Reg %>%
  filter(term == "Year")  # to isolate the effects of year and plot slopes and CIs

#Core Areas
Deepening_coefficients_CA <- D_data_CA %>%
  group_by(Period,Stratum, Season) %>%
  do({
    model <- lm(Depth_Mean ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
summary(Deepening_coefficients_CA)
filtered_CA <- Deepening_coefficients_CA %>%
  filter(term == "Year")  # to isolate the effects of year and plot slopes and CIs

#save data
write.csv(filtered_All,(here::here("2025-04-23/Output/Shift_Indicators/Deepening_Slope_All.csv")), row.names = FALSE)
write.csv(filtered_Reg,(here::here("2025-04-23/Output/Shift_Indicators/Deepening_Slope_Reg.csv")), row.names = FALSE)
write.csv(filtered_CA, (here::here("2025-04-23/Output/Shift_Indicators/Deepening_Slope_CA.csv")), row.names = FALSE)









