library(dplyr)
library(ggplot2)
library(RColorBrewer)
#install.packages("ggtext")
#install.packages("spatstat")
library(ggtext)
library(spatstat)
#may find some useful stuff in the DisMAP gitHub

SDM_data_All<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_All_May20.csv"))
SDM_data_Reg<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_Reg_May20.csv"))
SDM_data_CA<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_CA_May20.csv"))
unique(SDM_data_Reg$Area_km2)
unique(SDM_data_CA$Area_km2)

#Center of gravity (all, national, per core area), per year and season----
names(SDM_data_CA)
unique(SDM_data_CA$Stratum)
# Group dataframe by year and calculate weighted mean longitude and latitude

#we're calculating the weighted mean of lon/lat based on the abundance values, 
#range_5_95 quantifies the range between the 5th and 95th percentiles of tthe abundance-weighted coordinates
#I think we would need to look into how limited these would be by the bounding boxes
#We also have to get the weighted median and the weight 5 adn 95, start with median
# Function to calculate weighted median is in spatstat
centroid_data <- SDM_data_All %>%
  group_by(Year, Season) %>%
  summarize(
    centroid_longitude = weighted.mean(Lon, w = Abundance),
    centroid_latitude = weighted.mean(Lat, w = Abundance),
    centroid_latitude_Median = weighted.median(Lat, w = Abundance),
    centroid_longitude_Median = weighted.median(Lon, w = Abundance),
    centroid_latitude_Quantile_05 = weighted.quantile(Lat, w = Abundance, probs = 0.05), # 5th percentile 
    centroid_longitude_Quantile_05 = weighted.quantile(Lon, w = Abundance, probs = 0.05), 
    centroid_latitude_Quantile_95 = weighted.quantile(Lat, w = Abundance, probs = 0.95), # 95th percentile
    centroid_longitude_Quantile_95 = weighted.quantile(Lon, w = Abundance, probs = 0.95), 
  )
centroid_data_Reg <- SDM_data_Reg %>%
  group_by(Year, Season, Stratum) %>%
  summarize(
    centroid_longitude = weighted.mean(Lon, w = Abundance),
    centroid_latitude = weighted.mean(Lat, w = Abundance),
    centroid_latitude_Median = weighted.median(Lat, w = Abundance),
    centroid_longitude_Median = weighted.median(Lon, w = Abundance),
    centroid_latitude_Quantile_05 = weighted.quantile(Lat, w = Abundance, probs = 0.05), # 5th percentile 
    centroid_longitude_Quantile_05 = weighted.quantile(Lon, w = Abundance, probs = 0.05), 
    centroid_latitude_Quantile_95 = weighted.quantile(Lat, w = Abundance, probs = 0.95), # 95th percentile
    centroid_longitude_Quantile_95 = weighted.quantile(Lon, w = Abundance, probs = 0.95), 
  )
centroid_data_CA <- SDM_data_CA %>%
  group_by(Year, Season, Stratum) %>%
  summarize(
    centroid_longitude = weighted.mean(Lon, w = Abundance),
    centroid_latitude = weighted.mean(Lat, w = Abundance),
    centroid_latitude_Median = weighted.median(Lat, w = Abundance),
    centroid_longitude_Median = weighted.median(Lon, w = Abundance),
    centroid_latitude_Quantile_05 = weighted.quantile(Lat, w = Abundance, probs = 0.05), # 5th percentile 
    centroid_longitude_Quantile_05 = weighted.quantile(Lon, w = Abundance, probs = 0.05), 
    centroid_latitude_Quantile_95 = weighted.quantile(Lat, w = Abundance, probs = 0.95), # 95th percentile
    centroid_longitude_Quantile_95 = weighted.quantile(Lon, w = Abundance, probs = 0.95), 
  )


write.csv(centroid_data_CA,(here::here("2025-04-23/Output/Shift_Indicators/seasonal_centroid_data_CA.csv")), row.names = FALSE)
write.csv(centroid_data_Reg,(here::here("2025-04-23/Output/Shift_Indicators/seasonal_centroid_data_region.csv")), row.names = FALSE)
write.csv(centroid_data, (here::here("2025-04-23/Output/Shift_Indicators/seasonal_centroid_data.csv")), row.names = FALSE)
#END: go to script 5.2



#Exploratory plotting: plot COG SEASONAL
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
#install.packages("Kendall")
library(ggtext)
library(sf)
library(grid)
library(patchwork)
library(gridExtra)
#run sup4....R to get all the shapefiles for maps
#load data and make point data for centroid means and medians
All_region <- st_read(here::here("R/Shapefiles/IndexShapefiles/Full_RegionAl14.shp"))
crs <- st_crs(All_region)

Centroid_All <- read.csv(here::here("","2025-04-23/Output/Shift_Indicators/seasonal_centroid_data.csv"),sep = ",")
Centroid_All_sf <- st_as_sf(Centroid_All, coords = c("centroid_longitude", "centroid_latitude"))#represent the mean 
Centroid_Median_All_sf <- st_as_sf(Centroid_All, coords = c("centroid_longitude_Median", "centroid_latitude_Median"))#spatial shapefile for the centroid medians, other centroid variables can be ignored 
st_crs(Centroid_Median_All_sf) <- crs
st_crs(Centroid_All_sf) <- crs

Centroid_Reg <- read.csv(here::here("","2025-04-23/Output/Shift_Indicators/seasonal_centroid_data_region.csv"),sep = ",")
Centroid_Reg_sf <- st_as_sf(Centroid_Reg, coords = c("centroid_longitude", "centroid_latitude"))#represent the mean 
Centroid_Median_Reg_sf <- st_as_sf(Centroid_Reg, coords = c("centroid_longitude_Median", "centroid_latitude_Median"))#spatial shapefile for the centroid medians, other centroid variables can be ignored 
st_crs(Centroid_Median_Reg_sf) <- crs
st_crs(Centroid_Reg_sf) <- crs

Centroid_CA <- read.csv(here::here("","2025-04-23/Output/Shift_Indicators/seasonal_centroid_data_CA.csv"),sep = ",")
Centroid_CA_sf <- st_as_sf(Centroid_CA, coords = c("centroid_longitude", "centroid_latitude"))#represent the mean 
Centroid_Median_CA_sf <- st_as_sf(Centroid_CA, coords = c("centroid_longitude_Median", "centroid_latitude_Median"))#spatial shapefile for the centroid medians, other centroid variables can be ignored 
st_crs(Centroid_Median_Reg_sf) <- crs
st_crs(Centroid_CA_sf) <- crs

#factor data for plotting 
Centroid_CA$Stratum<-factor(Centroid_CA$Stratum,levels=c("EGOM","BOF","CapeBreton","HaliChan",
                                                         "CapeCod","Browns","Gully","GrandBanks",
                                                         "Nantucket","Georges","Sable","GBTail"))
#Is movement significant?
#CA: longitude vs latitute over time

Centroid_CA <- Centroid_CA %>%
  filter(Season == "Spring" ) 

LAT <- ggplot(Centroid_CA, aes(x = Year, y = centroid_latitude, colour = Season)) + 
  geom_line() + 
  geom_point(size = 1.25) +
  facet_wrap(. ~ Stratum, scales = "free") +  
  ggtitle("A) Latitude") +
  theme_bw()+   # Set theme_bw first
  theme(legend.position = "none")  

# Plot longitude over time
LON<-ggplot(Centroid_CA, aes(x = Year, y = centroid_longitude, colour = Season)) + 
  geom_line() + 
  geom_point(size=1.25) +
  facet_wrap(.~Stratum, scales = "free") +  
  ggtitle("B) Longitude") +
  theme_bw() +
  theme(legend.position = "none")  
  coord_flip()
#theme(legend.position = "none")  
grid.arrange(LAT, LON, ncol = 2, widths = c(4, 4))

# Plot longitude vs latitute over time.
ggplot(Centroid_CA, aes(y = centroid_latitude, x = centroid_longitude, colour = Year)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(.~Stratum, scales = "free") +  
  ggtitle("Center of Gravity temporal trends (Spring)") +
  theme_bw() 
#MAP CAs----
#mean spring, RUn sup2DataPlots.R to get the mapping data
Spring_Centroid_CA_sf <- subset(Centroid_CA_sf, Centroid_CA_sf$Season=="Spring")
ggplot() +
  geom_sf(data = contours, color="lightblue") +
  geom_sf(data = CoreAreas_df, fill = "transparent",lwd=1.3,color="steelblue",lty=1) +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black",lty=2,lwd=1.3) +
  geom_sf(data = NAFO,  fill = NA) +
  geom_sf(data = land, fill="grey") +
  geom_sf(data = Spring_Centroid_CA_sf, aes(color = Year),size =1, alpha = 1,shape=16) +  # Adjust size and alpha here
  labs(title = "Centre of Gravity (Mean) within Core Areas:Spring", x = "Longitude", y = "Latitude",
       color = "Year") +
  scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(Spring_Centroid_CA_sf$Year)) +
  # annotate("text",x=-69.7,y=40.5,label="Nantucket", color = "black",size = 4) +
  # annotate("text",x=-68.5,y=43.1,label="East Gulf\nof Maine", vjust = 0, color = "black",size = 4) +
  # annotate("text",x=-67.1,y=41,label="Georges\nBank", vjust = 0, color = "black",size = 4) +
  # annotate("text",x=-69.5,y=42.1,label="Cape\nCod", vjust = 0, color = "black",size = 4) +
  # annotate("text",x=-66.3,y=44.6,label="Bay of\nFundy", vjust = 0, color = "black",size = 4) +
  # annotate("text",x=-66,y=42.4,label="Browns\nBank", vjust = 0, color = "black",size = 4) +
  # annotate("text",x=-62,y=43.2,label="Sable", vjust = 0, color = "black",size = 4) +
  # annotate("text",x=-59,y=44.7,label="Gully", vjust = 0, color = "black",size = 4) +
  # annotate("text",x=-59,y=45.7,label="Cape\nBreton", vjust = 0, color = "black",size = 4) +
  # theme(text = element_text(size =12 ))+  
  xlim(-70.5, -48) + ylim(39.5, 48)+
  theme_bw()

#skip (not effeective): MAP CENtroid stats the COG and Q5/95 for individual core areas----
#prep all data for plotting
spring_centroid_data<-subset(Centroid_CA, Centroid_CA$Season=="Spring")
Spring_mean_sf <- st_as_sf(spring_centroid_data, coords = c("centroid_longitude", "centroid_latitude"))
st_crs(Spring_mean_sf) <- crs
Spring_Q5_sf <- st_as_sf(spring_centroid_data, coords = c("centroid_longitude_Quantile_05", "centroid_latitude_Quantile_05"))
st_crs(Spring_Q5_sf) <- crs
Spring_Q95_sf <- st_as_sf(spring_centroid_data, coords = c("centroid_longitude_Quantile_95", "centroid_latitude_Quantile_95"))
st_crs(Spring_Q95_sf) <- crs
Spring_medi_sf <- st_as_sf(spring_centroid_data, coords = c("centroid_longitude_Median", "centroid_latitude_Median"))
st_crs(Spring_medi_sf) <- crs

summary(Spring_medi_sf$Stratum)


#include the 5 ad 95%
ggplot() +
  geom_sf(data = contours, color="lightblue") +
  geom_sf(data = CoreAreas_df, fill = "transparent",lwd=1.3,color="steelblue",lty=1) +
  #geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black",lty=2,lwd=1.3) +
  geom_sf(data = NAFO,  fill = NA) +
  geom_sf(data = land, fill="grey") +
  labs(title = "Centroid Stats: Spring", x = "Longitude", y = "Latitude",
       color = "Year") +
  #mean
  geom_sf(data = Spring_mean_sf, aes(color = Year),size =2, alpha = 1,shape=16) +  # Adjust size and alpha here
  #Q5
  geom_sf(data = Spring_Q5_sf, aes(color = Year),size =2, alpha = 1,shape=16) +  # Adjust size and alpha here
  #Q95
  geom_sf(data = Spring_Q95_sf, aes(color = Year),size =2, alpha = 1,shape=16) +  # Adjust size and alpha here
  scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(Spring_mean_sf$Year)) +
  #scale_color_gradientn(colors = c("goldenrod1","yellowgreen", "darkolivegreen"), limits = range(centroid_sf$Year)) +
  #  annotate("text",x=-69.7,y=40.5,label="Nantucket", color = "black",size = 4) +
  #  annotate("text",x=-68.5,y=43.1,label="East Gulf\nof Maine", vjust = 0, color = "black",size = 4) +
  #  annotate("text",x=-67.1,y=41,label="Georges\nBank", vjust = 0, color = "black",size = 4) +
  #  annotate("text",x=-69.5,y=42.1,label="Cape\nCod", vjust = 0, color = "black",size = 4) +
  #  annotate("text",x=-66.3,y=44.6,label="Bay of\nFundy", vjust = 0, color = "black",size = 4) +
  #  annotate("text",x=-66,y=42.4,label="Browns\nBank", vjust = 0, color = "black",size = 4) +
  #  annotate("text",x=-62,y=43.2,label="Sable", vjust = 0, color = "black",size = 4) +
  #  annotate("text",x=-59,y=44.7,label="Gully", vjust = 0, color = "black",size = 4) +
  #  annotate("text",x=-59,y=45.7,label="Cape\nBreton", vjust = 0, color = "black",size = 4) +
  theme(text = element_text(size =12 ))+  
  xlim(-70.5, -48) + ylim(39.5, 48)+
  theme_bw()


#----
#PLOT  plot for whole region
Centroid_Spring<-subset(Centroid_All, Centroid_All$Season=="Spring")

ggplot(Centroid_Spring, aes(x = centroid_longitude, y = centroid_latitude, color = Year)) +
  geom_point(na.rm=TRUE,alpha=1,size=1.5,shape=19,stroke = 1) +
  labs(title = "Center of Gravity:Full Survey Region",
       x = "Longitude",
       y = "Latitude",
       color = "Year") +
  #scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+
  scale_color_gradientn(colors = c("blue", "deepskyblue1","darkorange", "red"), limits = range(Centroid_All$Year)) +
  #guides(color = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8,size=8),
        axis.text.y = element_text(hjust = 0.8,size=8),
        plot.margin=margin(3,4,4,0),plot.title=element_text(size=12,vjust=2),
        strip.text=element_text(size=10,margin = margin()),
        panel.spacing = unit(2, 'pt'),
        #strip.background = element_rect(size = 1,margin(t = 2, r = 0, b = 2, l = 0, unit = "pt")),
        #panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
        legend.background=element_rect(linewidth=.6,colour="black",fill="white"),
        legend.position = "right",
        #legend.position = c(0.84, 0.091),
        legend.direction = "vertical",  # Change legend direction
        legend.key.size = unit(.8, "cm"))+  # Change the size of legend items
  #panel.grid.major = element_line(color = "black"),)+
  #facet_grid(Season~.)+
  theme_bw()


#By Core area

COG_CA<-ggplot(Centroid_CA, aes(x = centroid_longitude, y = centroid_latitude, color = Year)) +
  geom_point(na.rm=TRUE,alpha=1,size=1.5,shape=19,stroke = 1) +
  labs(title = NULL,
       x = "Longitude",
       y = "Latitude",
       color = "Year") +
  scale_color_gradientn(colors = c("blue", "deepskyblue1","darkorange", "red"), limits = range(Centroid_CA$Year)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8,size=8),
        axis.text.y = element_text(hjust = 0.8,size=8),
        plot.margin=margin(3,4,4,0),plot.title=element_text(size=12,vjust=2),
        strip.text=element_text(size=10,margin = margin()),
        panel.spacing = unit(2, 'pt'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "gray", linetype = "solid", linewidth = 0.35),
        legend.background=element_rect(linewidth=.6,colour="black",fill="white"),
        legend.position = "right",
        legend.direction = "vertical",  # Change legend direction
        legend.key.size = unit(.8, "cm"))+  # Change the size of legend items
  facet_wrap(.~Stratum, scales = "free")
library(ggpubr)
#save this plot for appendix
ggsave(
  filename = "COG_FocalAreas.png",
  plot = COG_CA,          # optional if it's the last plot
  path =  here::here("2025-04-23/Output/GridPlot"),
  width = 6.5,
  height = 5,
  dpi = 300
)


#skip (not effective): Plot Temporal trend of mean, median and 5 to 95---
COG_Lat<-ggplot(Centroid_CA, aes(x = Year, y = centroid_latitude, color = Stratum)) +
  geom_point(na.rm=TRUE,size=1.5,shape=19,) +
  geom_line(stat="smooth",alpha=1,size=1)+
  #guides(color = guide_legend(reverse = TRUE))+  # Reverse the color legend
  theme(legend.position = "none") +#turned off guides and turned this on for patchwork plotting
  #shape = guide_legend(reverse = TRUE) +
  labs(title = NULL, #for patchwork
       #title = "Center of Gravity by Core Area",
       x = "Year",
       y = "Latitude",
       color = "BCA") +
  facet_grid(Season~.)

COG_Lon<-ggplot(Centroid_CA, aes(x = Year, y = centroid_longitude, color = Stratum)) +
  geom_point(na.rm=TRUE,size=1.5,shape=19,) +
  geom_line(stat="smooth",alpha=1,size=1)+
  guides(color = guide_legend(reverse = TRUE))+  # Reverse the color legend
  #shape = guide_legend(reverse = TRUE) +
  labs(title = NULL, #for patchwork
       #title = "Center of Gravity by Core Area",
       x = "Year",
       y = "Longitude",
       color = "Core Are") +
  facet_grid(Season~.)

combined_COG<-COG_Lat+COG_Lon

combined_COG_final <- ggdraw() +
  draw_label("Center of Gravity by Core Area", size = 12, fontface = "bold", x = 0.5, y = 0.98) +
  draw_plot(combined_COG, x = 0, y = 0, width = 1, height = 0.95)
combined_COG_final



LatProbDistribution <- lapply(names(grouped_data), function(group) {
  ggplot(grouped_data[[group]], aes(x = Year))+
    geom_point(aes(y = centroid_latitude),shape=16) +
    geom_point(aes(y = centroid_latitude_Median),shape=4) +
    geom_linerange(aes(ymin = centroid_latitude_Quantile_05, ymax = centroid_latitude_Quantile_95), color = "blue",size=.21) +
    guides(color = guide_legend(reverse = TRUE))+  # Reverse the color legend
    #shape = guide_legend(reverse = TRUE) +
    labs(title = paste("COG Central Distribution: Latitude,",group), x = "Year")+
    facet_wrap(~Core_Area,scales="free",nrow=3,ncol=3)
})
for (LatProbDistribution in LatProbDistribution) {
  print(LatProbDistribution)
}
LonProbDistribution <- lapply(names(grouped_data), function(group) {
  ggplot(grouped_data[[group]], aes(y = Year*-1))+
    geom_point(aes(x = centroid_longitude),shape=16) +
    geom_point(aes(x = centroid_longitude_Median),shape=4) +
    geom_linerange(aes(xmin = centroid_longitude_Quantile_05, xmax = centroid_longitude_Quantile_95), color = "blue",size=.21) +
    guides(color = guide_legend(reverse = TRUE))+  # Reverse the color legend
    #shape = guide_legend(reverse = TRUE) +
    labs(title = paste("COG Central Distribution: Longitude",group), x = "Year")+
    facet_wrap(~Core_Area,scales="free",nrow=3,ncol=3)
})
for (LonProbDistribution in LonProbDistribution) {
  print(LonProbDistribution)
}
plot(CAs)

