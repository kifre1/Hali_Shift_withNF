#Plot 1: Map of Core areas 
  # make colours match CAPlot
  #re-do all folder connections
  #add new core areas
  #Increase box area 
#Plot 2: A look at the distribution of RV Survey data
  #add NF
  #Plot presence only 

All_region <- st_read(here::here("", "R/Shapefiles/IndexShapefiles/Full_RegionAl14.shp"))
crs <- st_crs(All_region)

#bounding box for plotting
study_area_bbox <- st_bbox(c(xmin = -76, ymin = 35.5, xmax = -44, ymax = 48))
study_area_polygon <- st_as_sfc(study_area_bbox)
st_crs(study_area_polygon) <- crs

#Mapping shapefiles
EEZ <- st_read(here::here("", "Data/Mapping_shapefiles/EEZ.shp"))
land <- st_read(here::here("", "Data/Mapping_shapefiles/poly_NAD83.shp"))
contours <- st_read(here::here("", "Data/Mapping_shapefiles/DepthContours.shp"))
NAFO <- st_read(here::here("", "Data/Mapping_shapefiles/Divisions.shp"))
Hague <- st_read(here::here("", "Data/Mapping_shapefiles/HagueLine.shp"))

EEZ <- st_transform (EEZ, crs)
land <- st_transform (land, crs)
contours <- st_transform (contours, crs)
NAFO <- st_transform (NAFO, crs)
Hague <- st_transform (Hague, crs)

# Clip large shapefiles shapefile to  bounding box
EEZ <- st_intersection(EEZ, study_area_polygon)
#land <- st_intersection(land, study_area_polygon)#maybe comment out 
contours <- st_intersection(contours, study_area_polygon)
NAFO <- st_intersection(NAFO, study_area_polygon)

#Core areas 
CAs <- st_read(here::here("", "R/Shapefiles/CoreAreas/CoreAreas_Al14.shp"))
CAs <- st_transform (CAs, crs)
# Convert to data frames
All_region_df <- st_as_sf(data.frame(geometry = All_region))
CoreAreas_df <- st_as_sf(data.frame(geometry = CAs))

unique(CoreAreas_df$geometry.Region)
region_colours <- c(
  "EGOM" ="#004995",
  "BOF"  = "#8C510A",
  "CapeBreton" ="#4D4D4D",
  "HaliChan" ="#00441B",
  "CapeCod" ="#2171B5",
  "Browns" ="#D8781D",
  "Gully"="#7F7F7F",
  "GrandBanks" ="#238B45",
  "Nantucket" = "#56B4E9",
  "Georges" ="#EDA752",
  "Sable"  ="#C0C0C0",
  "GBTail" = 	"#81C784"
)
#factor the order 
CoreAreas_df$geometry.Region <- factor(CoreAreas_df$geometry.Region, levels = names(region_colours))

library(sf)
library(ggrepel)

# Plot CoreArea
CAMAP<-ggplot() +
  geom_sf(data = contours, color="lightblue") +
  geom_sf(data = CoreAreas_df, aes(fill = as.factor(geometry.Region))) +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = Hague, color="navy") +
  geom_sf(data = EEZ, color="navy", linetype = "dashed", size = 1.2) +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = land, fill="lightgrey") +
  scale_fill_manual(values = region_colors)+
  #scale_fill_manual(name = " ", values = c("#E41A1C","#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628","#F781BF", "#999999")) +
  labs(title="Core Areas")+
  xlim(-73, -48) + ylim(39.355, 48)+
  theme_bw()
CAMAP


#Plot 2: A look at the distribution of RV Survey data
Data = read.csv(here::here("R/data/halibut_all_data.csv"))

DFO<-subset(Data, Data$SURVEY=="DFO")
NMFS<-subset(Data, Data$SURVEY=="NMFS")

DFOplot<- ggplot() +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black", lwd = 1.2) +
  geom_sf(data = NAFO,  fill = NA) +
  geom_sf(data = land, fill="grey") +
  geom_point (data = NMFS, aes(x= DECDEG_BEGLON, y = DECDEG_BEGLAT), colour= "blue", alpha = .5)+
  geom_point (data = DFO, aes(x= DECDEG_BEGLON, y = DECDEG_BEGLAT), colour= "red", alpha = .5)+
  geom_sf(data = Hague, color="black", lwd=2) +
  xlim(-76, -55) + ylim(35.8, 47.5)+
  labs(title="", x= "", y="")+
  theme_bw()
NMFSplot<- ggplot() +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black", lwd = 1.2) +
  geom_sf(data = NAFO,  fill = NA) +
  geom_sf(data = land, fill="grey") +
  geom_point (data = DFO, aes(x= DECDEG_BEGLON, y = DECDEG_BEGLAT), colour= "red", alpha = .5)+
  geom_point (data = NMFS, aes(x= DECDEG_BEGLON, y = DECDEG_BEGLAT), colour= "blue", alpha = .5)+
  geom_sf(data = Hague, color="black", lwd=2) +
  xlim(-76, -55) + ylim(35.8, 47.5)+
  labs(title="", x= "", y="")+
  theme_bw()

library(patchwork)
#look at how they overlap
DFOplot + NMFSplot

