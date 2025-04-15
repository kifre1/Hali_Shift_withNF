#survey area stuff
library(fields)
library(R.matlab)
#library(rhdf5)
library(RNetCDF)
library(sp)
#library(SDMTools)
library(raster)
library(rgdal)
#library(animation)
library(base)
#library(rgeos)
#library(maptools)
sst = readMat('C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSsfce-20250319T175426Z-001/TSsfce/TSsfce_1990.mat', fixNames = TRUE) # load file
bt = readMat('C:/Users/fergusonk/Documents/Shapefiles/BNAM/Brickman2055/TSbtm-20250319T175520Z-001/TSbtm/TSbtm_1990.mat', fixNames = TRUE) # load file


str(sst)
sst$README


library(sp)
library(tidyverse)
library(sf)
library(rnaturalearth)
#library(gmRi)
library(here)
library(patchwork)
#save to archive for data prep



source("R/AA_Code/enhance_r_funcs.R")
#make a shapefile of the footprint of the surveys 
nf_all <- get(load("C:/Users/fergusonk/Documents/Halibut/Halibut_Paper1/data/TrawlSurvey_data/abun_nl_all85.Rdata")) 
nf_all$long.start<-nf_all$long.start*-1
install.packages("smoothr")
install.packages("concaveman")
library(concaveman)
points_sf <- st_as_sf(nf_all, coords = c("long.start", "lat.start"), crs = 4326)

# Create concave hull (alpha controls the shape tightness, lower = tighter)
concave_hull <- concaveman(points_sf, concavity = 2, length_threshold = 0)

# Plot concave hull
plot(st_geometry(points_sf), col = "red", pch = 20, main = "Concave Hull")
plot(st_geometry(concave_hull),  border = "blue", lwd = 2)


library(smoothr)
smoothed_polygon <- smooth(concave_hull, method = "ksmooth", smoothness = 2)
plot(st_geometry(concave_hull),  border = "blue", lwd = 2)



st_write(concave_hull, "C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/concave_hull_NF_smooth.shp")


#read in shapefiles involved and match crs
NF_survey <- st_read(here::here("C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/concave_hull_NF_smooth.shp"))
NF_Buff <- st_read(here::here("C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/NFBuffered.shp"))
NF_NAFO <- st_read(here::here("C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/NF_Survey_NAFOs.shp"))
All_region <- st_read(here::here("R/data/region_shapefile/full_survey_region_simple.shp"))
land <- st_read(here::here("", "R/data/Mapping_shapefiles/poly_NAD83.shp"))

crs <- st_crs(All_region)
NF_survey <- st_transform(NF_survey, crs)
NF_Buff <- st_transform(NF_Buff, crs)
NF_NAFO <- st_union(NF_NAFO)
NF_NAFO <- st_transform(NF_NAFO, crs)
land <- st_transform(land, crs)


p1<-ggplot() +
  geom_sf(data = NF_survey, fill = "blue", alpha = 0.5) +
  geom_sf(data = All_region, fill = "red", alpha = 0.5) +
  coord_sf(expand = FALSE) +  # Prevent auto-clipping
  #geom_sf(data = land, fill = "grey", alpha = 0.5) +
  theme_minimal()
p1 + 
  geom_sf(data = land, fill = "grey", alpha = 0.5) +
  coord_sf(xlim = c(min(st_coordinates(All_region)[,1]), max(st_coordinates(NF_survey)[,1])),
           ylim = c(min(st_coordinates(All_region)[,2]), max(st_coordinates(NF_survey)[,2])),
           expand = FALSE)

#clip the survey region by nafo 
NF_survey2 <- st_intersection(NF_survey, NF_NAFO)
#clip survey region by nf buffered coast
NF_survey3 <- st_difference(NF_survey2, NF_Buff)


p1<-ggplot() +
  geom_sf(data = NF_survey5, fill = "blue", alpha = 0.5) +
  geom_sf(data = All_region, fill = "red", alpha = 0.5) +
  coord_sf(expand = FALSE) +  # Prevent auto-clipping
  #geom_sf(data = land, fill = "grey", alpha = 0.5) +
  theme_minimal()
p1 + 
  geom_sf(data = land, fill = "grey", alpha = 0.5) +
  coord_sf(xlim = c(min(st_coordinates(All_region)[,1]), max(st_coordinates(NF_survey4)[,1])),
           ylim = c(min(st_coordinates(All_region)[,2]), max(st_coordinates(NF_survey4)[,2])),
           expand = FALSE)
#smooth polygon and remove islands
NF_survey4 <- smooth(NF_survey3, method = "ksmooth", smoothness = 6)
NF_survey5 <- st_union(NF_survey4)
plot(NF_survey5)
st_write(NF_survey5, "C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/NF_survey5.shp")
#got to arc and draw a clipper to  remove some yuckiness

#Bring data back in 
NF_Clilpper <- st_read("C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/NF_Clipper.shp")
NF_survey5 <- st_read("C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/NF_survey5.shp")
plot(NF_survey5)
NF_Survey_Region <- st_difference(NF_survey5, NF_Clilpper)
NF_Survey_Region <- NF_Survey_Region %>% select(-c("Shape_Area", "Shape_Leng"))
plot(NF_Survey_Region)

#make sure that NF has the same structure as US and canada 
NMFS <- st_read(here::here("R/data/index_shapefiles/NMFS.shp"))
DFO <- st_read(here::here("R/data/index_shapefiles/DFO.shp"))

NMFS <- st_transform(NMFS, crs)
DFO <- st_transform(DFO, crs)

ggplot() +
  geom_sf(data = NF_Survey_Region, fill = "brown4", alpha = 0.5) +
  geom_sf(data = DFO, fill = "red", alpha = 0.5) +
  geom_sf(data = NMFS, fill = "blue", alpha = 0.5) +
  coord_sf(expand = FALSE) +  # Prevent auto-clipping
  theme_minimal()

#join NF and DFO,  st_union, rename Region to Canada 
DFO$Region <- recode(DFO$Region,
                            "DFO" = "NS")
NF_Survey_Region$Region<-"NF"
NF_Survey_Region <- NF_Survey_Region %>% select(-FID)
Canada<- rbind(NF_Survey_Region,DFO)
Canada <- st_union(Canada)
Canada <- st_make_valid(Canada)  # Fix invalid geometries

plot(Canada)

#rename NMFS region to USA
NMFS$Region <- recode(NMFS$Region,
                      "NMFS" = "USA")
NMFS$Region <- "USA"
#Join Canada and USA, st_union, rename Region All
#ALL<- rbind(NMFS,Canada)
#ALL <- st_union(ALL)
plot(ALL)
#saveALL#save these shapefiles in R/data/index_shapefiles
#Save Canada...open in arc and correct the weird line
st_write(DFO, (here::here("R/data/index_shapefiles/NS_Survey_RegionMR27.shp")))
st_write(NF_Survey_Region, (here::here("R/data/index_shapefiles/NF_Survey_RegionMR27.shp")))
st_write(NMFS, (here::here("R/data/index_shapefiles/USA_Survey_RegionMR27.shp")))

#st_write(ALL, "C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/ALL_Survey_RegionMR27.shp")
#st_write(Canada, "C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/Canada_Survey_RegionMR27.shp")
125.59-108-11
#take these to arc to make unions (canada and all)
Canada <- st_read(here::here("R/Shapefiles/SurveyArea/Canada_Survey_RegionAL3.shp"))
NF<- st_read(here::here("R/Shapefiles/NF_Survey.shp"))
NS<- st_read(here::here("R/Shapefiles/IndexShapefiles/NS_Survey_RegionMR27.shp"))
USA <- st_read(here::here("R/Shapefiles/SurveyArea/USA_SURVEY_REGION.shp"))
FullRegion <- st_read(here::here("R/Shapefiles/SurveyArea/Full_survey_Area.shp"))

NF <- NF %>% select(-c("Shape_Area", "Shape_Leng"))
FullRegion <- FullRegion %>% select(-c("Shape_Area", "Shape_Leng"))
USA <- USA %>% select(-c("Shape_Area", "Shape_Leng"))
USA$Region <- "USA"
USA_2D <- st_zm(USA, drop = TRUE)
FullRegion_2D <- st_zm(FullRegion, drop = TRUE)

st_write(FullRegion, (here::here("R/shapefiles/IndexShapefiles/Full_RegionAl3.shp")))
st_write(Canada, (here::here("R/shapefiles/IndexShapefiles/Canada_RegionAl3.shp")))
st_write(USA, (here::here("R/shapefiles/IndexShapefiles/USA_RegionAl3.shp")))
st_write(NF, (here::here("R/shapefiles/IndexShapefiles/NF_RegionAl3.shp")))
st_write(NS, (here::here("R/shapefiles/IndexShapefiles/NS_RegionAl3.shp")))

ggplot() +
  geom_sf(data = FullRegion, fill = "red", alpha = 0.5) +
  geom_sf(data = Canada, fill = "yellow", alpha = 0.5) +
  #geom_sf(data = NS, fill = "orange", alpha = 0.5) +
  geom_sf(data = NF, fill = "green", alpha = 0.5) +
  geom_sf(data = USA, fill = "blue", alpha = 0.5) +
  coord_sf(expand = FALSE) +  # Prevent auto-clipping
  theme_minimal()     

FullRegion<- rbind(Canada,USA)
FullRegion <- st_union(FullRegion)
plot(FullRegion)
FullRegion <- st_make_valid(FullRegion)  # Fix invalid geometries

Canada <- Canada %>% select(-c("Shape_Area", "Shape_Leng"))
Canada$Region <- "Canada"
st_write(Canada, (here::here("R/Shapefiles/SurveyArea/Canada_Survey_RegionMR31.shp")))

#remove 3M
Canada <- st_read(here::here("R/shapefiles/IndexShapefiles/Canada_RegionAl3.shp"))
ALL<- st_read(here::here("R/shapefiles/IndexShapefiles/Full_RegionAl3.shp"))
NAFO<- st_read(here::here("Data/TempShapefiles/NAFO3M.shp"))
# Ensure both are in the same CRS
NAFO <- st_transform(NAFO, st_crs(ALL))

# Subtract NAFO polygons from ALL
ALL_minus_NAFO <- st_difference(ALL, st_union(NAFO))
plot(Canada_minus_NAFO)
Canada_minus_NAFO <- st_difference(Canada, st_union(NAFO))
st_write(Canada_minus_NAFO, (here::here("R/Shapefiles/SurveyArea/Canada_RegionAl14.shp")))
st_write(ALL_minus_NAFO, (here::here("R/Shapefiles/SurveyArea/Full_RegionAl14.shp")))
