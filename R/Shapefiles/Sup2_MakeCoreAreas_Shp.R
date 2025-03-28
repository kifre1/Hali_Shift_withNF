#save to archive for data prep

# Made core area shapefiles in Arc/pulled from areas of interest shapefiles----
#Break the area into regions in which to calculate the center of biomass instead of one as a whole 
#BOF, Gully, and Browns Bank are persistent core areas (Boudreau et al.2017)

#we decided that many of our original CAs were  too small to capture variation and range edges are being bound by these CAs
#changes: 

library(sf)
library(ggplot2)
library(dplyr)
library(usethis)
library(here)

#EGOM: extend to Hague line
#BOF: remove detail
#Cape cod: extend up coast and slightly East
#Browns Bank: make it more inspired by halibut distribution
#Georges: split into a US and a Canadian side...reversed this
#GUlly: extend NW
#sable and Nantucket: leave
#Add 4Vn, Nell told us about good the halibut fishery around Cape Breton 
Sable2 <- st_read(here::here("R/data/index_shapefiles/Sep18/Sable_Oc2.shp"))
Nantucket2 <- st_read(here::here("R/data/index_shapefiles/Sep18/Nantucket_SE18.shp"))
EGOM2 <- st_read(here::here("R/data/index_shapefiles/Sep18/eGOM_SE18.shp"))
BOF2 <- st_read(here::here("R/data/index_shapefiles/BOF2.shp"))
CapeCod2 <- st_read(here::here("R/data/index_shapefiles/CapeCod2.shp"))
Browns2 <- st_read(here::here("R/data/index_shapefiles/Sep18/Browns2_SE18.shp"))
Gully2 <- st_read(here::here("R/data/index_shapefiles/Sep18/Gully_Oc2.shp"))
Georges <- st_read(here::here("R/data/index_shapefiles/Sep18/Georges_SE18.shp"))
CapeBreton<- st_read(here::here("R/data/index_shapefiles/CB4Vn.shp"))


#CB4Vn <- CB4Vn[, c("Id", "geometry")]
NMFS <- st_read(here::here("R/data/index_shapefiles/NMFS.shp"))
DFO<- st_read(here::here("R/data/index_shapefiles/DFO.shp"))

All_region <- st_read(here::here("R/data/region_shapefile/full_survey_region_simple.shp"))
crs <- st_crs(All_region)

Browns2 <- Browns2 %>% select(-Region)
#Browns2 <- Browns2 %>% dplyr::select(-Region)
Standardize_shp <- function(CA, new_name, crs) {
#  # Transform the coordinate reference system
  CA <- st_transform(CA, crs)
 #  Assign the new name to the Id column
  CA$Id <- new_name
  # Rename the Id column to Region
  CA <- CA %>%
    rename(Region = Id)
  return(CA)
}

Sable2 <- Standardize_shp(Sable2, "Sable", crs)
Gully2 <- Standardize_shp(Gully2, "Gully", crs)
#BOF2 <- Standardize_shp(BOF2, "BOF", crs)
Browns2 <- Standardize_shp(Browns2, "Browns", crs)
#CapeCod2 <- Standardize_shp(CapeCod2, "CapeCod", crs)
Nantucket2 <- Standardize_shp(Nantucket2, "Nantucket", crs)
EGOM2 <- Standardize_shp(EGOM2, "EGOM", crs)
Georges <- Standardize_shp(Georges, "Georges", crs)
#CB4Vn <- Standardize_shp(CB4Vn, "CB4Vn", crs)
CapeBreton$Region <- recode(CapeBreton$Region,
                         "CB4Vn" = "CapeBreton")
#now that these have been standardized, save them for use in the fit model 
st_write(Sable2, (here::here("R/data/index_shapefiles/Sable2.shp")))
st_write(Gully2, (here::here("R/data/index_shapefiles/Gully2.shp")))
st_write(Browns2, (here::here("R/data/index_shapefiles/Browns2.shp")))
st_write(Nantucket2, (here::here("R/data/index_shapefiles/Nantucket2.shp")))
st_write(EGOM2, (here::here("R/data/index_shapefiles/EGOM2.shp")))
st_write(Georges, (here::here("R/data/index_shapefiles/Georges.shp")))
st_write(CapeBreton, (here::here("R/data/index_shapefiles/CapeBretonMR26.shp")))


#above have been matched and saved add the NF ones then join

Sable2 <- st_read(here::here("R/data/index_shapefiles/Sable2.shp"))
Nantucket2 <- st_read(here::here("R/data/index_shapefiles/Nantucket2.shp"))
EGOM2 <- st_read(here::here("R/data/index_shapefiles/EGOM2.shp"))
BOF2 <- st_read(here::here("R/data/index_shapefiles/BOF2.shp"))
CapeCod2 <- st_read(here::here("R/data/index_shapefiles/CapeCod2.shp"))
Browns2 <- st_read(here::here("R/data/index_shapefiles/Browns2.shp"))
Gully2 <- st_read(here::here("R/data/index_shapefiles/Gully2.shp"))
Georges <- st_read(here::here("R/data/index_shapefiles/Georges.shp"))
CapeBreton<- st_read(here::here("R/data/index_shapefiles/CapeBretonMR26.shp"))

#add NF
GrandBanks<- st_read("C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/GrandBanks.shp")
GBTail<- st_read("C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/GBTail.shp")
HaliChan<- st_read("C:/Users/fergusonk/Documents/Halibut/shapefiles/SurveyArea/HaliChan.shp")
GrandBanks <- GrandBanks %>% select(-c("Shape_Area", "Shape_Leng"))
GBTail <- GBTail %>% select(-c("Shape_Area", "Shape_Leng"))
HaliChan <- HaliChan %>% select(-c("Shape_Area", "Shape_Leng"))

crs <- st_crs(CapeBreton)
GrandBanks <- st_transform(GrandBanks, crs)
GBTail <- st_transform(GBTail, crs)
HaliChan <- st_transform(HaliChan, crs)
HaliChan <- HaliChan %>%
  mutate(Region = ifelse(is.na(Region), "HaliChan", Region))
  
#check
plot(st_geometry(All_region), col = "lightgray", main = "Areas of Interest")
plot(st_geometry(BOF2), col = "blue", add = TRUE)
plot(st_geometry(Gully2), col = "blue", add = TRUE)
plot(st_geometry(Browns2), col = "blue", add = TRUE)
plot(st_geometry(EGOM2), col = "blue", add = TRUE)
plot(st_geometry(Nantucket2), col = "blue", add = TRUE)
plot(st_geometry(CapeBreton), col = "blue", add = TRUE)
plot(st_geometry(Georges), col = "blue", add = TRUE)
plot(st_geometry(CB4Vn), col = "blue", add = TRUE)
plot(st_geometry(Sable2), col = "blue", add = TRUE)
plot(st_geometry(HaliChan), col = "blue", add = TRUE)
plot(st_geometry(GrandBanks), col = "blue", add = TRUE)
plot(st_geometry(GBTail), col = "blue", add = TRUE)

#Join all the shapefiles together
CoreAreas<- rbind(EGOM2,BOF2,CapeBreton,CapeCod2,Browns2,Sable2,Nantucket2,Georges,Gully2, HaliChan, GrandBanks, GBTail)
plot(CoreAreas,main = "Core Areas")
CoreAreas$Region

#will save this condensed shapefile for plotting
st_write(CoreAreas, (here::here("R/data/CoreAreas/CoreAreas_Mar26.shp")))
st_write(HaliChan, (here::here("R/data/index_shapefiles/HaliChan.shp")))
st_write(GrandBanks, (here::here("R/data/index_shapefiles/GrandBanks.shp")))
st_write(GBTail, (here::here("R/data/index_shapefiles/GBTail.shp")))


#condense a huge file to make a depth contours shapefile for plotting
All_region <- st_read(here::here("", "R/data/region_shapefile/full_survey_region.shp"))
crs <- st_crs(All_region)
study_area_bbox <- st_bbox(c(xmin = -76, ymin = 35.5, xmax = -53, ymax = 48))
study_area_polygon <- st_as_sfc(study_area_bbox)
st_crs(study_area_polygon) <- crs
DepthCont <- st_read("C:\\Users\\FergusonK\\Documents\\Shapefiles\\Isobaths\\Contour\\depthcontour100.shp")
DepthCont <- st_transform (DepthCont, crs)
DepthCont <- st_intersection(DepthCont, study_area_polygon)
DepthCont <- DepthCont %>%
  filter(CONTOUR < 900)
#st_write(DepthCont, (here::here("R/data/Mapping_shapefiles/DepthContours.shp")))


library(sf)
library(dplyr)
#Newfoundland data have been incorporated because it doesn't make sense to not model the whole population range, especially since there is such a strong signal at the edge of the NS range
#we decided to break things up based on NAFO regions instead of using the Core Areas because they were too small to capture meaningful trends
USA <- st_read(here::here("NewRegions/USA.shp"))#5YZew, clipped on hague
ESS <- st_read(here::here("NewRegions/ESS.shp")) #4X+5Ze clipped on hague
WSS <- st_read(here::here("NewRegions/WSS.shp"))#4VnsW
NL <- st_read(here::here("NewRegions/NL.shp"))#3NOPs

# Selected NAFO regions and clipped on hague in ArcGIS,
# Now, Dissolve polygons, aggregate AREA, add name 
USA <- USA %>%
  summarise(AREA = sum(AREA), .groups = "drop") %>%
  mutate(name = "USA") 
ESS <- ESS %>%
  summarise(AREA = sum(AREA), .groups = "drop") %>%
  mutate(name = "ESS") 
WSS <- WSS %>%
  summarise(AREA = sum(AREA), .groups = "drop") %>%
  mutate(name = "WSS") 
NL <- NL %>%
  summarise(AREA = sum(AREA), .groups = "drop") %>%
  mutate(name = "NL")  
st_write(USA, (here::here("NewRegions/USA2.shp")))
st_write(ESS, (here::here("NewRegions/ESS2.shp")))
st_write(WSS, (here::here("NewRegions/WSS2.shp")))
st_write(NL, (here::here("NewRegions/NL2.shp")))

USA <- st_read(here::here("NewRegions/USA2.shp"))#5YZew, clipped on hague
ESS <- st_read(here::here("NewRegions/ESS2.shp")) #4X+5Ze clipped on hague
WSS <- st_read(here::here("NewRegions/WSS2.shp"))#4VnsW
NL <- st_read(here::here("NewRegions/NL2.shp"))#3NOPs

New_regions <- st_as_sf(rbind(USA, ESS, WSS, NL))
st_write(New_regions, (here::here("NewRegions/New_regions.shp")))

#Clipped them to the EEZ and removed islands in Arc
NR <- st_read(here::here("R/data/NewRegions/NewRegions_EEZ.shp"))
plot(NR)

