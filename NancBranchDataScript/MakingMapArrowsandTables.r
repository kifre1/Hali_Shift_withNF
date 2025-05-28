library(geosphere)
library(dplyr)
library(sf)

#MAKING ARROWS Create arrow data by group----
  #  Find the earliest year in the first Period and latest year in the second Period for each Core Area
  #Create a single arrow connecting these points
  #Result in one directional arrow per Core Area showing the overall movement----
names(centroid_reg_sf_spr)
#Just arrows Result in one directional arrow per Core Area showing the overall movement----
arrow_dataRegion <- centroid_reg_sf_spr %>%
  group_by(Stratum) %>%
  summarize(
    start_point = first(geometry[Year == min(Year)]),  
    end_point = first(geometry[Year == max(Year)]),  
    start_year = min(Year),  # Get the start year
    end_year = max(Year)  
  ) %>%
  mutate(
    start_coords = st_coordinates(start_point),
    end_coords = st_coordinates(end_point),
    x = start_coords[,1],
    y = start_coords[,2],
    xend = end_coords[,1],
    yend = end_coords[,2],
    dx = xend - x,          
    dy = yend - y,          
    d_total = distHaversine(cbind(x, y), cbind(xend, yend))/1000,  # Great-circle distance in meters
    years_in_period = end_year - start_year,  # Compute the number of years in the period
    dist_per_year = d_total / years_in_period   # Distance per year
  ) %>%
  select(Stratum,  x, y, xend, yend,d_total, dist_per_year) %>%
  st_drop_geometry()
# Add text to your existing plot (before theme_bw())

arrow_data_CA <- centroid_CA_sf_spr %>%
  group_by(ordCoreArea) %>%
  summarize(
    # Start point - earliest year in first period
    #start_point = first(geometry[Year == min(Year[Period == first(Period)])]),
    start_point = first(geometry[Year == min(Year)]),
    #End point - latest year in second period
    end_point = first(geometry[Year == max(Year)]),
    #end_point = first(geometry[Year == max(Year[Period == last(Period)])])
    start_year = min(Year),  # Get the start year
    end_year = max(Year)     # Get the end year
    ) %>%
  mutate(
    start_coords = st_coordinates(start_point),
    end_coords = st_coordinates(end_point),
    x = start_coords[,1],   
    y = start_coords[,2],   
    xend = end_coords[,1],  
    yend = end_coords[,2],  
    dx = xend - x,          
    dy = yend - y,          
    d_total = distHaversine(cbind(x, y), cbind(xend, yend))/1000,  # Great-circle distance in meters
    years_in_period = end_year - start_year,  # Compute the number of years in the period
    dist_per_year = d_total / years_in_period   # Distance per year
  ) %>%
  select(ordCoreArea, x, y, xend, yend,d_total, dist_per_year) %>%
  st_drop_geometry()
  
#END Just arrows----
summary(arrow_data_CA)
#to compute the distance in periods----
library(dplyr)
library(sf)
library(geosphere)
# Group by both survey and period----
centroid_reg_sf_spr <- st_transform(centroid_reg_sf_spr, crs = 4326)
arrow_dataRegionPeriod <- centroid_reg_sf_spr %>%
  group_by(Stratum, Period) %>%  
  summarize(
    start_point = first(geometry[Year == min(Year)]),  
    end_point = first(geometry[Year == max(Year)]),  
    start_year = min(Year),  # Get the start year
    end_year = max(Year)     # Get the end year
  ) %>%
  mutate(
    start_coords = st_coordinates(start_point),
    end_coords = st_coordinates(end_point),
    x = start_coords[,1],   
    y = start_coords[,2],   
    xend = end_coords[,1],  
    yend = end_coords[,2],  
    dx = xend - x,          
    dy = yend - y,          
    d_total = distHaversine(cbind(x, y), cbind(xend, yend))/1000,  # Great-circle distance in meters
    years_in_period = end_year - start_year,  # Compute the number of years in the period
    dist_per_year = d_total / years_in_period   # Distance per year
  ) %>%
  select(Stratum, Period, x, y, xend, yend, d_total,  dist_per_year) %>%
  st_drop_geometry()
names(arrow_dataRegionPeriod)

# Rename the columns ADDING THIS FOR CORE AREAS AS WELL WHICH IS WHY column is named thus.
library(rlang)
COGRegMovement<- arrow_dataRegionPeriod %>%
  set_names(
    c("Region_CoreArea", "Period", "Long.Start", "Lat.Start", 
      "Long.End", "Lat.End", "Distance(km)", "Dist_per_year")
  )
names(COGRegMovement)
centroid_CA_sf_spr <- st_transform(centroid_CA_sf_spr, crs = 4326)


arrow_dataCoreAreabyPeriod <- centroid_CA_sf_spr %>%
  group_by(ordCoreArea, Period) %>%  
  summarize(
    start_point = first(geometry[Year == min(Year)]),  
    end_point = first(geometry[Year == max(Year)]),  
    start_year = min(Year),  # Get the start year
    end_year = max(Year)     # Get the end year
  ) %>%
  mutate(
    start_coords = st_coordinates(start_point),
    end_coords = st_coordinates(end_point),
    x = start_coords[,1],   
    y = start_coords[,2],   
    xend = end_coords[,1],  
    yend = end_coords[,2],  
    dx = xend - x,          
    dy = yend - y,          
    d_total = distHaversine(cbind(x, y), cbind(xend, yend))/1000,  # Great-circle distance in meters
    years_in_period = end_year - start_year,  # Compute the number of years in the period
    dist_per_year = d_total / years_in_period   # Distance per year
  ) %>%
  select(ordCoreArea, Period, x, y, xend, yend,d_total, dist_per_year) %>%
  st_drop_geometry()

names(arrow_dataCoreAreabyPeriod) 
unique(arrow_dataCoreAreabyPeriod$ordCoreArea)

names(COGRegMovement)
COGCoreMovement<- arrow_dataCoreAreabyPeriod %>%
  set_names(
    c("Region_CoreArea", "Period", "Long.Start", "Lat.Start", 
      "Long.End", "Lat.End", "Distance(km)","Dist_per_year")
  )

#JOIN TABLES.
library(flextable);library(officer)
COGSupptable<-merge(COGRegMovement,COGCoreMovement,all=T)
COGSupptable <- COGSupptable %>%
  mutate(Region_CoreArea= factor(Region_CoreArea, 
                                 levels = c("Canada", "USA", "Nantucket","Georges","CapeCod","EGOM","BOF","Browns","Sable","Gully","CapeBreton","GrandBanks","GBTail","HaliChan"), ordered = F))  %>%  # Custom order for Period
  arrange(Region_CoreArea, Period)

COGSupptable.ft <- flextable(COGSupptable)
# Format the numeric columns with 2 decimal places
COGSupptable.ft <- COGSupptable.ft%>%
  colformat_double(j = c("Long.Start", "Lat.Start",
                         "Long.End", "Lat.End", 
                         "Distance(km)","Dist_per_year"), digits = 2) %>%
  colformat_char(j = c("Region_CoreArea", "Period"))%>%
  set_table_properties(layout = "fixed")  # Ensure row order is maintained  # Ensure text columns are left as is



# Display the formatted table
COGSupptable.ft
autofit(COGSupptable.ft) #adjusts the column widths to fit the content.
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
COGSupptabledoc2 <- body_add_flextable(doc, value = COGSupptable.ft)

# Save the document
print(COGSupptabledoc2, target = here::here("NancBranchDataScript/FancyFiguresforMS/COGSupptabledoc2.docx"))

