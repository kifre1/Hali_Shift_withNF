
#Part 1 ; data prp
#Part 2 : plotting
#Distance from 'fixed' point (hague line), seasonal 
#for each group of mean/median/Q5/Q95, 
#find_nearest_point() : Identify  the closest point the Hague line for each value in the centroids_df timeseries 
#calculate_distances() : Measure the distance from the closest point to the respective centriod 
##calculate_distances is run on the 4  stats and then joined into one dataframe.
#repeat for core areas, use a subset of the Hague line for shelf CAs that would otherwise cross land to get to closest point

#Part 1 : Data Prep ----
## Calculate Distance from hague line to Centre of gravity for timeseries

library(sf)
library(dplyr)
library(purrr)
library(lwgeom)
#data needed
land <- st_read(here::here("", "Data/land_shapefile/ne_50m_land.shp"))
#CAs <- st_read(here::here("", "R/data/CoreA_shapefile/CoreAreas.shp"))
Hague <- st_read(here::here("", "Data/Mapping_shapefiles/HagueLine.shp"))

Centroids_CA <- read.csv(here::here("","2025-04-23/Output/Shift_Indicators/seasonal_centroid_data_CA.csv"),sep = ",")
Centroids_Reg <- read.csv(here::here("","2025-04-23/Output/Shift_Indicators/seasonal_centroid_data_region.csv"),sep = ",")
Centroids_All <- read.csv(here::here("","2025-04-23/Output/Shift_Indicators/seasonal_centroid_data.csv"),sep = ",")


#subset the Centroid DFs so that each stat can be calculated individually
#All
Centroids_Mean<- Centroids_All %>%
  select(-centroid_longitude_Median, -centroid_latitude_Median,
         -centroid_latitude_Quantile_05, -centroid_longitude_Quantile_05,
         -centroid_latitude_Quantile_95,-centroid_longitude_Quantile_95)
Centroids_Med<- Centroids_All %>%
  select(-centroid_latitude, - centroid_longitude,
         -centroid_latitude_Quantile_05, -centroid_longitude_Quantile_05,
         -centroid_latitude_Quantile_95,-centroid_longitude_Quantile_95)
Centroids_Q5<- Centroids_All %>%
  select(-centroid_latitude, - centroid_longitude,
         -centroid_longitude_Median, -centroid_latitude_Median,
         -centroid_latitude_Quantile_95,-centroid_longitude_Quantile_95)
Centroids_Q95<- Centroids_All %>%
  select(-centroid_latitude, - centroid_longitude,
         -centroid_latitude_Quantile_05, -centroid_longitude_Quantile_05,
         -centroid_longitude_Median, -centroid_latitude_Median)
#make data spatial and assign a matching crs (centroid longitude and latitude are based on weighted.mean(abuncance))
Centroids_Mean_sf <- st_as_sf(Centroids_Mean, coords = c("centroid_longitude", "centroid_latitude"))
Centroids_Med_sf <- st_as_sf(Centroids_Med, coords = c("centroid_longitude_Median", "centroid_latitude_Median"))
Centroids_Q5_sf <- st_as_sf(Centroids_Q5, coords = c("centroid_longitude_Quantile_05", "centroid_latitude_Quantile_05"))
Centroids_Q95_sf <- st_as_sf(Centroids_Q95, coords = c("centroid_longitude_Quantile_95", "centroid_latitude_Quantile_95"))
newcrs<-st_crs(Hague)
st_crs(Centroids_Mean_sf)<-newcrs
st_crs(Centroids_Med_sf)<-newcrs
st_crs(Centroids_Q5_sf)<-newcrs
st_crs(Centroids_Q95_sf)<-newcrs

#turn the hague to points (problematic if left as vector because then it connects to nearest node)
# break the hague into smaller segments
segmentized_hague <- st_segmentize(Hague, dfMaxLength = 1000)
# Sample points along this
sampled_points <- st_sample(segmentized_hague, size = 300)
# Combine the sampled points into a single multipoint geometry
hague_points <- st_combine(sampled_points)
st_crs(hague_points)<-newcrs
plot(hague_points)

#also need to create a subset of the hagueline that will prevent Browns, Sable, CB, HaliChan, and Gully from estimating across land
# Extract coordinates
Hague_coords <- st_coordinates(hague_points)
# Filter points below 43 latitude
filtered_Hague <- Hague_coords[Hague_coords[, 2] < 43, ]
# Create new sfc_MULTIPOINTS object with filtered coordinates
Hague_Shelf <- st_multipoint(filtered_Hague)
plot(Hague_Shelf)



#For all data---create a vector that has a closest point for each year,
find_nearest_point <- function(centroids_df, hague_points) {
  # Initialize an empty list to store results
  closest_points_list <- list()
  
  # Loop through each combination of Year and Season
  for (year in unique(centroids_df$Year)) {
    for (season in unique(centroids_df$Season)) {
      # Filter for the specified year and season
      filtered_df <- centroids_df %>% filter(Year == year, Season == season)
      
      # Extract coordinates
      point_coords <- st_coordinates(filtered_df)
      hague_coords <- st_coordinates(hague_points)
      
      # Initialize variables to store the minimum distance and index
      min_distance <- Inf
      min_index <- NULL
      
      # Calculate Euclidean distances between each point and the filtered points
      for (i in 1:nrow(point_coords)) {
        for (j in 1:nrow(hague_coords)) {
          distance <- sqrt(sum((point_coords[i, ] - hague_coords[j, ])^2))
          if (distance < min_distance) {
            min_distance <- distance
            min_index <- j
          }
        }
      }
      
      # Extract the nearest point coordinates
      nearest_point <- hague_coords[min_index, ]
      nearest_sfc_point <- data.frame(longitude = nearest_point[1], latitude = nearest_point[2])
      
      # Create a dataframe with Year, Season, and the nearest point coordinates
      closest_point_df <- data.frame(Year = year, Season = season, longitude = nearest_point[1], latitude = nearest_point[2])
      
      # Convert to sf dataframe
      sfc_point <- st_as_sf(closest_point_df, coords = c("longitude", "latitude"), crs = st_crs(hague_points))
      
      # Store the result in the list
      closest_points_list[[paste0("Year_", year, "_Season_", season)]] <- sfc_point
    }
  }
  
  # Combine all results into a single sf dataframe
  closest_points_sf <- do.call(rbind, closest_points_list)
  
  return(closest_points_sf)
}

Closest_Hague_all_mean <- find_nearest_point(Centroids_Mean_sf, hague_points)
Closest_Hague_all_med <- find_nearest_point(Centroids_Med_sf, hague_points)
Closest_Hague_all_Q5 <- find_nearest_point(Centroids_Q5_sf, hague_points)
Closest_Hague_all_Q95 <- find_nearest_point(Centroids_Q95_sf, hague_points)

print(Closest_Hague_all_mean)

#function to calculate distance from Hague Line (Closest_Hague_all...) to corresponding centroid locations
calculate_distances <- function(centroid_sf, closest_hague_df, distance_column_name) {
  # Initialize an empty vector to store distances
  distances <- numeric()
  # Loop through each row in centroid_sf
  for (i in 1:nrow(centroid_sf)) {
    # Find the corresponding row in closest_hague_df where Year and Season match
    match_row <- closest_hague_df$Year == centroid_sf$Year[i] & closest_hague_df$Season == centroid_sf$Season[i]
    # Calculate distance between the points
    distance <- st_distance(centroid_sf[i, ], closest_hague_df[match_row, ])
    # Convert distance to kilometers
    distance_km <- as.numeric(distance) / 1000
    # Append the distance to the distances vector
    distances <- c(distances, distance_km)
  }
  # Create a dataframe with years, seasons, and distances
  dist_df <- data.frame(
    Year = centroid_sf$Year,
    Season = centroid_sf$Season,
    Distance = distances
  )
  # Rename the distance column
  names(dist_df)[names(dist_df) == "Distance"] <- distance_column_name
  return(dist_df)
}

# Call the function--- NOTE  THAT FOR EACH STAT, THE CLOSEST PONINT FOR EACH YEAR IS FIXED TO THE MEAN
dist_hague_Mean <- calculate_distances(Centroids_Mean_sf, Closest_Hague_all_mean, "Dist_Mean")
dist_hague_Med <- calculate_distances(Centroids_Med_sf, Closest_Hague_all_med, "Dist_Med")
dist_hague_Q5 <- calculate_distances(Centroids_Q5_sf, Closest_Hague_all_Q5, "Dist_Q5")
dist_hague_Q95 <- calculate_distances(Centroids_Q95_sf, Closest_Hague_all_Q95, "Dist_Q95")
summary(dist_hague_Mean)

#join them all into one Df
dist_hague_all<- merge(dist_hague_Mean, dist_hague_Med, on=c("Year", "Season"))
dist_hague_all<- merge(dist_hague_all, dist_hague_Q5, on=c("Year", "Season"))
dist_hague_all<- merge(dist_hague_all, dist_hague_Q95, on=c("Year", "Season"))

head(dist_hague_all)

#REPEAT PER CORE AREA
Centroids_CA_Mean<- Centroids_CA %>%
  select(-centroid_longitude_Median, -centroid_latitude_Median,-centroid_latitude_Quantile_05, -centroid_longitude_Quantile_05,-centroid_latitude_Quantile_95,-centroid_longitude_Quantile_95)
Centroids_CA_Med<- Centroids_CA %>%
  select(-centroid_latitude, - centroid_longitude,-centroid_latitude_Quantile_05, -centroid_longitude_Quantile_05,-centroid_latitude_Quantile_95,-centroid_longitude_Quantile_95)
Centroids_CA_Q5<- Centroids_CA %>%
  select(-centroid_latitude, - centroid_longitude,-centroid_longitude_Median, -centroid_latitude_Median,-centroid_latitude_Quantile_95,-centroid_longitude_Quantile_95)
Centroids_CA_Q95<- Centroids_CA %>%
  select(-centroid_latitude, - centroid_longitude,-centroid_latitude_Quantile_05, -centroid_longitude_Quantile_05,-centroid_longitude_Median, -centroid_latitude_Median)
#make data spatial and assign a matching crs (centroid longitude and latitude are based on weighted.mean(abuncance))
Centroids_CA_Mean_sf <- st_as_sf(Centroids_CA_Mean, coords = c("centroid_longitude", "centroid_latitude"))
Centroids_CA_Med_sf <- st_as_sf(Centroids_CA_Med, coords = c("centroid_longitude_Median", "centroid_latitude_Median"))
Centroids_CA_Q5_sf <- st_as_sf(Centroids_CA_Q5, coords = c("centroid_longitude_Quantile_05", "centroid_latitude_Quantile_05"))
Centroids_CA_Q95_sf <- st_as_sf(Centroids_CA_Q95, coords = c("centroid_longitude_Quantile_95", "centroid_latitude_Quantile_95"))
newcrs<-st_crs(Hague)
st_crs(Centroids_CA_Mean_sf)<-newcrs
st_crs(Centroids_CA_Med_sf)<-newcrs
st_crs(Centroids_CA_Q5_sf)<-newcrs
st_crs(Centroids_CA_Q95_sf)<-newcrs

##use Hague_Shelf for Browns, Sable, and Gully to prevent them from estimating across land
find_nearest_point_CA <- function(centroids_df, hague_points, Hague_Shelf) {
  # Initialize an empty list to store results
  closest_points_list <- list()
  
  # Loop through each combination of Year, Season, and CA
  for (year in unique(centroids_df$Year)) {
    for (season in unique(centroids_df$Season)) {
      for (ca in unique(centroids_df$Stratum)) {
        # Filter for the specified year, season, and CA
        filtered_df <- centroids_df %>% filter(Year == year, Season == season, Stratum == ca)
        
        # Extract coordinates
        point_coords <- st_coordinates(filtered_df)
        
        # Determine which set of points to use based on CA
        if (ca %in% c("Browns", "Sable", "Gully", "CB4Vn", "HaliChan")) {
          hague_coords <- st_coordinates(Hague_Shelf)
        } else {
          hague_coords <- st_coordinates(hague_points)
        }
        
        # Initialize variables to store the minimum distance and index
        min_distance <- Inf
        min_index <- NULL
        
        # Calculate Euclidean distances between each point and the filtered points
        for (i in 1:nrow(point_coords)) {
          for (j in 1:nrow(hague_coords)) {
            distance <- sqrt(sum((point_coords[i, ] - hague_coords[j, ])^2))
            if (distance < min_distance) {
              min_distance <- distance
              min_index <- j
            }
          }
        }
        
        # Extract the nearest point coordinates
        nearest_point <- hague_coords[min_index, ]
        nearest_sfc_point <- data.frame(longitude = nearest_point[1], latitude = nearest_point[2])
        
        # Create a dataframe with Year, Season, CA, and the nearest point coordinates
        closest_point_df <- data.frame(Year = year, Season = season, Stratum = ca, longitude = nearest_point[1], latitude = nearest_point[2])
        
        # Convert to sf dataframe
        sfc_point <- st_as_sf(closest_point_df, coords = c("longitude", "latitude"), crs = st_crs(hague_points))
        
        # Store the result in the list
        closest_points_list[[paste0("Year_", year, "_Season_", season, "_CA_", ca)]] <- sfc_point
      }
    }
  }
  
  # Combine all results into a single sf dataframe
  closest_points_sf <- do.call(rbind, closest_points_list)
  
  return(closest_points_sf)
}


#run function to make df oc closest hague point for each Year/Season/CA
Closest_Hague_CA_mean <- find_nearest_point_CA(Centroids_CA_Mean_sf, hague_points, Hague_Shelf)
Closest_Hague_CA_med <- find_nearest_point_CA(Centroids_CA_Med_sf, hague_points, Hague_Shelf)
Closest_Hague_CA_Q5 <- find_nearest_point_CA(Centroids_CA_Q5_sf, hague_points, Hague_Shelf)
Closest_Hague_CA_Q95 <- find_nearest_point_CA(Centroids_CA_Q95_sf, hague_points, Hague_Shelf)

head(Closest_Hague_CA_mean)

#function to calculate distance from Hague Line (Closest_Hague_all) to corresponding centroid locations
calculate_distances_CA <- function(centroid_sf, closest_hague_df, distance_column_name) {
  # Initialize an empty vector to store distances
  distances <- numeric()
  
  # Loop through each combination of Year, Season, and CA
  for (i in 1:nrow(centroid_sf)) {
    year <- centroid_sf$Year[i]
    season <- centroid_sf$Season[i]
    ca <- centroid_sf$Stratum[i]
    
    # Find the corresponding row in closest_hague_df where Year, Season, and CA match
    match_row <- closest_hague_df$Year == year & closest_hague_df$Season == season & closest_hague_df$Stratum == ca
    
    # Calculate distance between the points
    distance <- st_distance(centroid_sf[i, ], closest_hague_df[match_row, ])
    
    # Convert distance to kilometers
    distance_km <- as.numeric(distance) / 1000
    
    # Append the distance to the distances vector
    distances <- c(distances, distance_km)
  }
  
  # Create a dataframe with years, seasons, CAs, and distances
  dist_df <- data.frame(
    Year = centroid_sf$Year,
    Season = centroid_sf$Season,
    Stratum = centroid_sf$Stratum,
    Distance = distances
  )
  
  # Rename the distance column
  names(dist_df)[names(dist_df) == "Distance"] <- distance_column_name
  
  return(dist_df)
}
names(Centroids_CA_Mean_sf)

# Call the function--- NOTE  THAT FOR EACH STAT, THE CLOSEST PONINT FOR EACH YEAR IS FIXED TO THE MEAN
dist_hague_Mean_CA <- calculate_distances_CA(Centroids_CA_Mean_sf, Closest_Hague_CA_mean, "Dist_Mean")
dist_hague_Med_CA <- calculate_distances_CA(Centroids_CA_Med_sf, Closest_Hague_CA_med, "Dist_Med")
dist_hague_Q5_CA <- calculate_distances_CA(Centroids_CA_Q5_sf, Closest_Hague_CA_Q5, "Dist_Q5")
dist_hague_Q95_CA <- calculate_distances_CA(Centroids_CA_Q95_sf, Closest_Hague_CA_Q95, "Dist_Q95")
summary(dist_hague_Mean_CA)

#join them all into one Df
dist_hague_CA<- merge(dist_hague_Mean_CA, dist_hague_Med_CA, on=c("Year", "Season", "Stratum"))
dist_hague_CA<- merge(dist_hague_CA, dist_hague_Q5_CA, on=c("Year", "Season", "Stratum"))
dist_hague_CA<- merge(dist_hague_CA, dist_hague_Q95_CA, on=c("Year", "Season", "Stratum"))

head(dist_hague_CA)

#run regionally 
Centroids_Reg_Mean<- Centroids_Reg %>%
  select(-centroid_longitude_Median, -centroid_latitude_Median,-centroid_latitude_Quantile_05, -centroid_longitude_Quantile_05,-centroid_latitude_Quantile_95,-centroid_longitude_Quantile_95)
Centroids_Reg_Med<- Centroids_Reg %>%
  select(-centroid_latitude, - centroid_longitude,-centroid_latitude_Quantile_05, -centroid_longitude_Quantile_05,-centroid_latitude_Quantile_95,-centroid_longitude_Quantile_95)
Centroids_Reg_Q5<- Centroids_Reg %>%
  select(-centroid_latitude, - centroid_longitude,-centroid_longitude_Median, -centroid_latitude_Median,-centroid_latitude_Quantile_95,-centroid_longitude_Quantile_95)
Centroids_Reg_Q95<- Centroids_Reg %>%
  select(-centroid_latitude, - centroid_longitude,-centroid_latitude_Quantile_05, -centroid_longitude_Quantile_05,-centroid_longitude_Median, -centroid_latitude_Median)
#make data spatial and assign a matching crs (centroid longitude and latitude are based on weighted.mean(abuncance))
Centroids_Reg_Mean_sf <- st_as_sf(Centroids_Reg_Mean, coords = c("centroid_longitude", "centroid_latitude"))
Centroids_Reg_Med_sf <- st_as_sf(Centroids_Reg_Med, coords = c("centroid_longitude_Median", "centroid_latitude_Median"))
Centroids_Reg_Q5_sf <- st_as_sf(Centroids_Reg_Q5, coords = c("centroid_longitude_Quantile_05", "centroid_latitude_Quantile_05"))
Centroids_Reg_Q95_sf <- st_as_sf(Centroids_Reg_Q95, coords = c("centroid_longitude_Quantile_95", "centroid_latitude_Quantile_95"))
newcrs<-st_crs(Hague)
st_crs(Centroids_Reg_Mean_sf)<-newcrs
st_crs(Centroids_Reg_Med_sf)<-newcrs
st_crs(Centroids_Reg_Q5_sf)<-newcrs
st_crs(Centroids_Reg_Q95_sf)<-newcrs

#run nearest from hague function
Closest_Hague_Reg_mean <- find_nearest_point_CA(Centroids_Reg_Mean_sf, hague_points)
Closest_Hague_Reg_med <- find_nearest_point_CA(Centroids_Reg_Med_sf, hague_points)
Closest_Hague_Reg_Q5 <- find_nearest_point_CA(Centroids_Reg_Q5_sf, hague_points)
Closest_Hague_Reg_Q95 <- find_nearest_point_CA(Centroids_Reg_Q95_sf, hague_points)

head(Closest_Hague_Reg_mean)

# run the distance the function--- NOTE  THAT FOR EACH STAT, THE CLOSEST PONINT FOR EACH YEAR IS FIXED TO THE MEAN
dist_hague_Mean_Reg <- calculate_distances_CA(Centroids_Reg_Mean_sf, Closest_Hague_Reg_mean, "Dist_Mean")
dist_hague_Med_Reg <- calculate_distances_CA(Centroids_Reg_Med_sf, Closest_Hague_Reg_med, "Dist_Med")
dist_hague_Q5_Reg <- calculate_distances_CA(Centroids_Reg_Q5_sf, Closest_Hague_Reg_Q5, "Dist_Q5")
dist_hague_Q95_Reg <- calculate_distances_CA(Centroids_Reg_Q95_sf, Closest_Hague_Reg_Q95, "Dist_Q95")
summary(dist_hague_Mean_Reg)

#join them all into one Df
dist_hague_Reg<- merge(dist_hague_Mean_Reg, dist_hague_Med_Reg, on=c("Year", "Season", "Stratum"))
dist_hague_Reg<- merge(dist_hague_Reg, dist_hague_Q5_Reg, on=c("Year", "Season", "Stratum"))
dist_hague_Reg<- merge(dist_hague_Reg, dist_hague_Q95_Reg, on=c("Year", "Season", "Stratum"))

str(dist_hague_CA)
str(dist_hague_Reg)
str(dist_hague_all)

write.csv(dist_hague_CA,(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_CA_seasonal.csv")), row.names = FALSE)
write.csv(dist_hague_all,(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_all_seasonal.csv")), row.names = FALSE)
write.csv(dist_hague_Reg,(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_Reg_seasonal.csv")), row.names = FALSE)
#End of Data preparation----

#part 2----plotting data

library(ggplot2)
dist_hague_all<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_all_seasonal.csv"))
dist_hague_CA<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_CA_seasonal.csv"))
dist_hague_Reg<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_Reg_seasonal.csv"))
head(dist_hague_Reg)

dist_hague_all$Dist_Q5<- dist_hague_all$Dist_Q5*-1 # becasue Dist_Q5 for the area as a whole is on the US side of the Hague 

dist_hague_all <- dist_hague_all %>%
  filter(Season == "Spring" ) 

#Overall "COG Distance from Hague Line (Spring)"
ggplot(dist_hague_all, aes(x = Year, y = Dist_Mean, color=Season)) +
  geom_point() +
  geom_errorbar(aes(ymin = Dist_Q5, ymax = Dist_Q95), width = 0.2,col="darkgrey") +  # Add error bars
  geom_point(aes(x = Year, y = Dist_Med),shape=1,size=2,stroke=1)+
  #facet_wrap(.~Season, scales="free") +
  labs(title = "COG Distance from Hague Line (Spring)", x = "Year", y = "km") +
  theme_bw()

#core area - start with spring
#Put Q5 in the US for Georges
dist_hague_CA <- dist_hague_CA %>%
  mutate(Dist_Q5 = if_else(Stratum == "Georges", Dist_Q5 * -1, Dist_Q5))
#subset spring
spring_dist_hague_CA <- dist_hague_CA %>%
  filter(Season == "Spring" ) 
#factor levels for plotting
spring_dist_hague_CA$Stratum<-factor(spring_dist_hague_CA$Stratum,levels=c("EGOM","BOF","CapeBreton","HaliChan",
                                                         "CapeCod","Browns","Gully","GrandBanks",
                                                         "Nantucket","Georges","Sable","GBTail"))
#"COG Distance from Hague Line: Spring" per core area
ggplot(spring_dist_hague_CA, aes(x = Year, y = Dist_Mean)) +
  geom_errorbar(aes(ymin = Dist_Q5, ymax = Dist_Q95), width = 0.2, col="darkgrey") +  # Add error bars
  geom_point(colour="blue") +
  geom_point(aes(x = Year, y = Dist_Med),shape=1,size=2,col="black",stroke=1)+
  facet_wrap(.~Stratum, scales="free") +
  labs(title = "COG Distance from Hague Line: Spring", x = "Year", y = "km") +
  theme_bw()
#----

#reverse US values so that Hague line acts as 0

library(dplyr)

dist_hague_ca_DisplayFromZero <- spring_dist_hague_CA %>%
  mutate(
    Dist_Mean = case_when(
      Stratum %in% c("EGOM", "CapeCod", "Nantucket") ~ Dist_Mean * -1,
      TRUE ~ Dist_Mean
    ),
    Dist_Med = case_when(
      Stratum %in% c("EGOM", "CapeCod", "Nantucket") ~ Dist_Med * -1,
      TRUE ~ Dist_Med
    ),
    Dist_Q5 = case_when(
      Stratum %in% c("EGOM", "CapeCod", "Nantucket") ~ Dist_Q5 * -1,
      TRUE ~ Dist_Q5
    ),
    Dist_Q95 = case_when(
      Stratum %in% c("EGOM", "CapeCod", "Nantucket") ~ Dist_Q95 * -1,
      TRUE ~ Dist_Q95
    )
  )

dist_hague_RegFromZero<-dist_hague_Reg %>%
  mutate(
    Dist_Mean = case_when(
      Stratum %in% c("USA") ~ Dist_Mean * -1,
      TRUE ~ Dist_Mean
    ),
    Dist_Med = case_when(
      Stratum %in% c("USA") ~ Dist_Med * -1,
      TRUE ~ Dist_Med
    ),
    Dist_Q5 = case_when(
      Stratum %in% c("USA") ~ Dist_Q5 * -1,
      TRUE ~ Dist_Q5
    ),
    Dist_Q95 = case_when(
      Stratum %in% c("USA") ~ Dist_Q95 * -1,
      TRUE ~ Dist_Q95
    )
  )


#regional plot
#subset spring
dist_hague_RegFromZero_Spring <- dist_hague_RegFromZero %>%
  filter(Season == "Spring" ) 
#
dist_hague_RegFromZero_Spring$Period[dist_hague_RegFromZero_Spring$Year<2006]<-"Before Warming"
dist_hague_RegFromZero_Spring$Period[dist_hague_RegFromZero_Spring$Year>2005]<-"During Warming"

#write.csv(dist_hague_RegFromZero_Spring,(here::here("2024-10-04/Output/Shift_Indicators/dist_hague_RegFromZero_Spring.csv")), row.names = FALSE)
df_REGtransformedSpringAgg <- dist_hague_RegFromZero_Spring %>%
  group_by(Stratum, Period) %>%
    summarize(
    Avg_Dist_Med.OPP = mean(Dist_Med, na.rm = TRUE),
    Avg_Dist_Mean.OPP = mean(Dist_Mean, na.rm = TRUE),
    Avg_Dist_Q5.OPP = mean(Dist_Q5, na.rm = TRUE),
    Avg_Dist_Q95.OPP = mean(Dist_Q95, na.rm = TRUE)
    )

#write.csv(df_REGtransformedSpringAgg,(here::here("2024-10-04/Output/Shift_Indicators/df_REGtransformedSpringAgg.csv")), row.names = FALSE)
#"Regional Distance from Hague Line (Spring)"
ggplot(dist_hague_RegFromZero_Spring, aes(x = Year, y = Dist_Mean, color=Stratum)) +
  geom_errorbar(aes(ymin=Dist_Q5,ymax=Dist_Q95)) +
  geom_point() +
  # geom_point(aes(y=Dist_Med),shape=1,size=2,col="black")+
  labs(title = "Regional COG Distance from \n Hague Line (Spring)", x = "Year", y = "km") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  scale_color_manual(values = c("darkorange", "blue")) +
  geom_point(aes(x = Year, y = Dist_Med),shape=1,size=2,col="black",stroke=1)+
  theme_bw()

ggplot(dist_hague_RegFromZero, aes(x = Year, y = Dist_Mean, color=Stratum)) +
  geom_errorbar(aes(ymin=Dist_Q5,ymax=Dist_Q95)) +
  geom_point() +
  # geom_point(aes(y=Dist_Med),shape=1,size=2,col="black")+
  labs(title = "Regional COG Distance from Hague Line", x = "Year", y = "km") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  scale_color_manual(values = c("darkorange","blue")) +
  geom_point(aes(x = Year, y = Dist_Med),shape=1,size=2,col="black",stroke=1)+
  facet_wrap(Season~., scales = "free") +
  theme_bw()


#plot on top of eachother
spring_dist_hague_Reg<-subset(dist_hague_Reg, dist_hague_Reg$Season=="Spring")

Canada <- subset(spring_dist_hague_Reg, Stratum == "Canada")
USA <- subset(spring_dist_hague_Reg, Stratum == "USA")
# Calculate the range for each dataset
USA_range <- range(USA$Dist_Mean)
Canada_range <- range(Canada$Dist_Mean)


par(mar = c(5, 4, 4, 8) + 0.1)  # Add space on the right side
plot(USA$Year,USA$Dist_Mean, type = "l", col = "blue", ylim = USA_range, 
     xlab = "Year", ylab = "USA Distance (km)")
# Add scale numbers for DFO data
axis(2, col ="blue",col.axis = "blue")
# Overlay NMFS data
par(new = TRUE)
plot(Canada$Year, Canada$Dist_Mean, type = "l", col = "darkorange", axes = FALSE, bty = "n", xlab = "", ylab = "")
# Add scale numbers for NMFS data
axis(4, ylim = Canada_range, col = "darkorange", col.axis = "darkorange")
# Change color of left y-axis label
mtext("Canada Distance (km)", side = 4, line = 3)
mtext('Distance from COG(mean) to Hague Line, Spring', side = 3)  # Add title here
abline(v = 2005, lty = 2, lwd = 2)

#----
#Look at EGOM and BOF as a pair
subCA_distHagueEGBOF <- dist_hague_ca_DisplayFromZero %>%
  filter(Stratum == "EGOM" | Stratum == "BOF")%>%
  filter(Season == "Spring" )


ggplot(subCA_distHagueEGBOF, aes(y= Year*-1, x = Dist_Mean, color=Stratum)) +
  geom_point(aes(x=Dist_Med),shape=1,size=2,col="black",stroke=1)+
  geom_errorbar(aes(xmin=Dist_Q5,xmax=Dist_Q95)) +
  #geom_point(pch=1,size=5,col="black") +
  geom_point() +
  scale_x_continuous(breaks=seq(-150,150,25)) +
  scale_color_manual(name = " ", values = c("darkorange", "blue")) +
  #labs(title = "COG Distance from Hague Line: Spring", x = "Year", y = "km") +
  labs(title = "COG Distance from Hague Line \n EGOM vs. BOF", y = "", x = "km") +  
  #geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  #annotate("text",x=1990,y=-20,label="USA", color = "black",size = 4) +
  #annotate("text",x=1990,y=20,label="Canada", vjust = 0, color = "black",size = 4) +
  theme_bw()


