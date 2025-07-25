#MakeSlopeFile of all indicators----

library(tidyverse)
library(broom)
library(dplyr)

library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)
library(RColorBrewer)
library(scales)


#Interpretations: 
#multiplicative_change_per_year: How much the Index_Estimate multiplies by each year (e.g., 1.05 = 5% increase)
#percent_change_per_year: Annual percent change (e.g., 5.0 = 5% increase per year)
#Confidence intervals for both transformations, so you can assess uncertainty in the original scale

#For example, if your slope estimate is 0.02 with 95% CI [0.01, 0.03]:
  
#  The Index_Estimate increases by 4.7% per year (95% CI: 2.3% to 7.2%)
#Each year, the index multiplies by 1.047 (95% CI: 1.023 to 1.072)

#This makes your results much more interpretable in terms of the actual abundance trends!

#Abundance----
FigAbd.Region.Spring <- read.csv(here::here("2025-04-23/Output/IndexAbundance/abundance_ind_Region.Spring.csv"),row.names=NULL)
names(FigAbd.Region.Spring)
FigAbd.Region.Spring$Period<-NULL
FigAbd.Region.Spring$Period[FigAbd.Region.Spring$Year<2006]<-"1990-2005"
FigAbd.Region.Spring$Period[FigAbd.Region.Spring$Year>2005]<-"2006-2023"

RegionAbd_coefficients_df <- FigAbd.Region.Spring %>%
  group_by(Region, Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    log_index_centered <- log10(.$Index_Estimate) - mean(log10(.$Index_Estimate), na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(log_index_centered ~ year_centered)
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

RegionAbd_coefficients_df2 <- RegionAbd_coefficients_df %>%
  filter(term == "year_centered") %>%
  mutate(
    # Transform slope back to original Index_Estimate scale
    multiplicative_change_per_year = 10^estimate,
    percent_change_per_year = (10^estimate - 1) * 100,
    
    # Transform confidence intervals to original scale
    multiplicative_change_lower = 10^conf.low,
    multiplicative_change_upper = 10^conf.high,
    percent_change_lower = (10^conf.low - 1) * 100,
    percent_change_upper = (10^conf.high - 1) * 100
  )
RegionAbd_coefficients_df2$Indicator<-NULL
RegionAbd_coefficients_df2$Indicator<-"Abundance"
# Save the coefficients to a CSV file
#write.csv(RegionAbd_coefficients_df2, here::here("2025-04-23/Output/IndexAbundance/RegionAbd_coefficients_df2.csv"), row.names = FALSE)
#END Abundance----
#AO----
Area_ThresholdsforEAO<- read.csv(here::here("R/DataforFinalFigs/Area_ThresholdsforEAO.csv"))
names(Area_ThresholdsforEAO)
Area_ThresholdsforEAO$Period<-NULL
Area_ThresholdsforEAO$Period[Area_ThresholdsforEAO$Year<2006]<-"1990-2005"
Area_ThresholdsforEAO$Period[Area_ThresholdsforEAO$Year>2005]<-"2006-2023"
Area_ThresholdsforEAO_coefficients_df <- Area_ThresholdsforEAO %>%filter(Threshold == 90)%>%
  group_by(Region,Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    log_AO_centered <- log10(.$Area_Threshold) - mean(log10(.$Area_Threshold), na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(log_AO_centered ~ year_centered)
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

Area_ThresholdsforEAO_coefficients_df2 <- Area_ThresholdsforEAO_coefficients_df %>%
  filter(term == "year_centered") %>%
  mutate(
    # Transform slope back to original Index_Estimate scale
    multiplicative_change_per_year = 10^estimate,
    percent_change_per_year = (10^estimate - 1) * 100,
    
    # Transform confidence intervals to original scale
    multiplicative_change_lower = 10^conf.low,
    multiplicative_change_upper = 10^conf.high,
    percent_change_lower = (10^conf.low - 1) * 100,
    percent_change_upper = (10^conf.high - 1) * 100
  )
Area_ThresholdsforEAO_coefficients_df2$Indicator<-NULL
Area_ThresholdsforEAO_coefficients_df$Indicator<-"Area Occupied"
#scaled slopes fro EAO vs Abundance----
Area_ThresholdsforEAO_coefficientsABD_df <- Area_ThresholdsforEAO %>%filter(Threshold == 90)%>%
  group_by(Region,Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    log_AO_centered <- log10(.$Area_Threshold) - mean(log10(.$Area_Threshold), na.rm = TRUE)
    logAB_centered <- log10(.$Total_Abundance) - mean(log10(.$Total_Abundance), na.rm = TRUE)
    
    model <- lm(log_AO_centered ~ logAB_centered)
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

Area_ThresholdsforEAO_coefficientsABD_df2 <- Area_ThresholdsforEAO_coefficientsABD_df %>%
  filter(term == "logAB_centered") %>%
  mutate(
    # In double-log models, the slope IS the elasticity
    # It represents % change in Area_Threshold for 1% change in Total_Abundance
    elasticity = estimate,
    elasticity_lower = conf.low,
    elasticity_upper = conf.high,
    
    # For interpretation: if Total_Abundance increases by X%, 
    # Area_Threshold changes by (elasticity * X)%
    # Example interpretations for different abundance changes:
    percent_change_AO_if_AB_increases_10pct = elasticity * 10,
    percent_change_AO_if_AB_doubles = elasticity * 100  # doubling = 100% increase
  )
Area_ThresholdsforEAO_coefficientsABD_df2$Indicator<-NULL
Area_ThresholdsforEAO_coefficientsABD_df2$Indicator<-"AO vs Abd"
# END AO----
#COG----
#COG
centroid_dataRegionalforFig<- read.csv(here::here("R/DataforFinalFigs/centroid_dataRegionalforFig.csv"))
names(centroid_dataRegionalforFig)
centroid_dataRegionalforFig$Period<-NULL
centroid_dataRegionalforFig$Period[centroid_dataRegionalforFig$Year<2006]<-"1990-2005"
centroid_dataRegionalforFig$Period[centroid_dataRegionalforFig$Year>2005]<-"2006-2023"
centroid_dataRegionalforFig$Region<-centroid_dataRegionalforFig$Stratum
centroid_dataRegionalforFigLAT_df <- centroid_dataRegionalforFig %>%
  filter(Season == "Spring") %>%
  group_by(Region, Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    cog_lat_centered <- .$centroid_latitude - mean(.$centroid_latitude, na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(cog_lat_centered ~ year_centered)
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

centroid_dataRegionalforFigLAT_df2 <- centroid_dataRegionalforFigLAT_df %>%
  filter(term == "year_centered") %>%
  mutate(
    # Linear relationship interpretation - slope is directly interpretable
    degrees_latitude_change_per_year = estimate,
    degrees_latitude_change_per_year_lower = conf.low,
    degrees_latitude_change_per_year_upper = conf.high,
    
    # Convert to more interpretable units
    # 1 degree latitude ≈ 111 km, so convert to km per year
    km_northward_shift_per_year = estimate * 111,
    km_northward_shift_per_year_lower = conf.low * 111,
    km_northward_shift_per_year_upper = conf.high * 111,
    
    # Calculate change over different time periods
    degrees_change_per_decade = estimate * 10,
    km_change_per_decade = estimate * 111 * 10,
    
    # Direction interpretation
    shift_direction = case_when(
      estimate > 0 ~ "Northward shift",
      estimate < 0 ~ "Southward shift", 
      estimate == 0 ~ "No latitudinal shift"
    ),
    
    # Statistical significance check
    is_significant_05 = ifelse(p.value < 0.05, "Significant", "Not significant"),
    
    # Practical interpretation text
    interpretation = paste0(
      "Centroid shifts ", round(abs(km_northward_shift_per_year), 2), " km ",
      ifelse(estimate > 0, "north", "south"), " per year",
      " (95% CI: ", round(abs(km_northward_shift_per_year_lower), 2), " to ", 
      round(abs(km_northward_shift_per_year_upper), 2), " km)"
    )
  )

centroid_dataRegionalforFigLAT_df2$Indicator<-NULL
centroid_dataRegionalforFigLAT_df2$Indicator<-"COG North"
#Now for COG East
#COG Lon----
centroid_dataRegionalforFigLON_df <- centroid_dataRegionalforFig %>%
  filter(Season == "Spring") %>%
  group_by(Region, Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    cog_lon_centered <- .$centroid_longitude - mean(.$centroid_longitude, na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(cog_lon_centered ~ year_centered)
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

centroid_dataRegionalforFigLON_df2 <- centroid_dataRegionalforFigLON_df %>%
  filter(term == "year_centered") %>%
  mutate(
    # Linear relationship interpretation - slope is directly interpretable
    degrees_longitude_change_per_year = estimate,
    degrees_longitude_change_per_year_lower = conf.low,
    degrees_longitude_change_per_year_upper = conf.high,
    
    # Convert to more interpretable units
    # For your study area (-50° to -70° longitude, 42°-50° latitude)
    # Using midpoint latitude of 46°N: 1 degree longitude ≈ 77.1 km
    km_per_degree_longitude = 111 * cos(46 * pi/180), # ≈ 77.1 km
    km_eastward_shift_per_year = estimate * km_per_degree_longitude,
    km_eastward_shift_per_year_lower = conf.low * km_per_degree_longitude,
    km_eastward_shift_per_year_upper = conf.high * km_per_degree_longitude,
    
    # Calculate change over different time periods
    degrees_change_per_decade = estimate * 10,
    km_change_per_decade = estimate * km_per_degree_longitude * 10,
    
    # Direction interpretation (positive longitude = eastward in Western Hemisphere)
    shift_direction = case_when(
      estimate > 0 ~ "Eastward shift",
      estimate < 0 ~ "Westward shift", 
      estimate == 0 ~ "No longitudinal shift"
    ),
    
    # Statistical significance check
    is_significant_05 = ifelse(p.value < 0.05, "Significant", "Not significant"),
    
    # Practical interpretation text
    interpretation = paste0(
      "Centroid shifts ", round(abs(km_eastward_shift_per_year), 2), " km ",
      ifelse(estimate > 0, "east", "west"), " per year",
      " (95% CI: ", round(abs(km_eastward_shift_per_year_lower), 2), " to ", 
      round(abs(km_eastward_shift_per_year_upper), 2), " km)"
    )
  )
centroid_dataRegionalforFigLON_df2$Indicator<-NULL
centroid_dataRegionalforFigLON_df2$Indicator<-"COG East"
#End COG Lon----
#Deepening----
D_data_Reg<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/Seasonal_Deepening_Reg.csv"))
D_data_Reg$Period<-NULL
D_data_Reg$Period[D_data_Reg$Year<2006]<-"1990-2005"
D_data_Reg$Period[D_data_Reg$Year>2005]<-"2006-2023"
D_data_Reg$Region<-D_data_Reg$Stratum
D_data_Reg_df <- D_data_Reg %>%
  filter(Season == "Spring") %>%
  group_by(Region, Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    depth_centered <- .$Depth_Mean - mean(.$Depth_Mean, na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(depth_centered ~ year_centered)
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

D_data_Reg_df2 <- D_data_Reg_df %>%
  filter(term == "year_centered") %>%
  mutate(
    # Linear relationship interpretation - slope is directly interpretable
    meters_depth_change_per_year = estimate,
    meters_depth_change_per_year_lower = conf.low,
    meters_depth_change_per_year_upper = conf.high,
    
    # Calculate change over different time periods
    meters_change_per_decade = estimate * 10,
    meters_change_per_half_century = estimate * 50,
    
    # Direction interpretation (positive depth = deeper, negative = shallower)
    depth_direction = case_when(
      estimate > 0 ~ "Moving deeper",
      estimate < 0 ~ "Moving shallower", 
      estimate == 0 ~ "No depth change"
    ),
    
    # Practical categories for magnitude of change
    change_magnitude = case_when(
      abs(estimate) < 0.5 ~ "Minimal depth change",
      abs(estimate) >= 0.5 & abs(estimate) < 2.0 ~ "Moderate depth change",
      abs(estimate) >= 2.0 & abs(estimate) < 5.0 ~ "Substantial depth change",
      abs(estimate) >= 5.0 ~ "Major depth change"
    ),
    
    # Statistical significance check
    is_significant_05 = ifelse(p.value < 0.05, "Significant", "Not significant"),
    
    # Practical interpretation text
    interpretation = paste0(
      "Mean depth changes by ", round(abs(meters_depth_change_per_year), 2), " m ",
      ifelse(estimate > 0, "deeper", "shallower"), " per year",
      " (95% CI: ", round(abs(meters_depth_change_per_year_lower), 2), " to ", 
      round(abs(meters_depth_change_per_year_upper), 2), " m)"
    ),
    
    # Decadal interpretation for easier communication
    decadal_interpretation = paste0(
      "Over a decade: ", round(abs(meters_change_per_decade), 1), " m ",
      ifelse(estimate > 0, "deeper", "shallower")
    )
  )
D_data_Reg_df2$Indicator<-NULL
D_data_Reg_df2$Indicator<-"Depth-wtd Abd"
#END Deepening----
#Distance ot Hague----
dist_hague_Reg<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_Reg_seasonal.csv"))

dist_hague_Reg$Period<-NULL
pd <- position_dodge(.75)
dist_hague_Reg$Period[dist_hague_Reg$Year<2006]<-"1990-2005"

dist_hague_Reg$Period[dist_hague_Reg$Year>2005]<-"2006-2023"
dist_hague_Reg$Region<-dist_hague_Reg$Stratum
Overall.DFHcoefficients_df.Spring <- dist_hague_Reg %>% 
  filter(Season == "Spring") %>%
  group_by(Region, Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    dist_centered <- .$Dist_Mean - mean(.$Dist_Mean, na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(dist_centered ~ year_centered)
    tidy(model, conf.int = TRUE)
  }) %>%
  ungroup() %>%
  filter(term == "year_centered") %>%
  mutate(
    # Direct interpretation in distance units per year (assuming km)
    km_distance_change_per_year = estimate,
    km_distance_change_per_year_lower = conf.low,
    km_distance_change_per_year_upper = conf.high,
    
    # Calculate change over different time periods
    km_change_per_decade = estimate * 10,
    km_change_per_half_century = estimate * 50,
    
    # Direction interpretation relative to border
    border_direction = case_when(
      estimate > 0 ~ "Moving away from border",
      estimate < 0 ~ "Moving toward border",
      estimate == 0 ~ "No change in distance to border"
    ),
    
    # Magnitude categories for ecological significance
    change_magnitude = case_when(
      abs(estimate) < 1 ~ "Minimal border proximity change",
      abs(estimate) >= 1 & abs(estimate) < 5 ~ "Moderate border proximity change",
      abs(estimate) >= 5 & abs(estimate) < 10 ~ "Substantial border proximity change",
      abs(estimate) >= 10 ~ "Major border proximity change"
    ),
    
    # Statistical significance
    is_significant_05 = ifelse(p.value < 0.05, "Significant", "Not significant"),
    
    # Management implications
    management_implication = case_when(
      estimate < 0 & p.value < 0.05 ~ "Increasing transboundary management concern",
      estimate > 0 & p.value < 0.05 ~ "Decreasing transboundary management concern",
      p.value >= 0.05 ~ "No significant change in transboundary status"
    ),
    
    # Interpretation text
    interpretation = paste0(
      "Distance to border changes by ", round(abs(km_distance_change_per_year), 2), " km ",
      ifelse(estimate > 0, "farther", "closer"), " per year",
      " (95% CI: ", round(abs(km_distance_change_per_year_lower), 2), " to ", 
      round(abs(km_distance_change_per_year_upper), 2), " km)"
    ),
    
    # Decadal interpretation for policy relevance
    decadal_interpretation = paste0(
      "Over a decade: ", round(abs(km_change_per_decade), 1), " km ",
      ifelse(estimate > 0, "farther from", "closer to"), " border"
    ),
    
    # Policy-relevant summary
    policy_summary = paste0(
      ifelse(estimate < 0, "Population centroid approaching ", "Population centroid moving away from "),
      "international boundary at ", round(abs(km_distance_change_per_year), 2), " km/year"
    )
  )
Overall.DFHcoefficients_df.Spring$Indicator<-NULL
Overall.DFHcoefficients_df.Spring$Indicator<-"Distance to Border"
View(Overall.DFHcoefficients_df.Spring)
#End DFH----
#Leading and Trailing Edges----

rangeedge<-read.csv(here::here("R/DataforFinalFigs/Edge_df_NSreshp.csv"))
range.spr<-rangeedge[rangeedge$Season=="Spring",]
range.spr$Period<-NULL
range.spr$Period[range.spr$Year<2006]<-"1990-2005"
range.spr$Period[range.spr$Year>2005]<-"2006-2023"
names(range.spr);summary(range.spr)
# Leading Edge East (95th percentile)----
range.sprE_coefficients_df <- range.spr %>% 
  group_by(Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    east_edge_centered <- .$Estimate_km_E_quantile_0.95 - mean(.$Estimate_km_E_quantile_0.95, na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(east_edge_centered ~ year_centered)
    tidy(model, conf.int = TRUE)
  }) %>%
  ungroup() %>%
  filter(term == "year_centered") %>%
  mutate(
    # Direct interpretation in km per year
    km_eastward_shift_per_year = estimate,
    km_eastward_shift_per_year_lower = conf.low,
    km_eastward_shift_per_year_upper = conf.high,
    
    # Calculate change over different time periods
    km_change_per_decade = estimate * 10,
    km_change_per_half_century = estimate * 50,
    
    # Direction interpretation
    shift_direction = case_when(
      estimate > 0 ~ "Expanding eastward",
      estimate < 0 ~ "Contracting westward",
      estimate == 0 ~ "No eastward range change"
    ),
    
    # Statistical significance
    is_significant_05 = ifelse(p.value < 0.05, "Significant", "Not significant"),
    
    # Interpretation text
    interpretation = paste0(
      "Leading edge shifts ", round(abs(km_eastward_shift_per_year), 2), " km ",
      ifelse(estimate > 0, "east", "west"), " per year",
      " (95% CI: ", round(abs(km_eastward_shift_per_year_lower), 2), " to ", 
      round(abs(km_eastward_shift_per_year_upper), 2), " km)"
    ),
    
    Indicator = "Leading Edge E"
  )
#END Leading edgeE----
# Leading Edge North (95th percentile)----
range.sprN_coefficients_df <- range.spr %>%
  group_by(Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    north_edge_centered <- .$Estimate_km_N_quantile_0.95 - mean(.$Estimate_km_N_quantile_0.95, na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(north_edge_centered ~ year_centered)
    tidy(model, conf.int = TRUE)
  }) %>%
  ungroup() %>%
  filter(term == "year_centered") %>%
  mutate(
    # Direct interpretation in km per year
    km_northward_shift_per_year = estimate,
    km_northward_shift_per_year_lower = conf.low,
    km_northward_shift_per_year_upper = conf.high,
    
    # Calculate change over different time periods
    km_change_per_decade = estimate * 10,
    km_change_per_half_century = estimate * 50,
    
    # Direction interpretation
    shift_direction = case_when(
      estimate > 0 ~ "Expanding northward",
      estimate < 0 ~ "Contracting southward",
      estimate == 0 ~ "No northward range change"
    ),
    
    # Statistical significance
    is_significant_05 = ifelse(p.value < 0.05, "Significant", "Not significant"),
    
    # Interpretation text
    interpretation = paste0(
      "Leading edge shifts ", round(abs(km_northward_shift_per_year), 2), " km ",
      ifelse(estimate > 0, "north", "south"), " per year",
      " (95% CI: ", round(abs(km_northward_shift_per_year_lower), 2), " to ", 
      round(abs(km_northward_shift_per_year_upper), 2), " km)"
    ),
    
    Indicator = "Leading Edge N"
  )
#END Leading edgeN----
# Trailing Edge East (5th percentile)----
range.sprE_coefficients_dfTR <- range.spr %>% 
  group_by(Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    east_trail_centered <- .$Estimate_km_E_quantile_0.05 - mean(.$Estimate_km_E_quantile_0.05, na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(east_trail_centered ~ year_centered)
    tidy(model, conf.int = TRUE)
  }) %>%
  ungroup() %>%
  filter(term == "year_centered") %>%
  mutate(
    # Direct interpretation in km per year
    km_eastward_shift_per_year = estimate,
    km_eastward_shift_per_year_lower = conf.low,
    km_eastward_shift_per_year_upper = conf.high,
    
    # Calculate change over different time periods
    km_change_per_decade = estimate * 10,
    km_change_per_half_century = estimate * 50,
    
    # Direction interpretation
    shift_direction = case_when(
      estimate > 0 ~ "Trailing edge moving east",
      estimate < 0 ~ "Trailing edge moving west",
      estimate == 0 ~ "No trailing edge change"
    ),
    
    # Statistical significance
    is_significant_05 = ifelse(p.value < 0.05, "Significant", "Not significant"),
    
    # Interpretation text
    interpretation = paste0(
      "Trailing edge shifts ", round(abs(km_eastward_shift_per_year), 2), " km ",
      ifelse(estimate > 0, "east", "west"), " per year",
      " (95% CI: ", round(abs(km_eastward_shift_per_year_lower), 2), " to ", 
      round(abs(km_eastward_shift_per_year_upper), 2), " km)"
    ),
    
    Indicator = "Trailing Edge E"
  )
#end trailedge E----
# Trailing Edge North (5th percentile)----
range.sprN_coefficients_dfTR <- range.spr %>%
  group_by(Period) %>%
  do({
    # Center variables by subtracting their means, but don't scale
    north_trail_centered <- .$Estimate_km_N_quantile_0.05 - mean(.$Estimate_km_N_quantile_0.05, na.rm = TRUE)
    year_centered <- .$Year - mean(.$Year, na.rm = TRUE)
    
    model <- lm(north_trail_centered ~ year_centered)
    tidy(model, conf.int = TRUE)
  }) %>%
  ungroup() %>%
  filter(term == "year_centered") %>%
  mutate(
    # Direct interpretation in km per year
    km_northward_shift_per_year = estimate,
    km_northward_shift_per_year_lower = conf.low,
    km_northward_shift_per_year_upper = conf.high,
    
    # Calculate change over different time periods
    km_change_per_decade = estimate * 10,
    km_change_per_half_century = estimate * 50,
    
    # Direction interpretation
    shift_direction = case_when(
      estimate > 0 ~ "Trailing edge moving north",
      estimate < 0 ~ "Trailing edge moving south",
      estimate == 0 ~ "No trailing edge change"
    ),
    
    # Statistical significance
    is_significant_05 = ifelse(p.value < 0.05, "Significant", "Not significant"),
    
    # Interpretation text
    interpretation = paste0(
      "Trailing edge shifts ", round(abs(km_northward_shift_per_year), 2), " km ",
      ifelse(estimate > 0, "north", "south"), " per year",
      " (95% CI: ", round(abs(km_northward_shift_per_year_lower), 2), " to ", 
      round(abs(km_northward_shift_per_year_upper), 2), " km)"
    ),
    
    Indicator = "Trailing Edge N"
  )
#end trail----
#Combine all coefficients into one data frame


# Function to extract core columns for plotting
# Function to extract core columns for plotting
extract_core_columns <- function(df, indicator_name) {
  df %>%
    select(any_of(c(
      "Period", "Region", "estimate", "std.error", 
      "statistic", "p.value", "conf.low", "conf.high"
    ))) %>%
    mutate(Indicator = indicator_name)
}

# Extract core columns from each dataframe with proper indicator names
combined_for_plot <- bind_rows(
  extract_core_columns(RegionAbd_coefficients_df2, "Abundance"),
  extract_core_columns(centroid_dataRegionalforFigLAT_df2, "COG North"), 
  extract_core_columns(centroid_dataRegionalforFigLON_df2, "COG East"),
  extract_core_columns(D_data_Reg_df2, "Depth-wtd Abd"),
  extract_core_columns(Overall.DFHcoefficients_df.Spring, "Distance to Border"),
  extract_core_columns(Area_ThresholdsforEAO_coefficients_df2, "Area Occupied"),
  extract_core_columns(Area_ThresholdsforEAO_coefficientsABD_df2, "AO vs Abd"),
  extract_core_columns(range.sprN_coefficients_df, "Leading Edge N"),
  extract_core_columns(range.sprE_coefficients_df, "Leading Edge E"), 
  extract_core_columns(range.sprE_coefficients_dfTR, "Trailing Edge E"),
  extract_core_columns(range.sprN_coefficients_dfTR, "Trailing Edge N")
)
View(combined_for_plot)
# Create thematic groupings
combined_for_plot <- combined_for_plot %>%
  mutate(
    Theme = case_when(
      Indicator %in% c("Abundance", "Area Occupied") ~ "Population Size",
      Indicator %in% c("AO vs Abd","Depth-wtd Abd") ~ "Pop Expansion Pattern",
      Indicator %in% c("Leading Edge N", "Leading Edge E", "Trailing Edge N", "Trailing Edge E") ~ "Range Edge",
      Indicator %in% c("COG North", "COG East" ,"Distance to Border") ~ "Geographic Position",
      TRUE ~ "Other"
    ),
    
    # Factor levels for consistent ordering
    Indicator = factor(Indicator, levels = c(
      "Abundance", "Area Occupied", "AO vs Abd",
      "COG North", "COG East", "Depth-wtd Abd", 
      "Distance to Border",
      "Leading Edge N", "Leading Edge E", 
      "Trailing Edge N", "Trailing Edge E"
    )),
    
    Theme = factor(Theme, levels = c("Population Size","Pop Expansion Pattern", "Geographic Position", 
                                     "Range Edge"))
  )

# Save the combined coefficients

write.csv(combined_for_plot, 
          here::here("2025-04-23/Output/Shift_Indicators/Combined_CentredSLopeBefAfterCoefficients.csv"), 
          row.names = FALSE)

# Optional: Create a summary for checking
summary_check <- combined_for_plot %>%
  group_by(Theme, Indicator, Period, Region) %>%
  summarise(
    n_obs = n(),
    mean_estimate = round(mean(estimate, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(Theme, Indicator, Period, Region)

print("Summary of combined data by Period and Region:")
print(summary_check)
print("Summary of combined data:")
print(summary_check)

# Plot by theme
# Plotting the Scaled Slope Indicators
pd<-position_dodge(.3)
indicator_dividers <- data.frame(xintercept = c(1.5,2.5, 5.5,7.5,8.5,10.5))  # Replace with your actual breaks

# Ensure proper facet order
combined_for_plot$Theme <- factor(combined_for_plot$Theme, levels = c("Range", "Population "))

ggplot(combined_for_plot, aes(
  y = estimate,
  x = Indicator,
  fill = Period,
  shape = Region
)) +
  # Error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.7), 
                width = 0.3, linewidth = 0.6) +
  
  # Points
  geom_point(position = position_dodge(width = 0.7), 
             size = 3, stroke = 0.3, color = "black") +
  
  # Baseline line
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
  
  # Flip axes
  coord_flip() +
  
  # Manual fill color for Period
  scale_fill_manual(values = c("steelblue", "orangered")) +
  
  # Facet by Theme (Range on top, Population below)
  facet_wrap(~Theme, ncol = 1, scales = "free_y", labeller = label_wrap_gen(width = 15)) +
  
  # Labels
  labs(
    y = "Centred Slope of Indicator",
    x = NULL,
    fill = "Period",
    shape = "Region"
  ) +
  
  # Theme and styling
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.key = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 12),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Display the plot