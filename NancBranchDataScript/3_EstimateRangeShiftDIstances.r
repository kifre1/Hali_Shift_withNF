rangeedge<-read.csv(here::here("R/DataforFinalFigs/Edge_df_NSreshp.csv"))
range.spr<-rangeedge[rangeedge$Season=="Spring",]
range.spr$Period<-NULL
range.spr$Period[range.spr$Year<2006]<-"1990-2005"
range.spr$Period[range.spr$Year>2005]<-"2006-2023"
names(range.spr);
summary(range.spr)


# Calculate the distance between the start and end points
# First, let's check your data ranges
print("Data ranges:")
print(paste("E range:", min(range.spr$Estimate_km_E_quantile_0.05, na.rm=TRUE), "to", max(range.spr$Estimate_km_E_quantile_0.95, na.rm=TRUE)))
print(paste("N range:", min(range.spr$Estimate_km_N_quantile_0.05, na.rm=TRUE), "to", max(range.spr$Estimate_km_N_quantile_0.95, na.rm=TRUE)))

# Method 1: Calculate shifts for leading and trailing edges separately using Euclidean distance
shiftleadtrail <- range.spr %>%
  # Group by period and calculate mean positions for each edge
  group_by(Period) %>%
  summarize(
    # Trailing edge (0.05 quantile - southern/retreating edge)
    trailing_E = mean(Estimate_km_E_quantile_0.05, na.rm = TRUE),
    trailing_N = mean(Estimate_km_N_quantile_0.05, na.rm = TRUE),
    
    # Leading edge (0.95 quantile - northern/advancing edge)  
    leading_E = mean(Estimate_km_E_quantile_0.95, na.rm = TRUE),
    leading_N = mean(Estimate_km_N_quantile_0.95, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  
  # Check if we have both periods
  {
    if(nrow(.) != 2) {
      stop("Expected exactly 2 periods, got: ", nrow(.))
    }
    .
  } %>%
  
  # Calculate shifts between periods using Euclidean distance
  summarize(
    # Trailing edge shift - using Euclidean distance
    trailing_shift_km = sqrt(
      (trailing_E[Period == "2006-2023"] - trailing_E[Period == "1990-2005"])^2 +
        (trailing_N[Period == "2006-2023"] - trailing_N[Period == "1990-2005"])^2
    ),
    
    # Leading edge shift - using Euclidean distance
    leading_shift_km = sqrt(
      (leading_E[Period == "2006-2023"] - leading_E[Period == "1990-2005"])^2 +
        (leading_N[Period == "2006-2023"] - leading_N[Period == "1990-2005"])^2
    )
  )

# Method 2: More explicit approach with pivot - using Euclidean distance
shiftleadtrail_alt <- range.spr %>%
  # Group by period and calculate mean positions for each edge
  group_by(Period) %>%
  summarize(
    trailing_E = mean(Estimate_km_E_quantile_0.05, na.rm = TRUE),
    trailing_N = mean(Estimate_km_N_quantile_0.05, na.rm = TRUE),
    leading_E = mean(Estimate_km_E_quantile_0.95, na.rm = TRUE),
    leading_N = mean(Estimate_km_N_quantile_0.95, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # Pivot to get period comparisons
  pivot_wider(names_from = Period, 
              values_from = c(leading_E, leading_N, trailing_E, trailing_N),
              names_sep = "_") %>%
  
  # Calculate shifts using Euclidean distance
  mutate(
    trailing_shift_km = sqrt(
      (`trailing_E_2006-2023` - `trailing_E_1990-2005`)^2 +
        (`trailing_N_2006-2023` - `trailing_N_1990-2005`)^2
    ),
    
    leading_shift_km = sqrt(
      (`leading_E_2006-2023` - `leading_E_1990-2005`)^2 +
        (`leading_N_2006-2023` - `leading_N_1990-2005`)^2
    )
  ) %>%
  
  # Keep only the shift calculations
  select(trailing_shift_km, leading_shift_km)

# Method 3: Detailed approach with directional information - using Euclidean distance
shiftleadtrail_detailed <- range.spr %>%
  group_by(Period) %>%
  summarize(
    trailing_E = mean(Estimate_km_E_quantile_0.05, na.rm = TRUE),
    trailing_N = mean(Estimate_km_N_quantile_0.05, na.rm = TRUE),
    leading_E = mean(Estimate_km_E_quantile_0.95, na.rm = TRUE),
    leading_N = mean(Estimate_km_N_quantile_0.95, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  pivot_wider(names_from = Period, 
              values_from = c(trailing_E, trailing_N, leading_E, leading_N),
              names_sep = "_") %>%
  
  mutate(
    # Distance shifts using Euclidean distance
    trailing_shift_km = sqrt(
      (`trailing_E_2006-2023` - `trailing_E_1990-2005`)^2 +
        (`trailing_N_2006-2023` - `trailing_N_1990-2005`)^2
    ),
    
    leading_shift_km = sqrt(
      (`leading_E_2006-2023` - `leading_E_1990-2005`)^2 +
        (`leading_N_2006-2023` - `leading_N_1990-2005`)^2
    ),
    
    # Directional shifts (positive = north/east, negative = south/west)
    trailing_shift_E_km = (`trailing_E_2006-2023` - `trailing_E_1990-2005`),
    trailing_shift_N_km = (`trailing_N_2006-2023` - `trailing_N_1990-2005`),
    leading_shift_E_km = (`leading_E_2006-2023` - `leading_E_1990-2005`),
    leading_shift_N_km = (`leading_N_2006-2023` - `leading_N_1990-2005`)
  )

# Print results
print("Leading and Trailing Edge Shifts:")
print(shiftleadtrail_detailed)
View(shiftleadtrail_detailed) 


