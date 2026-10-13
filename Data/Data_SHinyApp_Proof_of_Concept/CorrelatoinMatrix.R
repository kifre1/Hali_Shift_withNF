
#STARTING POINT FOR CORRELATION MATRIX

#I REMOVED DISTANCE TO BORDER BECASUE WE MAKE NO MENTION OF IT, THOUH I DO NOT INCORPORATE PROPORTION BECAUSE WE DO NOT HAVE STATISTICS FOR IT YET 
#Abundance: Total Estimated abundance abundance within the region of interest. Reflects changes in abundance over time.
#Centre of Gravity(COG): The coordinate location of the midpoint of regional abundance (50%) relative to starting location. Reflects shifts in the average COG over time.
#Area occupied: Total cumulative area  containing 95% of the regional population. Indicating expansion or contraction in regional occupancy through time. 
#Leading Edge: the coordination location of upper end of the population’s range (95th percentile), relative to the starting location. Indicates expansion or contraction at the edges of the range over time. 
#Trailing Edge: the coordination location of lower end of the population’s range (5th percentile), relative to the starting location, showing temporal movement 
#Mean Depth: the average depth, where 50% of the total regional abundance occur. Shows any changes in depth niche over time. 
#Other potential indicators to include: proportion of total abundance relative to other regions
#Pearson correlation coefficients: +/- 1 = perfect  correlation, 0 = no linear correlation 
  # p < 0.05 → correlation is unlikely to have occurred by chance.
  # relationships between significant correlations are plotted, NA are left blank 

#looking for correlation between shift indicator pairing trends 
#1. bring in the data for all of the SI trends
merged_Abun<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_Abundance.csv"))
merged_AO<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AreaOccupied.csv")) 
merged_COG<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_COG.csv"))
merged_RE<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_RangeEdge.csv"))  
merged_DtoB<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_DtoB.csv"))  
merged_AWD<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AWD.csv"))  

#2. merge them into a single DF (isolate the variable and year then join)....
  #here we remove region too because we are only plotting canada for example
  #However will need to include all moving forward and come up with a rule for range egde because it is not a regional variable 
library(dplyr)
library(purrr)
library(ggcorrplot)

#2.1 isolate the variable
merged_Abun2 <- merged_Abun %>%
  rename(
    Estimate_Abun = Estimate,
    SD_Abun = SD
  ) %>%
  subset(Region =="Canada") %>%
  select(-X, -Region)

merged_AO2 <- merged_AO %>%
  rename(
    Total_AO_95 = Area_Threshold,
  ) %>%
  subset(Threshold == 95) %>%
  subset(Region =="Canada") %>%
  select(Total_AO_95, Year)

merged_COG2 <- merged_COG %>%
  rename(
    Estimate_COG_long = centroid_longitude,
    Estimate_COG_lat = centroid_latitude,
  ) %>%
  subset(Region =="Canada") %>%
  select(Estimate_COG_long, Estimate_COG_lat,  Year)

merged_RE2 <- merged_RE %>%
  rename(
    Leading_Edge_E = Estimate_km_E_quantile_0.95,
    Leading_Edge_N = Estimate_km_N_quantile_0.95,
    Trailing_Edge_E = Estimate_km_E_quantile_0.05,
    Trailing_Edge_N = Estimate_km_N_quantile_0.05,
  ) %>%
  select(Leading_Edge_E, Leading_Edge_N, Trailing_Edge_E, Trailing_Edge_N, Year )

merged_DtoB2 <- merged_DtoB %>%
  rename(
    Est_DtoB = Dist_Mean,
  ) %>%
  subset(Region =="Canada") %>%
  select(Est_DtoB, Year)

merged_AWD2 <- merged_AWD %>%
  rename(
    Est_Depth = Depth_Mean,
  ) %>%
  subset(Region =="Canada") %>%
  select(Est_Depth, Year)

#2.2 Join to one df
final_df <- list(
  merged_Abun2,
  merged_AO2,
  merged_COG2,
  merged_RE2,
  merged_DtoB2,
  merged_AWD2
) %>%
  reduce(full_join, by = c("Year"))

#2.3 Turn  COG and RE into the overall displacement from the starting point
  #could use both separately to also get the directional componenet but here we are going for displacement 
final_df2 <- final_df %>%
  arrange( Year) %>%
  #group_by(Region) %>%
  mutate(
    COG_dist = sqrt(
      (Estimate_COG_long - first(Estimate_COG_long))^2 +
        (Estimate_COG_lat  - first(Estimate_COG_lat))^2
    ),
    Leading_Edge_dist = sqrt(
      (Leading_Edge_E - first(Leading_Edge_E))^2 +
        (Leading_Edge_N - first(Leading_Edge_N))^2
    ),
    Trailing_Edge_dist = sqrt(
      (Trailing_Edge_E - first(Trailing_Edge_E))^2 +
        (Trailing_Edge_N - first(Trailing_Edge_N))^2
    )
  ) %>%
  ungroup()

#2.4 Assign cleaner names for plotting
names(final_df2)
names(final_df2)[names(final_df2) == "Estimate_Abun"] <- "Abundance"
names(final_df2)[names(final_df2) == "COG_dist"] <- "Centre of Gravity"
names(final_df2)[names(final_df2) == "Total_AO_95"] <- "Area Occupied"
names(final_df2)[names(final_df2) == "Leading_Edge_dist"] <- "Leading Edge"
names(final_df2)[names(final_df2) == "Trailing_Edge_dist"] <- "Trailing Edge"
names(final_df2)[names(final_df2) == "Est_Depth"] <- "Mean Depth"


#3. calculate pairwise correlation matrix (pearson) with p-values
library(Hmisc) 
res <- rcorr(as.matrix(final_df2[, c("Mean Depth", "Trailing Edge", "Leading Edge",
                                     "Centre of Gravity", "Area Occupied", "Abundance" )]), 
             type = "pearson")
# Extract results
correlation_matrix <- res$r    #Pearson correlation coefficients: +/- 1 = perfect  correlation, 0 = no linear correlation 
Significance_matrix   <- res$P    # p < 0.05 → correlation is unlikely to have occurred by chance.

#4. Plot 
library(reshape2)
library(ggplot2)
library(ggcorrplot)
library(ggtext)

# Melt correlation matrix into long format
corr_long <- melt(correlation_matrix)
colnames(corr_long) <- c("Var1", "Var2", "r") 

# Melt p-value matrix similarly
p_long <- melt(Significance_matrix)
colnames(p_long) <- c("Var1", "Var2", "p")

# Combine correlation and p-value
corr_long$p <- p_long$p

# Keep only upper triangle
corr_long <- corr_long[as.numeric(factor(corr_long$Var1)) >= as.numeric(factor(corr_long$Var2)), ]

# Add significance stars
corr_long$stars <- ifelse(corr_long$p < 0.001, "***",
                   ifelse(corr_long$p < 0.01, "**",
                   ifelse(corr_long$p < 0.05, "*",
                          "")))

# For plotting: determine fill color
corr_long$fill_color <- ifelse(corr_long$Var1 == corr_long$Var2, NA,    # diagonal: NA for white
                               ifelse(corr_long$p < 0.05, corr_long$r, NA))     # significant vs non-significant
# Create a new column for labels (keep numbers for all, we’ll strike through non-significant)
corr_long$plot_label <- ifelse(corr_long$Var1 == corr_long$Var2, "",          # diagonal: no label
                               paste0(round(corr_long$r,2), corr_long$stars))  # everything else
corr_long$plot_label <- ifelse(corr_long$Var1 == corr_long$Var2, "",  # diagonal
                               ifelse(is.na(corr_long$fill_color), "NA",     # non-significant (grey)
                                      paste0(round(corr_long$r,2), corr_long$stars)))  # significant
# Plot
correlation_matrix<- ggplot(corr_long, aes(x = Var2, y = Var1, fill = fill_color)) +
  # Diagonal white
  geom_tile(data = subset(corr_long, Var1 == Var2), fill = "white", color = "black") +
  # Non-significant cells grey
  geom_tile(data = subset(corr_long, Var1 != Var2 & is.na(fill_color)), fill = "grey80", color = "black") +
  # Significant correlations colored
  geom_tile(data = subset(corr_long, !is.na(fill_color) & Var1 != Var2), color = "black") +
  # Add labels (numbers + stars)
  geom_text(aes(label = plot_label), size = 3.5, color = "black") +
  # Add strikethrough for non-significant cells
#  geom_segment(data = subset(corr_long, !is.na(fill_color) == FALSE & Var1 != Var2),
#               aes(x = as.numeric(Var2) - 0.25, xend = as.numeric(Var2) + 0.25,
#                   y = as.numeric(Var1), yend = as.numeric(Var1)),
#               color = "black", size = 0.5) +
  # Color scale
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1,1), name = "Correlation") +
  # Top x-axis
  scale_x_discrete(position = "top") +
  theme_bw(base_size = 12, base_family = "serif") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9.5, family = "serif", angle = 45, hjust = 0,  colour= "black"),
        axis.text.y = element_text(size = 9.5, family = "serif", colour= "black"),
        legend.text = element_text(size = 9, family = "serif"),
        legend.title = element_text(size = 9.5,, family = "serif", colour= "black"),  
        panel.grid = element_blank(),
        panel.border = element_rect(color = NA, fill = NA))
correlation_matrix
#5. Save the plot
ggsave(
  filename = "Correlation_Matrix.png",
  plot = correlation_matrix,          
  path =  ("C:/Users/fergusonk/Documents/DSAF/Perspectives Paper"),
  width = 6,
  height = 5,
  dpi = 500
)



####old version of plotting without stars####
#4. Plot 
#Define stars based on p-values:
sig_levels <- ifelse(Significance_matrix < 0.001, "***",
                     ifelse(Significance_matrix < 0.01, "**",
                            ifelse(Significance_matrix < 0.05, "*", "")))
#combine with the correlation coefficients for plottingorrela
lab_matrix <- matrix(paste0(round(correlation_matrix, 2), sig_levels),
                     nrow = nrow(correlation_matrix))

library(ggcorrplot)
library(ggplot2)
ggcorrplot(correlation_matrix,
           method = "square",       # square tiles
           type = "upper",          # only lower triangle
           lab = TRUE,        # TRUE or custom 
           lab_size = 4,            # size of numbers
           lab_col = "black",       # number color
           tl.cex = 10,             # label size
           colors = c("blue", "white", "red"),  # color scale
           outline.color = "black",
           p.mat = Significance_matrix,   # provide p-values
           sig.level = 0.05,        # correlations above this level are faded
           insig = "blank")+     # blanks out the insignificant ones 
  #forcing a rename on the legend 
  scale_fill_gradient2(name = "Correlation",  # <-- sets legend title
                       low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  scale_x_discrete(position = "top") + 
  theme_bw(base_size = 12, base_family = "serif") +  # set serif font and size
  theme(
    axis.title.x = element_blank(),      
    axis.title.y = element_blank(),      
    axis.text.x = element_text(size = 10, family = "serif", angle = 90, hjust = 0, colour= "black"), 
    axis.text.y = element_text(size = 10, family = "serif", colour= "black"),  # y-axis labels
    legend.text = element_text(size = 12, family = "serif"),          # legend font
    panel.grid = element_blank(),         # remove grid
    panel.border = element_rect(color = NA, fill = NA), 
  )







####old, fishers method####
#populate matrix using fisher's method
merged_coefficients_df<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/Scaled_Slopes_all_indicators.csv"))  

save_data<-merged_coefficients_df
merged_coefficients_df<-save_data
unique(merged_coefficients_df$Indicator)
#all indicators needed 
selected_indicators <- c("Abundance",  "Area Occupied", "Leading_Edge_East", "Leading_Edge_North","Trailing_Edge_East", "Trailing_Edge_North",
                         "COG_East", "COG_North",   "Distance to Border" , "Abundance Weighted Depth")

merged_coefficients_df<-subset(merged_coefficients_df, merged_coefficients_df$Indicator %in% selected_indicators)
unique(merged_coefficients_df$Indicator)

#make df wide
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
#take only canadian data
Canada.p<-subset(merged_coefficients_df, merged_coefficients_df$Region =="Canada")

Canada.p_wide <- Canada.p %>%
  select(Region, Indicator, p.value) %>%    
  pivot_wider(
    names_from = Indicator,                   
    values_from = p.value                    
  )
#range edge values have different names because candaa is the leading and usa is the trailing
names(Canada.p_wide)
names(Canada.p_wide)[c(4,5)] <- c("Range_Edge_East", "Range_Edge_North")


#Step 1: Fisher’s Method function
#Standard version: small combined p-values mean a stronger joint relationship between indicator pairings.
combine_fisher <- function(p1, p2) {
  stat <- -2 * (log(p1) + log(p2))
  df <- 4  # 2 * number of p-values combined
  combined_p <- 1 - pchisq(stat, df)
  return(combined_p)
}

#Step 2: Function to combine indicators within a single-region dataframe
calculate_fisher_pairs <- function(df) {
  # Combine east/north indicators into single mean-based metrics
  df <- df %>%
    mutate(
      COG = rowMeans(select(., COG_East, COG_North), na.rm = TRUE),
      Range_Edge = rowMeans(select(., Range_Edge_East, Range_Edge_North), na.rm = TRUE)
    ) %>%
    select(-COG_East, -COG_North, -Range_Edge_East, -Range_Edge_North)
  # Create all unique indicator pairs
  pairs <- combn(names(df)[!names(df) %in% "Region"], 2, simplify = FALSE)
  # Compute combined p-values for each pair
  fisher_results <- map_dfr(pairs, function(x) {
    p1 <- df[[x[1]]]
    p2 <- df[[x[2]]]
    combined <- combine_fisher(p1, p2)
    data.frame(Indicator1 = x[1],
               Indicator2 = x[2],
               Combined_P = combined)
  }) %>%
    arrange(Combined_P)
  
  return(fisher_results)
}

canada_fisher <- calculate_fisher_pairs(Canada.p_wide)
head(canada_fisher)

#3. Add some new fields: significance and rankings
#classify the combined p.values
canada_fisher <- canada_fisher %>%
  mutate(
    p.significant = case_when(
      Combined_P < 0.001 ~ "Very strong",
      Combined_P < 0.01  ~ "Strong",
      Combined_P < 0.05  ~ "Moderate",
      TRUE ~ "Not significant"
    )
  )

# make sure p.significant is an ordered factor
canada_fisher$p.significant <- factor(
  canada_fisher$p.significant,
  levels = c("Not significant", "Marginal", "Strong", "Very strong")
)
#rank the combined p.values from smalles to largest
canada_fisher <- canada_fisher %>%
  mutate(Rank = rank(Combined_P, ties.method = "first"))
str(canada_fisher)

canada_fisher$Combined_P<-as.numeric(canada_fisher$Combined_P)

#Step 4: Plotting/ Interpretation
library(ggplot2)

# rename the indicators so it plots easier
unique(canada_fisher$Indicator1)
unique(canada_fisher$Indicator2)

canada_fisher <- canada_fisher %>%
  mutate(
    Indicator1 = recode(Indicator1,
                        "COG" = "COG",                     
                        "Area Occupied" = "AO" ,          
                        "Range_Edge"  =  "RE",          
                        "Abundance" = "Abun"),
    Indicator2 = recode(Indicator2,
                        "COG" = "COG",                     
                        "Area Occupied" = "AO" ,          
                        "Range_Edge"  =  "RE",          
                        "Abundance" = "Abun"))

#Plot Canada Matrix
#factor levels for even plotting
all_levels <- union(
  unique(na.omit(canada_fisher$Indicator1)),
  unique(na.omit(canada_fisher$Indicator2))
)
canada_fisher$Indicator1 <- factor(canada_fisher$Indicator1, levels = all_levels)
canada_fisher$Indicator2 <- factor(canada_fisher$Indicator2, levels = all_levels)

ggplot(canada_fisher, aes(x = Indicator2, y = Indicator1, fill = Combined_P)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2e", Combined_P)), size = 4)+
  scale_x_discrete(drop = FALSE, limits = rev, position = "top") +   # keep all levels
  scale_y_discrete(drop = FALSE, limits = rev) + 
  scale_fill_gradient(
    low = "dodgerblue4",
    high = "slategrey",
    guide = guide_colorbar(reverse = TRUE),
    name = "High"
  ) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle =0, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Significance",
    title = "Ranked Indicator Pair Relationships"
  )


