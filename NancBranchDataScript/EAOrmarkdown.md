---
title: "Area vs Abundance Analysis"
author: "Nancy"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: true
    theme: cosmo
    highlight: tango
    code_folding: show
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, 
                     fig.width = 10, fig.height = 6)
```
## Introduction and data prep

This analysis explores the relationship between abundance and the area containing different percentages of abundance across regions and years. We'll examine how the spatial distribution of abundance varies over time and across different regions.
The dataset used in this analysis contains abundance estimates for different regions and years, along with the area of each region. The goal is to calculate the area required to contain a certain percentage of total abundance (e.g., 50%, 75%, 90%, 95%) and visualize the results.
```{r load-libraries}
library(ggplot2)
library(tidyverse)
abdest<- read.csv("~/My_Program_Files/R/Hali_Shift_withNF/2025-04-23/Output/IndexAbundance/AbundanceEstimates_GridCentriods_Reg.csv")
dim(abdest)
summary(abdest)
View(abdest)
unique(abdest$Area_km2)
abdest$Region<-factor(abdest$Stratum)
#Note there are 3 seasons that all have different Area_km2 values so x table 
abdest %>%
  group_by(Stratum,Season, Area_km2) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = Area_km2, values_from = n)
#Select for Spring
abdest.spr<- abdest %>%
  filter(Season == "Spring")

abdest.spr<-abdest.spr %>%
  group_by(Region,Year) %>%
  mutate(AnnualAbundance = sum(Abundance))
dim(abdest.spr);names(abdest.spr)

```

```{r summarize-data}
# Display dataset dimensions and column names
dim(abdest.spr)
names(abdest.spr)

# Calculate annual abundance by region and year
abdest.spr <- abdest.spr %>%
  group_by(Region, Year) %>%
  mutate(AnnualAbundance = sum(Abundance))
```

## Calculating Area Thresholds

Next, we'll calculate the area containing different percentages of abundance (50%, 75%, 90%, 95%) for each region and year:

```{r calculate-areas}
# Function to calculate area for different abundance thresholds
calculate_areas <- function(data, thresholds = c(50, 75, 90, 95)) {
  result_list <- list()
  
  for (threshold in thresholds) {
    threshold_result <- data %>%
      group_by(Region, Year) %>%
      mutate(Total_Abundance = sum(Abundance)) %>%
      arrange(Region, Year, desc(Abundance/Area_km2)) %>%
      mutate(
        Cumulative_Abundance = cumsum(Abundance),
        Percent_Abundance = Cumulative_Abundance / Total_Abundance * 100,
        Cumulative_Area = cumsum(Area_km2)
      ) %>%
      filter(Percent_Abundance <= threshold) %>%
      summarize(
        Threshold = threshold,
        Area_Threshold = sum(Area_km2),
        Total_Area = sum(Area_km2, na.rm = TRUE),
        Percent_Area_Used = Area_Threshold / sum(Area_km2, na.rm = TRUE) * 100,
        Total_Abundance = first(Total_Abundance),
        n_cells = n()
      )
    
    result_list[[as.character(threshold)]] <- threshold_result
  }
  
  # Combine all results
  bind_rows(result_list)
}

# Apply the function to calculate areas for different thresholds
area_thresholds <- calculate_areas(abdest.spr)

# Calculate area efficiency (area per unit of abundance)
area_thresholds <- area_thresholds %>%
  mutate(Area_Efficiency = Area_Threshold / Total_Abundance)

# Preview the results
head(area_thresholds)
```

## Visualizing the Results

### 1. Area Containing 90% of Abundance by Region and Year

```{r plot-area-by-region-year, fig.height=7}
ggplot(area_thresholds %>% filter(Threshold == 90), 
       aes(x = as.factor(Year), y = Area_Threshold, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Area Containing 90% of Abundance", 
       x = "Year", 
       y = "Area (km²)",
       fill = "Region") +
  theme_minimal() +
  scale_fill_viridis_d() # Color-blind friendly palette
```

### 2. Trends Over Time

```{r plot-trends, fig.height=7}
ggplot(area_thresholds %>% filter(Threshold == 90), 
       aes(x = Year, y = Area_Threshold, color = Region, group = Region)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Trend in Area Containing 90% of Abundance", 
       x = "Year", 
       y = "Area (km²)",
       color = "Region") +
  theme_minimal() +
  scale_color_viridis_d()
```

### 3. Multiple Thresholds Comparison

```{r plot-thresholds, fig.height=8}
ggplot(area_thresholds, 
       aes(x = as.factor(Threshold), y = Area_Threshold, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Year, scales = "free_y") +
  labs(title = "Area Containing Different Percentages of Abundance", 
       x = "Abundance Threshold (%)", 
       y = "Area (km²)",
       fill = "Region") +
  theme_minimal() +
  scale_fill_viridis_d()
```

### 4. Percentage of Total Area Used

```{r plot-percent-area, fig.height=7}
ggplot(area_thresholds %>% filter(Threshold == 90), 
       aes(x = as.factor(Year), y = Percent_Area_Used, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Total Area Containing 90% of Abundance", 
       x = "Year", 
       y = "% of Total Area",
       fill = "Region") +
  theme_minimal() +
  scale_fill_viridis_d() +
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = 92, label = "90% line", color = "red")
```

### 5. All Thresholds Over Time

```{r plot-all-thresholds, fig.height=8}
ggplot(area_thresholds, 
       aes(x = Year, y = Area_Threshold, color = as.factor(Threshold), group = Threshold)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~Region, scales = "free_y") +
  labs(title = "Area Required for Different Abundance Thresholds Over Time", 
       x = "Year", 
       y = "Area (km²)",
       color = "Abundance\nThreshold (%)") +
  theme_minimal() +
  scale_color_viridis_d()
```

## Area vs Annual Abundance Analysis

### 1. Scatter Plot by Region

```{r scatter-by-region, fig.height=7}
ggplot(area_thresholds %>% filter(Threshold == 90),
       aes(x = Total_Abundance, y = Area_Threshold, color = Region)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(aes(label = Year), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Area Containing 90% of Abundance vs Total Annual Abundance",
       x = "Total Annual Abundance",
       y = "Area (km²)",
       color = "Region") +
  theme_minimal() +
  scale_color_viridis_d()
```

### 2. Trajectory Over Time

```{r trajectory-plot, fig.height=7}
ggplot(area_thresholds %>% filter(Threshold == 90),
       aes(x = Total_Abundance, y = Area_Threshold, color = Region)) +
  geom_path(aes(group = Region), arrow = arrow(length = unit(0.25, "cm"), 
                                              ends = "last", type = "closed"),
            size = 1) +
  geom_point(aes(size = Year), alpha = 0.7) +
  labs(title = "Trajectory of Area vs Annual Abundance Over Time",
       subtitle = "Arrows indicate direction of time progression",
       x = "Total Annual Abundance",
       y = "Area Containing 90% of Abundance (km²)",
       color = "Region",
       size = "Year") +
  theme_minimal() +
  scale_color_viridis_d()
```

### 3. Faceted by Region

```{r facet-by-region, fig.height=8}
ggplot(area_thresholds %>% filter(Threshold == 90),
       aes(x = Total_Abundance, y = Area_Threshold, color = as.factor(Year))) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(aes(group = Region)) +
  facet_wrap(~Region, scales = "free") +
  labs(title = "Area vs Annual Abundance by Region",
       x = "Total Annual Abundance",
       y = "Area (km²)",
       color = "Year") +
  theme_minimal() +
  scale_color_viridis_d()
```

### 4. Area Efficiency Over Time

```{r efficiency-plot, fig.height=7}
ggplot(area_thresholds %>% filter(Threshold == 90),
       aes(x = Year, y = Area_Efficiency, color = Region, group = Region)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Area Efficiency Over Time", 
       subtitle = "Area (km²) needed per unit of abundance",
       x = "Year", 
       y = "Area per Abundance Unit (km²)",
       color = "Region") +
  theme_minimal() +
  scale_color_viridis_d()
```

### 5. Multi-dimensional View

```{r bubble-plot, fig.height=7}
ggplot(area_thresholds %>% filter(Threshold == 90),
       aes(x = Total_Abundance, y = Area_Threshold, size = Area_Efficiency, color = Region)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = Year), vjust = -1, size = 3) +
  labs(title = "Multi-dimensional View: Area, Abundance and Efficiency",
       x = "Total Annual Abundance",
       y = "Area (km²)",
       size = "Area per\nAbundance Unit",
       color = "Region") +
  theme_minimal() +
  scale_color_viridis_d() +
  scale_size_continuous(range = c(3, 10))
```

## Abundance Concentration Efficiency

```{r concentration-plot, fig.height=8}
ggplot(area_thresholds, 
       aes(x = Percent_Area_Used, y = Threshold, color = Region)) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(Region, Year))) +
  facet_wrap(~Year) +
  labs(title = "Abundance Concentration Efficiency", 
       x = "% of Total Area Used", 
       y = "% of Total Abundance",
       color = "Region") +
  theme_minimal() +
  scale_color_viridis_d() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  annotate("text", x = 75, y = 25, label = "Equal distribution line", 
           color = "gray", angle = 45)
```

## Conclusion

This analysis provides insights into how abundance is distributed spatially across regions and how this distribution changes over time. The area required to contain 90% of abundance varies by region and year, potentially indicating changes in population concentration or dispersion over time.

The area efficiency metric (area per unit of abundance) helps identify regions where abundance is more concentrated or dispersed. Lower values indicate greater concentration of abundance in smaller areas.
