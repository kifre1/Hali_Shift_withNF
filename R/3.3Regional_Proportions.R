#Preparing Estimated Abundance data and plotting the time series to compare the abundance trends of National and Core Area stratum. 
#Using the generated indexed abundance data from get_vast_index_timeseries() becasue the standard errors are not available at scale of grid location 

#Paste workflow from readme 

#Percent Abundance per region & CA
#Proportion of Estimated Abundance per region & CA
#Proportional density per CA (Relative Density)
#TO DO: incorporate "other" it CA data, representing areas unallocated to a CA



# Load packages----
library(VAST)
library(stringr)
library(patchwork)
library(tidyr)
library(dplyr)
library(ggplot2)
library(raster)
library(gstat)
library(rasterize)
library(here)
library(splines)
library(tidyverse)
library(lubridate)
library(sf)
library(googledrive)
library(ggforce)
library(patchwork)
library(DHARMa)
library(forecast)
library(sp)
library(RColorBrewer)
library(tibble)

devtools::source_url("https://raw.github.com/aallyn/TargetsSDM/main/R/vast_functions.R")
source(here::here("R/VAST_functions/kf_vast_function_edits.R"))
# Andrew's Functions, I cloned his directory here: C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/
source(here::here("R/VAST_functions/dfo_functions.R"))
source(here::here("R/VAST_functions/nmfs_functions.R"))
source(here::here("R/VAST_functions/combo_functions.R"))
source(here::here("R/VAST_functions/enhance_r_funcs.R"))
#source(here::here("R/VAST_functions/vast_functions.R"))
source(here::here("R/VAST_functions/covariate_functions.R"))
source(here::here("R/VAST_functions/project.fit_model_aja.R"))
source(here::here("R/VAST_functions/DHARMa utilities.R"))
source(here::here("R/VAST_functions/SDM_PredValidation_Functions.R"))#PresenceAbsence library is gone so this doesn't work anymore- 
source(here::here("R/VAST_functions/vast_function_edits.R"))
source(here::here("R/VAST_functions/vast_plotting_functions.R"))


#STEP 1: get annual regional proportionsr ----
#1.1 prep data----
fit<- readRDS( here::here("2025-04-23/Halibut_BC/SpST_mod_fit.rds")) 

all_times<-unique(fit$data_frame$t_i)
abundance_ind<- get_vast_index_timeseries(vast_fit = fit, nice_category_names = "Halibut", index_scale = c("raw"), all_times = all_times, out_dir = here::here("2025-04-23/Output/IndexAbundanceFun"))

summary(abundance_ind)
#lets add season and Year to these data and subset the core areas from the region so that we can plot them properly 
years_Spring <- as.numeric(seq(0, 101, by = 3))
years_Summer <- as.numeric(seq(1, 101, by = 3))
years_Fall <- as.numeric(seq(2, 101, by = 3))
abundance_ind$Season <- ifelse(abundance_ind$Year %in% years_Fall, "Fall",
                               ifelse(abundance_ind$Year %in% years_Spring, "Spring",
                                      ifelse(abundance_ind$Year %in% years_Summer, "Summer", NA)))

# Group the data into threes and reassign values starting at 1985
increment_value <- 3
#group Time intervals by year
abundance_ind <- abundance_ind %>%
  mutate(YearGroup = (Time %/% increment_value) + 1)
head(abundance_ind)
#Assign actual years to these groups
abundance_ind <- abundance_ind %>%
  mutate(Year = (YearGroup + 1989))
head(abundance_ind)
summary(abundance_ind)
#time steps to not represent years because they include seasons (3 per year)
#1.2 add a column for square area----
#get the data from the Prediction data df (already added these data there)
Reg_data<-read.csv(here::here("2025-04-23/Output/Abun_Est_GridData/AbundanceEstimates_GridCentriods_Reg.csv"))
CA_data<-read.csv(here::here("2025-04-23/Output/Abun_Est_GridData/AbundanceEstimates_GridCentriods_CA.csv"))
# Select the unique combinations of CA_Area_km2 and Core_Area
CA_square_area <- unique(CA_data[, c("Area_km2", "Stratum")])
R_square_area <- unique(Reg_data[, c("Area_km2", "Stratum")])
#Rename the columns to match the abundance data
CA_square_area$Km2 <- CA_square_area$Area_km2
CA_square_area$Index_Region <- CA_square_area$Stratum
R_square_area$Km2 <- R_square_area$Area_km2
R_square_area$Index_Region <- R_square_area$Stratum  
#Combine the two  data frames
combined_areas <- rbind(
  CA_square_area[, c("Index_Region", "Km2")],
  R_square_area[, c("Index_Region", "Km2")]
)

# Merge with the abundance df
abundance_ind_proportions <- merge(
  abundance_ind,
  combined_areas,
  by = "Index_Region",
  all.x = TRUE
)


# select for region and CA, and Group by Year and Season to calculate total estimates and proportions
abundance_ind_proportions_Region<-subset(
  abundance_ind_proportions, 
  abundance_ind_proportions$Index_Region=="USA"|abundance_ind_proportions$Index_Region=="Canada")
abundance_ind_proportions_CA <- subset(
  abundance_ind_proportions, 
  !(Index_Region == "USA" | Index_Region == "Canada"
    )
)
unique(abundance_ind_proportions_Region$Index_Region)
unique(abundance_ind_proportions_CA$Index_Region)

str(abundance_ind_proportions_Region) 
#1.3 calculate annual regional proportional abundance and density for each stratum grouping (National, Core Area) ----
#proportional abundance
abundance_ind_proportions_Region <- abundance_ind_proportions_Region %>%
  group_by(Year, Season) %>%                               
  mutate(
    Total_Estimate = sum(Index_Estimate),#total catch for the year
    Proportion =  round(Index_Estimate / Total_Estimate, 3),  # Proportion per region rounded to 3 decimals
    Proportion_SD = round(Index_SD / Total_Estimate, 3) #the SD also needs to be scaled by the same factor in order to remain consistent
  ) %>%
  ungroup()   

#proportional density : same as above but standardized by square area
unique(abundance_ind_proportions_Region$Km2)
TotalArea<- 705009.0 + 147581.2 #total area
abundance_ind_proportions_Region <- abundance_ind_proportions_Region %>%
  group_by(Year, Season) %>%                               
  mutate(
    # Standardize Index_Estimate and Total_Estimate by Km2
    Regional_density_Estimate = Index_Estimate / Km2, # Total regional standardized catch for the year/season
    Total_density_Estimate = sum(Regional_density_Estimate),  # Total standardized catch for the year/season
    Regional_density_SD = Index_SD/ Km2, #the SD also needs to be scaled by the same factor in order to remain consistent
    # Calculate proportions using standardized values
    ProportionalDensity = round(Regional_density_Estimate / Total_density_Estimate, 3),
    ProportionalDensity_SD = round(Regional_density_SD /  Total_density_Estimate, 3)
  ) %>%
  ungroup()

#as a percentage
abundance_ind_proportions_Region$Proportion <- abundance_ind_proportions_Region$Proportion * 100
abundance_ind_proportions_Region$Proportion_SD <- abundance_ind_proportions_Region$Proportion_SD * 100
abundance_ind_proportions_Region$ProportionalDensity <- abundance_ind_proportions_Region$ProportionalDensity * 100
abundance_ind_proportions_Region$ProportionalDensity_SD <- abundance_ind_proportions_Region$ProportionalDensity_SD * 100
#just spring
abundance_ind_proportions_Region_Spring<-abundance_ind_proportions_Region%>%filter(Season=="Spring")

#Per CA, calculate regional proportional abundance and density----
abundance_ind_proportions_CA <- abundance_ind_proportions_CA %>%
  group_by(Year, Season) %>%                               
  mutate(
    Total_Estimate = Index_Estimate[Index_Region == "All"], #total catch for the year, is not the sum of Index_Estimate becasue the CA's do not cover ALL of the area, so the all data have been retained in this df
    Proportion =  round(Index_Estimate / Total_Estimate, 3) , # Proportion of All per CA rounded to 3 decimals...will not add up to 100%
    Proportion_SD = round(Index_SD / Total_Estimate, 3)#the SD also needs to be scaled by the same factor in order to remain consistent
  ) %>%
  ungroup()    



abundance_ind_proportions_CA <- abundance_ind_proportions_CA %>%
  group_by(Year, Season) %>%                               
  mutate(
    # Standardize Index_Estimate and Total_Estimate by Km2
    Total_density_Estimate = Index_Estimate[Index_Region == "All"]/TotalArea,#standardize by dividing the "all' estimates by the total area 
    All_density_SD = Index_SD/TotalArea,#Check
    CA_density_Estimate = Index_Estimate/ Km2,
    CA_density_SD = Index_SD/Km2,
    ProportionalDensity =  round(CA_density_Estimate / Total_density_Estimate, 3) , # Proportion rounded to 3 decimals
    ProportionalDensity_SD = round(CA_density_SD / Total_density_Estimate, 3) 

  ) %>%
  ungroup()
abundance_ind_proportions_CA$Proportion <- abundance_ind_proportions_CA$Proportion * 100
abundance_ind_proportions_CA$Proportion_SD <- abundance_ind_proportions_CA$Proportion_SD * 100
abundance_ind_proportions_CA$ProportionalDensity <- abundance_ind_proportions_CA$ProportionalDensity * 100
abundance_ind_proportions_CA$ProportionalDensity_SD <- abundance_ind_proportions_CA$ProportionalDensity_SD * 100
str(abundance_ind_proportions_CA)
str(abundance_ind_proportions_Region)
abundance_ind_proportions_CA<-subset(abundance_ind_proportions_CA, abundance_ind_proportions_CA$Index_Region!="All")#not needed anymore

#save----
write.csv(abundance_ind_proportions_CA, (here::here("2025-04-23/Output/Proportions/proportions_and_density_CA.csv")))
write.csv(abundance_ind_proportions_Region, (here::here("2025-04-23/Output/Proportions/proportions_and_density_Regional.csv")))


#STEP 2: Compare these data between before and during warming timeframes ----
#incorporate timeframes----
CA_Proportion<-read.csv(here::here("2025-04-23/Output/Proportions/proportions_and_density_CA.csv"))
Regional_Proportion<- read.csv(here::here("2025-04-23/Output/Proportions/proportions_and_density_Regional.csv"))


# add the timeframe to the data
CA_Proportion <- CA_Proportion %>%
  mutate(
    timeframe = case_when(
      Year %in% 1990:2005 ~ "Before",
      Year %in% 2006:2023 ~ "After",
      TRUE ~ NA_character_  # Handles any years outside these ranges
    )
  )
Regional_Proportion <- Regional_Proportion %>%
  mutate(
    timeframe = case_when(
      Year %in% 1990:2005 ~ "Before",
      Year %in% 2006:2023 ~ "After",
      TRUE ~ NA_character_  # Handles any years outside these ranges
    )
  )


#Regional: Calculate the mean proportion for each Region and timeframe----
TF_Avg_R <- Regional_Proportion %>%
  group_by(Index_Region, timeframe, Season) %>%
  summarize(
    Mean_Proportion = mean(Proportion, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = timeframe, values_from = Mean_Proportion) %>%
  ungroup()
TF_Avg_R$PercentChange = ((TF_Avg_R$After - TF_Avg_R$Before) / TF_Avg_R$Before) * 100
TF_Avg_R$Difference = TF_Avg_R$After - TF_Avg_R$Before
TF_Avg_R <- TF_Avg_R[, c("Index_Region", "Season", "Before", "After", "Difference", "PercentChange")]
TF_Avg_R <- TF_Avg_R %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
#add the SD
SD_R <- Regional_Proportion %>%
  group_by(Index_Region, timeframe, Season) %>%
  summarize(
    Mean_SD = mean(Proportion_SD, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = timeframe, values_from = Mean_SD) %>%
  ungroup()
SD_R$PercentChangeSD = ((SD_R$After - SD_R$Before) / SD_R$Before) * 100
SD_R$DifferenceSD = SD_R$After - SD_R$Before
SD_R <- SD_R[, c("Index_Region", "Season", "Before", "After", "DifferenceSD", "PercentChangeSD")]
names(SD_R)[3] <- "BeforeSD"
names(SD_R)[4] <- "AfterSD"
SD_R <- SD_R %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

Regional_Proportions_TF <- TF_Avg_R %>% inner_join(SD_R, by = c("Index_Region", "Season"))
head(Regional_Proportions_TF)

#Regional repeat for proportional density ----
names(CA_Proportion)

PD_Avg_REG <- Regional_Proportion %>%
  group_by(Index_Region, timeframe, Season) %>%
  summarize(
    Mean_ProportionalDensity = mean(ProportionalDensity, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = timeframe, values_from = Mean_ProportionalDensity) %>%
  ungroup()
PD_Avg_REG$PD_PercentChange = ((PD_Avg_REG$After - PD_Avg_REG$Before) / PD_Avg_REG$Before) * 100
PD_Avg_REG$PD_Difference = PD_Avg_REG$After - PD_Avg_REG$Before
PD_Avg_REG <- PD_Avg_REG[, c("Index_Region", "Season", "Before", "After", "PD_Difference", "PD_PercentChange")]
names(PD_Avg_REG)[3] <- "PD_Before"
names(PD_Avg_REG)[4] <- "PD_After"
PD_Avg_REG <- PD_Avg_REG %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

#add the SD
ST_SD_REG <- Regional_Proportion %>%
  group_by(Index_Region, timeframe, Season) %>%
  summarize(
    Mean_PD_SD = mean(ProportionalDensity_SD, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = timeframe, values_from = Mean_PD_SD) %>%
  ungroup()
ST_SD_REG$PD_PercentChangeSD = ((ST_SD_REG$After - ST_SD_REG$Before) / ST_SD_REG$Before) * 100
ST_SD_REG$PD_DifferenceSD = ST_SD_REG$After - ST_SD_REG$Before
ST_SD_REG <- ST_SD_REG[, c("Index_Region", "Season", "Before", "After", "PD_DifferenceSD", "PD_PercentChangeSD")]
names(ST_SD_REG)[3] <- "PD_BeforeSD"
names(ST_SD_REG)[4] <- "PD_AfterSD"
ST_SD_REG <- ST_SD_REG %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

REG_ST_Proportions_TF <- PD_Avg_REG %>% inner_join(ST_SD_REG, by = c("Index_Region", "Season"))
str(REG_ST_Proportions_TF)
str(Regional_Proportions_TF)

# Core areas: Calculate the mean proportion for each CA and timeframe----

TF_Avg_CA <- CA_Proportion %>%
  group_by(Index_Region, timeframe, Season) %>%
  summarize(
    Mean_Proportion = mean(Proportion, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = timeframe, values_from = Mean_Proportion) %>%
  ungroup()
TF_Avg_CA$PercentChange = ((TF_Avg_CA$After - TF_Avg_CA$Before) / TF_Avg_CA$Before) * 100
TF_Avg_CA$Difference = TF_Avg_CA$After - TF_Avg_CA$Before
TF_Avg_CA <- TF_Avg_CA[, c("Index_Region", "Season", "Before", "After", "Difference", "PercentChange")]
TF_Avg_CA <- TF_Avg_CA %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
#add the SD
SD_CA <- CA_Proportion %>%
  group_by(Index_Region, timeframe, Season) %>%
  summarize(
    Mean_SD = mean(Proportion_SD, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = timeframe, values_from = Mean_SD) %>%
  ungroup()
SD_CA$PercentChangeSD = ((SD_CA$After - SD_CA$Before) / SD_CA$Before) * 100
SD_CA$DifferenceSD = SD_CA$After - SD_CA$Before
SD_CA <- SD_CA[, c("Index_Region", "Season", "Before", "After", "DifferenceSD", "PercentChangeSD")]
names(SD_CA)[3] <- "BeforeSD"
names(SD_CA)[4] <- "AfterSD"
SD_CA <- SD_CA %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

CA_Proportions_TF <- TF_Avg_CA %>% inner_join(SD_CA, by = c("Index_Region", "Season"))

#Core areas, repeat for proportional density ----
names(CA_Proportion)

PD_Avg_CA <- CA_Proportion %>%
  group_by(Index_Region, timeframe, Season) %>%
  summarize(
    Mean_ProportionalDensity = mean(ProportionalDensity, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = timeframe, values_from = Mean_ProportionalDensity) %>%
  ungroup()
PD_Avg_CA$PD_PercentChange = ((PD_Avg_CA$After - PD_Avg_CA$Before) / PD_Avg_CA$Before) * 100
PD_Avg_CA$PD_Difference = PD_Avg_CA$After - PD_Avg_CA$Before
PD_Avg_CA <- PD_Avg_CA[, c("Index_Region", "Season", "Before", "After", "PD_Difference", "PD_PercentChange")]
names(PD_Avg_CA)[3] <- "PD_Before"
names(PD_Avg_CA)[4] <- "PD_After"
PD_Avg_CA <- PD_Avg_CA %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
#add the SD
ST_SD_CA <- CA_Proportion %>%
  group_by(Index_Region, timeframe, Season) %>%
  summarize(
    Mean_PD_SD = mean(ProportionalDensity_SD, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = timeframe, values_from = Mean_PD_SD) %>%
  ungroup()
ST_SD_CA$PD_PercentChangeSD = ((ST_SD_CA$After - ST_SD_CA$Before) / ST_SD_CA$Before) * 100
ST_SD_CA$PD_DifferenceSD = ST_SD_CA$After - ST_SD_CA$Before
ST_SD_CA <- ST_SD_CA[, c("Index_Region", "Season", "Before", "After", "PD_DifferenceSD", "PD_PercentChangeSD")]
names(ST_SD_CA)[3] <- "PD_BeforeSD"
names(ST_SD_CA)[4] <- "PD_AfterSD"
ST_SD_CA <- ST_SD_CA %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

CA_ST_Proportions_TF <- PD_Avg_CA %>% inner_join(ST_SD_CA, by = c("Index_Region", "Season"))
str(CA_ST_Proportions_TF)
str(CA_Proportions_TF)

#save----mean proportion for each grouping and timeframe----
write.csv(Regional_Proportions_TF, (here::here("2025-04-23/Output/Proportions/Mean_Timeframes/Reg_Proportions_TF.csv")))
write.csv(REG_ST_Proportions_TF, (here::here("2025-04-23/Output/Proportions/Mean_Timeframes/Reg_RelativeDensity_TF.csv")))
write.csv(CA_ST_Proportions_TF, (here::here("2025-04-23/Output/Proportions/Mean_Timeframes/CA_RelativeDensity_TF.csv")))
write.csv(CA_Proportions_TF, (here::here("2025-04-23/Output/Proportions/Mean_Timeframes/CA_Proportions_TF.csv")))

#STEP 3: visualize and compare averaged proportional abundance and proportional density values----
require(DT)

#load data and plotting settings----
#timeseries: proportional abundance and density
Reg_Prop<-read.csv(here::here("2025-04-23/Output/Proportions/proportions_and_density_Regional.csv"))
CA_Prop<-read.csv(here::here("2025-04-23/Output/Proportions/proportions_and_density_CA.csv"))

#timeseries data aggregated by before vs after/during warming period timeframes
Reg_RelativeDensity_TF<-read.csv(here::here("2025-04-23/Output/Proportions/Mean_Timeframes/Reg_RelativeDensity_TF.csv"))
Reg_Proportions_TF<-read.csv(here::here("2025-04-23/Output/Proportions/Mean_Timeframes/Reg_Proportions_TF.csv"))
CA_RelativeDensity_TF<-read.csv(here::here("2025-04-23/Output/Proportions/Mean_Timeframes/CA_RelativeDensity_TF.csv"))
CA_Proportions_TF<-read.csv(here::here("2025-04-23/Output/Proportions/Mean_Timeframes/CA_Proportions_TF.csv"))


library(dplyr)

library(broom)

library(ggplot2)

library(patchwork)

library(grid)  # For unit() function
theme_replace(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour="black"),
              
              strip.background=element_rect(colour="black",fill="white"))

theme_set(theme_bw())

theme_replace(legend.key =element_rect(colour="black",fill="white"),
              
              plot.margin = unit(c(1.5,3,1.5,1), "cm"),
              
              #plot.margin=margin(3,4,4,0),
              
              #plot.margin=margin(3,4,4,0),
              
              plot.title=element_text(size=12,vjust=1.5,family="serif"),
              
              legend.background=element_rect(size=.9,colour="white",fill="white"),
              
              strip.text=element_text(size=11,family="serif",angle=0),
              
              panel.border = element_rect(colour = "black",fill=NA),
              
              panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              
              strip.background=element_rect(colour="black",fill="white"),
              
              axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1,size = 10,family="serif"),
              
              axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size = 10,family="serif"),
              
              axis.title.x=element_text(size=10,hjust=0.5,vjust=-2,family="serif"))

pd <- position_dodge(.5)


#plot 1: plotting the regional trends of estimated abundance across the time series ----
regpal<- c("orange", "darkblue")

str(Reg_Prop)
RegionalPlot<- ggplot(data = Reg_Prop, aes(x = Year, y = Proportion, color = Index_Region))+
  geom_point()+
 # scale_y_continuous(labels = scales::scientific) +
  geom_line()+
  geom_errorbar(data = Reg_Prop, aes(x = Year, ymin = (Proportion - Proportion_SD), ymax = (Proportion + Proportion_SD), color = Index_Region, group = Index_Region), alpha = 0.65) +
  labs(title="Regional Proportions of Estimated Abundance", y="Percentage", x="Year")+
  scale_color_manual(values = regpal) +
  guides(color = guide_legend(title = NULL))+
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)+
  facet_wrap(.~Season)

RegionalPlot

#lets group this seasonal data by year (mean )
Reg_Prop_avg <- Reg_Prop %>%
  group_by(Year, Index_Region) %>%
  summarise(Proportion = mean(Proportion, na.rm = TRUE),
            ProportionalDensity = mean(ProportionalDensity, na.rm = TRUE), .groups = "drop")
  
Regiona_Proportion_Plot<-ggplot(Reg_Prop_avg, aes(x = Year, y = Proportion, fill = Index_Region)) +
  geom_area() +
  scale_fill_manual(values = c("orange", "darkblue")) +
  labs(
    title = "Mean Proportion of Total Est. Abun.",
    x = "Year",
    y = "Proportion of Total (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)
Regiona_RelativeDensity_Plot<-ggplot(Reg_Prop_avg, aes(x = Year, y = ProportionalDensity, fill = Index_Region)) +
  geom_area() +
  scale_fill_manual(values = c("orange", "darkblue")) +
  labs(
    title = "Mean Relative Density",
    x = "Year",
    y = "Proportion of Total (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)
library(patchwork)
Regiona_Proportion_Plot+Regiona_RelativeDensity_Plot

#plot 2: basic  trends for each CA across the time series with the percent change noted in the legend----
names(CA_Proportions_TF)
#get mean annual data
#before v after absolute %
CA_Prop_TF_avg <- CA_Proportions_TF %>%
  group_by(Index_Region) %>%
  summarise(Before = mean(Before, na.rm = TRUE),
            After = mean(After, na.rm = TRUE),
            Difference = mean(Difference, na.rm = TRUE),
            PercentChange = mean(PercentChange, na.rm = TRUE),  .groups = "drop")
#before v after relative density
CA_Relative_TF_avg <- CA_RelativeDensity_TF %>%
  group_by(Index_Region) %>%
  summarise(PD_Before = mean(PD_Before, na.rm = TRUE),
            PD_After = mean(PD_After, na.rm = TRUE),
            PD_Difference = mean(PD_Difference, na.rm = TRUE),
            PD_PercentChange = mean(PD_PercentChange, na.rm = TRUE),  .groups = "drop")
#timeseries: proportional abundance and density
CA_Prop_avg <- CA_Prop %>%
  group_by(Year, Index_Region) %>%
  summarise(Proportion = mean(Proportion, na.rm = TRUE),
            Proportion_SD = mean(Proportion_SD, na.rm = TRUE),
            ProportionalDensity = mean(ProportionalDensity, na.rm = TRUE),
            ProportionalDensity_SD = mean(ProportionalDensity_SD, na.rm = TRUE), .groups = "drop")

names(CA_Prop_TF_avg)
unique(CA_Prop_avg$Index_Region)
#Make a custom colour pallet in the order that i would like to factor 
region_colors <- c(
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



CA_Prop_TF_avg$Index_Region <- factor(CA_Prop_TF_avg$Index_Region, levels = names(region_colors))
CA_Relative_TF_avg$Index_Region <- factor(CA_Relative_TF_avg$Index_Region, levels = names(region_colors))
CA_Prop_avg$Index_Region <- factor(CA_Prop_avg$Index_Region, levels = names(region_colors))


#(this will print the difference between timeframes in the legend )
custom_labels <- paste(
  levels(CA_Prop_TF_avg$Index_Region),  # Factor levels in the correct order
  "(", 
  # Extract Difference values corresponding to each factor level
  with(CA_Prop_TF_avg, 
       round(Difference[match(levels(CA_Prop_TF_avg$Index_Region), Index_Region)], 2)
  ), 
  ")", 
  sep = ""
)

print(custom_labels)


# Plot with custom legend labels
ggplot(data = CA_Prop_avg, aes(x = Year, y = Proportion, color = Index_Region)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  geom_errorbar(
    data = CA_Prop_avg,
    aes(
      x = Year,
      ymin = (Proportion - Proportion_SD),
      ymax = (Proportion + Proportion_SD),
      color = Index_Region,
      group = Index_Region
    ),
    alpha = 0.65
  ) +
  scale_color_manual(values = region_colors, labels = custom_labels)+
  labs(
    title = "Proportion of Estimated Abundance \n per Core Area",
    y = "Percent",
    x = "Year"
  ) +
  guides(color = guide_legend(title = NULL)) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)

#Stacked area: CA seasonal proportion
# This orders from largest to smallest
CA_Prop_avg$Index_Region <- factor(CA_Prop_avg$Index_Region, 
                                   levels = CA_Prop_avg %>% 
                                     group_by(Index_Region) %>% 
                                     summarise(Total_Proportion = sum(Proportion)) %>%
                                     arrange(desc(Total_Proportion)) %>%
                                     pull(Index_Region))  
#plot stacked
ggplot(CA_Prop_avg, aes(x = Year, y = Proportion, fill = Index_Region)) +
  geom_area() +
  scale_fill_manual(values = region_colors)+

  #facet_wrap(~ Season) + 
  #scale_fill_brewer(palette = "Set1") +  # Add custom labels here
  labs(
    title = "Proportion of Estimated Abundance \n per Core Area",
    x = "Year",
    y = "Proportion of Total (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)
#reset factor
CA_Prop_avg$Index_Region <- factor(CA_Prop_avg$Index_Region, levels = names(region_colors))

#Plot 3:relative density----
#(this will print the difference between timeframes in the legend )
custom_labels_Relative <- paste(
  levels(CA_Relative_TF_avg$Index_Region),  # Factor levels in the correct order
  "(", 
  # Extract Difference values corresponding to each factor level
  with(CA_Relative_TF_avg, 
       round(PD_Difference[match(levels(CA_Relative_TF_avg$Index_Region), Index_Region)], 2)
  ), 
  ")", 
  sep = ""
)

print(custom_labels_Relative)


# Plot with custom legend labels
ggplot(data = CA_Prop_avg, aes(x = Year, y = ProportionalDensity, color = Index_Region)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  geom_errorbar(
    data = CA_Prop_avg,
    aes(
      x = Year,
      ymin = (ProportionalDensity - ProportionalDensity_SD),
      ymax = (ProportionalDensity + ProportionalDensity_SD),
      color = Index_Region,
      group = Index_Region
    ),
    alpha = 0.65
  ) +
  scale_color_manual(values = region_colors, labels = custom_labels_Relative)+
  labs(
    title = "Relative Density \n per Core Area",
    y = "Percent",
    x = "Year"
  ) +
  guides(color = guide_legend(title = NULL)) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)

#Stacked area: CA seasonal proportion
# This orders from largest to smallest
CA_Prop_avg$Index_Region <- factor(CA_Prop_avg$Index_Region, 
                                   levels = CA_Prop_avg %>% 
                                     group_by(Index_Region) %>% 
                                     summarise(Total_Proportion = sum(ProportionalDensity)) %>%
                                     arrange(desc(Total_Proportion)) %>%
                                     pull(Index_Region))  
#plot stacked
ggplot(CA_Prop_avg, aes(x = Year, y = ProportionalDensity, fill = Index_Region)) +
  geom_area() +
  scale_fill_manual(values = region_colors)+
  
  #facet_wrap(~ Season) + 
  #scale_fill_brewer(palette = "Set1") +  # Add custom labels here
  labs(
    title = "Relative Density \n per Core Area",
    x = "Year",
    y = "(%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)
#reset factor
CA_Prop_avg$Index_Region <- factor(CA_Prop_avg$Index_Region, levels = names(region_colors))

#Plot 4: gain/loss maps----
#first run Sup4_Map_+GenericSettings
#Relative Density
ggplot() +
  geom_sf(data = contours, color = "lightblue") +
  geom_sf(data = CoreAreas_df, aes(fill = CA_Relative_TF_avg$PD_PercentChange[match(geometry.Region, CA_Relative_TF_avg$Index_Region)])) +
  geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = EEZ, color = "blue") +
  geom_sf(data = NAFO, fill = NA) +
  geom_sf(data = land, fill = "grey") +
  scale_fill_gradientn(
    name = "% Change",
    colors = c("darkred", "gray94", "darkblue"), # Transition from red to blue
    values = scales::rescale(c(min(CA_Relative_TF_avg$PD_PercentChange), 0, max(CA_Relative_TF_avg$PD_PercentChange))),
    limits = c(min(CA_Relative_TF_avg$PD_PercentChange), max(CA_Relative_TF_avg$PD_PercentChange)) # Set limits to the range of your data
  ) +
  labs(title = "Percent Change in Relative Density") +
  xlim(-73, -48) + ylim(39.355, 48)
#Proportion of estimated abundance
ggplot() +
  geom_sf(data = contours, color = "lightblue") +
  geom_sf(data = CoreAreas_df, aes(fill = CA_Prop_TF_avg$PercentChange[match(geometry.Region, CA_Prop_TF_avg$Index_Region)])) +
  geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = EEZ, color = "blue") +
  geom_sf(data = NAFO, fill = NA) +
  geom_sf(data = land, fill = "grey") +
  scale_fill_gradientn(
    name = "% Change",
    colors = c("darkred", "gray94", "darkblue"), # Transition from red to blue
    values = scales::rescale(c(min(CA_Prop_TF_avg$PercentChange), 0, max(CA_Prop_TF_avg$PercentChange))),
    limits = c(min(CA_Prop_TF_avg$PercentChange), max(CA_Prop_TF_avg$PercentChange)) # Set limits to the range of your data
  ) +
  labs(title = "Percent Change in Proportion of Estimated Abundance") +
  xlim(-73, -48) + ylim(39.355, 48)

#Step 4: slopes, estimate LM: rate of change in proportional abundance per time period----
#load timeseries data for proportional abundance and density
library(dplyr)
library(tidyverse)
library(broom)
library(ggplot2)
library(patchwork)
library(forcats)
library(grid) # For unit() function 

Reg_Prop<-read.csv(here::here("2025-04-23/Output/Proportions/proportions_and_density_Regional.csv"))
CA_Prop<-read.csv(here::here("2025-04-23/Output/Proportions/proportions_and_density_CA.csv"))

#Assign Period
#Regional
Reg_Prop$Period<-NULL
Reg_Prop$Period[Reg_Prop$Year<2006]<-"Before"
Reg_Prop$Period[Reg_Prop$Year>2005]<-"After"

#Proportion
Regional_Proportion_coefficients_df <- Reg_Prop %>%
  group_by(Season,Index_Region,Period) %>%
  do({
    model <- lm((Proportion) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
names(Regional_Proportion_coefficients_df)

Regional_Proportion_coefficients_df.Spring <- Regional_Proportion_coefficients_df[Regional_Proportion_coefficients_df$Season=="Spring",]%>%
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept
Regional_Proportion_coefficients_df.Spring$ordRegion<-factor(Regional_Proportion_coefficients_df.Spring$Index_Region,levels=c("USA", "Canada"))

ggplot(Regional_Proportion_coefficients_df.Spring  , aes(x = factor(ordRegion), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  coord_flip()+
  scale_fill_manual(values=c("steelblue", "orangered"))+
  #geom_vline(xintercept = seq(1.5, length(unique(filtered_df$ordCore_Area)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  # ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change in Proportional Abundance %/yr")+
  ggtitle("Rate of change in Proportional Abundance \n Before warming and Warming, Spring ")

RPC<- Regional_Proportion_coefficients_df%>%
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept

#Abundance
Regional_Abundance_coefficients_df <- Reg_Prop %>%
  group_by(Season,Index_Region,Period) %>%
  do({
    model <- lm((Index_Estimate) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
names(Regional_Abundance_coefficients_df)

Regional_Abundance_coefficients_df.Spring <- Regional_Abundance_coefficients_df[Regional_Abundance_coefficients_df$Season=="Spring",]%>%
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept
Regional_Abundance_coefficients_df.Spring$ordRegion<-factor(Regional_Abundance_coefficients_df.Spring$Index_Region,levels=c("USA", "Canada"))
names(Regional_Abundance_coefficients_df.Spring)

ggplot(Regional_Abundance_coefficients_df.Spring  , aes(x = factor(ordRegion), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  coord_flip()+
  scale_fill_manual(values=c("steelblue", "orangered"))+
  #geom_vline(xintercept = seq(1.5, length(unique(filtered_df$ordCore_Area)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  # ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change in  Abundance count/yr")+
  ggtitle("Rate of change in  Abundance \n Before warming and during accelerated Warming, Spring ")

#Per COre Area

CA_Prop$Period<-NULL
CA_Prop$Period[CA_Prop$Year<2006]<-"Before"
CA_Prop$Period[CA_Prop$Year>2005]<-"After"

CA_Prop_Spring<-subset(CA_Prop, CA_Prop$Season=="Spring")

#Proportional Abundance
CA_Proportion_coefficients_df <- CA_Prop_Spring %>%
  group_by(Index_Region,Period) %>%
  do({
    model <- lm((Proportion) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

CA_Proportion_coefficients_df <- CA_Proportion_coefficients_df%>%
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept
CA_Proportion_coefficients_df$ordRegion<-factor(CA_Proportion_coefficients_df$Index_Region,levels=c("BOF", "Browns","CapeBreton", "CapeCod", "EGOM", "GBTail", "Georges", "GrandBanks", "Gully",     
                                                                                                    "HaliChan", "Nantucket",  "Sable"))

ggplot(CA_Proportion_coefficients_df  , aes(x =  fct_rev(factor(ordRegion)), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  coord_flip()+
  scale_fill_manual(values=c("steelblue", "orangered"))+
  #geom_vline(xintercept = seq(1.5, length(unique(filtered_df$ordCore_Area)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  # ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change in Proportional Abundance %/yr")+
  ggtitle("Rate of change in Proportional Abundance \n Before warming and Warming, Spring ")

#Proportional Density
CA_PD_coefficients_df <- CA_Prop_Spring %>%
  group_by(Index_Region,Period) %>%
  do({
    model <- lm((ProportionalDensity) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

CA_PD_coefficients_df <- CA_PD_coefficients_df%>%
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept
CA_PD_coefficients_df$ordRegion<-factor(CA_PD_coefficients_df$Index_Region,levels=c("BOF", "Browns","CapeBreton", "CapeCod", "EGOM", "GBTail", "Georges", "GrandBanks", "Gully",     
                                                                                    "HaliChan", "Nantucket",  "Sable"))

ggplot(CA_PD_coefficients_df  , aes(x =  fct_rev(factor(ordRegion)), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  coord_flip()+
  scale_fill_manual(values=c("steelblue", "orangered"))+
  #geom_vline(xintercept = seq(1.5, length(unique(filtered_df$ordCore_Area)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  # ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change in Relative Density %/yr")+
  ggtitle("Rate of change in Relative Density \n Before warming and Warming, Spring ")

# Abundance
CA_Abundance_coefficients_df <- CA_Prop_Spring %>%
  group_by(Index_Region,Period) %>%
  do({
    model <- lm((Index_Estimate) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

CA_Abundance_coefficients_df <- CA_Abundance_coefficients_df%>%
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept
CA_Abundance_coefficients_df$ordRegion<-factor(CA_Abundance_coefficients_df$Index_Region,levels=c("BOF", "Browns","CapeBreton", "CapeCod", "EGOM", "GBTail", "Georges", "GrandBanks", "Gully",     
                                                                                                    "HaliChan", "Nantucket",  "Sable"))

ggplot(CA_Abundance_coefficients_df  , aes(x =  fct_rev(factor(ordRegion)), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  coord_flip()+
  scale_fill_manual(values=c("steelblue", "orangered"))+
  #geom_vline(xintercept = seq(1.5, length(unique(filtered_df$ordCore_Area)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  # ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change in Abundance count/yr")+
  ggtitle("Rate of change in Abundance Before \n and during accelerated warming , Spring ")



