#Percent biomass (abundance) per region & CA
#Proportion of Estimated Abundance per region & CA
#Proportional density per CA (Relative Density)
#TO DO: incorporate "other" it CA data, representing areas unallocated to a CA

#Create regional proportions data for USA each of the Core areas, Canada, andUSA

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


#STEP 1: get annual regional proportions----

#prep data----
fit<- readRDS( here::here("2024-10-04/Halibut_BC/SpST_mod_fit.rds")) 

all_times<-unique(fit$data_frame$t_i)
abundance_ind<- get_vast_index_timeseries(vast_fit = fit, nice_category_names = "Halibut", index_scale = c("raw"), all_times = all_times, out_dir = here::here("2024-10-04/Output/Index_Abundance"))

#lets add season and Year to these data and subset the core areas from the region so that we can plot them properly 
years_Spring <- as.numeric(seq(0, 104, by = 3))
years_Summer <- as.numeric(seq(1, 104, by = 3))
years_Fall <- as.numeric(seq(2, 104, by = 3))
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
  mutate(Year = (YearGroup + 1984))
head(abundance_ind)

# make a proportional abundance (Percent abuncance) df for canada vs us counts, as well as for each CA relative to the whole stdy region
abundance_ind_proportions <- abundance_ind[, !names(abundance_ind) %in% c("Time", "Category")] #remove unneeded vars
#tidy up some of the region/CA names for consistency
abundance_ind_proportions <- abundance_ind_proportions %>%
  mutate(Index_Region = ifelse(Index_Region == "NMFS", "USA", Index_Region))
abundance_ind_proportions <- abundance_ind_proportions %>%
  mutate(Index_Region = ifelse(Index_Region == "DFO", "Canada", Index_Region))
abundance_ind_proportions <- abundance_ind_proportions %>%
  mutate(Index_Region = ifelse(Index_Region == "CB4Vn", "CapeBreton", Index_Region))

#add a column for square area
#get the data from the Prediction data df
SDM_data<- read.csv(here::here("2024-10-04/Output/PredictionData_for_ShiftAnalysis.csv"))
# Isolate core areas
filtered_data <- SDM_data[!is.na(SDM_data$Core_Area), ]
# Select the unique combinations of CA_Area_km2 and Core_Area
CA_square_area <- unique(filtered_data[, c("CA_Area_km2", "Core_Area")])
filtered_dataR <- SDM_data[!is.na(SDM_data$survey), ]
R_square_area <- unique(filtered_dataR[, c("Reg_Area_km2", "survey")])
#rename some of the vars
R_square_area <- R_square_area %>%
  mutate(survey = ifelse(survey == "NMFS", "USA", survey))
R_square_area <- R_square_area %>%
  mutate(survey = ifelse(survey == "DFO", "Canada", survey))
#Rename the columns to match the abundance data
CA_square_area$Km2 <- CA_square_area$CA_Area_km2
CA_square_area$Index_Region <- CA_square_area$Core_Area
R_square_area$Km2 <- R_square_area$Reg_Area_km2
R_square_area$Index_Region <- R_square_area$survey  
#Combine the two  data frames
combined_areas <- rbind(
  CA_square_area[, c("Index_Region", "Km2")],
  R_square_area[, c("Index_Region", "Km2")]
)

# Merge with the abundance df
abundance_ind_proportions <- merge(
  abundance_ind_proportions,
  combined_areas,
  by = "Index_Region",
  all.x = TRUE
)
# select for region and CA, and Group by Year and Season to calculate total estimates and proportions
abundance_ind_proportions_Region<-subset(abundance_ind_proportions, abundance_ind_proportions$Index_Region=="USA"|abundance_ind_proportions$Index_Region=="Canada")
abundance_ind_proportions_CA<-subset(abundance_ind_proportions,abundance_ind_proportions$Index_Region== "All"| abundance_ind_proportions$Index_Region=="CapeCod" |abundance_ind_proportions$Index_Region=="EGOM"|abundance_ind_proportions$Index_Region=="BOF" |
                           abundance_ind_proportions$Index_Region=="Nantucket" |abundance_ind_proportions$Index_Region=="Georges"|abundance_ind_proportions$Index_Region=="Browns" |
                           abundance_ind_proportions$Index_Region=="Sable" |abundance_ind_proportions$Index_Region=="Gully"|abundance_ind_proportions$Index_Region=="CapeBreton")


#calculate regional proportional abundance and density----
abundance_ind_proportions_Region <- abundance_ind_proportions_Region %>%
  group_by(Year, Season) %>%  #if i include indes_region here, all proportions will be 1 because the other region will not be included in the Total_estimate                             
  mutate(
    Total_Estimate = sum(Index_Estimate),#total catch for the year
    Proportion =  round(Index_Estimate / Total_Estimate, 3),  # Proportion per region rounded to 3 decimals
    Proportion_SD = round(Index_SD / Total_Estimate, 3) #the SD also needs to be scaled by the same factor in order to remain consistent
  ) %>%
  ungroup()   

#proportional density 
abundance_ind_proportions_Region <- abundance_ind_proportions_Region %>%
  group_by(Year, Season) %>%                               
  mutate(
    # Standardize Index_Estimate and Total_Estimate by Km2
    Regional_Standardized_Index_Estimate = Index_Estimate / Km2, #standardize per region by square area
    Total_Standardized_Estimate = sum(Regional_Standardized_Index_Estimate),  # Total standardized catch for the year/season
    Standardized_SD = Index_SD/ Km2, #the SD also needs to be scaled by the same factor in order to remain consistent
    
    # Calculate proportions using standardized values
    ST_Proportion = round(Regional_Standardized_Index_Estimate / Total_Standardized_Estimate, 3),
    ST_Proportion_SD = round(Standardized_SD /  Total_Standardized_Estimate, 3)
  ) %>%
  ungroup()

#as a percentage
abundance_ind_proportions_Region$Proportion <- abundance_ind_proportions_Region$Proportion * 100
abundance_ind_proportions_Region$Proportion_SD <- abundance_ind_proportions_Region$Proportion_SD * 100
abundance_ind_proportions_Region$ST_Proportion <- abundance_ind_proportions_Region$ST_Proportion * 100
abundance_ind_proportions_Region$ST_Proportion_SD <- abundance_ind_proportions_Region$ST_Proportion_SD * 100
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

unique(abundance_ind_proportions_Region$Km2)
TotalArea<- 214703.7 + 189358.5 #total area

#redo this one because my ST_Proportion_SD is off
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

#save----
write.csv(abundance_ind_proportions_CA, (here::here("2024-10-04/Output/proportions_CA.csv")))
write.csv(abundance_ind_proportions_Region, (here::here("2024-10-04/Output/proportions_Regional.csv")))
write.csv(abundance_ind_proportions_Region_Spring,(here::here("2024-10-04/Output/proportions_Regional_Spring.csv")))


#STEP 2: Compare these data between before and during warming timeframes ----
#incorporate timeframes----
CA_Proportion<-read.csv(here::here("2024-10-04/Output/proportions_CA.csv"))
Regional_Proportion<- read.csv(here::here("2024-10-04/Output/proportions_Regional.csv"))
CA_Proportion<-subset(CA_Proportion, CA_Proportion$Index_Region!="All")#not needed anymore


# add the timeframe to the data
CA_Proportion <- CA_Proportion %>%
  mutate(
    timeframe = case_when(
      Year %in% 1985:2005 ~ "Before",
      Year %in% 2006:2019 ~ "After",
      TRUE ~ NA_character_  # Handles any years outside these ranges
    )
  )

Regional_Proportion <- Regional_Proportion %>%
  mutate(
    timeframe = case_when(
      Year %in% 1985:2005 ~ "Before",
      Year %in% 2006:2019 ~ "After",
      TRUE ~ NA_character_  # Handles any years outside these ranges
    )
  )


# Regional: Calculate the mean proportion for each Region and timeframe----
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
class(Regional_Proportions_TF)

Regional_Proportions_TF_Spring<-
  as_tibble(Regional_Proportions_TF)%>% filter(Season=="Spring")

Regional_Proportions_TF_table<-Regional_Proportions_TF_Spring[,c(1,3,4,6)]
names(Regional_Proportions_TF_table)<-c("Nation","Before","During","PerChange")
Regional_Proportions_TF_table<-Regional_Proportions_TF_table%>%mutate(across(where(is.numeric), ~ round(.x, 2)))

library(gridExtra)
library(grid)
table_grobPROP <- tableGrob(Regional_Proportions_TF_table, rows = NULL)


# Per CA: Calculate the mean proportion for each CA and timeframe----

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

#repeat for proportional density 
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

#save----
write.csv(Regional_Proportions_TF, (here::here("2024-10-04/Output/Reg_Proportions_TF.csv")))
write.csv(CA_ST_Proportions_TF, (here::here("2024-10-04/Output/CA_ProportionalDensity_TF.csv")))
write.csv(CA_Proportions_TF, (here::here("2024-10-04/Output/CA_Proportions_TF.csv")))



#STEP 3: visualize and compare averaged proportional abundance and proportional density values----
require(DT)


#load data and plotting settings----
#timeseries: proportional abundance and density
Reg_Prop<-read.csv(here::here("2024-10-04/Output/proportions_Regional.csv"))
CA_Prop<-read.csv(here::here("2024-10-04/Output/proportions_CA.csv"))
CA_Prop<-subset(CA_Prop, CA_Prop$Index_Region!="All")#not needed anymore

#timeseries data aggregated by before vs after/during warming period timeframes
Reg_Proportions_TF<-read.csv(here::here("2024-10-04/Output/Reg_Proportions_TF.csv"))
CA_ProportionalDensity_TF<-read.csv(here::here("2024-10-04/Output/CA_ProportionalDensity_TF.csv"))
CA_Proportions_TF<-read.csv(here::here("2024-10-04/Output/CA_Proportions_TF.csv"))


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
              
              axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1,size=10,family="serif"),
              
              axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=10,family="serif"),
              
              axis.title.x=element_text(size=10,hjust=0.5,vjust=-2,family="serif"))

pd <- position_dodge(.5)


#plot 1: plotting the regional trends  of estimated abundance across the time series ----
regpal<- c("orange", "darkblue")

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
#regional seasonal proportion, stacked area
ggplot(Reg_Prop, aes(x = Year, y = Proportion, fill = Index_Region)) +
  geom_area() +
  facet_wrap(~ Season) + 
  scale_fill_manual(values = c("orange", "darkblue"))+
  labs(
    title = "Regional Proportion of Estimated Abundance",
    x = "Year",
    y = "Proportion of Total (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)

#plot 2: basic  trends for each CA across the time series with the percent change noted in the legend----
CA_TF_Spring<-subset(CA_Proportions_TF, CA_ProportionalDensity_TF$Season=="Spring")
#(this will print the difference between timeframes in the legend )
custom_labels <- with(CA_TF_Spring, 
                      paste(
                        Index_Region, 
                        "(", 
                        ifelse(Difference > 0, paste0("+", round(Difference, 2)), round(Difference, 2)), 
                        ")", 
                        sep = ""
                      )
)

# Ensure the names of the vector match the values in CA_Proportion$Index_Region
names(custom_labels) <- CA_TF_Spring$Index_Region

CA_Prop_Spring<-subset(CA_Prop, CA_Prop$Season=="Spring")

names(CA_Prop_Spring)
# Plot with custom legend labels
ggplot(data = CA_Prop_Spring, aes(x = Year, y = Proportion, color = Index_Region)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  geom_errorbar(
    data = CA_Prop_Spring,
    aes(
      x = Year,
      ymin = (Proportion - Proportion_SD),
      ymax = (Proportion + Proportion_SD),
      color = Index_Region,
      group = Index_Region
    ),
    alpha = 0.65
  ) +
  scale_color_brewer(palette = "Set1", labels = custom_labels) +  # Add custom labels here
  labs(
    title = "Proportion of Estimated Abundance \n per Core Area, Spring",
    y = "Percent",
    x = "Year"
  ) +
  guides(color = guide_legend(title = NULL)) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)

#Stacked area: CA seasonal proportion
ggplot(CA_Prop, aes(x = Year, y = Proportion, fill = Index_Region)) +
  geom_area() +
  facet_wrap(~ Season) + 
  scale_fill_brewer(palette = "Set1") +  # Add custom labels here
  labs(
    title = "Proportion of Estimated Abundance \n per Core Area",
    x = "Year",
    y = "Proportion of Total (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)

#Stacked Area: CA seasonal proportion, spring
CA_Prop_Spring<-subset(CA_Prop, CA_Prop$Season=="Spring")
ggplot(CA_Prop_Spring, aes(x = Year, y = Proportion, fill = Index_Region)) +
  geom_area() +
  scale_fill_brewer(palette = "Set1", labels = custom_labels) +  # Add custom labels here
  labs(
    title = "Proportion of Estimated Abundance \n per Core Area, Spring",
    x = "Year",
    y = "Proportion of Total (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)
#Plot 3:relative density----
CA_PD_TF_Spring<-subset(CA_ProportionalDensity_TF, CA_ProportionalDensity_TF$Season=="Spring")

#(this will print the difference between timeframes in the legend )
PD_custom_labels <- with(CA_PD_TF_Spring, 
                      paste(
                        Index_Region, 
                        "(", 
                        ifelse(PD_Difference > 0, paste0("+", round(PD_Difference, 2)), round(PD_Difference, 2)), 
                        ")", 
                        sep = ""
                      )
)

# Ensure the names of the vector match the values in CA_Proportion$Index_Region
names(PD_custom_labels) <- CA_PD_TF_Spring$Index_Region

names(CA_Prop_Spring)
# Plot with custom legend labels
ggplot(data = CA_Prop_Spring, aes(x = Year, y = ProportionalDensity, color = Index_Region)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  geom_errorbar(
    data = CA_Prop_Spring,
    aes(
      x = Year,
      ymin = (ProportionalDensity - ProportionalDensity_SD),
      ymax = (ProportionalDensity + ProportionalDensity_SD),
      color = Index_Region,
      group = Index_Region
    ),
    alpha = 0.65
  ) +
  scale_color_brewer(palette = "Set1", labels = PD_custom_labels) +  # Add custom labels here
  labs(
    title = "Relative Density of Estimated Abundance \n per Core Area, Spring",
    y = "Percent",
    x = "Year"
  ) +
  guides(color = guide_legend(title = NULL)) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)

#Stacked area: CA seasonal proportion
ggplot(CA_Prop, aes(x = Year, y = ProportionalDensity, fill = Index_Region)) +
  geom_area() +
  facet_wrap(~ Season) + 
  scale_fill_brewer(palette = "Set1") +  # Add custom labels here
  labs(
    title = "Relative Density of Estimated Abundance per Core Area",
    x = "Year",
    y = "Relative Density (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)

#Stacked Area: CA seasonal proportion, spring
CA_Prop_Spring<-subset(CA_Prop, CA_Prop$Season=="Spring")
ggplot(CA_Prop_Spring, aes(x = Year, y = ProportionalDensity, fill = Index_Region)) +
  geom_area() +
  scale_fill_brewer(palette = "Set1", labels = PD_custom_labels) +  # Add custom labels here
  labs(
    title = "Relative Density of Estimated Abundance \n per Core Area, Spring",
    x = "Year",
    y = "Relative Density (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)



#Plot 4: gain/loss maps----
#first run Sup4_Map_+GenericSettings
# want to colour it based on %change from CA_PD_TF_Spring$PD_PercentChange and CA_TF_Spring$PercentChang
#Relative Density
ggplot() +
  geom_sf(data = contours, color = "lightblue") +
  geom_sf(data = CA_df, aes(fill = CA_PD_TF_Spring$PD_PercentChange[match(geometry.Region, CA_PD_TF_Spring$Index_Region)])) +
  geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = EEZ, color = "blue") +
  geom_sf(data = NAFO, fill = NA) +
  geom_sf(data = land, fill = "grey") +
  scale_fill_gradientn(
    name = "% Change",
    colors = c("darkred", "gray94", "darkblue"), # Transition from red to blue
    values = scales::rescale(c(min(CA_PD_TF_Spring$PD_PercentChange), 0, max(CA_PD_TF_Spring$PD_PercentChange))),
    limits = c(min(CA_PD_TF_Spring$PD_PercentChange), max(CA_PD_TF_Spring$PD_PercentChange)) # Set limits to the range of your data
  ) +
  labs(title = "Percent Change in Relative Density") +
  xlim(-73, -55) + 
  ylim(39.355, 47.2) 
#Proportion of estimated abundance
ggplot() +
  geom_sf(data = contours, color = "lightblue") +
  geom_sf(data = CA_df, aes(fill = CA_TF_Spring$PercentChange[match(geometry.Region, CA_TF_Spring$Index_Region)])) +
  geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = EEZ, color = "blue") +
  geom_sf(data = NAFO, fill = NA) +
  geom_sf(data = land, fill = "grey") +
  scale_fill_gradientn(
    name = "% Change",
    colors = c("darkred", "gray94", "darkblue"), # Transition from red to blue
    values = scales::rescale(c(min(CA_TF_Spring$PercentChange), 0, max(CA_TF_Spring$PercentChange))),
    limits = c(min(CA_TF_Spring$PercentChange), max(CA_TF_Spring$PercentChange)) # Set limits to the range of your data
  ) +
  labs(title = "Percent Change in Proportion of Estimated Abundance") +
  xlim(-73, -55) + 
  ylim(39.355, 47.2) 
#slopes, estimate LM: rate of change in proportional abundance per time period----
#load timeseries data for proportional abundance and density
Reg_Prop<-read.csv(here::here("2024-10-04/Output/proportions_Regional.csv"))
CA_Prop<-read.csv(here::here("2024-10-04/Output/proportions_CA.csv"))
CA_Prop<-subset(CA_Prop, CA_Prop$Index_Region!="All")#not needed anymore

#Assign Period
#Regional
Reg_Prop$Period<-NULL
Reg_Prop$Period[Reg_Prop$Year<2006]<-"Earlier 1985-2005"
Reg_Prop$Period[Reg_Prop$Year>2005]<-"Later 2006-2019"


library(dplyr)
library(tidyverse)
library(broom)
library(ggplot2)
library(patchwork)
library(forcats)

library(grid)  # For unit() function
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

#Per COre Area

CA_Prop$Period<-NULL
CA_Prop$Period[CA_Prop$Year<2006]<-"Earlier 1985-2005"
CA_Prop$Period[CA_Prop$Year>2005]<-"Later 2006-2019"

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
CA_Proportion_coefficients_df$ordRegion<-factor(CA_Proportion_coefficients_df$Index_Region,levels=c("EGOM","BOF","CapeBreton", "CapeCod","Browns","Sable","Nantucket","Georges","Gully"))

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
CA_PD_coefficients_df$ordRegion<-factor(CA_PD_coefficients_df$Index_Region,levels=c("EGOM","BOF","CapeBreton", "CapeCod","Browns","Sable","Nantucket","Georges","Gully"))

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


#how to transform proportion data for linear regression 
#modelling change in proportion
#logistic regression with logodds
# it lis a logit transformation

#methods, find some thorson refs to make an outline for writing the methods
#range edge
#look to alexa's Git to show range edge as one graph
#looking for a north/east vector
each plot to have a temporal pattern and the rates of change within the period 