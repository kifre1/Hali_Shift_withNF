
#----------------------------------
## Libraries
#----------------------------------

library(tidyverse)
library(sf)
library(rnaturalearth)
#library(gmRi)
library(here)
library(patchwork)
source("R/Functions/enhance_r_funcs.R")

# Helper function to convert season names to middle-of-season dates
season_to_date <- function(year_season) {
  # Split all year_season values at once
  parts <- do.call(rbind, strsplit(year_season, "-"))
  years <- as.numeric(parts[, 1])
  seasons <- tolower(parts[, 2])
  
  # Define middle dates for each season
  season_dates <- list(
    winter = "02-15",
    spring = "05-15",
    summer = "08-15",
    fall = "11-15"
  )
  
  # Create the date strings
  date_str <- paste0(years, "-", season_dates[seasons])
  return(date_str)
}

month_season_table <- data.frame("Month" = str_pad(seq(from = 1, to = 12, by = 1), 2, "left", 0), "Season" = c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Fall", "Fall", "Fall", "Winter"))

#----------------------------------
## Load, filter and save unique tows
#----------------------------------
#ma_tows <- read_rds(here("Data/TrawlSurvey_data/mass_weight_at_length.rds")) |>
#  ungroup() |>
#  dplyr::select(longitude, latitude, trawl_id, season, year, survey, date) |>
#  distinct() |>
#  mutate(date = lubridate::date(date)) |>
#  mutate(swept = 0.0132)# https://www.bio.gc.ca/info/intercol/trac-cert/documents/working_papers/TRAC_WP_2014_14.pdf

dfo_tows <- read_rds(here("Data/TrawlSurvey_data/dfo_weight_at_length.rds")) |>
  ungroup() |>
  dplyr::select(longitude, latitude, trawl_id, season, year, survey, date) |>
  distinct() |>
  mutate(
    date = lubridate::date(date),
    year = as.numeric(as.character(format(date, "%Y")))
  ) |>
  mutate(swept = 0.0405)

nefsc_tows <- read_rds(here("Data/TrawlSurvey_data/nefsc_both_weight_at_length.rds")) |>
  ungroup() |>
  dplyr::select(longitude, latitude, trawl_id, season, year, survey, date) |>
  distinct() |>
  mutate(date = lubridate::date(date)) |>
  mutate(swept = 0.0384)

#me_tows <- read_rds(here("Data/TrawlSurvey_data/me_both_weight_at_length.rds")) |>
#  ungroup() |>
#  dplyr::select(longitude, latitude, trawl_id, season, year, survey, date) |>
#  distinct() |>
#  mutate(date = lubridate::date(date))|>
#  mutate(swept = 0.0198)

nf_all <- get(load(here("Data/TrawlSurvey_data/abun_nl_all85to24.Rdata"))) |>
  ungroup() |>
  filter(NAFOdiv %in% c("3K", "3L", "3P", "3N", "3O", "4V", "4U")) |> #take out the stuff that is super north (cut at 3K, 2J border )
  distinct(year, month, day, lat.start, long.start, NAFOdiv, dist.towed) |>
  mutate(trawl_id = as.character(row_number()), survey = "NF")
# Yankee or Engel wing spread is roughly 45 feet (up to 1995)
# Campelen or Modified Campelen, it’s 55.25 feet. 
#Wing spread * tow distance will get you the offset you need for tow area.
#convert feet distance towed units	1 mile = 5280 feet,	1 nautical mile = 6076.12 feet, 1 km = 3280.84 feet

nf_all <- nf_all %>%
  #mutate(WingSpread = ifelse(year < 1996, 0.013716, 0.0168402)) %>% #Km
  mutate(WingSpread = ifelse(year < 1996, 0.007406042, 0.009092974)) %>% #nautical miles
  mutate(swept = dist.towed * WingSpread) %>%
  mutate(swept = swept * 3.4299)
#hist(nf_all$swept)

nf_tows<- nf_all |>
  mutate(
    month = str_pad(month, 2, "left", 0),
    date = lubridate::date(paste(year, month, day, sep = "-")),
    long.start = -1 * long.start
  ) |>
  left_join(month_season_table, by = c("month" = "Month")) |>
  rename(c(longitude = long.start, latitude = lat.start, season = Season)) |>
  dplyr::select(longitude, latitude, trawl_id, season, year, survey, date, swept) |>
  distinct() 

#tows_df <- rbind(ma_tows, dfo_tows, nefsc_tows, me_tows, nf_tows) |>
tows_df <- rbind( dfo_tows, nefsc_tows, nf_tows) |>
  mutate(season = str_to_sentence(season), survey_season = paste(survey, season, sep = "_"))


extent <- tows_df |>
  distinct(longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

ggplot() +
  geom_sf(data = extent)

write_rds(tows_df, here("Data/Derived/all_unique_towsAl14.rds"), compress = "gz")

#----------------------------------
## Load, filter and save catch
#----------------------------------
#ma_catch <- read_rds(here("Data/TrawlSurvey_data/mass_weight_at_length.rds")) |>
#  ungroup() |>
#  filter(scientific_name == "Hippoglossus hippoglossus") |>
#  mutate(date = lubridate::date(date)) |>
#  dplyr::select(longitude, latitude, trawl_id, season, year, date, length_cm, number_at_length, weight_at_length, catchsex, survey) |>
#  mutate(total_weight_at_length = number_at_length * weight_at_length) |>
#  group_by(trawl_id, longitude, latitude, season, year, survey, date) |>
#  summarize("total_biomass" = sum(total_weight_at_length), "total_abundance" = sum(number_at_length)) |>
#  mutate(swept = 0.0132) |>
#  full_join(ma_tows)

#ma_catch$total_biomass[is.na(ma_catch$total_biomass)] <- 0
#ma_catch$total_abundance[is.na(ma_catch$total_abundance)] <- 0

#all(ma_catch$trawl_id %in% ma_tows$trawl_id)
#all(ma_tows$trawl_id %in% ma_catch$trawl_id)

dfo_catch <- read_rds(here("Data/TrawlSurvey_data/dfo_weight_at_length.rds")) |>
  ungroup() |>
  filter(scientific_name == "Hippoglossus hippoglossus") |>
  mutate(
    date = lubridate::date(date),
    year = as.numeric(as.character(format(date, "%Y")))
  ) |>
  dplyr::select(longitude, latitude, trawl_id, season, year, date, length_cm, number_at_length, weight_at_length, catchsex, survey) |>
  mutate(total_weight_at_length = number_at_length * weight_at_length) |>
  group_by(trawl_id, longitude, latitude, season, year, survey, date) |>
  summarize("total_biomass" = sum(total_weight_at_length), "total_abundance" = sum(number_at_length)) |>
  mutate(swept = 0.0405) |>
  full_join(dfo_tows)

dfo_catch$total_biomass[is.na(dfo_catch$total_biomass)] <- 0
dfo_catch$total_abundance[is.na(dfo_catch$total_abundance)] <- 0

all(dfo_catch$trawl_id %in% dfo_tows$trawl_id)
all(dfo_tows$trawl_id %in% dfo_catch$trawl_id)

nefsc_catch <- read_rds(here("Data/TrawlSurvey_data/nefsc_both_weight_at_length.rds")) |>
  ungroup() |>
  filter(scientific_name == "hippoglossus hippoglossus") |>
  mutate(date = lubridate::date(date)) |>
  dplyr::select(longitude, latitude, trawl_id, season, year, date, length_cm, number_at_length, weight_at_length, catchsex, survey) |>
  mutate(total_weight_at_length = number_at_length * weight_at_length) |>
  group_by(trawl_id, longitude, latitude, season, year, survey, date) |>
  summarize("total_biomass" = sum(total_weight_at_length), "total_abundance" = sum(number_at_length)) |>
  mutate(swept = 0.0384) |>
  full_join(nefsc_tows)

nefsc_catch$total_biomass[is.na(nefsc_catch$total_biomass)] <- 0
nefsc_catch$total_abundance[is.na(nefsc_catch$total_abundance)] <- 0

all(nefsc_catch$trawl_id %in% nefsc_tows$trawl_id)
all(nefsc_tows$trawl_id %in% nefsc_catch$trawl_id)

#me_catch <- read_rds(here("Data/TrawlSurvey_data/me_both_weight_at_length.rds")) |>
#  ungroup() |>
#  filter(scientific_name == "Hippoglossus hippoglossus") |>
#  mutate(date = lubridate::date(date)) |>
#  dplyr::select(longitude, latitude, trawl_id, season, year, date, length_cm, number_at_length, weight_at_length, catchsex, survey) |>
#  mutate(total_weight_at_length = number_at_length * weight_at_length) |>
#  group_by(trawl_id, longitude, latitude, season, year, survey, date) |>
#  summarize("total_biomass" = sum(total_weight_at_length), "total_abundance" = sum(number_at_length)) |>
#  mutate(swept = 0.0198) |>
#  full_join(me_tows)

#me_catch$total_biomass[is.na(me_catch$total_biomass)] <- 0
#me_catch$total_abundance[is.na(me_catch$total_abundance)] <- 0

#all(me_catch$trawl_id %in% me_tows$trawl_id)
#all(me_tows$trawl_id %in% me_catch$trawl_id)

nf_catch <- get(load(here("Data/TrawlSurvey_data/abun_nl_all85to24.Rdata"))) |>
  ungroup() |>
  filter(spec == 893) |> #select only halibut
  filter(NAFOdiv %in% c("3K", "3L", "3P", "3N", "3O", "4V", "4U")) |> #take out the stuff that is super north (cut at 3K, 2J border )
  
  filter(halibut_presence > 0) |>
  full_join(nf_all) |>
  mutate(
    month = str_pad(month, 2, "left", 0),
    date = lubridate::date(paste(year, month, day, sep = "-")),
    survey = "NF",
    total_weight_at_length = number * weight,
    long.start = -1 * long.start
  ) |>
  left_join(month_season_table, by = c("month" = "Month")) |>
  rename(c(longitude = long.start, latitude = lat.start, season = Season)) |>
  group_by(trawl_id, longitude, latitude, season, year, survey, date, swept) |>
  summarize("total_biomass" = sum(total_weight_at_length), , "total_abundance" = sum(number)) |>
  ungroup() 

nf_catch$total_biomass[is.na(nf_catch$total_biomass)] <- 0
nf_catch$total_abundance[is.na(nf_catch$total_abundance)] <- 0
str(nf_catch)
str(nf_tows)
# Double check NF
all(nf_catch$trawl_id %in% nf_tows$trawl_id)
all(nf_tows$trawl_id %in% nf_catch$trawl_id)

#catch_df <- rbind(ma_catch, dfo_catch, nefsc_catch, me_catch, nf_catch) |>
catch_df <- rbind(dfo_catch, nefsc_catch, nf_catch) |>
  mutate(season = str_to_sentence(season), survey_season = paste(survey, season, sep = "_"))
summary(catch_df)
table(tows_df$year, tows_df$season, tows_df$survey)

write_rds(catch_df, here("Data/Derived/all_raw_halibut_catchAl14.rds"), compress = "gz")

library(dplyr)
library(lubridate)  
#structure that i am aiming for 
SampleData <- read.csv("C:/Users/fergusonk/Documents/Halibut/CA_Halibut_Shift/R/data/halibut_all_data.csv")
catch_data <- read_rds("Data/Derived/all_raw_halibut_catchAl14.rds") 


str(SampleData)
str(catch_data)

#change names and add columns to match catch_data to the sample as closely as possible 
catch_data <- catch_data |> 
  ungroup() |> 
  mutate(X = row_number())|> 
  mutate(NMFS_SVSPP = 101)|> 
  mutate(DFO_SPEC = 30)|>
  mutate(trawl_id = gsub("-", "", trawl_id))
names(catch_data)[names(catch_data) == "trawl_id"] <-"ID" #renamed but not reformatted   
names(catch_data)[names(catch_data) == "date"] <-"DATE"          
names(catch_data)[names(catch_data) == "year"] <- "EST_YEAR"      
names(catch_data)[names(catch_data) == "season"] <-"SEASON"        
names(catch_data)[names(catch_data) == "survey"] <-"SURVEY"       
"SVVESSEL"  #I do not have anything like this, the closest thing is survey_season, which i will keep because I think it is harmless 
names(catch_data)[names(catch_data) == "latitude"] <-"DECDEG_BEGLAT" 
names(catch_data)[names(catch_data) == "longitude"] <-"DECDEG_BEGLON" 
catch_data <- catch_data |> 
  mutate(PRESENCE = if_else(total_abundance > 0, 1, 0))  
names(catch_data)[names(catch_data) == "swept"] <-"Swept" 
names(catch_data)[names(catch_data) == "total_biomass"] <-"BIOMASS"       
names(catch_data)[names(catch_data) == "total_abundance"] <-"ABUNDANCE"     
catch_data <- catch_data %>%
  dplyr::select(X, ID, DATE, EST_YEAR, SEASON, 
                SURVEY, survey_season, DECDEG_BEGLAT, DECDEG_BEGLON, NMFS_SVSPP, 
                DFO_SPEC, PRESENCE, BIOMASS, ABUNDANCE, Swept)

catch_data %>%
  group_by(SURVEY, PRESENCE) %>%
  summarize(count = n(), .groups = "drop")

#remove years before 1990
catch_data <- catch_data %>%
  filter(EST_YEAR >= 1990)

catch_data %>%
  group_by(SURVEY, SEASON) %>%
  summarize(count = n(), .groups = "drop")
#there are NAs
catch_data_test <- catch_data %>%
  filter(is.na(SEASON))
#look into winter in nf
catch_data_test <- catch_data %>%
  filter(SEASON=="Winter")
sum(catch_data_test$PRESENCE, na.rm = TRUE)#66

#Assign season when NA, and take out Winter because we only have it in NF
catch_data <- catch_data %>%
  mutate(
    SEASON = case_when(
      is.na(SEASON) & month(DATE) %in% c(3, 4, 5)  ~ "Spring",
      is.na(SEASON) & month(DATE) %in% c(6, 7, 8)  ~ "Summer",
      is.na(SEASON) & month(DATE) %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ SEASON  # Keep existing SEASON values
    )
  )

catch_data <- catch_data %>%
  filter(SEASON !="Winter")
catch_data <- catch_data %>%
  filter(EST_YEAR !=2024)
names(catch_data)

catch_data %>%
  group_by(SURVEY, Swept) %>%
  summarize(count = n(), .groups = "drop")
hist(catch_data$Swept)

write_rds(catch_data, here("Data/Derived/all_raw_halibut_catch_formattedAl14.rds"), compress = "gz")
#go to 1.2 data_prep_ExtractBNAM to get covariates 


