#####
## Halibut data and CMIP data prep
#####
library(tidyverse)
library(here)
library(zonator)
source("~/GitHub/TargetsSDM/R/covariate_functions.R")

# Function to convert points to sf object
points_to_sf<- function(points){
  sf_out<- st_as_sf(points, coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"), crs = 4326, remove = FALSE)
  return(sf_out)
}

# These datasets are generated as part of a different project and within GMRI's TargetsSDM workflow.
# Halibut data and observed conditions
halibut_data <- readRDS("~/GitHub/COCA19_Projections/data/combined/tidy_mod_data.rds") %>%
    filter(., NMFS_SVSPP == 101 & EST_YEAR >= 1985)
write.csv(halibut_data, here::here("data", "halibut_all_data.csv"))


