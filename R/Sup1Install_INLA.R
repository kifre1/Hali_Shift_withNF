#see https://github.com/James-Thorson-NOAA/VAST for installing VAST and TMB
#Installing INLA was complicated and required a lot of trial and error..THe INLA website as well as 
#I needed to be in version 4.4.1, and install the appropriate version of Rtools,
R.version #check what you have
#install.packages("devtools")# you might need this
# List of required packages
required_packages <- c("Matrix", "methods", "stats", "graphics", "grDevices", "utils", "tools",
                       "sp", "rgdal", "rgeos", "raster", "splancs", "maptools", "mgcv", "ggplot2", "fields")

# Function to check and install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Install missing packages
sapply(required_packages, install_if_missing)
#after all these dependencies are installed, restart r and run ...to be honest i do not remember which of the following worked...but one should 

install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), type = "source")
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

