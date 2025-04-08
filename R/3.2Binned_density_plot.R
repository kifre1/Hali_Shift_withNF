# Mapping the predicted density from knot locations, smoothed over a regular grid. 
#group …come up with 3-4 sensible bins 

# Load package
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


source(here::here("R/VAST_functions/vast_functions.R"))#the file is different when you pull it straight from Andrew's directory...need to use this one in order for the plots to work
source(here::here("R/VAST_functions/kf_vast_function_edits.R"))


# Extracting and plotting predicted density at knot locations, smooth over a regular grid, bin the years meaningfully
fit<- readRDS( here::here("2024-10-04/Halibut_BC/SpST_mod_fit.rds")) 

region_shape <- st_read(here::here("R/data/region_shapefile", "full_survey_region_simple.shp"))
all_times<-as.character(0:104)
spring_times<- as.character(seq(0, 104, by = 3)) 

spring_range <- range(as.numeric(spring_times))
bin_edges <- c(spring_range[1], 57, spring_range[2])# Bin 1: 1985 to 2004 # Bin 2: 2005 to 2019
bin_years <- cut(as.numeric(spring_times), breaks = bin_edges, labels = FALSE, include.lowest = TRUE)
print(bin_years)
print(spring_times)
print(table(bin_years))

land_use<-st_read(here::here("R/data/land_shapefile/", "ne_50m_land.shp"))

xlim_use <- c(-73, -55)
ylim_use <- c(36, 48.25)

out_dir <- here::here("2024-10-04/Output/GridPlot")

lab_lat = 37
lab_lon = -70 
start_year<-1985
season_increment <- 3 
legend_title<- "Log(Density)" 

vast_fit_plot_spatial_kf_binned_new <- function(vast_fit, manual_pred_df, pred_grid, spatial_var, nice_category_names, pred_label, 
                                                climate_scenario = "", mask, all_times = all_times, plot_times = NULL, land_sf, xlim, ylim, 
                                                lab_lat = NULL, lab_lon = NULL, panel_or_gif = "panel", out_dir, land_color = "#d9d9d9", 
                                                panel_cols = NULL, panel_rows = NULL, bins = NULL, bin_years = NULL, start_year = NULL, 
                                                season_increment = NULL, legend_title = NULL, ...) {
  
  # If the user has not provided custom year ranges for bins, auto-calculate based on the data
  if (is.null(bin_years)) {
    year_range <- range(as.numeric(all_times))
    bin_years <- cut(as.numeric(all_times), breaks = bins, labels = FALSE)
  } else {
    bin_years <- bin_years
  }
  
  # Ensure plot_times corresponds to the time bins
  if (!is.null(plot_times)) {
    plot_times <- plot_times[plot_times %in% all_times]
    bin_years <- cut(as.numeric(plot_times), breaks = bin_edges, labels = FALSE, include.lowest = TRUE)
  } else {
    plot_times <- all_times
    bin_years <- cut(as.numeric(all_times), breaks = bin_edges, labels = FALSE, include.lowest = TRUE)
  }
  
  spatial_var <- "D_gct"
  
  # Data preparation and spatial variable check
  if (!spatial_var %in% c("D_gct", "R1_gct", "R2_gct", "P1_gct", "P2_gct", "Omega1_gc", "Omega2_gc", "Epsilon1_gct", "Epsilon2_gct")) {
    stop(print("Check `spatial_var` input. Must be one of `D_gct`, `R1_gct`, `R2_gct`, `P1_gct`, `P2_gct`, `Omega1_gc`, `Omega2_gc`, `Epsilon1_gct`, `Epsilon2_gct`."))
  }
  
  # Extract prediction array
  pred_array <- vast_fit$Report[[spatial_var]]
  if (spatial_var == "D_gct") {
    pred_array <- log(pred_array)
  }
  
  spring_indices <- match(spring_times, all_times)
  
  # Binning time_indices into the corresponding years
  bin_means <- lapply(1:max(bin_years, na.rm = TRUE), function(bin_idx) {
    time_indices <- spring_indices[which(bin_years == bin_idx)]
    mean_pred_array <- apply(pred_array[, , time_indices, drop = FALSE], c(1, 2), mean, na.rm = TRUE)
    list(bin_idx = bin_idx, mean_data = mean_pred_array)
  })
  
  # Determine spatial location data
  spat_data <- if (vast_fit$spatial_list$fine_scale == TRUE) vast_fit$extrapolation_list else vast_fit$spatial_list
  locs <- if (vast_fit$spatial_list$fine_scale == TRUE) spat_data$Data_Extrap[which(spat_data$Data_Extrap[, "Include"] > 0), c("Lon", "Lat")] else spat_data$latlon_s[1:spat_data$n_x, ]
  row.names(locs) <- NULL
  CRS_orig <- sp::CRS("+proj=longlat")
  land_sf <- st_crop(land_sf, xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2])
  
  # Loop through the bins, generate plots for each bin
  rasts_out <- vector("list", bins)
  rast_lims <- c(min(pred_array), max(pred_array))
  
  for (bin_idx in 1:bins) {
    mean_data <- bin_means[[bin_idx]]$mean_data
    data_df <- data.frame(locs, z = as.vector(mean_data)) %>%
      distinct(Lon, Lat, z)
    
    pred_df <- na.omit(data.frame("x" = data_df$Lon, "y" = data_df$Lat, "layer" = data_df$z))
    pred_df_interp <- interp(pred_df[, 1], pred_df[, 2], pred_df[, 3],
                             duplicate = "mean", extrap = TRUE,
                             xo = seq(-87.99457, -57.4307, length = 113),
                             yo = seq(22.27352, 48.11657, length = 133))
    
    pred_df_interp_final <- data.frame(expand.grid(x = pred_df_interp$x, y = pred_df_interp$y), z = c(round(pred_df_interp$z, 2)))
    pred_sp <- st_as_sf(pred_df_interp_final, coords = c("x", "y"), crs = CRS_orig)
    pred_df_temp <- pred_sp[which(st_intersects(pred_sp, mask, sparse = FALSE) == TRUE), ]
    coords_keep <- as.data.frame(st_coordinates(pred_df_temp))
    row.names(coords_keep) <- NULL
    pred_df_use <- data.frame(cbind(coords_keep, "z" = as.numeric(pred_df_temp$z)))
    names(pred_df_use) <- c("x", "y", "z")
    
    # Year range for title
    spring_years_in_bin <- as.numeric(spring_times[which(bin_years == bin_idx)])
    if (length(spring_years_in_bin) == 0 || all(is.na(spring_years_in_bin))) {
      bin_year_range <- c(NA, NA)
    } else {
      actual_years <- start_year + (spring_years_in_bin %/% season_increment)
      bin_year_range <- range(actual_years, na.rm = TRUE)
    }
 #   if (!any(is.na(bin_year_range))) {
 #     plot_title <- paste("Years:", bin_year_range[1], "-", bin_year_range[2])
 #   } else {
 #     plot_title <- "Years: NA"
 #   }
    if (bin_idx == 1) {
      plot_title <- "Before Warming"
    } else if (bin_idx == 2) {
      plot_title <- "After Warming"
    } else {
      plot_title <- paste("Years:", bin_year_range[1], "-", bin_year_range[2])  # Fallback if more bins exist
    }   
    # Add custom legend title
    legend_title_final <- ifelse(is.null(legend_title), spatial_var, legend_title)
    
    rasts_out[[bin_idx]] <- ggplot() +
      geom_tile(data = pred_df_use, aes(x = x, y = y, fill = z)) +
      #scale_fill_viridis_c(name = legend_title_final, option = "viridis", na.value = "transparent", limits = rast_lims) +
    #  scale_fill_gradientn(colors = c("darkblue", "lightblue", "orange2", "orangered2"), na.value = "transparent", limits = rast_lims) +
      scale_fill_gradientn(colors = c("darkblue", "lightblue", "orange2"), na.value = "transparent", limits = rast_lims, name=legend_title) +
      annotate("text", x = lab_lon, y = lab_lat, label = plot_title) +
      geom_sf(data = land_sf, fill = land_color, lwd = 0.2, na.rm = TRUE) +
      coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(fill = NA), 
            axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.title = element_blank(), plot.margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "pt"))
  }
  
  if (panel_or_gif == "panel") {
    all_plot <- wrap_plots(rasts_out, ncol = panel_cols, nrow = panel_rows, guides = "collect", 
                           theme(plot.margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "pt")))
    ggsave(filename = paste0(out_dir, "/", nice_category_names, "_", pred_label, "_", spatial_var, ".png"), 
           all_plot, width = 11, height = 8, units = "in")
    return(all_plot)
  } else {
    plot_loop_func <- function(plot_list) {
      for (i in seq_along(plot_list)) {
        plot_use <- plot_list[[i]]
        print(plot_use)
      }
    }
    invisible(save_gif(plot_loop_func(rasts_out), paste0(out_dir, "/", nice_category_names, "_", pred_label, "_", spatial_var, ".gif"), delay = 0.75, progress = FALSE))
  }
}


vast_fit_plot_spatial_kf_binned_new(vast_fit = fit, manual_pred_df=NULL, pred_label="Spring",spatial_var = "D_gct", 
                                    nice_category_names = "Atlantic Halibut_2_27", mask = region_shape, all_times = all_times, 
                                    plot_times = spring_times, land_sf = land_use, xlim = xlim_use, ylim = ylim_use, bin_years = bin_years,
                                    lab_lat = lab_lat, lab_lon = lab_lon, panel_or_gif = "panel", out_dir = out_dir, land_color = "#d9d9d9", 
                                    panel_cols = 2, panel_rows = 1, bins=2, start_year=start_year, season_increment=season_increment,legend_title=legend_title )    
