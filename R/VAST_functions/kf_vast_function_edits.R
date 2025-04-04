#Kiyomi edit to plot_vast_index_timeseries: plot_vast_index_timeseries_seasonal  facets by "Season", I add season data in after get_vast_index_timeseries (see Abundance_indices$Season in temp.diagnostics)
plot_vast_index_timeseries_seasonal <- function(index_res_df, year_stop = NULL, index_scale, nice_category_names, nice_xlab, nice_ylab, paneling = c("category", "index_region", "none"), color_pal = c("#66c2a5", "#fc8d62", "#8da0cb"), out_dir) {
  if (FALSE) {
    tar_load(biomass_indices)
    index_res_df <- index_res_out
    index_res_df <- biomass_indices
    nice_category_names <- "American lobster"
    nice_xlab <- "Year-Season"
    nice_ylab <- "Biomass index (metric tons)"
    color_pal <- NULL
    paneling <- "none"
    date_breaks <- "5 year"
    out_dir <- paste0(res_root, "plots_maps")
    
    index_res_df <- biomass_indices
    index_scale <- "raw"
    nice_category_names <- nice_category_names
    nice_xlab <- "Year"
    nice_ylab <- "Biomass index (metric tons)"
    paneling <- "none"
    color_pal <- NULL
    out_dir <- here::here("", "Objective 3/Temp_Results")
    
    index_res_df = bio_index_df
    index_scale = "log"
    nice_category_names = "Atlantic cod"
    nice_xlab = "Year-Season"
    nice_ylab = "Biomass index (metric tons)"
    paneling = "none"
    color_pal = NULL
    out_dir = date_dir
  }
  
  if (paneling == "none") {
    if (!is.null(color_pal)) {
      colors_use <- color_pal
    } else {
      color_pal <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")
      colors_use <- color_pal[1:length(unique(index_res_df$Index_Region))]
    }
    
    # Filter based on years to plot
    if (!is.null(year_stop)) {
      index_res_df <- index_res_df %>%
        filter(., Year < year_stop)
    }
    
    # Date axis
    #date_breaks <- seq.Date(from = as.Date(paste(min(index_res_df$Year), "06-15", sep = "-")), to = as.Date(paste(max(index_res_df$Year), "06-15", sep = "-")), by = "year")
    plot_out <- ggplot() +
      geom_errorbar(data = index_res_df, aes(x = Year, ymin = (Index_Estimate - Index_SD), ymax = (Index_Estimate + Index_SD), color = Index_Region, group = Index_Region), alpha = 0.65) +
      geom_point(data = index_res_df, aes(x = Year, y = Index_Estimate, color = Index_Region)) +
      geom_line(data = index_res_df, aes(x = Year, y = Index_Estimate, color = Index_Region)) +
      scale_color_manual(values = colors_use) +
      #scale_x_date(breaks = date_breaks, date_labels = "%Y") +
      xlab({{ nice_xlab }}) +
      ylab({{ nice_ylab }}) +
      ggtitle({{ nice_category_names }}) +
      theme_bw() +
      facet_grid(.~Season)
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  
  # Save and return the plot
  ggsave(plot_out, file = paste(out_dir, "/Abundance_Index_", index_scale, "_", nice_category_names, ".jpg", sep = ""))
  return(plot_out)
}


plot_vast_index_timeseries_subest_season <- function(index_res_df, year_stop = NULL, index_scale, nice_category_names, nice_xlab, nice_ylab, paneling = c("category", "index_region", "none"), color_pal = c("#66c2a5", "#fc8d62", "#8da0cb"), out_dir, subsetseason) {
  if (FALSE) {
    tar_load(biomass_indices)
    index_res_df <- index_res_out
    index_res_df <- biomass_indices
    nice_category_names <- "American lobster"
    nice_xlab <- "Year-Season"
    nice_ylab <- "Biomass index (metric tons)"
    color_pal <- NULL
    paneling <- "none"
    date_breaks <- "5 year"
    out_dir <- paste0(res_root, "plots_maps")
    
    index_res_df <- biomass_indices
    index_scale <- "raw"
    nice_category_names <- nice_category_names
    nice_xlab <- "Year"
    nice_ylab <- "Biomass index (metric tons)"
    paneling <- "none"
    color_pal <- NULL
    out_dir <- here::here("", "Objective 3/Temp_Results")
    
    index_res_df = bio_index_df
    index_scale = "log"
    nice_category_names = "Atlantic cod"
    nice_xlab = "Year-Season"
    nice_ylab = "Biomass index (metric tons)"
    paneling = "none"
    color_pal = NULL
    out_dir = date_dir
  }
  index_res_df<-subset(index_res_df,index_res_df$Season==subsetseason)
  
  if (paneling == "none") {
    if (!is.null(color_pal)) {
      colors_use <- color_pal
    } else {
      color_pal <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")
      colors_use <- color_pal[1:length(unique(index_res_df$Index_Region))]
    }
    
    # Filter based on years to plot
    if (!is.null(year_stop)) {
      index_res_df <- index_res_df %>%
        filter(., Year < year_stop)
    }
    
    # Date axis
    #date_breaks <- seq.Date(from = as.Date(paste(min(index_res_df$Year), "06-15", sep = "-")), to = as.Date(paste(max(index_res_df$Year), "06-15", sep = "-")), by = "year")
    plot_out <- ggplot() +
      geom_errorbar(data = index_res_df, aes(x = Year, ymin = (Index_Estimate - Index_SD), ymax = (Index_Estimate + Index_SD), color = Index_Region, group = Index_Region), alpha = 0.65) +
      geom_point(data = index_res_df, aes(x = Year, y = Index_Estimate, color = Index_Region)) +
      geom_line(data = index_res_df, aes(x = Year, y = Index_Estimate, color = Index_Region)) +
      scale_color_manual(values = colors_use) +
      #scale_x_date(breaks = date_breaks, date_labels = "%Y") +
      xlab({{ nice_xlab }}) +
      ylab({{ nice_ylab }}) +
      ggtitle({{ nice_category_names }}) +
      theme_bw() +
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  
  # Save and return the plot
  ggsave(plot_out, file = paste(out_dir, "/Abundance_Index_", index_scale, "_", nice_category_names, ".jpg", sep = ""))
  return(plot_out)
}

plot_vast_projected_index <- function(vast_projections, year_stop = NULL, index_scale, nice_category_names = nice_category_names, climate_scenario = climate_scenario, nice_times = nice_times, region_keep = c("DFO", "NMFS", "GoM", "SNE_and_MAB"), nice_xlab, nice_ylab, paneling = c("category", "index_region", "none"), color_pal = c("#66c2a5", "#fc8d62", "#8da0cb"), out_dir) {
  if (FALSE) {
    tar_load(vast_projection_summ_index)
    vast_projections <- vast_projection_summ_index
    vast_projections <- res_out
    year_stop <- NULL
    index_scale <- "log"
    nice_category_names <- nice_category_names
    climate_scenario <- climate_scenario
    nice_times <- nice_times
    nice_xlab <- "Year-Season"
    nice_ylab <- "Log Biomass index (metric tons)"
    color_pal <- NULL
    paneling <- "none"
    date_breaks <- "5 year"
    out_dir <- paste0(res_root, "plots_maps")
    
    vast_projections <- res_out
    region_keep<- NULL
    year_stop <- NULL
    index_scale <- "log"
    nice_category_names <- "Atlantic cod"
    climate_scenario <- "SSP5_85_Mean"
    nice_times <- nice_times
    nice_xlab <- "time"
    nice_ylab<- "Log Biomass index (metric tons)"
    color_pal = c("#66c2a5", "#fc8d62", "#8da0cb")
    paneling <- "none"
    date_breaks <- "10 year"
    out_dir = date_dir
  }
  
  if (paneling == "none") {
    if (!is.null(color_pal)) {
      colors_use <- color_pal
    } else {
      color_pal <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")
      colors_use <- color_pal[1:length(unique(vast_projections$Region))]
    }
    
    # Filter based on years to plot
    if (!is.null(year_stop)) {
      index_res_df <- vast_projections %>%
        filter(., Time < year_stop)
    } else {
      index_res_df <- vast_projections
    }
    
    if (!is.null(region_keep)) {
      index_res_df <- index_res_df %>%
        dplyr::filter(., Region %in% region_keep)
      index_res_df$Region <- factor(index_res_df$Region, levels = region_keep)
    }
    
    if (index_scale == "log") {
      index_res_df <- index_res_df %>%
        mutate_at(., c("Prob_0.1", "Prob_0.5", "Prob_0.9"), log)
    }
    
    # Date axis
    date_breaks <- seq.Date(from = as.Date(paste(min(index_res_df$Time), "06-15", sep = "-")), to = as.Date(paste(max(index_res_df$Time), "06-15", sep = "-")), by = date_breaks)
    
    plot_out <- ggplot(data = index_res_df, aes(x = Time, ymin = Prob_0.1, ymax = Prob_0.9, fill = Region)) +
      geom_ribbon(alpha = 0.75) +
      geom_line(data = index_res_df, aes(x = Time, y = Prob_0.5, color = Region)) +
      scale_color_manual(values = colors_use) +
      scale_fill_manual(values = colors_use) +
      # scale_x_date(breaks = date_breaks, date_labels = "%Y") +
      xlab({{ nice_xlab }}) +
      ylab({{ nice_ylab }}) +
      ggtitle({{ nice_category_names }}) +
      theme_bw() +
      theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(~Region)
  }
  
  # Save and return the plot
  ggsave(plot_out, file = paste(out_dir, "/Biomass_Index_", index_scale, "_", nice_category_names, "_", climate_scenario, ".jpg", sep = ""))
  return(plot_out)
}



#' @title Plot VAST model spatial and spatio-temporal surfaces
#'
#' @description Creates either a panel plot or a gif of VAST model spatial or spatio-temporal parameter surfaces or derived quantities
#'
#' @param vast_fit = A VAST `fit_model` object
#' @param manaul_pred_df = Data frame with location, time and predicted value information OR NULL if using vast_fit
#' @param spatial_var = An estimated spatial coefficient or predicted value or NULL. Currently works for `D_gct`, `R1_gct`, `R2_gct`, `P1_gct`, `P2_gct`, `Omega1_gc`, `Omega2_gc`, `Epsilon1_gct`, `Epsilon2_gct`.
#' @param nice_category_names = A character string to define species/model run and used in naming the output prediction file.
#' @param pred_label = Explain
#' @param all_times = A vector of all of the unique time steps available from the VAST fitted model
#' @param plot_times = Either NULL to make a plot for each time in `all_times` or a vector of all of the times to plot, which must be a subset of `all_times`
#' @param land_sf = Land sf object
#' @param xlim = A two element vector with the min and max longitudes
#' @param ylim = A two element vector with the min and max latitudes
#' @param panel_or_gif = A character string of either "panel" or "gif" indicating how the multiple plots across time steps should be displayed
#' @param out_dir = Output directory to save the panel plot or gif
#'
#' @return A VAST fit_model object, with the inputs and and outputs, including parameter estimates, extrapolation gid info, spatial list info, data info, and TMB info.
#'
#' @export

vast_fit_plot_spatial_kf <- function(vast_fit, manual_pred_df, pred_grid, spatial_var, nice_category_names, pred_label, 
                                     climate_scenario = "", mask, all_times = all_times, plot_times = NULL, land_sf, xlim, ylim, 
                                     lab_lat = 33.75, lab_lon = -67.5, panel_or_gif = "gif", out_dir, land_color = "#d9d9d9", 
                                     panel_cols = NULL, panel_rows = NULL, ...) {
  if (FALSE) {
    tar_load(vast_fit)
    template <- raster("~/GitHub/sdm_workflow/scratch/aja/TargetsSDM/data/supporting/HighResTemplate.grd")
    tar_load(vast_seasonal_data)
    all_times <- as.character(levels(vast_seasonal_data$VAST_YEAR_SEASON))
    plot_times <- NULL
    tar_load(land_sf)
    tar_load(region_shapefile)
    mask <- region_shapefile
    land_color <- "#d9d9d9"
    res_data_path <- "~/Box/RES_Data/"
    xlim <- c(-78.5, -56)
    ylim <- c(35, 48)
    panel_or_gif <- "gif"
    panel_cols <- NULL
    panel_rows <- NULL
    grid_space_use <- 10
    lab_lat <- 36
    lab_lon <- -60
    spatial_var <- "D_gct"
    
    # Manual
    vast_fit <- readRDS("/Users/aallyn/Library/CloudStorage/Box-Box/Mills Lab/Projects/sdm_workflow/targets_output/mod_fits/American_lobster_STnoRW_fitted_vast.rds")
    manual_pred_df <- read.csv(paste0(res_root, "prediction_df/American_lobster_SSP5_85_mean_projections.csv"))
    tar_load(region_shapefile)
    mask <- region_shapefile
    nice_category_names <- "American_lobster"
    pred_label <- "SSP5_85_mean"
    all_times <- unique(manual_pred_df$Real_Date)
    plot_times <- NULL
    tar_load(land_sf)
    xlim <- c(-78.5, -55)
    ylim <- c(34.5, 48.25)
    panel_or_gif <- "gif"
    panel_cols <- NULL
    panel_rows <- NULL
    grid_space_utm <- 25000
    lab_lat <- 36
    lab_lon <- -67.5
    out_dir <- paste0(res_root, "plots_maps")
    land_color <- "#d9d9d9"
    
    # From ModelDiagnostics_PostFitProjections
    vast_fit = mod_use
    manual_pred_df = NULL
    pred_grid = NULL
    spatial_var = "Epsilon1_gct"
    nice_category_names = "Atlantic Cod - Spatial 1"
    pred_label = ""
    mask = mask_use
    all_times = nice_times
    plot_times = NULL
    land_sf = land_use
    xlim = c(-78.5, -55)
    ylim = c(34.5, 48.25)
    lab_lat = 33.75
    lab_lon = -67.5
    panel_or_gif = "panel"
    out_dir = "~/Desktop/"
    land_color = "#d9d9d9"
    panel_cols = 10
    panel_rows = 11
  }
  
  if (is.null(manual_pred_df)) {
    # Plotting at spatial knots...
    # First check the spatial_var, only a certain subset are being used...
    if (!spatial_var %in% c("D_gct", "R1_gct", "R2_gct", "P1_gct", "P2_gct", "Omega1_gc", "Omega2_gc", "Epsilon1_gct", "Epsilon2_gct")) {
      stop(print("Check `spatial_var` input. Currently must be one of `D_gct`, `R1_gct`, `R2_gct`, `P1_gct`, `P2_gct`, `Omega1_gc`, `Omega2_gc`, `Epsilon1_gct`, `Epsilon2_gct`."))
    }
    
    # Getting prediction array
    pred_array <- vast_fit$Report[[{{ spatial_var }}]]
    if (spatial_var == "D_gct") {
      pred_array <- log(pred_array)
      pred_array<- drop_units(pred_array)
    }
    # Indexes of the third dimension of pred_array that correspond to plot_times
    time_indices <- which(all_times %in% plot_times)#kiyomi edit
    
    # Subset pred_array based on time_indices
    pred_array <- pred_array[, , time_indices, drop = FALSE]#kiyomi edit
    
    # Getting time info
    if (!is.null(plot_times)) {
      #plot_times <- all_times[which(all_times) %in% plot_times]
      plot_times <- all_times[all_times %in% plot_times]#kiyomi edit
    } else {
      # plot_times <- all_times
      plot_times <- plot_times
    }
    
    # Getting spatial information
    if (vast_fit$spatial_list$fine_scale == TRUE) {
      spat_data <- vast_fit$extrapolation_list
      locs <- data.frame(spat_data$Data_Extrap[which(spat_data$Data_Extrap[, "Include"] > 0), c("Lon", "Lat")]) 
    } else {
      spat_data <- vast_fit$spatial_list
      locs <- spat_data$latlon_s[1:spat_data$n_x, ]
    }
    
    
    CRS_orig <- sp::CRS("+proj=longlat")
    CRS_proj <- sp::CRS(spat_data$projargs)
    land_sf <- st_crop(land_sf, xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2])
    
    # Looping through...
    rasts_out <- vector("list", dim(pred_array)[length(dim(pred_array))])
    rasts_range <- pred_array
    # rast_lims_min <- ifelse(spatial_var %in% c("D_gct", "R1_gct", "R2_gct", "P1_gct", "P2_gct"), 0, min(rasts_range))
    # rast_lims_max <- ifelse(spatial_var %in% c("D_gct", "R1_gct", "R2_gct", "P1_gct", "P2_gct"), round(max(rasts_range) + 0.0000001, 2), max(rasts_range))
    rast_lims <- c(min(rasts_range), max(rasts_range))
    
    if (length(dim(pred_array)) == 2) {
      data_df <- data.frame(locs, z = pred_array) %>%
        distinct(Lon, Lat, X1)
      names(data_df)[3]<- "z"
      
      # Interpolation
      pred_df <- na.omit(data.frame("x" = data_df$Lon, "y" = data_df$Lat, "layer" = data_df$z))
      pred_df_interp <- interp(pred_df[, 1], pred_df[, 2], pred_df[, 3],
                               duplicate = "mean", extrap = TRUE,
                               xo = seq(-87.99457, -57.4307, length = 113),
                               yo = seq(22.27352, 48.11657, length = 113)
      )
      pred_df_interp_final <- data.frame(expand.grid(x = pred_df_interp$x, y = pred_df_interp$y), z = c(round(pred_df_interp$z, 2)))
      pred_sp <- st_as_sf(pred_df_interp_final, coords = c("x", "y"), crs = CRS_orig)
      
      pred_df_temp <- pred_sp[which(st_intersects(pred_sp, mask, sparse = FALSE) == TRUE), ]
      coords_keep <- as.data.frame(st_coordinates(pred_df_temp))
      row.names(coords_keep) <- NULL
      pred_df_use <- data.frame(cbind(coords_keep, "z" = as.numeric(pred_df_temp$z)))
      names(pred_df_use) <- c("x", "y", "z")
      
      plot_out <- ggplot() +
        geom_tile(data = pred_df_use, aes(x = x, y = y, fill = z)) +
        scale_fill_viridis_c(name = spatial_var, option = "viridis", na.value = "transparent", limits = rast_lims) +
        annotate("text", x = lab_lon, y = lab_lat, label = spatial_var) +
        geom_sf(data = land_sf, fill = land_color, lwd = 0.2, na.rm = TRUE) +
        coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
        theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(fill = NA), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), plot.margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "pt"))
      
      ggsave(filename = paste(out_dir, "/", nice_category_names, "_", climate_scenario, "_", spatial_var, ".png", sep = ""), plot_out, width = 11, height = 8, units = "in")
      return(plot_out)
    } else {
      for (tI in 1:dim(pred_array)[3]) {
        data_df <- data.frame(locs, z = pred_array[, 1, tI]) %>%
          distinct(Lon, Lat, z)
        
        # Interpolation
        pred_df <- na.omit(data.frame("x" = data_df$Lon, "y" = data_df$Lat, "layer" = data_df$z))
        pred_df_interp <- interp(pred_df[, 1], pred_df[, 2], pred_df[, 3],
                                 duplicate = "mean", extrap = TRUE,
                                 xo = seq(-87.99457, -57.4307, length = 113),
                                 yo = seq(22.27352, 48.11657, length = 133)
        )
        pred_df_interp_final <- data.frame(expand.grid(x = pred_df_interp$x, y = pred_df_interp$y), z = c(round(pred_df_interp$z, 2)))
        pred_sp <- st_as_sf(pred_df_interp_final, coords = c("x", "y"), crs = CRS_orig)
        
        pred_df_temp <- pred_sp[which(st_intersects(pred_sp, mask, sparse = FALSE) == TRUE), ]
        coords_keep <- as.data.frame(st_coordinates(pred_df_temp))
        row.names(coords_keep) <- NULL
        pred_df_use <- data.frame(cbind(coords_keep, "z" = as.numeric(pred_df_temp$z)))
        names(pred_df_use) <- c("x", "y", "z")
        
        # krig_mod <- Krig(data.frame("x" = data_df$Lon, "y" = data_df$Lat), data_df$z)
        # pred_df_interp <- as.data.frame(interpolate(pred_grid, krig_mod), xy = TRUE)
        # pred_sp <- st_as_sf(pred_df_interp, coords = c("x", "y"), crs = CRS_orig)
        
        # pred_df_temp <- pred_sp[which(st_intersects(pred_sp, mask, sparse = FALSE) == TRUE), ]
        # pred_df_use <- data.frame(st_coordinates(pred_df_temp), "z" = as.numeric(pred_df_temp$layer))
        # names(pred_df_use) <- c("x", "y", "z")
        
        time_plot_use <- plot_times[tI]
        
        rasts_out[[tI]] <- ggplot() +
          geom_tile(data = pred_df_use, aes(x = x, y = y, fill = z)) +
          scale_fill_viridis_c(name = spatial_var, option = "viridis", na.value = "transparent", limits = rast_lims) +
          annotate("text", x = lab_lon, y = lab_lat, label = time_plot_use) +
          geom_sf(data = land_sf, fill = land_color, lwd = 0.2, na.rm = TRUE) +
          coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
          theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(fill = NA), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), plot.margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "pt"))
      }
      if (panel_or_gif == "panel") {
        # Panel plot
        all_plot <- wrap_plots(rasts_out, ncol = panel_cols, nrow = panel_rows, guides = "collect", theme(plot.margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "pt")))
        ggsave(filename = paste0(out_dir, "/", nice_category_names, "_", pred_label, "_", spatial_var, ".png"), all_plot, width = 11, height = 8, units = "in")
        return(all_plot)
      } else {
        # Make a gif
        plot_loop_func <- function(plot_list) {
          for (i in seq_along(plot_list)) {
            plot_use <- plot_list[[i]]
            print(plot_use)
          }
        }
        invisible(save_gif(plot_loop_func(rasts_out), paste0(out_dir, "/", nice_category_names, "_", pred_label, "_", spatial_var, ".gif"), delay = 0.75, progress = FALSE))
      }
    }
  } else {
    # Using manual post fit predictions data frame
    # Getting time info
    if (!is.null(plot_times)) {
      #plot_times <- all_times[which(all_times) %in% plot_times]
      plot_times <- all_times[all_times %in% plot_times]#kiyomi edit
    } else {
      #plot_times <- all_times
      plot_times <- plot_times#kiyomi edit
      
    }
    
    # Getting spatial information
    land_sf <- st_transform(land_sf, crs = 32619)
    
    # Converting limits...
    plot_coords <- st_sfc(st_point(c(xlim[1], ylim[1])), st_point(c(xlim[2], ylim[2])), crs = 4326) %>%
      st_transform(., crs = 32619) %>%
      st_coordinates(.)
    
    lab_coords <- st_sfc(st_point(c(lab_lon[1], lab_lat[1])), crs = 4326) %>%
      st_transform(., crs = 32619) %>%
      st_coordinates(.)
    
    # We want a prediction grid...
    mask_utm <- vast_mesh_to_sf(vast_fit)$triangles %>%
      st_union() %>%
      st_transform(., crs = 32619)
    pred_grid <- mask_utm %>%
      st_make_grid(., cellsize = grid_space_utm, what = "polygons", square = TRUE)
    
    pred_grid2 <- pred_grid %>%
      st_intersection(., mask_utm)
    
    # Raster storage and limits
    rasts_out <- vector("list", length(plot_times))
    rast_lims <- c(0, max(log((625 * manual_pred_df$Dens) + 1)))
    
    for (tI in seq_along(plot_times)) {
      time_plot_use <- plot_times[tI]
      
      plot_label <- paste(format(as.Date(time_plot_use), "%Y"), ifelse(grepl("03-16", time_plot_use), "SPRING", ifelse(grepl("07-16", time_plot_use), "SUMMER", "FALL")), sep = " ")
      
      data_df <- manual_pred_df %>%
        dplyr::filter(., Real_Date == time_plot_use) %>%
        st_as_sf(., coords = c("Lon", "Lat"), crs = 4326, remove = FALSE) %>%
        st_transform(., crs = 32619) %>%
        dplyr::select(., Lon, Lat, Dens, geometry)
      
      pred_idw <- idw(Dens ~ 1, data_df, pred_grid2) %>%
        mutate(.,
               "Biomass" = var1.pred * 625,
               "Log_Biomass" = log(Biomass + 1)
        )
      
      plot_out <- ggplot() +
        geom_sf(data = pred_idw, aes(fill = Log_Biomass, color = Log_Biomass)) +
        scale_fill_viridis_c(name = "Log biomass (kg)", option = "viridis", na.value = "transparent", limits = rast_lims) +
        scale_color_viridis_c(name = "Log biomass (kg)", option = "viridis", na.value = "transparent", limits = rast_lims) +
        annotate("text", x = lab_lon, y = lab_lat, label = plot_label) +
        geom_sf(data = land_sf, fill = land_color, lwd = 0.2, na.rm = TRUE) +
        coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, crs = 4326) +
        theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(fill = NA), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), plot.margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "pt"))
    }
    if (panel_or_gif == "panel") {
      # Panel plot
      all_plot <- wrap_plots(rasts_out, ncol = panel_cols, nrow = panel_rows, guides = "collect", theme(plot.margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "pt")))
      ggsave(filename = paste0(out_dir, "/", nice_category_names, "_", pred_label, "_", "density.png"), all_plot, width = 11, height = 8, units = "in")
      return(all_plot)
    } else {
      # Make a gif
      plot_loop_func <- function(plot_list) {
        for (i in seq_along(plot_list)) {
          plot_use <- plot_list[[i]]
          print(plot_use)
        }
      }
      invisible(save_gif(plot_loop_func(rasts_out), paste0(out_dir, "/", nice_category_names, "_", pred_label, "_", "density.gif"), delay = 0.75, progress = FALSE))
    }
  }
}


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
    if (!any(is.na(bin_year_range))) {
      plot_title <- paste("Years:", bin_year_range[1], "-", bin_year_range[2])
    } else {
      plot_title <- "Years: NA"
    }
    
    # Add custom legend title
    legend_title_final <- ifelse(is.null(legend_title), spatial_var, legend_title)
    
    rasts_out[[bin_idx]] <- ggplot() +
      geom_tile(data = pred_df_use, aes(x = x, y = y, fill = z)) +
      scale_fill_viridis_c(name = legend_title_final, option = "viridis", na.value = "transparent", limits = rast_lims) +
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