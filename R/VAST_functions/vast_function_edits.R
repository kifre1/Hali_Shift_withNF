Prepare_User_Extrapolation_Data_Fn<- function (input_grid, strata.limits = NULL, projargs = NA, zone = NA,  flip_around_dateline = TRUE, ...) 
{

    if (FALSE) {
        input_grid = extrap_df
        strata.limits = data.frame("STRATA" = c("All", "NMFS", "DFO"))
        projargs = NA
        zone = NA
        flip_around_dateline = TRUE
    }
    
    if (is.null(strata.limits)) {
        strata.limits = data.frame(STRATA = "All_areas")
    }
    message("Using strata ", strata.limits)
    Data_Extrap <- input_grid
    Area_km2_x = Data_Extrap[, "Area_km2"]
    Tmp = cbind(BEST_LAT_DD = Data_Extrap[, "Lat"], BEST_LON_DD = Data_Extrap[,  "Lon"])
    if ("Depth" %in% colnames(Data_Extrap)) {
        Tmp = cbind(Tmp, BEST_DEPTH_M = Data_Extrap[, "Depth"])
    }
    if("STRATA" %in% colnames(Data_Extrap)){
        Tmp = cbind(Tmp, BEST_STRATA = as.character(Data_Extrap[, "STRATA"]))
    }
    a_el = as.data.frame(matrix(NA, nrow = nrow(Data_Extrap), ncol = nrow(strata.limits), dimnames = list(NULL, strata.limits[, "STRATA"])))
    for (l in 1:ncol(a_el)) {
        a_el[, l] = apply(Tmp, MARGIN = 1, FUN = match_strata_fn, 
            strata_dataframe = strata.limits[l, , drop = FALSE])
        a_el[, l] = ifelse(is.na(a_el[, l]), 0, Area_km2_x)
    }
    tmpUTM = project_coordinates(X = Data_Extrap[, "Lon"], Y = Data_Extrap[, 
        "Lat"], projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline)
    Data_Extrap = cbind(Data_Extrap, Include = 1)
    if (all(c("E_km", "N_km") %in% colnames(Data_Extrap))) {
        Data_Extrap[, c("E_km", "N_km")] = tmpUTM[, c("X", "Y")]
    } else {
        Data_Extrap = cbind(Data_Extrap, E_km = tmpUTM[, "X"], N_km = tmpUTM[, "Y"])
    }
    Return = list(a_el = a_el, Data_Extrap = Data_Extrap, zone = attr(tmpUTM, 
        "zone"), projargs = attr(tmpUTM, "projargs"), flip_around_dateline = flip_around_dateline, 
        Area_km2_x = Area_km2_x)
    return(Return)
}

match_strata_fn<-  function (x, strata_dataframe)  {
    match_latitude_TF = match_longitude_TF = match_depth_TF = match_strata_TF = rep(TRUE, nrow(strata_dataframe))
    if (all(c("south_border", "north_border") %in% names(strata_dataframe))) {
        match_latitude_TF = as.numeric(x["BEST_LAT_DD"]) > strata_dataframe[, "south_border"] & as.numeric(x["BEST_LAT_DD"]) <=  strata_dataframe[, "north_border"]
    }
    if (all(c("west_border", "east_border") %in% names(strata_dataframe))) {
        match_longitude_TF = as.numeric(x["BEST_LON_DD"]) > strata_dataframe[, "west_border"] & as.numeric(x["BEST_LON_DD"]) <=  strata_dataframe[, "east_border"]
    }
    if (all(c("shallow_border", "deep_border") %in% names(strata_dataframe))) {
        match_depth_TF = as.numeric(x["BEST_DEPTH_M"]) > strata_dataframe[, "shallow_border"] & as.numeric(x["BEST_DEPTH_M"]) <= strata_dataframe[, "deep_border"]
    }
    if(names(strata_dataframe) == "STRATA"){
        match_strata_TF = as.character(x["BEST_STRATA"]) == strata_dataframe[, "STRATA"]
    }
    Char = as.character(strata_dataframe[match_latitude_TF & match_longitude_TF &  match_depth_TF & match_strata_TF, "STRATA"])
    return(ifelse(length(Char) == 0, NA, Char))
}

cog_from_dens<- function(vast_fit, proj_dens, proj_ind){

    if (FALSE) {
        # vast_fit = vast_fit
        # proj_dens = dens_df
        # proj_ind = ind_df
    }
    
    # Check units
    units(proj_dens$D_gct) <- units(vast_fit$Report$D_gct)
    units(proj_ind$Index)<- units(vast_fit$Report$Index_ctl)
    
    ## Calculate Index_gctl (D_gct * a_gl)
    # Get extrapolation info
    if (vast_fit$data_list$n_l > 1) {
        extrap_dat <- data.frame(vast_fit$extrapolation_list$Data_Extrap, vast_fit$extrapolation_list$a_el) %>%
            filter(., Region == "All") %>%
            rename(., "a_gl" = "All")
        proj_ind <- proj_ind %>%
            filter(., Region == "All")
    } else {
        extrap_dat <- data.frame(vast_fit$extrapolation_list$Data_Extrap, "a_gl" = vast_fit$extrapolation_list$a_el)
    }
    
    # Join to area and multiply --- this is different!
    if(FALSE){
        check <- data.frame(vast_fit$Report$Index_gctl[, , 1, ]) 
        index_gctl <- proj_dens %>%
            left_join(., extrap_dat) %>%
            drop_na() %>%
            mutate(.,
                "Index_gctl" = D_gct * a_gl
            )
            mutate("RowID" = seq(from = 1, to = nrow(.))) %>%
            pivot_longer(., !RowID, names_to = "Strata", values_to = "Index_gctl") %>%
            filter(., Strata == "Stratum_1")
        str(check)
    }

    index_gctl <- proj_dens %>%
        left_join(., extrap_dat) %>%
        drop_na() %>%
        mutate(.,
            "Index_gctl" = D_gct * a_gl
        )
    
    # Join to overall index ctl
    index_gctl <- index_gctl %>%
        left_join(., proj_ind) %>%
        mutate(., "Index_Prop" = Index_gctl / Index) %>%
        distinct(., Lat, Lon, Time, Sim_Scenario, D_gct, E_km, N_km, Index_gctl, Index, Index_Prop) %>%
        mutate(.,
            "Lon_wt" = E_km * Index_Prop,
            "Lat_wt" = N_km * Index_Prop
        )
    
    # Now get COG measures by multiplying Index_Prob by lon/lat
    cog_out <- index_gctl %>%
        group_by(Time, Sim_Scenario) %>%
        summarize(.,
            "Mean_Lon" = sum(Lon_wt),
            "Mean_Lat" = sum(Lat_wt)
        )
    return(cog_out)
}


eff_area_from_dens<- function(vast_fit, proj_dens, proj_ind){

    if (FALSE) {
        # vast_fit = vast_fit
        # proj_dens = dens_df
        # proj_ind = ind_df
    }
    
    # Check units
    proj_dens <- proj_dens %>%
        drop_units()
    proj_ind <- proj_ind %>%
        drop_units
    
    # One region?
    if(vast_fit$data_list$n_l == 1){
        # Getting mean density
        # Extrapolation info
        extrap_dat <- data.frame(vast_fit$extrapolation_list$Data_Extrap, "a_gl" = vast_fit$extrapolation_list$a_el)
        
        # First, need index_gctl - density multiplied by area of knot
        index_gctl <- proj_dens %>%
            left_join(., extrap_dat, by = c("Lon" = "Lon", "Lat" = "Lat")) %>%
            drop_na() %>%
            mutate(.,
                "Index_gctl" = D_gct * a_gl
            )
        
        # Next, need information on the total index within the area
        index_gctl <- index_gctl %>%
            left_join(., proj_ind)
        
        # Finally, get the mean density and then use that to get the effective area occupied Summarize across knots and get effective area
        eff_area <- index_gctl %>%
            mutate(., "mean_d_ctl_temp" = D_gct * Index_gctl / Index) %>%
            group_by(., Time, Sim_Scenario, Region) %>%
            summarize(.,
                "mean_d_ctl" = sum(mean_d_ctl_temp, na.rm = TRUE)
            ) %>%
            left_join(., proj_ind) %>%
                mutate(., "Eff_Area" = Index / mean_d_ctl) %>%
                dplyr::select(., Time, Sim_Scenario, Region, Eff_Area)
    } else {
        # Multiple regions...
        regs_vec <- as.vector(unlist(vast_fit$settings$strata.limits))
        
        for(i in seq_along(regs_vec)){
            # Extrapolation info
            extrap_dat <- data.frame(vast_fit$extrapolation_list$Data_Extrap, vast_fit$extrapolation_list$a_el) %>%
                filter(., Region == regs_vec[i]) %>%
                rename(., "a_gl" = regs_vec[i])
            proj_ind_use2 <- proj_ind %>%
                filter(., Region == regs_vec[i])
            
            # First, need index_gctl - density multiplied by area of knot
            index_gctl <- proj_dens %>%
                left_join(., extrap_dat, by = c("Lon" = "Lon", "Lat" = "Lat")) %>%
                drop_na() %>%
                mutate(.,
                    "Index_gctl" = D_gct * a_gl
                )
            
            # Next, need information on the total index within the area
            index_gctl <- index_gctl %>%
                left_join(., proj_ind_use2)
                   
            # Finally, get the mean density and then use that to get the effective area occupied Summarize across knots and get effective area
            eff_area_temp <- index_gctl %>%
                mutate(., "mean_d_ctl_temp" = D_gct * Index_gctl / Index) %>%
                group_by(., Time, Sim_Scenario, Region) %>%
                summarize(.,
                    "mean_d_ctl" = sum(mean_d_ctl_temp, na.rm = TRUE)
                ) %>%
                left_join(., proj_ind_use2) %>%
                    mutate(., "Eff_Area" = Index / mean_d_ctl) %>%
                    dplyr::select(., Time, Sim_Scenario, Region, Eff_Area)
                
            if(i == 1){
                eff_area<- eff_area_temp
            } else {
                eff_area<- bind_rows(eff_area, eff_area_temp)
            }
        }
    }
    return(eff_area)
}

summary.sim_results<- function (vast_fit, sim_obj, resp_scale, nice_times = NULL, out_t_scale = NULL, nice_category_names = nice_category_names, climate_scenario = climate_scenario, out_dir, alt_axis = NULL) {
    if (FALSE) {
        # tar_load(vast_fit)
        # tar_load(vast_projections)
        # sim_obj <- vast_projections
        # what <- "Index_ctl"
        # nice_times <- nice_times
        # out_t_scale <- "annual"
        # probs <- c(0.1, 0.5, 0.9)
        # mean_instead <- FALSE
        # nice_category_names <- nice_category_names
        # climate_scenario <- climate_scenario
        # out_dir <- paste0(res_root, "prediction_df")
        
        date_dir <- res_dir_nefsc
        nice_category_names = nice_nefsc_name
        mods<- c("Null_5thpercentile", "Null_mean", "Null_95thpercentile", "Sp", "SpST")
        i = 1
        clim_scenario<- "SSP5_85"
        fit <- readRDS(paste0(date_dir, "EnvOnly_mod_fit.rds"))
        fit <- reload_model(fit)
        uncert_res <- readRDS(file = paste0(date_dir, mods[i], "_none_ProjectionsList.rds"))[[1]]
        
        vast_fit = fit
        sim_obj = uncert_res
        resp_scale = "raw"
        nice_times <- as.Date(c(paste0(seq(from = 1985, to = 2100, by = 1), "-03-16"), paste0(seq(from = 1985, to = 2100, by = 1), "-07-16"), paste0(seq(from = 1985, to = 2100, by = 1), "-10-16")))
        nice_times <- nice_times[order(nice_times)]
        nice_times = nice_times
        out_t_scale = NULL
        nice_category_names = paste0(nice_category_names, mod_desc)
        climate_scenario = paste0(mods[i], "_SSP5_85")
        out_dir = date_dir
        alt_axis = dist_dat_utm

        nefsc_dfo_domain<- st_read(paste0(date_dir, "full_survey_region.shp"))

        cape_res_dir <- "~/GitHub/mixedmod_projections/2023-02-17/Capelin_BC/"
        vast_fit = fit_full
        sim_obj = uncert_res_full
        resp_scale = "raw"
        nice_times = nice_times
        out_t_scale = NULL
        nice_category_names = "Capelin_Random"
        climate_scenario = paste0(gcms[i], "_SpST")
        out_dir = date_dir
    }
    
    time_ind <- seq(from = 1, to = length(nice_times))
    time_labels <- nice_times
    # index_regions_ind <- seq(from = 1, to = vast_fit$data_list$n_l)
    index_regions <- vast_fit$settings$strata.limits$STRATA
    categories_ind <- seq(from = 1, to = vast_fit$data_list$n_c)
    grid_ind <- seq(from = 1, to = vast_fit$data_list$n_g)

    # Getting the index by class and region
    res_out_ind<- data.frame(sapply(sim_obj, FUN = function(x) { x$Index_ctl[,,] }))
    colnames(res_out_ind)<- gsub("X", "Sim_", colnames(res_out_ind))
    res_out_ind$Time<- nice_times
    res_out_ind<- res_out_ind %>%
        pivot_longer(., -Time, names_to = "Sim_Scenario", values_to = "Index") %>%
        mutate(., "Region" = rep(index_regions, each = length(sim_obj)*length(nice_times)))

    # Getting proportional index of total for each of the regions
    if(length(unique(res_out_ind$Region)) > 1){
        sub_regions<- unique(res_out_ind$Region)[-1]

        ind_all<- res_out_ind %>%
            dplyr::filter(., Region == "All") %>%
            dplyr::select(., -Region)

        ind_strata<- res_out_ind %>%
            dplyr::filter(., Region != "All") %>%
            group_by(., Time, Sim_Scenario) %>%
            pivot_wider(., 
                names_from = Region,  
                values_from = c(Index)
            )
        
        # Join to get proportion for each time step/Sim run
        ind_strata<- ind_strata %>%
            left_join(., ind_all)
            
        # Caculate proportion
        res_out_ind_prop<- ind_strata %>%
            mutate(., "Prop_NMFS" = NMFS/Index,
                "Prop_DFO" = DFO/Index) %>%
            dplyr::select(., -c(NMFS, DFO, Index))

    } else {
        res_out_ind_prop<- NA
    }
      
    if(FALSE){
        i = 1
        uncert_res_full <- readRDS(file = paste0(date_dir, gcms[i], "_spST_mod_ProjectionsList.rds"))[[1]]
        B_tr_me = data.frame(sapply(uncert_res_full, FUN=function(x){x$Index_ctl[1,,1]})) %>%
            pivot_longer(., everything(), names_to = "Simulation", values_to = "Index") %>%
            mutate(., "Time" = rep(nice_times, 500))

        identical(res_out_ind$Index, B_tr_me$Index)
    
        B_tr = sapply(uncert_res_full, FUN=function(x){x$Index_ctl[,,1]})
        test<- res_out_ind$Index[res_out_ind$Time == min(res_out_ind$Time)]
        test2<- B_tr[1,]
        identical(res_out_ind$Index[res_out_ind$Time == max(res_out_ind$Time)], B_tr[nrow(B_tr),])
        Y_tq = t(apply(B_tr, MARGIN=1, FUN=quantile, probs=c(0.1,0.5,0.9) )) 

        B_tr_me_summ<- res_out_ind %>%
            group_by(Time) %>%
            summarize(., "Prob_0.1" = quantile(Index, probs = c(0.1)),
            "Prob_0.5" = quantile(Index, probs = 0.5),
            "Prob_0.9" = quantile(Index, probs = 0.9))

        identical(Y_tq[,1], B_tr_me_summ$Prob_0.1)
    }

    # Getting density
    res_out_dens <- data.frame(sapply(sim_obj, FUN = function(x) { x$D_gct[,1,] })) 
    colnames(res_out_dens)<- gsub("X", "Sim_", colnames(res_out_dens))
    res_out_dens$Site<- rep(dimnames(sim_obj[[1]]$D_gct)$Site, times = length(nice_times)) 
    res_out_dens$Time<- rep(nice_times, each = length(unique(res_out_dens$Site)))
    res_out_dens<- res_out_dens %>%
        pivot_longer(., -c(Site, Time), names_to = "Sim_Scenario", values_to = "D_gct") %>%
        mutate(., "Lat" = rep(vast_fit$spatial_list$latlon_g[, "Lat"], length(nice_times)*length(sim_obj)),
        "Lon" = rep(vast_fit$spatial_list$latlon_g[, "Lon"], length(nice_times)*length(sim_obj)))
    
    # Center of gravity
    res_out_cog<- data.frame(sapply(sim_obj, FUN = function(x) { x$mean_Z_ctm[,,] }))
    colnames(res_out_cog)<- gsub("X", "Sim_", colnames(res_out_cog))
    res_out_cog$Time<- rep(nice_times, 2)
    res_out_cog<- res_out_cog %>%
        pivot_longer(., -Time, names_to = "Sim_Scenario", values_to = "Coordinate") %>%
        mutate(., "Coordinate_Name" = rep(c("Eastings", "Northings"), each = length(sim_obj)*length(nice_times))) %>%
        pivot_wider(., id_cols = c(Sim_Scenario, Time), names_from = Coordinate_Name, values_from = Coordinate)
    
    if(FALSE){
        t<- res_out_cog %>%
            group_by(., Time) %>%
            summarize(., "Mean_East" = quantile(Eastings, probs = c(0.5)),
                "Mean_North" = quantile(Northings, probs = c(0.5))) 
        t$Season<- ifelse(format(t$Time, "%m") == "03", "Spring", ifelse(format(t$Time, "%m") == "07", "Summer", "Fall"))
        ggplot() + 
            geom_sf(data = st_transform(nefsc_dfo_domain, crs = vast_fit$extrapolation_list$projargs  )) +
            geom_point(data = t, aes(x = Mean_East, y = Mean_North, color = Time)) + 
            facet_wrap(~Season)
    }
    
    # Add in other axis?
    if(!is.null(alt_axis)){
        res_out_cog<- res_out_cog %>%
            st_as_sf(., coords = c("Eastings", "Northings"), remove = FALSE, crs = st_crs(dist_dat_utm)) %>%
            st_join(., dist_dat_utm, join = st_nearest_feature) %>%
            st_drop_geometry() %>%
            dplyr::select(., Sim_Scenario, Time, Eastings, Northings, lengthfromhere)
    } else {
        res_out_cog$lengthfromhere<- NA
    }
    
    # Getting effective area class and region
    res_out_eff_area<- data.frame(sapply(sim_obj, FUN = function(x) { x$effective_area_ctl[,,] }))
    colnames(res_out_eff_area)<- gsub("X", "Sim_", colnames(res_out_eff_area))
    res_out_eff_area$Time<- nice_times
    res_out_eff_area<- res_out_eff_area %>%
        pivot_longer(., -Time, names_to = "Sim_Scenario", values_to = "Eff_Area") %>%
        mutate(., "Region" = rep(index_regions, each = length(sim_obj)*length(nice_times)))

    # Calculate summaries across all runs
    res_out_ind <- res_out_ind %>%
        group_by(., Time, Region) %>%
        summarise(
            Prob_0.5 = quantile(Index, probs = 0.5, na.rm = TRUE),
            Prob_0.1 = quantile(Index, probs = 0.1, na.rm = TRUE),
            Prob_0.9 = quantile(Index, probs = 0.9, na.rm = TRUE)
        )
    
    if(length(unique(res_out_ind$Region)) > 1){
        res_out_ind_prop <- res_out_ind_prop %>%
            group_by(., Time) %>%
            summarise(
                Prob_0.1_NMFS = quantile(Prop_NMFS, probs = 0.1, na.rm = TRUE),
                Prob_0.1_DFO = quantile(Prop_DFO, probs = 0.1, na.rm = TRUE),
                Prob_0.5_NMFS = quantile(Prop_NMFS, probs = 0.5, na.rm = TRUE),
                Prob_0.5_DFO = quantile(Prop_DFO, probs = 0.5, na.rm = TRUE),
                Prob_0.9_NMFS = quantile(Prop_NMFS, probs = 0.9, na.rm = TRUE),
                Prob_0.9_DFO = quantile(Prop_DFO, probs = 0.9, na.rm = TRUE)
        )
    } else {
        res_out_ind_prop<- NA
    }
    
    res_out_dens <- res_out_dens %>%
        group_by(., Lat, Lon, Time) %>%
        summarise(
            Prob_0.5 = quantile(D_gct, probs = 0.5, na.rm = TRUE),
            Prob_0.1 = quantile(D_gct, probs = 0.1, na.rm = TRUE),
            Prob_0.9 = quantile(D_gct, probs = 0.9, na.rm = TRUE)
        )
    
    res_out_cog<- res_out_cog %>%
        group_by(., Time) %>%
        summarise(
            Lon_Prob_0.5 = quantile(Eastings, probs = 0.5, na.rm = TRUE),
            Lon_Prob_0.1 = quantile(Eastings, probs = 0.1, na.rm = TRUE),
            Lon_Prob_0.9 = quantile(Eastings, probs = 0.9, na.rm = TRUE),
            Lat_Prob_0.5 = quantile(Northings, probs = 0.5, na.rm = TRUE),
            Lat_Prob_0.1 = quantile(Northings, probs = 0.1, na.rm = TRUE),
            Lat_Prob_0.9 = quantile(Northings, probs = 0.9, na.rm = TRUE),
            Length_From_Here_0.1 = quantile(lengthfromhere, probs = 0.1, na.rm = TRUE),
            Length_From_Here_0.5 = quantile(lengthfromhere, probs = 0.5, na.rm = TRUE),
            Length_From_Here_0.9 = quantile(lengthfromhere, probs = 0.9, na.rm = TRUE)
        )
    
    res_out_eff_area <- res_out_eff_area %>%
        group_by(., Time, Region) %>%
        summarise(
            Prob_0.5 = quantile(Eff_Area, probs = 0.5, na.rm = TRUE),
            Prob_0.1 = quantile(Eff_Area, probs = 0.1, na.rm = TRUE),
            Prob_0.9 = quantile(Eff_Area, probs = 0.9, na.rm = TRUE)
        )
    
    res_out<- list("Index" = res_out_ind, "Index_Prop" = res_out_ind_prop, "Dens" = res_out_dens, "COG" = res_out_cog, "EffArea" = res_out_eff_area)
 
    saveRDS(res_out, file = paste0(out_dir, "/", nice_category_names, "_", climate_scenario, ".rds"))
    return(res_out)
}
    