#to do: 

# covariate effects plot: this function plots the covariate effects for each linear predictor, 
#the vast_fit$X1_formula has the scaled data in it, but i would like to replace 
#it with the unscaled data, which are also available in the vast_fit$effects$catchability_data_full

#plot_vast_covariate_effects() plots using the scaled data so i need to back transform to plot meningfully 
Hali_Env$effects$covariate_data_full
Hali_Env$effects$catchability_data_full
Hali_Env$X1_formula

get_vast_covariate_effects <- function(vast_fit, params_plot, params_plot_levels, effects_pad_values, nice_category_names, out_dir, ...) {
  if (FALSE) {
    tar_load(vast_fit)
    params_plot <- c(
      "Depth", "SST_seasonal", "BT_seasonal",
      "SS_seasonal", "BS_seasonal"
    )
    params_plot_levels <- 100
    effects_pad_values <- c(1)
    nice_category_names <- nice_category_names
    out_dir <- paste0(res_root, "tables")
    
    vast_fit = mod_comp_res$Fitted_Mod[[1]]
    params_plot = c("index")
    params_plot_levels = 100
    effects_pad_values = c(1)
    nice_category_names = "Capelin"
  }
  assign("covariate_data_full", vast_fit$effects$covariate_data_full, 
         envir = .GlobalEnv)
  assign("catchability_data_full", vast_fit$effects$catchability_data_full, 
         envir = .GlobalEnv)
  x1_rescale <- function(x) plogis(x)
  x2_rescale <- function(x) exp(x)
  
  for (i in seq_along(params_plot)) {
    if (any(grepl(params_plot[i], labels(terms(vast_fit$X1_formula))))) {
      pred_dat_temp_X1 <- data.frame(Effect.fit_model_aja(focal.predictors = params_plot[i], mod = vast_fit, which_formula = "X1", xlevels = params_plot_levels, pad_values = effects_pad_values)) %>%
        mutate(., Lin_pred = "X1")
    }
    
    if(any(grepl(params_plot[i], labels(terms(vast_fit$X2_formula))))){
      pred_dat_temp_X2 <- data.frame(Effect.fit_model_aja(focal.predictors = params_plot[i], mod = vast_fit, which_formula = "X2", xlevels = params_plot_levels, pad_values = effects_pad_values)) %>% 
        mutate(., Lin_pred = "X2")
    }
    
    if(exists("pred_dat_temp_X1") & !exists("pred_dat_temp_X2")){
      pred_dat_out_temp <- pred_dat_temp_X1
      rm(pred_dat_temp_X1)
    }
    if(!exists("pred_dat_temp_X1") & exists("pred_dat_temp_X2")){
      pred_dat_out_temp <- pred_dat_temp_X2
      rm(pred_dat_temp_X2)
    }
    if(exists("pred_dat_temp_X1") & exists("pred_dat_temp_X2")) {
      pred_dat_out_temp <- bind_rows(pred_dat_temp_X1, pred_dat_temp_X2)
      rm(pred_dat_temp_X1)
      rm(pred_dat_temp_X2)
    }
    
    if (i == 1) {
      pred_dat_out <- pred_dat_out_temp
    } else {
      pred_dat_out <- bind_rows(pred_dat_out, pred_dat_out_temp)
    }
  }
  pred_dat_out <- pred_dat_out %>%
    pivot_longer(., !c(fit, se, lower, upper, Lin_pred), names_to = "Covariate", values_to = "Value") %>%
    drop_na()
  saveRDS(pred_dat_out, file = paste(out_dir, "/", nice_category_names, "_covariate_effects.rds", sep = ""))
  return(pred_dat_out)
}

#Kiyomi Edited Functions
get_vast_covariate_effects_kf <- function(vast_fit, params_plot, params_plot_levels, effects_pad_values, nice_category_names, out_dir, ...) {
  assign("covariate_data_full", vast_fit$effects$covariate_data_full, envir = .GlobalEnv)
  assign("catchability_data_full", vast_fit$effects$catchability_data_full, envir = .GlobalEnv)
  
  x1_rescale <- function(x) plogis(x)
  x2_rescale <- function(x) exp(x)
  
  for (i in seq_along(params_plot)) {
    if (any(grepl(params_plot[i], labels(terms(vast_fit$X1_formula))))) {
      pred_dat_temp_X1 <- data.frame(Effect.fit_model_aja(
        focal.predictors = params_plot[i], mod = vast_fit,
        which_formula = "X1", xlevels = params_plot_levels,
        pad_values = effects_pad_values
      )) %>% mutate(Lin_pred = "X1")
    }
    
    if (any(grepl(params_plot[i], labels(terms(vast_fit$X2_formula))))) {
      pred_dat_temp_X2 <- data.frame(Effect.fit_model_aja(
        focal.predictors = params_plot[i], mod = vast_fit,
        which_formula = "X2", xlevels = params_plot_levels,
        pad_values = effects_pad_values
      )) %>% mutate(Lin_pred = "X2")
    }
    
    if (exists("pred_dat_temp_X1") & !exists("pred_dat_temp_X2")) {
      pred_dat_out_temp <- pred_dat_temp_X1
      rm(pred_dat_temp_X1)
    }
    if (!exists("pred_dat_temp_X1") & exists("pred_dat_temp_X2")) {
      pred_dat_out_temp <- pred_dat_temp_X2
      rm(pred_dat_temp_X2)
    }
    if (exists("pred_dat_temp_X1") & exists("pred_dat_temp_X2")) {
      pred_dat_out_temp <- bind_rows(pred_dat_temp_X1, pred_dat_temp_X2)
      rm(pred_dat_temp_X1)
      rm(pred_dat_temp_X2)
    }
    
    if (i == 1) {
      pred_dat_out <- pred_dat_out_temp
    } else {
      pred_dat_out <- bind_rows(pred_dat_out, pred_dat_out_temp)
    }
  }
  
  # Reshape to long format
  pred_dat_out <- pred_dat_out %>%
    pivot_longer(!c(fit, se, lower, upper, Lin_pred), names_to = "Covariate", values_to = "Value") %>%
    drop_na()
  
  # Attempt to back-transform using unscaled values from catchability_data_full
  for (param in params_plot) {
    unscaled_name <- sub("_sc$", "", param)  # assumes '_sc' suffix
    if (unscaled_name %in% names(vast_fit$effects$catchability_data_full)) {
      unscaled_values <- vast_fit$effects$catchability_data_full[[unscaled_name]]
      
      # Get mean and SD used in scaling
      scaled_values <- vast_fit$effects$catchability_data_full[[param]]
      if (!is.null(scaled_values)) {
        mean_val <- mean(unscaled_values, na.rm = TRUE)
        sd_val <- sd(unscaled_values, na.rm = TRUE)
        
        # Back-transform scaled values in prediction data
        pred_dat_out <- pred_dat_out %>%
          mutate(
            Value = ifelse(Covariate == param, Value * sd_val + mean_val, Value),
            Covariate = ifelse(Covariate == param, unscaled_name, Covariate)
          )
      }
    }
  }
  
  saveRDS(pred_dat_out, file = file.path(out_dir, paste0(nice_category_names, "_covariate_effects.rds")))
  return(pred_dat_out)
}
