###############################################################################################################################
##
##  DHARMa utility functions -- from A Gruss
##
###############################################################################################################################

######## Define the "getQuantile" function 
getQuantile <- function ( simulations, observed, integerResponse, method = c( "PIT", "traditional" ) ) {

	method = match.arg( method )
	n = length( observed )
  	if ( nrow( simulations ) != n ) stop( "Wrong dimension of simulations" )
  	nSim = ncol( simulations )
	if ( method == "traditional" ) {
		if ( integerResponse == F ) {
			if( any( duplicated( observed ) ) ) 
				message( paste0( "Model family was recognized or set as continuous, ", 
					"but duplicate values were detected in the response. ", 
					"Consider whether you are fitting an appropriate model." ) )
			values = as.vector( simulations )[duplicated( as.vector( simulations ) )]
      		if ( length( values ) > 0 ) {
        			if ( all( values %% 1 == 0 ) ) {
          				integerResponse = T
          				message( paste0( "Model family was recognized or set as continuous, ", 
						"but duplicate values were detected in the simulation - ", 
						"changing to integer residuals (see ?simulateResiduals for details)" ) )
        			} else {
          				message( paste0( "Duplicate non-integer values found in the simulation. ", 
						"If this is because you are fitting a non-inter valued discrete response model, ", 
						"note that DHARMa does not perform appropriate randomization for such cases." ) )
        			}
			}
		}
		scaledResiduals = rep( NA, n )
    		for ( i in 1 : n ) {
 			if ( integerResponse == T ) {
				scaledResiduals[i] <- DHARMa.ecdf( simulations[i,] + 
					runif( nSim, -0.5, 0.5 ) ) ( observed[i] + runif( 1, -0.5, 0.5 ) )
			} else {
				scaledResiduals[i] <- DHARMa.ecdf( simulations[i,] )( observed[i] )
			}
    		}
	} else {
		scaledResiduals = rep( NA, n ) 
    		for ( i in 1 : n ) {
      		minSim <- mean( simulations[i,] < observed[i] ) 
      		maxSim <- mean( simulations[i,] <= observed[i] ) 
      		if ( minSim == maxSim ) scaledResiduals[i] = minSim
      		else scaledResiduals[i] = runif( 1, minSim, maxSim )
    		}
  	}
  	return( scaledResiduals )

}

######## Define the "getRandomState" function 
getRandomState <- function ( seed = NULL ) {
  
	current = mget( ".Random.seed", envir = .GlobalEnv, ifnotfound = list( NULL ) )[[1]]
	if ( !is.null( seed ) && is.logical( seed ) && seed == F ) {
    			restoreCurrent <- function() { }    
  	} else {
    		restoreCurrent <- function() {
      		if ( is.null( current ) ) 
				rm( ".Random.seed", envir = .GlobalEnv ) 
			else 
				assign( ".Random.seed", current , envir = .GlobalEnv )
    		}    
  	}

  	#### Set seed
  	if ( is.numeric( seed ) ) set.seed( seed )

  	#### Ensure that RNG has been initialized
  	if ( is.null( current ) ) runif( 1 ) 
  
  	randomState = list( seed, state = get( ".Random.seed", globalenv() ), 
		kind = RNGkind(), restoreCurrent = restoreCurrent )  
  	return( randomState )

}

######## Define the "create_DHARMa" function 
create_DHARMa <- function ( simulatedResponse, observedResponse, fittedPredictedResponse = NULL, 
	integerResponse = F, seed = 123, method = c( "PIT", "traditional" ) ) {

		randomState <- getRandomState( seed )
  		on.exit( { randomState$restoreCurrent() } )
  		match.arg( method )
		out = list()
  		out$simulatedResponse = simulatedResponse
  		out$refit = F
  		out$integerResponse = integerResponse
  		out$observedResponse = observedResponse
		if ( !is.matrix( simulatedResponse ) & !is.null( observedResponse ) ) 
			stop( "Either scaled residuals or simulations and observations have to be provided" )
  		if ( ncol( simulatedResponse ) < 2 ) 
			stop( "simulatedResponse with less than 2 simulations provided - cannot calculate residuals on that." )
		if ( ncol( simulatedResponse ) < 10 ) 
			warning( "simulatedResponse with less than 10 simulations provided. This rarely makes sense" )
		out$nObs = length( observedResponse )
		if ( out$nObs < 3 ) 
			stop( "Warning - number of observations < 3 ... this rarely makes sense" )
		if ( ! ( out$nObs == nrow( simulatedResponse ) ) ) 
			stop( "Dimensions of observedResponse and simulatedResponse do not match" )
		out$nSim = ncol( simulatedResponse )
		out$scaledResiduals = getQuantile( simulations = simulatedResponse, observed = observedResponse, 
			integerResponse = integerResponse, method = method )
		if ( is.null( fittedPredictedResponse ) ) {
    			message( "No fitted predicted response provided, using the mean of the simulations" )
    			fittedPredictedResponse = apply( simulatedResponse, 1, mean )
  		}
  		out$fittedPredictedResponse = fittedPredictedResponse
  		out$randomState = randomState
  		class( out ) = "DHARMa"
  		return( out )

}

plot_DHARMa_res<- function(n_samples, fit, response_units =  NULL, out_dir, out_file){

    # Extracting objects from fitted model
    Obj = fit$tmb_list$Obj
    n_g_orig = Obj$env$data$n_g
    Obj$env$data$n_g = 0
    
    # Simulating response
    b_iz = matrix(NA, nrow = length(fit$data_frame$b_i), ncol = n_samples)
    for (zI in 1:n_samples) {
        if (zI %% max(1, floor(n_samples / 10)) == 0) {
            message("  Finished sample ", zI, " of ", n_samples)
        }
        b_iz[, zI] = simulate_data(fit = list(tmb_list = list(Obj = Obj)), type = 1)$b_i
    }
    
    if (any(is.na(b_iz))) {
        stop("Check simulated residuals for NA values")
    }
    
    if (is.null(response_units)) {
        b_iz <- as_units(b_iz, units(fit$data_frame$b_i))
        dharmaRes = create_DHARMa(simulatedResponse = b_iz, observedResponse = fit$data_frame$b_i, fittedPredictedResponse = fit$Report, integer = FALSE)
    } else {
        b_iz <- as_units(b_iz, response_units)
        dharmaRes = create_DHARMa(simulatedResponse = b_iz, observedResponse = as_units(fit$data_frame$b_i, response_units), fittedPredictedResponse = fit$Report, integer = FALSE)
    }
    
    prop_lessthan_i = apply( as.numeric( b_iz ) < outer( as.numeric( fit$data_frame$b_i ), rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
    prop_lessthanorequalto_i = apply(as.numeric(b_iz) <= outer(
        as.numeric(fit$data_frame$b_i),
        rep(1, n_samples)
    ), MARGIN = 1, FUN = mean)
    PIT_i = runif(min = prop_lessthan_i, max = prop_lessthanorequalto_i, n = length(prop_lessthan_i))
    dharmaRes$scaledResiduals = PIT_i
    
    #### Save the DHARMa residuals
    save(dharmaRes, file = paste0(out_dir, out_file, "_dharmaRes.RData"))
    
    #### Produce and save an histogram of DHARMa residuals
    val = dharmaRes$scaledResiduals
    val[val == 0] = -0.01
    val[val == 1] = 1.01
    # val = data.frame(dharmaRes$scaledResiduals)
    # val[val == 0] = -0.01
    # val[val == 1] = 1.01

    # out_hist <- ggplot() +
    #     geom_histogram(data = val, aes(x = dharmaRes.scaledResiduals), breaks = seq(-0.02, 1.02, len = 53))
    jpeg(paste0(out_dir, out_file, "_Histogram_of_DHARMa_residuals.png"), width = 6, height = 7, units = "in", res = 600)
    hist(val,
        breaks = seq(-0.02, 1.02, len = 53), col = c("red", rep("lightgrey", 50), "red"),
        main = "", xlab = "Residuals", cex.main = 2.5
    )
    dev.off()
    
    #### Produce and save a QQ-plot of DHARMa residuals
    jpeg(paste0(out_dir, out_file, "_QQplot_of_DHARMa_residuals.png"), width = 6, height = 7, units = "in", res = 600)
    gap::qqunif(dharmaRes$scaledResiduals, pch = 2, bty = "n", logscale = F, col = "black", cex = 0.6, main = "", cex.main = 2.5)
    dev.off()
}


