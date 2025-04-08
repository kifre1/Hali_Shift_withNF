#i honestly don't know how, but running this code and then clearing the environment 
#solved the error i was getting from TMB (Error in .Call("TMBconfig", e, as.integer(1), PACKAGE = DLL) : "TMBconfig" not available for .Call() for package "VAST_v14_0_1_TMBad") )

#NOTE* you don't have to wait for the fit to rully run, let it run a couple itterations and then hit stop then clear your environment before proceeding 

#it was found here as an "indes Standardization code"
#https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization

# Load package
library(VAST)

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
example = load_example( data_set="EBS_pollock" )

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 100, 
                          Region = example$Region, 
                          purpose = "index2", 
                          bias.correct = FALSE )

# Run model...it can take a while but you can stop it early and the fix should still work
fit = fit_model( settings = settings, 
                 Lat_i = example$sampling_data[,'Lat'], 
                 Lon_i = example$sampling_data[,'Lon'], 
                 t_i = example$sampling_data[,'Year'], 
                 b_i = example$sampling_data[,'Catch_KG'], 
                 a_i = example$sampling_data[,'AreaSwept_km2'] )

# Plot results
plot( fit )