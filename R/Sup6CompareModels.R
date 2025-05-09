#1, 2: RhoConfig = c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0)
Hali_Null<- readRDS( here::here("2025-04-16_1/Halibut_BC/Null_mod_fit.rds")) 
Hali_Env<- readRDS( here::here("2025-04-16_1/Halibut_BC/EnvOnly_mod_fit.rds")) 
Hali_Sp<- readRDS( here::here("2025-04-16_1/Halibut_BC/EnvOnly_mod_fit.rds")) 

Hali_Null$Report$deviance 
Hali_Env$Report$deviance 
Hali_Null$Report$jnll #4985.97
Hali_Env$Report$jnll #4614.79
Hali_Sp$Report$jnll #4439.483

Hali_Null$parameter_estimates$AIC #9982.94
Hali_Env$parameter_estimates$AIC #9110.729
Hali_Sp$parameter_estimates$AIC #7872.661
#RhoConfig = c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)

#RhoConfig = c("Beta1" = 4, "Beta2" = 4, "Epsilon1" = 0, "Epsilon2" = 0)
Hali_Null<- readRDS( here::here("2025-04-16/Halibut_BC/Null_mod_fit.rds")) 
Hali_Null$Report$deviance 
Hali_Null$Report$jnll #4546.306
Hali_Null$parameter_estimates$AIC #8965.338


#RhoConfig = c("Beta1" = 2, "Beta2" = 4, "Epsilon1" = 0, "Epsilon2" = 0),#Al15: "Beta2" = 4, means AR1 = TRUE, to help account for the space/time autocorrelation witbout including other complexites 
Hali_Null<- readRDS( here::here("2025-04-16_2/Halibut_BC/Null_mod_fit.rds")) 
Hali_Env<- readRDS( here::here("2025-04-16_2/Halibut_BC/EnvOnly_mod_fit.rds")) 
Hali_Sp<- readRDS( here::here("2025-04-16_2/Halibut_BC/Sp_mod_fit.rds")) 

Hali_Null$Report$deviance 
Hali_Env$Report$deviance
Hali_Sp$Report$deviance
Hali_Null$Report$jnll #4812.822
Hali_Env$Report$jnll #4426.258
Hali_Sp$Report$jnll #4263.74

Hali_Null$parameter_estimates$AIC #9457.271
Hali_Env$parameter_estimates$AIC #8644.671
Hali_Sp$parameter_estimates$AIC #7571.378

#env_sp_sets$RhoConfig <- c("Beta1" = 2, "Beta2" = 2, "Epsilon1" = 0, "Epsilon2" = 0) 
#2 Assumes covariate effects evolve smoothly over time. Smooth change over years (e.g., productivity shifts due to climate).
Hali_Null<- readRDS( here::here("2025-04-16_3/Halibut_BC/Null_mod_fit.rds")) 
Hali_Env<- readRDS( here::here("2025-04-16_3/Halibut_BC/EnvOnly_mod_fit.rds")) 
Hali_Sp<- readRDS( here::here("2025-04-16_3/Halibut_BC/Sp_mod_fit.rds")) 

Hali_Null$Report$deviance 
Hali_Env$Report$deviance
Hali_Sp$Report$deviance
Hali_Null$Report$jnll #4808.85
Hali_Env$Report$jnll #4423.787
Hali_Sp$Report$jnll #4298.137

Hali_Null$parameter_estimates$AIC #9564.66
Hali_Env$parameter_estimates$AIC #8632.323
Hali_Sp$parameter_estimates$AIC #7601.035


#env_sp_sets$RhoConfig <- c("Beta1" = 2, "Beta2" = 4, "Epsilon1" = 0, "Epsilon2" = 0)  obsModel(0.7)
#2 Assumes covariate effects evolve smoothly over time. Smooth change over years (e.g., productivity shifts due to climate).
Hali_Null<- readRDS( here::here("2025-04-16_5/Halibut_BC/Null_mod_fit.rds")) 
Hali_Env<- readRDS( here::here("2025-04-16_5/Halibut_BC/EnvOnly_mod_fit.rds")) 
Hali_Sp<- readRDS( here::here("2025-04-16_5/Halibut_BC/Sp_mod_fit.rds")) 

Hali_Null$Report$deviance 
Hali_Env$Report$deviance
Hali_Sp$Report$deviance
Hali_Null$Report$jnll #3530.704
Hali_Env$Report$jnll #3210.156
Hali_Sp$Report$jnll #3346.457

Hali_Null$parameter_estimates$AIC #6880.991
Hali_Env$parameter_estimates$AIC #6185.902
Hali_Sp$parameter_estimates$AIC #5504.174

#c(11,0)#fail, beta2 approaching 0
#c(5,0)#zero inflated negative binomial,fail after null

