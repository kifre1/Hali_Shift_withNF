#Create range edge data. 
#see https://rdrr.io/github/James-Thorson/VAST/man/plot_range_edge.html for reference 
#he used functions that i had to break down because i ran into TMB versioning issues
#Weighted quantile of the coordinate values, where weights are the density estimates
#5th percentile (trailing edge), 95th percentile (leading edge) of the species' spatial distribution)
#For each subsequent year, the edge of the distribution is compared to the same quantile location in the first year.
#o look at absolute shifts over time relative to the baseline year

#if you get a "TMBconfig"error, run Sup3TMB_Error_fix.R first 

library(TMB)
library(dplyr)
library(ggplot2)
#Step1

out_dir<- here::here("2025-04-23/Output/Shift_Indicators")
fit<- readRDS( here::here("2025-04-23/Halibut_BC/SpST_mod_fit.rds")) 

#parameters in range_Edge()
fit.model<-fit
strata_names<-NULL
category_names<-NULL
quantiles<-c(0.05,0.5,0.95) 
n_samples<-100
interval_width<-1
width<-NULL
height<-NULL
calculate_relative_to_average<-FALSE  #calculate relative to first year

#unpacking the data
Sdreport<-fit.model$parameter_estimates$SD #I believe this is where we get the error bars 
Report<-fit.model$Report # this has all of the estimates (count/km2)
Reportcopy<-fit.model$Report # this has all of the estimates (count/km2)
TmbData<-fit.model$data_list #this has a bunch of the parameters 
Obj<-fit.model$tmb_list$Obj# par has all of the parameters and their weights?
year_labels<-fit.model$year_labels #0-104
#I want to be able to select for season
years_Spring <- as.character(seq(0, 101, by = 3))
years_Summer <- as.character(seq(1, 101, by = 3))
years_Fall <- as.character(seq(2, 101, by = 3))

TMB_Z_gm<-TmbData$Z_gm # range edges: E_km & N_km (657X2 events, not sure why there are duplicate rows on everything (1, and 1.1...)...there are 1583 sites so it must be grouping in pairs)...only one set (no time var...so this would be the median across all years: calculate_relative_to_average=FALSE
TMB::summary.sdreport(Sdreport)
"ln_Index_ctl" %in% rownames(TMB::summary.sdreport(Sdreport))#T: this is the name of my parameter   
"ln_Index_cyl" %in% rownames(TMB::summary.sdreport(Sdreport))#F; ok
"ln_Index_tl" %in% rownames(TMB::summary.sdreport(Sdreport))#F: ok
#py_parameter has estimates and standard errors on 1:315 indices...315/3 = 105, so are there three per timeframe? ...need to figure out what these correspond to
My_Parameter <- summary.sdreport(Sdreport)[rownames(summary.sdreport(Sdreport)) == "ln_Index_ctl", , drop = FALSE]


#this is how we want to subset it
Year_Set<-1:TmbData$n_t #1:105...but i want these seasonally, next
Years2Include_Spring<-years_Spring    #recall 
strata_names<-1:TmbData$n_l#not sure if stata will be useful here, but taking anyway 
strata_named <- fit.model$settings$strata.limits$STRATA[strata_names]#THESE are the actual strata names

category_names <- 1:TmbData$n_c #only halibut here
m_labels <- colnames(TmbData$Z_gm) #recall we have E_km & N_km from TMB_Z_gm  ##MARKER 1, m_labels is km_E, km_N...this is the order they appear in in TmbData

# create D_gctr: the sample_variable() function doesn't work so i have broken it down to remove the TMB conflict----
#D_gctr <- sample_variable( Sdreport=Sdreport, Obj=Obj, variable_name="D_gct", n_samples=n_samples )

#these are the parameters and i am not able to get them the way he suggests so will be experimenting 
#'Sdreport: TMB output from \code{TMB::sdreport(Obj)}
#'variable_name name of variable available in report using \code{Obj$report()} or parameters using \code{Obj$env$parList()}
#'n_samples number of samples from the joint predictive distribution for fixed and random effects.  Default is 100, which is slow.
#'seed integer used to set random-number seed when sampling variables, as passed to \code{set.seed(.)}
#'sample_fixed whether to sample fixed and random effects, \code{sample_fixed=TRUE} as by default, or just sample random effects, \code{sample_fixed=FALSE}

#define the parameters that don't already exist
#variable_name<-"D_gct"
variable_name<-"Index_gctl" #switched with D_gct because this one has the proper bias corrected estimates, means that we have 1 (so we need to slice out stratum1 which represents ALL data)
seed <- 12345
sample_fixed<-TRUE

"jointPrecision" %in% names(Sdreport)#TRUE:  rmvnorm_prec() requires this
#if( !("jointPrecision" %in% names(Sdreport)) ){
#  stop("jointPrecision not present in Sdreport; please re-run with `getJointPrecision=TRUE`")
#}
# Combine Report and ParHat
Report <- Obj$report()# ERRORS this is where the TMB config error starts- run sup3_TMB_Error_fix, clear the env and start over  if you get an error 
#not sure what the funciton does..it converts the reportenv object into a list 
#function (par = last.par) 
#{
#  f(par, order = 0, type = "double")
#  as.list(reportenv)
#}

#Maybe it is as simple as just getting the report form the fit model?
#Report<-fit.model$Report#already read in above but this won't hurt
#Reportcopy<-fit.model$Report
ParHat <- Obj$env$parList()
ParHatCopy <- Obj$env$parList()
Intersect <- intersect(names(Report), names(ParHat))
all.equal(Report[Intersect],ParHat[Intersect])#TRUE
#if( isFALSE(all.equal(Report[Intersect],ParHat[Intersect])) ){
#  stop("Duplicate entries in `Obj$report()` and `Obj$env$parList()` are not identical when calling `sample_variable`")
#}
Output <- c( Report, ParHat)
OutputCopy<-Output
# Check that variable_name is available
variable_name %in% names(Output)#TRUE: it was brought over from the Report
#if( !("variable_name" %in% names(Output)) ){
#  stop( variable_name, " not found in `Obj$report()` or `Obj$env$parList()`; please choose check your requested variable name from available list: ", paste(names(Output),collapse=", ") )
#}

#### Local function
# Sample from GMRF using sparse precision
rmvnorm_prec <- function(mu, prec, n.sims, seed) {
  set.seed(seed)
  z <- matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
  L <- Matrix::Cholesky(prec, super=TRUE)
  z <- Matrix::solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
  z <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
  z <- as.matrix(z)
  return(mu + z)
}
# Sample from joint distribution
if( sample_fixed==TRUE ){
  u_zr = rmvnorm_prec( mu=Obj$env$last.par.best, prec=Sdreport$jointPrecision, n.sims=n_samples, seed=seed)
  # apply( u_zr, MARGIN=2, FUN=function(vec){sum(abs(vec)==Inf)})
  # u_zr[-Obj$env$random,1]
}else{
  u_zr = Obj$env$last.par.best %o% rep(1, n_samples)
  MC = Obj$env$MC( keep=TRUE, n=n_samples, antithetic=FALSE )
  u_zr[Obj$env$random,] = attr(MC, "samples")
}
#now i have u_zr which is a matrix which for each model parameter 96384, I have 100 values (samples)

# Extract variable (Index_gctl) for each sample
for( rI in 1:n_samples ){
  #this is a counter to see the progress from 1:n_samples, gives a warnign every 10 minutes
  if( rI%%max(1,floor(n_samples/10)) == 0 ){
    message( "  Finished sample ", rI, " of ",n_samples )
  }
  #rI<-2#temporaty, lets just see if we can run the loop once
  Report = Obj$report( par=u_zr[,rI] )#doesnt work, but perhaps we just need to list u_zr[,rI], next
  #Report<-fit.model$Report#this could potential be a problem becasue i didn't change anythign
  ParHat<- Obj$env$parList( x=u_zr[,rI][Obj$env$lfixed()], par=u_zr[,rI] )
  if( isFALSE(all.equal(Report[Intersect],ParHat[Intersect])) ){
    stop("Duplicate entries in `Obj$report()` and `Obj$env$parList()` are not identical when calling `sample_variable`")
  }
  Output = c( Report, ParHat)
  # Var = Output[["D_gct"]]
  Var = Output[["Index_gctl"]]
  if(is.vector(Var)) Var = as.array(Var)
  if(rI==1) Var_zr = Var
  if(rI>=2){
    # Var_zr = abind(Var_zr, Var, along = length(dim(Var)) + 1)
    Var_zr = abind::abind(Var_zr, Var, along=length(dim(Var))+1 )
  }
}
#becasue the 4th dimension (sample itteration comes up NULL)
str(Var_zr)
print(Var_zr[1:3,1,1:3,1,1:2])

dimnames(Var_zr)[[5]] <- 1:n_samples
#we only want to take stratum 1 (dim4)
dimnames(Var_zr)[[4]] <- 1:15
# Extract only "Stratum_1" and drop the 4th dimension
Var_zr <-Var_zr[,,, "1",, drop = FALSE]

#now i have gotten Var_zr a long way by breaking down the functions
##end of sample_variable() breakdown----
str(Var_zr)# [1:3247, 1(halibut), 1:102,1(stratum1), 1:100]  a variable for each n_sample (100) across the space/time series 
D_gctr<-Var_zr#renaming the object to match the way sample_variable() wanted to do it

#get the associated estimates
Index_gctl_estimates<-Report$Index_gctl
str(Index_gctl_estimates)
dimnames(Index_gctl_estimates)[[4]] <- 1:15
Index_gctl_slice <-Index_gctl_estimates[,,, "1", drop = FALSE]
#Index_gctl_slice <- Index_gctl_estimates[,,,1]
str(Index_gctl_slice)#[1:3247, 1, 1:102, 1] an esitmate for each location across the timeseries
str(Report$D_gct)
# Calculate quantiles from observed and sampled Index_gctl densities

E_zctm = array(NA, dim=c(length(quantiles),dim(Index_gctl_slice)[2:3],ncol(TmbData$Z_gm)) )
E_zctmr = array(NA, dim=c(length(quantiles),dim(Index_gctl_slice)[2:3],ncol(TmbData$Z_gm),n_samples) )
Mean_cmr = array(NA, dim=c(dim(Index_gctl_slice)[2],ncol(TmbData$Z_gm),n_samples) )
prop_zctm = array(NA, dim=c(dim(Index_gctl_slice)[1:3],ncol(TmbData$Z_gm)) )
prop_zctmr = array(NA, dim=c(dim(Index_gctl_slice)[1:3],ncol(TmbData$Z_gm),n_samples) )
for( rI in 0:n_samples ){
  for( mI in 1:ncol(TmbData$Z_gm) ){
    order_g = order(TmbData$Z_gm[,mI], decreasing=FALSE)
    if(rI==0) prop_zctm[,,,mI] = apply( Index_gctl_slice, MARGIN=2:3, FUN=function(vec){cumsum(vec[order_g])/sum(vec)} )
    if(rI>=0) prop_zctmr[,,,mI,rI] = apply( D_gctr[,,,,rI,drop=FALSE], MARGIN=2:3, FUN=function(vec){cumsum(vec[order_g])/sum(vec)} )
    
    # Calculate edge
    for( cI in 1:dim(E_zctm)[2] ){
      if(rI>=1){
        if( calculate_relative_to_average==TRUE ){
          Mean_cmr[cI,mI,rI] = weighted.mean( as.vector(TmbData$Z_gm[,mI]%o%rep(1,dim(Index_gctl_slice)[3])), w=as.vector(D_gctr[,cI,,,rI]) )
        }else{
          Mean_cmr[cI,mI,rI] = 0
        }
      }
      for( zI in 1:dim(E_zctm)[1] ){
        for( tI in 1:dim(E_zctm)[3] ){
          if(rI==0){
            index_tmp = which.min( (prop_zctm[,cI,tI,mI]-quantiles[zI])^2 )
            E_zctm[zI,cI,tI,mI] = TmbData$Z_gm[order_g[index_tmp],mI]
          }
          if(rI>=1){
            index_tmp = which.min( (prop_zctmr[,cI,tI,mI,rI]-quantiles[zI])^2 )
            E_zctmr[zI,cI,tI,mI,rI] = TmbData$Z_gm[order_g[index_tmp],mI] - Mean_cmr[cI,mI,rI]
          }
        }}
    }
  }}
E_zctmr2<-E_zctmr
str(E_zctmr)
class(E_zctmr)
dimnames(E_zctmr) <- list(quantile = paste0("Q", 1:3), 
                          category = "Cat", 
                          year_grp = NULL, 
                          axis = c("km_E", "km_N"), #MARKER2 I reversed these, E is supposed to be the first axis
                          n_sample = paste0("n", 1:100))
#print(E_zctmr[1,,1,1,1:50])
SE_zctm = apply( E_zctmr, MARGIN=1:4, FUN=sd ) #calculate the sd on the n_samples
str(SE_zctm)


print(SE_zctm[,,1:5,1])
#if you want standard error instead, use the following:
#se <- function(x) sd(x) / sqrt(length(x))
#SE_zctm <- apply(E_zctmr, MARGIN = 1:4, FUN = se)

#joining the sd or se with the estimates
Edge_zctm = abind::abind( "Estimate"=E_zctm, "Std_Dev"=SE_zctm, along=5 )
dimnames(Edge_zctm)[[1]] = paste0("quantile_",quantiles)
str(Edge_zctm)
#test plot: using thorson's function, this function is too constrictive for me though to i need to break it down too----
mi=1
Index_zct = array(Edge_zctm[,,,mI,'Estimate'],dim(Edge_zctm)[1:3])
sd_Index_zct = array(Edge_zctm[,,,mI,'Std_Dev'],dim(Edge_zctm)[1:3])
plot_index( Index_ctl = aperm(Index_zct,c(2,3,1)),
            sd_Index_ctl = aperm(sd_Index_zct,c(2,3,1)),
            year_labels = year_labels,
            years_to_plot = NULL,
            strata_names = quantiles,
            category_names = category_names,
            DirName = out_dir,
            PlotName = paste0("RangeEdge_",m_labels[mI],".png"),##MARKER3, the titles are applied based on the order that the labels 
            Yrange = c(NA,NA),
            interval_width = interval_width,
            width = width,
            height = height,
            xlab = "Year",
            ylab = paste0("Quantiles (",m_labels[mI],")") )
#this isn't plotting it the way that i need so now i need to manipulate it to show seasons the way i wouldlike
#end of test plot----

# transform quantiles matrix into a dataframe 
Edge_df <- reshape2::melt(Edge_zctm)
str(Edge_df)
Edge_df$Var2 <- NULL# we can remove because its just one species 
head(Edge_df)
colnames(Edge_df) <- c("quantile","time","axis","Units","value")
head(Edge_df)
#AddSeason
SPRING<- c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79,82,85,88,91,94,97,100)
SUMMER<- c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89,92,95,98,101)
FALL<- c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99,102)

Edge_df$Season<-NA
Edge_df$Season <- ifelse(Edge_df$time  %in% SPRING, "Spring", Edge_df$Season)
Edge_df$Season <- ifelse(Edge_df$time  %in% SUMMER, "Summer", Edge_df$Season)
Edge_df$Season <- ifelse(Edge_df$time  %in% FALL, "Fall", Edge_df$Season)
head(Edge_df)
#Add year
# Group the data into threes and reassign values starting at 1985
increment_value <- 3

#group Time intervals by year
Edge_df <- Edge_df %>%
  mutate(YearGroup = ((time-1) %/% increment_value)+1)

Edge_df <- Edge_df %>%
  mutate(Year = (YearGroup + 1989))
head(Edge_df)
summary(Edge_df)
write.csv(Edge_df, (here::here("2025-04-23/Output/Shift_Indicators/Range_Edge.csv")))

#plot and reshape data for figures
# Step 2: Plotting Range Edge
library(tidyr)
library(dplyr)
Edge_df<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/Range_Edge.csv"))
#quantile: .05, .5, .95
#year_grp:  broken down into Year and season
#axis: 1 is North, 2 is East
#Units (estimate or Std.Error), describes the:
#value in Km N/E
head(Edge_df)
#unstack the Units so that we have a column for the estimate and the standard deviation
Edge_df_wide <- pivot_wider(data = Edge_df, 
                            id_cols = c("quantile", "YearGroup", "axis", "Year", "Season"), 
                            names_from = Units, 
                            values_from = value)
Edge_df_wide$axis<-factor(Edge_df_wide$axis,levels=c("km_N","km_E"))

head(Edge_df_wide)

#subset spring
spring_Edge_df<-subset(Edge_df_wide, Edge_df_wide$Season=="Spring")
names(spring_Edge_df)

#plot trend for Range Edge (spring) 
ggplot(spring_Edge_df, aes(x = Year, y = Estimate, color = quantile)) +
  geom_line(lwd = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - Std_Dev, ymax = Estimate + Std_Dev), width = 0.2) +  # Add error bars
  labs(title = "Range Edge: Spring", x = "Year", y = "Range Edge (km)") +
  facet_wrap(.~axis, scales = "free") +
  theme_classic()


head(spring_Edge_df)
#try to plot together
#Unstack further based on Direction
Edge_df_NSreshp <- pivot_wider(data = Edge_df[Edge_df$Season=="Spring",], 
                               id_cols = c("YearGroup", "Year", "Season"), 
                               names_from = c(Units,axis,quantile),
                               values_from = value)%>%
  # Arrange by Year, Season, and Qu
  arrange(Year)
head(Edge_df_NSreshp)
names(Edge_df_NSreshp)
ggplot(Edge_df_NSreshp, aes(x = Estimate_km_E_quantile_0.5, y = Estimate_km_N_quantile_0.5, color = Year)) +
  geom_point(na.rm=TRUE,alpha=1,size=1.5,shape=19) 
write.csv(Edge_df_NSreshp ,here::here("R/DataforFinalFigs/Edge_df_NSreshp.csv"),row.names = F)  