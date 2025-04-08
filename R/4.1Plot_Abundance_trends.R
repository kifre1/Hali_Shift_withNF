#Step 1: Plot Abundance Trends

#Abundance trend Plots
library(dplyr)

library(broom)

library(ggplot2)

library(patchwork)

library(grid)  # For unit() function
theme_replace(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour="black"),
              
              strip.background=element_rect(colour="black",fill="white"))

theme_set(theme_bw())

theme_replace(legend.key =element_rect(colour="black",fill="white"),
              
              plot.margin = unit(c(1.5,3,1.5,1), "cm"),
              
              #plot.margin=margin(3,4,4,0),
              
              #plot.margin=margin(3,4,4,0),
              
              plot.title=element_text(size=16,vjust=1.5,family="serif"),
              
              legend.background=element_rect(size=.9,colour="white",fill="white"),
              
              strip.text=element_text(size=14,family="serif",angle=0),
              
              panel.border = element_rect(colour = "black",fill=NA),
              
              panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              
              strip.background=element_rect(colour="black",fill="white"),
              
              axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1,size=12,family="serif"),
              
              axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=12,family="serif"),
              
              axis.title.x=element_text(size=12,hjust=0.5,vjust=-2,family="serif"))


# Plot abuncance indexed regions 
#SpSt_DF<-read.csv(here::here("2024-10-04/Output/PredictionData_for_ShiftAnalysis.csv"))

# Figure 2, 

abundance_ind_Region<-read.csv(here::here("2024-10-04/Output/abundance_ind_Region.csv"))
abundance_ind_CA<-read.csv(here::here("2024-10-04/Output/abundance_ind_CA.csv"))
#plot the regional abundance seasonally 
#remove "all", isolate spring, rename regions
abundance_ind_Region<-subset(abundance_ind_Region, abundance_ind_Region$Index_Region=="NMFS"|abundance_ind_Region$Index_Region=="DFO")
abundance_ind_Region<-subset(abundance_ind_Region, abundance_ind_Region$Season=="Spring")
abundance_ind_Region$Index_Region[abundance_ind_Region$Index_Region == "NMFS"] <- "USA"
abundance_ind_Region$Index_Region[abundance_ind_Region$Index_Region == "DFO"] <- "Canada"

regpal<- c("orange", "darkblue")
RegionalPlot<- ggplot(data = abundance_ind_Region, aes(x = Year, y = Index_Estimate, color = Index_Region))+
  geom_point()+
  scale_y_continuous(labels = scales::scientific) +
  geom_line()+
  geom_errorbar(data = abundance_ind_Region, aes(x = Year, ymin = (Index_Estimate - Index_SD), ymax = (Index_Estimate + Index_SD), color = Index_Region, group = Index_Region), alpha = 0.65) +
  labs(title="Estimated Abundance \nAggregated by Region: Spring", y="Count", x="Year")+
  scale_color_manual(values = regpal) +
guides(color = guide_legend(title = NULL))
RegionalPlot

abundance_ind_CA$Index_Region[abundance_ind_CA$Index_Region == "CB4Vn"] <- "CapeBreton"
abundance_ind_CA<-subset(abundance_ind_CA, abundance_ind_CA$Season=="Spring")
CAPlot<- ggplot(data = abundance_ind_CA, aes(x = Year, y = Index_Estimate, color = Index_Region))+
  geom_point(size=2)+
  scale_y_continuous(labels = scales::scientific) +
  geom_line(size=1)+
  geom_errorbar(data = abundance_ind_CA, aes(x = Year, ymin = (Index_Estimate - Index_SD), ymax = (Index_Estimate + Index_SD), color = Index_Region, group = Index_Region), alpha = 0.65) +
  #facet_grid(.~Season)+
  scale_color_brewer(palette = "Set1") +  # Try "Set1", "Dark2", etc.
  labs(title="Estimated Abundance \nAggregated by CoreArea: Spring", y="Count", x="Year")+
  guides(color = guide_legend(title = NULL))
#theme(legend.position = "none")  # Remove legend
CAPlot

#Figure4: plot abundance trends with a map of the core areas 
#go to Sup2DataPlots.R to make CAMAP
(CAMAP / CAPlot)+ plot_layout(heights = c(1,1))

#Calculate the change in slope
#Fit a linear regression to the slope to calculate the rate of change
# the data are abundance_ind_Region, abundance_ind_CA

#the coefficient for Year represents the slope of the regression line, which will indicate how Index_Estimate changes over time.
# CA
CA_whole <- lm(Index_Estimate ~ Year, data = abundance_ind_CA)
summary(CA_whole)
#only years since 2005
abundance_ind_CA_since2005 <- subset(abundance_ind_CA, Year >= 2005)
CA_since2005 <- lm(Index_Estimate ~ Year, data = abundance_ind_CA_since2005)
summary(CA_since2005)
# Regional
R_whole <- lm(Index_Estimate ~ Year, data = abundance_ind_Region)
summary(R_whole)
#only years since 2005
abundance_ind_R_since2005 <- subset(abundance_ind_Region, Year >= 2005)
R_since2005 <- lm(Index_Estimate ~ Year, data = abundance_ind_R_since2005)
summary(R_since2005)

library(dplyr)
#weighted least squares regression:By doing this, data points with more confidence will have more influence 
#CA
CA_weighted <- abundance_ind_CA %>%
  group_by(Index_Region) %>%
  do(model = lm(Index_Estimate ~ Year, data = ., weights = 1 / (Index_SD^2)))
# Extract the coefficients for each CA
CA_coefficients <- CA_weighted %>%
  summarise(
    Index_Region = first(Index_Region),  # Include Index_Region in the summary
    Intercept = coef(model)[1],          # Intercept of the model
    Slope = coef(model)[2]               # Slope of the model
  )
print(CA_coefficients)
#only years since 2005
abundance_ind_CA_since2005 <- subset(abundance_ind_CA, Year >= 2005)
CA_since2005_weighted <- abundance_ind_CA_since2005 %>%
  group_by(Index_Region) %>%
  do(model = lm(Index_Estimate ~ Year, data = ., weights = 1 / (Index_SD^2)))
CA_since2005_coefficients <- CA_since2005_weighted %>%
  summarise(
    Index_Region = first(Index_Region), 
    Intercept = coef(model)[1],     
    Slope = coef(model)[2]              
  )
print(CA_since2005_coefficients)

#Regional
R_weighted <- abundance_ind_Region %>%
  group_by(Index_Region) %>%
  do(model = lm(Index_Estimate ~ Year, data = ., weights = 1 / (Index_SD^2)))
# Extract the coefficients for each CA
R_coefficients <- R_weighted %>%
  summarise(
    Index_Region = first(Index_Region),  # Include Index_Region in the summary
    Intercept = coef(model)[1],          # Intercept of the model
    Slope = coef(model)[2]               # Slope of the model
  )
print(R_coefficients)
#only years since 2005
abundance_ind_R_since2005 <- subset(abundance_ind_Region, Year >= 2005)
R_since2005_weighted <- abundance_ind_R_since2005 %>%
  group_by(Index_Region) %>%
  do(model = lm(Index_Estimate ~ Year, data = ., weights = 1 / (Index_SD^2)))
R_since2005_coefficients <- R_since2005_weighted %>%
  summarise(
    Index_Region = first(Index_Region), 
    Intercept = coef(model)[1],     
    Slope = coef(model)[2]              
  )
print(R_since2005_coefficients)

