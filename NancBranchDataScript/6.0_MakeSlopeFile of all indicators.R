#MakeSlopeFile of all indicators----
#Abundance
FigAbd.Region.Spring <- read.csv(here::here("2025-04-23/Output/IndexAbundance/abundance_ind_Region.Spring.csv"),row.names=NULL)
names(FigAbd.Region.Spring)

RegionAbd_coefficients_df <- FigAbd.Region.Spring %>%
  group_by(Region,Period) %>%
  do({
    model <- lm(scale((log10(Index_Estimate))) ~ scale(Year), data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
RegionAbd_coefficients_df<- RegionAbd_coefficients_df%>%
  filter(term == "scale(Year)")
RegionAbd_coefficients_df$Indicator<-NULL
RegionAbd_coefficients_df$Indicator<-"Abundance"
#Area Occupied----
Area_ThresholdsforEAO<- read.csv(here::here("R/DataforFinalFigs/Area_ThresholdsforEAO.csv"))
names(Area_ThresholdsforEAO)
#slopes for EAO over time----
Area_ThresholdsforEAO_coefficients_df <- Area_ThresholdsforEAO %>%filter(Threshold == 90)%>%
  group_by(Region,Period) %>%
  do({
    model <- lm(scale(log10((Area_Threshold))) ~ scale(Year), data = .)
    #model <- lm(log10((Area_Threshold)) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
Area_ThresholdsforEAO_coefficients_df<- Area_ThresholdsforEAO_coefficients_df%>%
  filter(term == "scale(Year)")
  #filter(term == "Year")
Area_ThresholdsforEAO_coefficients_df$Indicator<-NULL
Area_ThresholdsforEAO_coefficients_df$Indicator<-"Area Occupied"
#EAO
#scaled slopes fro EAO vs Abundance----
Area_ThresholdsforEAO_coefficientsABD_df <- Area_ThresholdsforEAO %>%filter(Threshold == 90)%>%
  group_by(Region,Period) %>%
  do({
    model <- lm(scale(log10((Area_Threshold))) ~ scale(log10(Total_Abundance)), data = .)
   # model <- lm(log10((Area_Threshold)) ~ log10(Total_Abundance), data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
Area_ThresholdsforEAO_coefficientsABD_df<- Area_ThresholdsforEAO_coefficientsABD_df%>%
  filter(term == "scale(log10(Total_Abundance))")
  #filter(term == "log10(Total_Abundance)")

Area_ThresholdsforEAO_coefficientsABD_df$Indicator<-NULL
Area_ThresholdsforEAO_coefficientsABD_df$Indicator<-"AOvsAbd"
#COG
centroid_dataRegionalforFig<- read.csv(here::here("R/DataforFinalFigs/centroid_dataRegionalforFig.csv"))
names(centroid_dataRegionalforFig)
Reg_SprCOGcoefficientsLat_df <- centroid_dataRegionalforFig %>%filter(Season=="Spring")%>%
  group_by(Stratum,Period) %>%
  do({
    model <- lm(scale(centroid_latitude) ~ scale(Year), data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
Reg_SprCOGcoefficientsLat_df$Indicator<-"COG_North"

Reg_SprCOGcoefficientsLon_df <- centroid_dataRegionalforFig %>%filter(Season=="Spring")%>%
  group_by(Stratum,Period) %>%
  do({
    modlong<-lm(scale(centroid_longitude)~scale(Year), data=.)
    data.frame(t(coef(modlong)))
    tidy(modlong, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

Reg_SprCOGcoefficientsLon_df$Indicator<-"COG_East"

Reg_SprCOGcoefficientsJt_df<-merge (Reg_SprCOGcoefficientsLat_df,Reg_SprCOGcoefficientsLon_df,all=T)

summary(Reg_SprCOGcoefficientsJt_df)

Reg_ScaledCOGtoYearCoefficients <- Reg_SprCOGcoefficientsJt_df %>%
  filter(term == "scale(Year)" )  # Rep


#Deepening
D_data_Reg<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/Seasonal_Deepening_Reg.csv"))
#Regional Scaled
Deepening_coefficients_Reg_ScaledSpr <- D_data_Reg %>%filter(Season=="Spring") %>%
  group_by(Period,Stratum) %>%
  do({
    model <- lm(scale(Depth_Mean) ~ scale(Year), data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
summary(Deepening_coefficients_Reg_ScaledSpr)
filtered_Deepening_coefficients_Reg_ScaledSpr <- Deepening_coefficients_Reg_ScaledSpr %>%
  filter(term == "scale(Year)")  # to isolate the effects of year and plot slopes and CIs
filtered_Deepening_coefficients_Reg_ScaledSpr$Indicator<-"Deepening"

#Distance to Border
dist_hague_Reg<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_Reg_seasonal.csv"))

dist_hague_Reg$Period<-NULL
pd <- position_dodge(.75)
dist_hague_Reg$Period[dist_hague_Reg$Year<2006]<-"1990-2005"

dist_hague_Reg$Period[dist_hague_Reg$Year>2005]<-"2006-2023"
#Scaled Region Slopes
Overall.DFHcoefficients_df.Scaled.Spring <- dist_hague_Reg %>% filter(Season == "Spring")%>%
  group_by(Stratum,Period) %>%
  
  do({
    
    model <- lm(scale(Dist_Mean) ~ scale(Year), data = .)
    
    data.frame(t(coef(model)))
    
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
    
  }) %>%
  ungroup()

Overall.DFHcoefficients_df.Scaled.Spring<- Overall.DFHcoefficients_df.Scaled.Spring%>%
  filter(term == "scale(Year)")  # Replace "x" with "Intercept" to plot intercept
Overall.DFHcoefficients_df.Scaled.Spring$Indicator<-"Dist to Border"
##Leading EdgeEN
rangeedge<-read.csv(here::here("R/DataforFinalFigs/Edge_df_NSreshp.csv"))
range.spr<-rangeedge[rangeedge$Season=="Spring",]
range.spr$Period<-NULL
range.spr$Period[range.spr$Year<2006]<-"Before Warming"
range.spr$Period[range.spr$Year>2005]<-"During Warming"
names(range.spr);summary(range.spr)
##slope for range ----
#Leading Edge N95 dna E 95 From Canada?
range.sprE_coefficients_df <- range.spr %>% 
  group_by(Period) %>%
  do({
    model <- lm(scale((Estimate_km_E_quantile_0.95)) ~ scale(Year), data = .)
    # model <- lm(Estimate_km_E_quantile_0.5~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
range.sprN_coefficients_df <- range.spr %>%
  group_by(Period) %>%
  do({
    model <- lm(scale((Estimate_km_N_quantile_0.95)) ~ scale(Year), data = .)
    #model <- lm(Estimate_km_N_quantile_0.5 ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
range.sprN_coefficients_df$Indicator<-"Leading Edge N"
range.sprE_coefficients_df <- range.sprE_coefficients_df%>%
  filter(term == "scale(Year)")
# filter(term == "Year")
range.sprN_coefficients_df <- range.sprN_coefficients_df%>%
  filter(term == "scale(Year)")
range.sprE_coefficients_df$Indicator<-"Leading Edge E"

#Trailing 05 and assign to  USA?

range.sprE_coefficients_dfTR <- range.spr %>% 
  group_by(Period) %>%
  do({
    model <- lm(scale((Estimate_km_E_quantile_0.05)) ~ scale(Year), data = .)
    # model <- lm(Estimate_km_E_quantile_0.5~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
range.sprN_coefficients_dfTR <- range.spr %>%
  group_by(Period) %>%
  do({
    model <- lm(scale((Estimate_km_N_quantile_0.05)) ~ scale(Year), data = .)
    #model <- lm(Estimate_km_N_quantile_0.5 ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

range.sprE_coefficients_dfTR <- range.sprE_coefficients_dfTR%>%
  filter(term == "scale(Year)")
# filter(term == "Year")
range.sprN_coefficients_dfTR <- range.sprN_coefficients_dfTR%>%
  filter(term == "scale(Year)")
range.sprE_coefficients_dfTR$Indicator<-"Trailing Edge E"
range.sprN_coefficients_dfTR$Indicator<-"Trailing Edge N"

#Combine all indicators
RegionAbd_coefficients_df
Area_ThresholdsforEAO_coefficients_df
Area_ThresholdsforEAO_coefficientsABD_df
names(Reg_ScaledCOGtoYearCoefficients)[1]
Reg_ScaledCOGtoYearCoefficients <- Reg_ScaledCOGtoYearCoefficients %>%
    rename(Region = Stratum)
filtered_Deepening_coefficients_Reg_ScaledSpr<-filtered_Deepening_coefficients_Reg_ScaledSpr%>%
  rename(Region = Stratum)
Overall.DFHcoefficients_df.Scaled.Spring<-Overall.DFHcoefficients_df.Scaled.Spring%>%
  rename(Region = Stratum)
range.sprN_coefficients_df<-range.sprN_coefficients_df%>%
 mutate(Region = "Canada")
range.sprE_coefficients_df<-range.sprE_coefficients_df%>%
  mutate(Region = "Canada")
range.sprN_coefficients_dfTR<-range.sprN_coefficients_dfTR%>%
  mutate(Region = "USA")
range.sprE_coefficients_dfTR<-range.sprE_coefficients_dfTR%>%
  mutate(Region = "USA")
#Merge
ScaledSlopeIndicators<-merge(RegionAbd_coefficients_df,Area_ThresholdsforEAO_coefficients_df, all = TRUE)  
ScaledSlopeIndicators <- rbind(ScaledSlopeIndicators, Area_ThresholdsforEAO_coefficientsABD_df)
ScaledSlopeIndicators <- merge(ScaledSlopeIndicators, Reg_ScaledCOGtoYearCoefficients, all = TRUE)
ScaledSlopeIndicators <- merge(ScaledSlopeIndicators, filtered_Deepening_coefficients_Reg_ScaledSpr, all = TRUE)
ScaledSlopeIndicators <- merge(ScaledSlopeIndicators, Overall.DFHcoefficients_df.Scaled.Spring, all = TRUE)
ScaledSlopeIndicators <- merge(ScaledSlopeIndicators, range.sprN_coefficients_df, all = TRUE)
ScaledSlopeIndicators <- merge(ScaledSlopeIndicators, range.sprE_coefficients_df, all = TRUE)
ScaledSlopeIndicators <- merge(ScaledSlopeIndicators, range.sprN_coefficients_dfTR, all = TRUE)
ScaledSlopeIndicators <- merge(ScaledSlopeIndicators, range.sprE_coefficients_dfTR, all = TRUE)
ScaledSlopeIndicators$Period <- factor(ScaledSlopeIndicators$Period, levels = c("Before Warming", "During Warming"), ordered = TRUE)
ScaledSlopeIndicators$OrdIndicator<-factor(ScaledSlopeIndicators$Indicator, 
                                          levels = c("Abundance","COG_North", "COG_East", "Deepening", "Area Occupied", "AOvsAbd",   "Leading Edge N", "Leading Edge E", 
                                                     "Trailing Edge N", "Trailing Edge E","Dist to Border", ordered = TRUE))

#Save
2025-04-23/Output/Shift_Indicators
write.csv(ScaledSlopeIndicators,here::here("2025-04-23/Output/Shift_Indicators/ScaledSlopeIndicators.csv"),row.names = F)
ScaledSlopeIndicators<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/ScaledSlopeIndicators.csv"),row.names = NULL)
# Plotting the Scaled Slope Indicators
pd<-position_dodge(.3)
indicator_dividers <- data.frame(xintercept = c(1.5,2.5, 5.5,7.5,8.5,10.5))  # Replace with your actual breaks
ggplot(ScaledSlopeIndicators,aes(y = estimate, x = fct_rev(OrdIndicator),fill=Period)) +
  # Points 
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = pd) +
    geom_point(shape = 21, size = 3, position = pd) +
  geom_vline(data = indicator_dividers, aes(xintercept = xintercept),
             linetype = "solid", color = "grey60", size = 0.4) +
  guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
    scale_fill_manual(values = c("steelblue","orangered" ))+ #Reverse the colors
          coord_flip() +  
  geom_hline(yintercept = 0, linetype = "dashed",size=.8) + # Dashed line for y=0
  #ylim(-0.1,0.1)+
    # theme_minimal() + # A cleaner minimal theme
    theme(
      text = element_text(family = "serif"),  
      legend.position = "top",
      #legend.position.inside = c(0.25, 0.6),               # Position of the legend
      legend.box.background = element_blank(), # Transparent legend box
      legend.title = element_blank(),             # Hide legend title
      legend.text = element_text(size = 12, family = "serif"), # Customize legend text
      axis.text = element_text(size = 14),      # Customize x-axis label
      axis.title.y = element_blank(),             # Remove y-axis label
      axis.title.x = element_text(size = 14),      # Customize x-axis label
      plot.margin=margin(0,0,0,0))+
    xlab("")    +                                 # Clear x-axis label
    ylab("")+
    # Faceting by region
facet_wrap(Region ~ ., 
           scales = "fixed",
           labeller = label_wrap_gen(width = 15)) +  # Wrap long region names
   # Improved labels with log notation
  labs(title = "",
       y = expression("Scaled Slope of Indicator"),
       y = expression(""),
       color = "Period") +
  #  Remove fill from legend, keep only color
  guides(fill = "none") +
  # Clean theme
  theme_bw() +
  theme(
    text = element_text(family = "serif"),  
    legend.box.background = element_blank(), # Transparent legend box
    #legend.position = c(.5,.1),
    legend.position = "top",
    axis.title = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 12, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, family = "serif"),
    legend.key = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 11, family = "serif"),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major = element_blank(),  #
    #legend.position = "bottom"  # Move legend to bottom for better space usage
  )

# Display the plot
print(PlotEAOAbd)
