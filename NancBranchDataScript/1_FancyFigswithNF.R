library(tidyverse)
library(broom)
library(dplyr)

library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)
library(RColorBrewer)
library(scales)

library(grid)  # For unit() function
#theme----
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

pd <- position_dodge(.5)
#END theme----
#For ABUNDANCE PLOTS, fo to 2_FancyFigs_Abundance.r
# For EAO PLOTS, go to 3_FancyFigs_EAO.r
#EAO ----
#NOTE a lot of this is derived from EAOrmarkdown which is in the NancBranchDataScript folder
#NancBranchDataScript/EAOrmarkdown.Rmd
#where spring was isolated
# PLOT EAO,----MUCH OF THIS moved to FancyFigs_RangeEdgeEAO.r
Area_ThresholdsforEAO<- read.csv(here::here("R/DataforFinalFigs/Area_ThresholdsforEAO.csv"))
names(Area_ThresholdsforEAO)
#slopes for EAO over time----

Area_ThresholdsforEAO_coefficients_df <- Area_ThresholdsforEAO %>%filter(Threshold == 90)%>%
  group_by(Region,Period) %>%
  do({
    #model <- lm(scale(log10((Area_Threshold))) ~ scale(Year), data = .)
    model <- lm(log10((Area_Threshold)) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
Area_ThresholdsforEAO_coefficients_df<- Area_ThresholdsforEAO_coefficients_df%>%
  #filter(term == "scale(Year)")
  filter(term == "Year")
#END slopes for EAO over time----
##PlotEOA----
library(ggpmisc)

#END scaled slopes fro EAO vs Abundance----

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

range.sprE_coefficients_df <- range.sprE_coefficients_df%>%
  filter(term == "scale(Year)")
 # filter(term == "Year")
range.sprN_coefficients_df <- range.sprN_coefficients_df%>%
  filter(term == "scale(Year)")
  #filter(term == "Year")
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
#filter(term == "Year")


##END slope for range----
#COP Maps----
#LOAD Sup2DataPlots.R and alter MakingMapArrowsandTables.r (has spatial libraries)
cogreg<- read.csv(here::here("R/DataforFinalFigs/centroid_dataRegionalforFig.csv"))
names(cogreg)
cogregslope<- read.csv(here::here("R/DataforFinalFigs/COGSlopeCI_Regional.csv"))
names(cogregslope)
cogregslope[cogregslope$Season=="Spring" & cogregslope$AxesNE=="Longitude",]
unique(cogreg$Season)
cogreg_spring<-cogreg[cogreg$Season=="Spring",]
centroid_reg_sf_spr <- st_as_sf(cogreg_spring, coords = c("centroid_longitude", "centroid_latitude"))
st_crs(centroid_reg_sf_spr) <- crs
centroid_reg_sf_spr <- na.omit(centroid_reg_sf_spr)

#CA
cogCA<- read.csv(here::here("R/DataforFinalFigs/centroid_dataCAforFig.csv"))
names(cogCA)
cogCA_spr<- subset(cogCA, cogCA$Season=="Spring")
unique(cogCA$Stratum)
cogCA_spr$ordCoreArea<-factor(cogCA_spr$Stratum, levels=c("Nantucket","Georges","CapeCod","EGOM","BOF","Browns","Sable","Gully","CapeBreton","HaliChan","GrandBanks","GBTail"))
unique(cogCA_spr$ordCoreArea)
centroid_CA_sf_spr <- st_as_sf(cogCA_spr, coords = c("centroid_longitude", "centroid_latitude"))
st_crs(centroid_CA_sf_spr) <- crs
centroid_CA_sf_spr <- na.omit(centroid_CA_sf_spr)

cogCAslope<- read.csv(here::here("R/DataforFinalFigs/COGSlopeCI_CoreAreas.csv"))

names(cogregslope)
#Scaled slopes for COG by region period 
#modified in 5.2 and save there as write.csv(Reg_ScaledCOGtoYearCoefficients,here::here("R/DataforFinalFigs/Reg_ScaledCOGtoYearCoefficients.csv"),row.names = F)
#Scaled slopes for COG by region period----

#Plot COGMap----
COG_Reg_map<-ggplot() +
  geom_sf(data = contours, color="lightblue",lwd=.5) +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = NAFO, color="black",lty=1,lwd=.8,fill = NA) +
  geom_sf(data = EEZ, color="black",lty=1,lwd=.8) +
  geom_sf(data = Hague,  fill = NA,lty=1,lwd=.8) +
  geom_sf(data = land, fill = "cornsilk") +  
  geom_sf(data = centroid_reg_sf_spr, aes(color = Year),size =3, alpha = .5,shape=16) +  
  xlim(-70.1, -49.7) + ylim(40, 47.5)+
  labs(title = "", x = "",y = "",
       color = "Year") +
  #scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(centroid_reg_sf_spr$Year)) +
  scale_color_gradient2(low = "steelblue3",   mid = "yellow", 
high = "orangered", 
midpoint = 2005, 
name = "Year",limits = range(cogCA_spr$Year)) +
  geom_segment(data = arrow_dataRegion,
               aes(x = x + 0.4,        # Offset values
                   y = y + 0.4, 
                   xend = xend + 0.4, 
                   yend = yend + 0.4),
               arrow = arrow(length = unit(0.35, "cm")),
               color = "black",
               alpha = .7,
               size = 2)+
  annotate("text",x=-70,y=44.5,label="USA", vjust = 0,color = "black",size = 4) +
  annotate("text",x=-65.9,y=46,label="Canada", vjust = 0,color = "black",size = 4) +
  # #a
  theme_bw()+
  theme(text = element_text(family = "serif",size =12),  
        legend.key.size = unit(.7, "cm"),  # Reduce legend key size
        legend.position = c(0.68, 0.23),                 # Position of the legend
        legend.direction = "horizontal",
        legend.box.background =element_rect(color = "black",fill = "white",linewidth = .5),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 10),
        legend.title = element_blank(),             # Hide legend title
        legend.text = element_text(angle=45,size = 10, family = "serif",h=1), # Customize legend text
        axis.title.y = element_blank(),             # Remove y-axis label
        axis.title.x = element_blank(),      # Customize x-axis label
        #axis.text.x = element_blank(),      # Customize x-axis label
        plot.margin=margin(0,0,0,0))
COG_Reg_map
#SAVING COG Map for SHinyApp----
ggsave(
  filename = "COG_Reg_map.png",
  plot = COG_Reg_map,          # optional if it's the last plot
  path =  here::here("NancBranchDataScript/ShinyApp/www/"),
  width = 8,
  height = 6,
  dpi = 300
)
#END READ PLOT COG Region ----
#Plot COG CA----
#Don't forget to run arrows in MakingMapArrowsandTables.r

COG_CA_map<-ggplot() +
  geom_sf(data = contours, color="lightblue") +
  geom_sf(data = CoreAreas_df, fill = "transparent",lwd=1,color="steelblue",lty=1) +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black",lty=1,lwd=.8) +
  geom_sf(data = Hague,  fill = NA,lty=1,lwd=.8) +
  geom_sf(data = land, fill="cornsilk") +
  geom_sf(data = centroid_CA_sf_spr, aes(color = Year),size =3, alpha = .5,shape=16) +  # Adjust size and alpha here
  #labs(title = "Centre of Gravity (Mean) within Core Areas:Spring", x = "Longitude", y = "Latitude",
  #   color = "Year") +
  #scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(centroid_CA_sf_spr$Year)) +
  scale_color_gradient2(low = "steelblue3",   mid = "yellow", 
                        high = "orangered", 
                        midpoint = 2005, 
                        name = "Year",limits = range(cogCA_spr$Year)) +
  annotate("text",x=-70,y=44.5,label="USA", vjust = 0,color = "black",size = 4) +
  annotate("text",x=-65.9,y=46,label="Canada", vjust = 0,color = "black",size = 4) +
  # #annotateCA----
#annotate("text",x=-69.7,y=40.5,label="NanSh", color = "black",size = 2.9) +
#  annotate("text",x=-68.5,y=43.1,label="EGoM", vjust = 0, color = "black",size = 2.9) +
 # annotate("text",x=-67.1,y=41,label="GB", vjust = 0, color = "black",size = 2.9) +
  #annotate("text",x=-69.5,y=42.1,label="CC", vjust = 0, color = "black",size = 2.9) +
  #annotate("text",x=-66.75,y=43.81,label="BoF", vjust = 0, color = "black",size = 2.9) +
  #annotate("text",x=-66.3,y=42.5,label="BB", vjust = 0, color = "black",size = 2.9) +
  #annotate("text",x=-61.75,y=43.7,label="Sable", vjust = 0, color = "black",size = 2.9) +
  #annotate("text",x=-59,y=44.5,label="Gully", vjust = 0, color = "black",size = 2.9) +
  #annotate("text",x=-59,y=45.8,label="CB", vjust = 0,color = "black",size = 2.9) +
  #annotate("text",x=-69,y=45,label="USA", vjust = 0,color = "black",size = 4) +
  #annotate("text",x=-66.1,y=46,label="Canada", vjust = 0,color = "black",size = 4) +
  #ENDannotateCA----
xlim(-70.1, -49.7) + ylim(40, 47.5)+
  labs(title = "", x = "",y = "",color = "Year") +
  geom_segment(data = arrow_data_CA,
                 aes(x = x + 0.2,        # Offset values
                     y = y + 0.2, 
                     xend = xend + 0.2, 
                     yend = yend + 0.2),
                 arrow = arrow(length = unit(0.35, "cm")),
                 color = "black",
                 alpha = .7,
                 size = 1.5)+
      theme_bw()+
  theme(text = element_text(family = "serif",size=12),  
        legend.position = "none",               # Position of the legend
        legend.direction = "horizontal",
        legend.box.background =element_rect(color = "black",fill = "white",linewidth = .5),
        legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.title = element_blank(),             # Hide legend title
        legend.text = element_text(angle=45,size = 12, family = "serif",h=1), # Customize legend text
        axis.title.y = element_blank(),             # Remove y-axis label
        #axis.title.x = element_blank(),      # Customize x-axis label
        plot.margin=margin(0,0,0,0))
COG_CA_map
#END PLOT COG CA ----
# Ensure both plots have identical margins
COG_Reg_map2 <- COG_Reg_map + 
  theme(plot.margin = margin(t=0, r=15, b=-5, l=0, "pt"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),  # Remove x-axis title too
        panel.spacing = unit(0, "lines"),
        axis.text.x = element_blank(),   # Remove x-axis text if not needed
        axis.ticks.x = element_blank())  # Remove x-axis ticks

COG_CA_map2 <- COG_CA_map + 
  theme(plot.margin = margin(t=-5, r=10, b=0, l=15, "pt"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),  # Remove x-axis title too
        panel.spacing = unit(0, "lines"))
# Create the combined plot with precise spacing control using egg
library(gridExtra)
library(egg)

COGCombo <- ggarrange(
  COG_Reg_map2, 
  COG_CA_map2,
  nrow = 2,
  heights = c(1, 1),
  labels = c("(a)", "(b)"),
  label.args = list(gp = gpar(fontsize = 12))
)

COGCombo
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/COGCombo.jpeg"), plot = COGCombo, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg") 

#Making supplemental tables for distances and for slopes----

#Distance estimates MADE IN MakingMapArrowsandTables.r

COGSupptable<-merge(COGRegMovement,COGCoreMovement,all=T)
COGSupptable <- COGSupptable %>%
  mutate(Region_CoreArea= factor(Region_CoreArea, 
                                 levels = c("Canada", "USA", "Nantucket","Georges","CapeCod","EGOM","BOF","Browns","Sable","Gully","CapeBreton","GrandBanks","GBTail","HaliChan"), ordered = F))  %>%  # Custom order for Period
  arrange(Region_CoreArea, Period)

#Find range of distances travelled for just core areas

COGSupptable.ft <- flextable(COGSupptable)
# Format the numeric columns with 2 decimal places
COGSupptable.ft <- COGSupptable.ft%>%
  colformat_double(j = c("Long.Start", "Lat.Start",
                         "Long.End", "Lat.End", 
                         "Distance(km)","Dist_per_year"), digits = 2) %>%
  colformat_char(j = c("Region_CoreArea", "Period"))%>%
  set_table_properties(layout = "fixed")  # Ensure row order is maintained  # Ensure text columns are left as is



# Display the formatted table
COGSupptable.ft
autofit(COGSupptable.ft) #adjusts the column widths to fit the content.
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
COGSupptabledoc2 <- body_add_flextable(doc, value = COGSupptable.ft)

# Save the document
print(COGSupptabledoc2, target = here::here("NancBranchDataScript/FancyFiguresforMS/COGSupptabledoc2.docx"))
#SLOPES Table
cogregslope_spr<-cogregslope[cogregslope$Season=="Spring",]
cogCAslope_spr<-cogCAslope[cogCAslope$Season=="Spring",]

COGSlopeSupptable<-merge(cogregslope_spr,cogCAslope_spr,all=T)
names(COGSlopeSupptable)
COGSlopeSupptable <- COGSlopeSupptable %>%
  mutate(Region_CoreArea= factor(Stratum, 
                                 levels = c("Canada", "USA", "Nantucket","Georges","CapeCod","EGOM","BOF","Browns","Sable","Gully","CapeBreton","GrandBanks","GBTail","HaliChan"), ordered = F))  %>%  # Custom order for Period
  arrange(Region_CoreArea, Period)
COGSlopeSupptable
df <- df %>% select(col3, col1, col2)
COGSlopeSupptable <- COGSlopeSupptable %>%
  select("Region_CoreArea", "Period","AxesNE",
         "term", "estimate", "std.error", "statistic",
         "p.value", "conf.low", "conf.high") 

COGSlopeSupptable.ft <- flextable(COGSlopeSupptable)
names(COGSlopeSupptable)
# Format the numeric columns with 2 decimal places
COGSlopeSupptable.ft <- COGSlopeSupptable.ft%>%
  colformat_double(j = c("term","estimate","std.error","statistic",
                         "p.value","conf.low", "conf.high"), digits = 2) %>%  
    colformat_char(j = c("Region_CoreArea", "Period","AxesNE"))%>%
  set_table_properties(layout = "autofit")  # Ensure row order is maintained  # Ensure text columns are left as is

COGSlopeSupptable.ft
autofit(COGSlopeSupptable.ft) #adjusts the column widths to fit the content.
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
COGSlopeSupptabledoc2 <- body_add_flextable(doc, value = COGSlopeSupptable.ft)

# Save the document
print(COGSlopeSupptabledoc2, target = here::here("NancBranchDataScript/FancyFiguresforMS/COGSlopeSupptabledoc2.docx"))
#END Making supplemental tables for distances and for slopes----

#FOR COG SUPPLEMENTAL USING CA DATA----

#Plot script from 05_Centre of gravity
#plot for Supplemental COG Lat vs Long by Core Area----

suppcogca<-ggplot(cogCA_spr, aes(x = centroid_longitude, y = centroid_latitude, color = Year)) +
  geom_point(na.rm=TRUE,alpha=1,size=1.5,shape=19) +
  labs(title = "Center of Gravity by Core Area: Spring",
       x = "Longitude",
       y = "Latitude",
       color = "Year") +
  #scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(cogCA_spr$Year)) +
  scale_color_gradient2(low = "steelblue3",   mid = "yellow", high = "orangered", midpoint = 2005, 
                        name = "Year",limits = range(cogCA_spr$Year)) +
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  facet_wrap(.~ordCoreArea,scales="free",nrow=3)
suppcogca
#END plot for Supplemental COG Lat vs Long by Core Area----
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/FigureSUPPCOG_CAMap.jpeg"), plot =suppcogca, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg") 

# FOR CA SLOPES got to SupplementalSlopePlots.R

#PLOT Distance to Hague Line
#Figure DIST REG AND CA Trends ----
# Processing distance data for REG and CA----

dist_hague_Reg<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_Reg_seasonal.csv"))
dist_hague_Reg$Period<-NULL
dist_hague_Reg$Period[dist_hague_Reg$Year<2006]<-"1990-2005"
dist_hague_Reg$Period[dist_hague_Reg$Year>2005]<-"2006-2023"
dist_hague_Reg_spr<-dist_hague_Reg[dist_hague_Reg$Season=="Spring",]
dist_hague_Reg_spr_agg<-dist_hague_Reg_spr %>%
  group_by(Stratum,Period) %>%
    summarise(
    Dist_Mean = mean(Dist_Mean, na.rm = TRUE),
    Dist_Med = mean(Dist_Med, na.rm = TRUE),
    Dist_Q5 = mean(Dist_Q5, 0.05, na.rm = TRUE),
    Dist_Q95 = mean(Dist_Q95, 0.95, na.rm = TRUE)
  ) %>%
  ungroup()

dist_hague_CA<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_CA_seasonal.csv"))
dist_hague_CA$Period<-NULL
dist_hague_CA$Period[dist_hague_CA$Year<2006]<-"1990-2005"
dist_hague_CA$Period[dist_hague_CA$Year>2005]<-"2006-2023"
dist_hague_CA_spr<-dist_hague_CA[dist_hague_CA$Season=="Spring",]
#aggregate values by Stratum and Period
dist_hague_CA_spr_agg<-dist_hague_CA_spr %>%
  group_by(Stratum,Period) %>%
  summarise(
    Dist_Mean = mean(Dist_Mean, na.rm = TRUE),
    Dist_Med = mean(Dist_Med, na.rm = TRUE),
    Dist_Q5 = mean(Dist_Q5, 0.05, na.rm = TRUE),
    Dist_Q95 = mean(Dist_Q95, 0.95, na.rm = TRUE)
  ) %>%
  ungroup()
#Must TRANSFORM USA distances to be negative for plotting----
dist_hague_RegFromZero<-dist_hague_Reg_spr_agg %>%
  mutate(
    Dist_Mean = case_when(
      Stratum %in% c("USA") ~ Dist_Mean * -1,
      TRUE ~ Dist_Mean
    ),
    Dist_Med = case_when(
      Stratum %in% c("USA") ~ Dist_Med * -1,
      TRUE ~ Dist_Med
    ),
    Dist_Q5 = case_when(
      Stratum %in% c("USA") ~ Dist_Q5 * -1,
      TRUE ~ Dist_Q5
    ),
    Dist_Q95 = case_when(
      Stratum %in% c("USA") ~ Dist_Q95 * -1,
      TRUE ~ Dist_Q95
    )
  )

#selected_categories <- c("CapeCod","EGOM","Nantucket","Georges")
selected_categories <- c("CapeCod","EGOM","Nantucket")

dist_hague_CAFromZero<-dist_hague_CA_spr_agg %>%
  mutate(
    Dist_Mean = case_when(
      Stratum %in% selected_categories ~ Dist_Mean * -1,
      TRUE ~ Dist_Mean
      ),
Dist_Med = case_when(
  Stratum %in% selected_categories ~ Dist_Med * -1,
  TRUE ~ Dist_Med
),
Dist_Q5 = case_when(
  Stratum %in% selected_categories ~ Dist_Q5 * -1,
  TRUE ~ Dist_Q5
),
Dist_Q95 = case_when(
  Stratum %in% selected_categories ~ Dist_Q95 * -1,
  TRUE ~ Dist_Q95
  )
)
# Now just Georges q5

dist_hague_CAFromZero2<-dist_hague_CAFromZero %>%
  mutate(
    Dist_Q5= case_when(
      Stratum %in% "Georges" ~ Dist_Q5 * -1,
      TRUE ~ Dist_Q5
    )
  ) 
#END Must TRANSFORM USA distances to be negative for plotting----
#END Processing distance data for REG and CA----
   
pd <- position_dodge(.7)
##Plot Distance for REG----
#have to set order for region because plot is flipped
dist_hague_RegFromZero$Ord2Region<-factor(dist_hague_RegFromZero$Stratum, levels=c("USA","Canada"))
DistReg<-ggplot(dist_hague_RegFromZero, aes(x = Ord2Region, y = Dist_Mean,fill=Period)) +
  geom_linerange(aes(ymin = Dist_Q5, ymax =Dist_Q95),position=pd,color="darkgrey",size=1.5)+
  geom_point(shape=21, size = 3,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  coord_flip()+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  xlab("")+  ylab("")+
  #ylab("Distance from Hague line (km)")+
  scale_y_continuous(breaks = seq(-400, 1500, by = 200),limits=c(-400,1500)) +
  theme_bw() + # A clean theme
  theme(
    text = element_text(family = "serif",size=14),  
    axis.text = element_text(family = "serif",size=12),      
    legend.position = "none",
    #legend.position = c(0.75, 0.2),               # Position of the legend
    legend.box.background = element_blank(), # Transparent legend box
    legend.title = element_blank(),             # Hide legend title
    legend.text = element_text(size = 12, family = "serif"), # Customize legend text
    axis.title.y = element_blank(),             # Remove y-axis label
    axis.title.x = element_text(size = 14),      # Customize x-axis label
    panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
    plot.margin=margin(10,40,20,30)
  )
DistReg
##Plot Distance for CA----
dist_hague_CAFromZero2
dist_hague_CAFromZero2$ordCoreArea<-factor(dist_hague_CAFromZero2$Stratum, levels=c("Nantucket","CapeCod","EGOM","Georges","BOF","Browns","Sable","CapeBreton","Gully","HaliChan","GrandBanks","GBTail"))
View(dist_hague_CAFromZero2)
pd <- position_dodge(.5)
DistHag<-ggplot(dist_hague_CAFromZero2, aes(x = factor(ordCoreArea), y = Dist_Mean,fill=Period)) +
  geom_linerange(aes(ymin = Dist_Q5, ymax =Dist_Q95),position=pd,color="darkgrey",size=1.5)+
  geom_point(shape=21, size = 3,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  coord_flip()+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  xlab("")+  ylab("Distance to Border (km)")+
  scale_y_continuous(breaks = seq(-400, 1500, by = 200)) +  # set tick marks
  theme_bw() + # A clean theme
  theme(
    text = element_text(family = "serif",size=14),  
    axis.text = element_text(family = "serif",size=12),           
    legend.position = c(0.75, 0.2),               # Position of the legend
    #legend.box.background = element_blank(), # Transparent legend box
    legend.title = element_blank(),             # Hide legend title
    legend.text = element_text(size = 14, family = "serif"), # Customize legend text
    axis.title.y = element_blank(),             # Remove y-axis label
    axis.title.x = element_text(size = 14),      # Customize x-axis label
    panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
    plot.margin=margin(10,40,20,30)
  )
DistHag
##END DistHag----
DistHagCombo<-plot_grid(DistReg, DistHag, nrow = 2,rel_heights = c(1, 2),labels = c("(a)", "(b)"),align = "v", axis = "lr") # Add labels
DistHagCombo


ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/DistHag.jpeg"), plot = DistHagCombo, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
#
#END PLOT Distance to Hague Line
#
library(dplyr)
library(ggplot2)
library(stringr)
#find the hypoteneuse for sqrt((250^2)+(125^2))
hypotenuse <- function(a, b) {
  sqrt(a^2 + b^2)
}
# Calculate the distance between two points (x1, y1) and (x2, y2)
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Calculate the angle in radians between two points (x1, y1) and (x2, y2)
