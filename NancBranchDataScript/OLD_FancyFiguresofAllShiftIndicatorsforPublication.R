
library(dplyr); library(broom); library(ggplot2); library(patchwork);library(RColorBrewer)
library(grid)
library(ggtext)
library(cowplot)
# 
#theme ----
theme_replace(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour="black"),
              
              strip.background=element_rect(colour="black",fill="white"))

theme_set(theme_bw())

theme_replace(legend.key =element_rect(colour="black",fill="white"),
              #plot.margin = unit(c(1.5,3,1.5,1), "cm"),
              plot.margin=margin(0,0,0,0),
              plot.title=element_text(size=16,vjust=1.5,family="serif"),
              #legend.background=element_rect(size=.5,colour="white",fill="white"),
              legend.background=element_blank(),
              strip.text=element_text(size=14,family="serif",angle=0),
              panel.border = element_rect(colour = "black",fill=NA),
              panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              strip.background=element_rect(colour="black",fill="white"),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=.5,size=14,family="serif"),
              axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0,size=14,family="serif"),
              axis.title.x=element_text(size=14,hjust=0.5,vjust=-2,family="serif"),
              axis.title.y=element_text(size=14,hjust=0.5,vjust=4,angle=90,family="serif"))
#END theme----
# Plot abuncance indexed regions ----

FigAbd.Region.Spring <- read.csv(here::here("R/DataforFinalFigs/Abd.Region.Spring.csv"))
names(FigAbd.Region.Spring)
#From 03.1_FitLMandPlotAbundanceCARegion....
#making sure file exists
file.exists(here::here("R/DataforFinalFigs/Overall.ABDcoefficients_df.Spring.csv"))
# From 03.1_FitLMandPlotAbundancesCARegion.R
#coefficients to plot rates.
FigAbd.RegionCoef<-read.csv(here::here("R/DataforFinalFigs/Overall.ABDcoefficients_df.Spring.csv"))

display.brewer.all(colorblindFriendly = TRUE)

##Figure ABD Panel A----
regpal<- c("orange", "darkblue")
colbline<-brewer.pal(3, "Dark2")
custom_labels <- c(
    "Canada: Before (96%), During (94%) Change (2%)",
    "USA:    Before ( 4%), During ( 6%) Change (35%)"
)
ARegionalPlot<- 
     ggplot(data = FigAbd.Region.Spring, aes(x = Year, y = Index_Estimate/1000000),group=Region)+
    geom_vline(xintercept=2005,lty=2,lwd=1.2)+
  #geom_errorbar(data =  FigAbd.Region.Spring, aes(x = Year, ymin = (Index_Estimate/1000000 - Index_SD/1000000), ymax = (Index_Estimate/1000000 + Index_SD/1000000), color = Region, group = Region), alpha = 0.65) +
  geom_ribbon(aes(x = Year, ymin = Index_Estimate/1000000 - Index_SD/1000000, 
                  ymax = Index_Estimate/1000000 +Index_SD/1000000, 
                  fill = Region), alpha = 0.12) +  # Use geom_ribbon for the SE band
  geom_line(aes(color =  Region), linewidth = 1) +                         # Line for the group
  geom_point(aes(color = Region), shape = 19,size=2.5) +                        # Points for data
  scale_fill_manual(values = regpal) +                    # Custom fill colors
  scale_color_manual(values = regpal) +
  # Combine legends
  guides(color = guide_legend(title = ""),
         fill = "none") + 
    labs(y="Modelled Abundance (Millions)", x="")+
  guides(color = guide_legend(title = ""))+
  annotate("text", x = 1993, y = 7, label = "Before Warming", color = "black", size = 5,family = "serif") +
  annotate("text", x = 2012, y = 7, label = "During Warming", color = "black", size = 5,family = "serif") +
  #Before
  annotate("text", x = 1995, y = 3.1, label = "96%", color = "black", size = 4) +
  annotate("segment", x = 1995, xend = 1995, y = 2.7, yend = 2, arrow = arrow(length = unit(.2, "cm"),type="closed")) +
 annotate("text", x = 1992, y = .85, label = "4%", color = "black", size = 4) +
 annotate("segment", x = 1992, xend = 1992, y = .7, yend = .3, arrow = arrow(length = unit(.2, "cm"),type="closed")) +
 #After
  annotate("text", x = 2015, y = 6.2, label = "94%", color = "black", size = 4) +
 annotate("segment", x = 2015, xend = 2015, y = 5.7, yend = 5.2, arrow = arrow(length = unit(.2, "cm"),type="closed")) +
  annotate("text", x = 2015, y = .9, label = "6%", color = "black", size = 4) +
 annotate("segment", x = 2015, xend = 2015, y = .7, yend = .3, arrow = arrow(length = unit(.2, "cm"),type="closed")) +
   #annotate("rect", xmin = 2004, xmax = 2014, ymin = 3, ymax = 5, alpha = 0.2, fill = "red") +
 theme(text = element_text(family = "serif"),  
  legend.box.background = element_blank(), # Transparent legend box
  legend.position = c(.15,.7),
  legend.text = element_text(size = 14,family="serif"),
  plot.margin=margin(10, 5, 10, 15))
ARegionalPlot
#END Figure ABD A----
#Figure ABD RATE----
FigAbd.RegionCoef$Ord2Region<-factor(FigAbd.RegionCoef$Region, levels=c("USA","Canada"))
#FigAbd.RegionCoef$RevPeriod <- factor(FigAbd.RegionCoef$Period, levels = c("During Warming", "Before Warming")) 
RegionRatesPlot<-
ggplot(FigAbd.RegionCoef, aes(x = factor(Ord2Region), y = estimate,fill=Period)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = pd) +
  geom_point(shape = 21, size = 3, position = pd) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(values = c("steelblue","orangered" ))+ #Reverse the colors
  geom_hline(yintercept = 0, linetype = "dashed") + # Dashed line for y=0
  ylim(-0.06, 0.06) +
  #theme_minimal() + # A cleaner minimal theme
  theme(
    text = element_text(family = "serif"),  
    legend.position = c(0.25, 0.6),               # Position of the legend
    legend.box.background = element_blank(), # Transparent legend box
    legend.title = element_blank(),             # Hide legend title
    legend.text = element_text(size = 12, family = "serif"), # Customize legend text
    axis.text = element_text(size = 14),      # Customize x-axis label
    axis.title.y = element_blank(),             # Remove y-axis label
    axis.title.x = element_text(size = 14),      # Customize x-axis label
   plot.margin=margin(10,40,20,30))+
  xlab("") +                                     # Clear x-axis label
  ylab("Rate of change in Abundance (log10) /year")
RegionRatesPlot
Figure2AbdAbdRates<-plot_grid(ARegionalPlot, RegionRatesPlot, nrow = 2,rel_heights = c(2, 1),labels = c("(a)", "(b)"))#,align = "v", axis = "lr") # Add labels
Figure2AbdAbdRates
#ggsave(here::here("R/DataforFinalFigs/Figure2AbdAbdRates.tiff"), plot = Figure2AbdAbdRates, dpi = 600, width = 8, height = 6, units = "in", device = "tiff")
# END Plot abundance indexed regions  ----
ggsave(here::here("R/DataforFinalFigs/Figure2AbdAbdRates.jpeg"), plot = Figure2AbdAbdRates, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
#EOA plots and table for supplemental----
# PLOT EAO,----
##Effective Area Occupied: Measures the area (in units square km) required to contain a population given its average pop density (kg km−2).
#Assuming that total abundance (ct) = average density (mt) x area occupied (ht) (ct = mt × ht), Total abundance (total catch)
#and average density (catch per area) are calculated per time (t) and strata, and effective area occupied ht is their ratio (ht= ct/mt)
##Pulls indexed data,  plots trends and writes Effective_Area_Occupied.csv

EAOcc_DF_Region_Spring <- read.csv(here::here("R/DataforFinalFigs/EAOcc_DF_Region_SpringforFig.csv"))
names(EAOcc_DF_Region_Spring)

##PlotEOA----
EAOplot<-ggplot(EAOcc_DF_Region_Spring,aes(x = Year, y = EffectiveArea/1000)) +
  geom_point(aes(color = Region),shape = 19,size=2.5) +
  geom_ribbon(aes(x = Year, 
                  ymin = EffectiveArea/1000 - SE/1000, 
                  ymax = EffectiveArea/1000 + SE/1000, 
                  fill = Region), alpha = 0.12) +  # Use geom_ribbon for the SE band
  geom_line(aes(x = Year, y = EffectiveArea/1000, color = Region)) +
  scale_fill_manual(values = regpal) +                    # Custom fill colors
  scale_color_manual(values = regpal) +
  # Combine legends
  guides(color = guide_legend(title = ""),
         fill = "none") + 
  ylab("Effective Area Occupied (1000 sqkm)")+xlab("")+
  theme_bw() + theme(text = element_text(family = "serif"),  
                     legend.box.background = element_blank(), # Transparent legend box
                     legend.direction = "horizontal",
                     legend.position =  c(.4,.9),
                     legend.text = element_text(size = 10,family="serif"),
                     plot.margin=margin(10, 5, 10, 15))
##END PlotEOA----
#Plot EAO rates----
EAOcoefficients_df_Spring <- read.csv(here::here("R/DataforFinalFigs/EAOcoefficients_df_SpringforFig.csv"))
EAOcoefficients_df_Spring$Ord2Region<-factor(EAOcoefficients_df_Spring$Region, levels=c("USA","Canada"))
EAOrate<-ggplot(EAOcoefficients_df_Spring, aes(x = factor(Ord2Region), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = pd) +
  geom_point(shape = 21, size = 3, position = pd) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(values = c("orangered","steelblue" ))+ #Reverse the colors
  geom_hline(yintercept = 0, linetype = "dashed") + # Dashed line for y=0
  ylim(-0.15, 0.15) +
  theme_bw() + # A clean theme
  theme(
    text = element_text(family = "serif",size=14),  
    legend.position = c(0.15, 0.67),               # Position of the legend
    legend.box.background = element_blank(), # Transparent legend box
    legend.title = element_blank(),             # Hide legend title
    legend.text = element_text(size = 10, family = "serif"), # Customize legend text
    axis.title.y = element_blank(),             # Remove y-axis label
    axis.title.x = element_text(size = 12),      # Customize x-axis label
    plot.margin=margin(10,40,20,30))+
  xlab("")+  ylab("Rate of change in Effective Area Occupied")

FigureXEAOrate<-plot_grid(EAOplot, EAOrate, nrow = 2,rel_heights = c(2, 1),labels = c("(a)", "(b)"))#,align = "v", axis = "lr") # Add labels
FigureXEAOrate
# END Plot EAO rates----
ggsave(here::here("R/DataforFinalFigs/FigureXEAOrate.jpeg"), plot =FigureXEAOrate, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")

##PlotEAO-
EAOplot<-ggplot(EAOcc_DF_Region_Spring,aes(x = Year, y = EffectiveArea/1000)) +
  geom_point(aes(color = Region),shape = 19,size=2.5) +
  geom_ribbon(aes(x = Year, 
                  ymin = EffectiveArea/1000 - SE/1000, 
                  ymax = EffectiveArea/1000 + SE/1000, 
                  fill = Region), alpha = 0.12) +  # Use geom_ribbon for the SE band
  geom_line(aes(x = Year, y = EffectiveArea/1000, color = Region)) +
  scale_fill_manual(values = regpal) +                    # Custom fill colors
  scale_color_manual(values = regpal) +
  # Combine legends
  guides(color = guide_legend(title = ""),
         fill = "none") + 
  ylab("Effective Area Occupied (1000 sqkm)")+xlab("")+
  theme_bw() + theme(text = element_text(family = "serif"),  
                     legend.box.background = element_blank(), # Transparent legend box
                     legend.direction = "horizontal",
                     legend.position =  c(.4,.9),
                     legend.text = element_text(size = 10,family="serif"),
                     plot.margin=margin(10, 5, 10, 15))
##END PlotEOA----
##Plot EOA_Abd relationship.
EAO_abd<-read.csv(here::here("R/DataforFinalFigs/EAO_abd.csv"))

FigureXEAOABD<-ggplot(EAO_abd, aes(x = LogAbd, y = log_EffectiveArea, color = Period)) +
  geom_point(shape = 19, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("steelblue", "orangered")) +
  facet_wrap(Region~.,nrow=2) +
  theme(
    plot.tag.position = c(0.3, 1.05),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )
#SUPPLEMENTAL TABLE MADE IN 04.1_FitLM-PlotSlopesEAO.R Look there.
FigureXEAOrate<-plot_grid(EAOplot, EAOrate, nrow = 2,rel_heights = c(2, 1),labels = c("(a)", "(b)"))#,align = "v", axis = "lr") # Add labels
FigureXEAOrate
# END Plot EAO rates----
ggsave(here::here("R/DataforFinalFigs/FigureXEAOrate.jpeg"), plot =FigureXEAOrate, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
ggsave(here::here("R/DataforFinalFigs/FigureXEAOABD.jpeg"), plot =FigureXEAOABD, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
#Figure CA Trends in Abundance and Distribution.FOR SUPP (TO DO)
#READ IN FILE AND PLOT COG Region ----
centroid_data_region <- read.csv(here::here("R/DataforFinalFigs/centroid_dataRegionalforFig.csv"))
names(centroid_data_region)
unique(centroid_data_region$Season)
centr_region_spring<-centroid_data_region[centroid_data_region$Season=="Spring",]
centroid_reg_sf <- st_as_sf(centr_region_spring, coords = c("centroid_longitude", "centroid_latitude"))
st_crs(centroid_reg_sf) <- newcrs
centroid_reg_sf <- na.omit(centroid_reg_sf)

COG_Reg_map<-ggplot() +
  geom_sf(data = contours, color="lightblue") +
    geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black",lty=1,lwd=.8) +
  geom_sf(data = Hague,  fill = NA,lty=1,lwd=.8) +
    geom_sf(data = land, fill = "grey") +  
  geom_sf(data = centroid_reg_sf, aes(color = Year),size =1.5, alpha = .5,shape=16) +  
  xlim(-70.5, -58) + ylim(39.5, 47.2)+
  labs(title = "", x = "",y = "",
       color = "Year") +
  scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(centroid_reg_sf$Year)) +
  geom_segment(data = arrow_dataRegion,aes(x = x,y = y,xend = xend,yend = yend),
    arrow = arrow(length = unit(0.3, "cm")),color = "black",
    alpha = 0.8,
    size = 0.7
  )+
    annotate("text",x=-69,y=45,label="USA", vjust = 0,color = "black",size = 4) +
    annotate("text",x=-66.1,y=46,label="Canada", vjust = 0,color = "black",size = 4) +
      theme_bw()+
theme(text = element_text(family = "serif",size =12),  
                  legend.key.size = unit(0.3, "cm"),  # Reduce legend key size
                  legend.position = c(0.75, 0.2),                 # Position of the legend
                  legend.direction = "horizontal",
                  legend.box.background =element_rect(color = "black",fill = "white",linewidth = .5),
                  legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
                  legend.title = element_blank(),             # Hide legend title
                  legend.text = element_text(angle=45,size = 7, family = "serif",h=1), # Customize legend text
                  axis.title.y = element_blank(),             # Remove y-axis label
                  axis.title.x = element_blank(),      # Customize x-axis label
                  #axis.text.x = element_blank(),      # Customize x-axis label
                  plot.margin=margin(0,0,0,0))
#END READ IN FILE AND PLOT COG Region ---- 

#Figure Centre of Gravity for CA----
## code to save centroid data originally from 05.3_Fit_LM_COG_PlotSlopes.R
#slopes from 05.3
COGSlopeCI <- read.csv(here::here("R/DataforFinalFigs/COGSlopeCI.csv"))
COGSlopeCI$ordCore_Area<-factor(COGSlopeCI$ordCore_Area,levels=c("CapeCod","EGOM","BOF","Nantucket","Georges","Browns","Sable","Gully","CapeBreton"))
centroid_dataCA <- read.csv(here::here("R/DataforFinalFigs/centroid_dataCAforFig.csv"))
names(centroid_dataCA)

#set the data up for order
centroid_dataCA$Core_Area<-factor(centroid_dataCA$Core_Area,levels=c("EGOM","BOF","CapeBreton", "CapeCod","Browns","Sable","Nantucket","Georges","Gully"))
centroid_dataCA$ordCore_Area<-factor(centroid_dataCA$Core_Area,levels=c("CapeCod","EGOM","BOF","Nantucket","Georges","Browns","Sable","Gully","CapeBreton"))
centroid_dataCA$Period<-NULL
centroid_dataCA$Period[centroid_dataCA$Year<2006]<-"Before Warming"
centroid_dataCA$Period[centroid_dataCA$Year>2005]<-"During Warming"

Spring_centroid_dataCA<-subset(centroid_dataCA, centroid_dataCA$Season=="Spring")

#Turn  into sf
centroid_sf <- st_as_sf(centroid_dataCA, coords = c("centroid_longitude", "centroid_latitude"))#spatial shapefile for the centroid means, other centroid variables can be ignored  
st_crs(centroid_sf) <- crs
names(centroid_sf)
centroid_Median_sf <- st_as_sf(centroid_dataCA, coords = c("centroid_longitude_Median", "centroid_latitude_Median"))#spatial shapefile for the centroid median, other centroid variables can be ignored 
st_crs(centroid_Median_sf) <- crs
names(centroid_Median_sf)
#Plot the COG CA map ----
Spring_centroid_sf <- subset(centroid_sf, centroid_sf$Season=="Spring")
COG_CA_map<-ggplot() +
   geom_sf(data = contours, color="lightblue") +
  geom_sf(data = CA_df, fill = "transparent",lwd=1,color="steelblue",lty=1) +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black",lty=1,lwd=.8) +
  geom_sf(data = Hague,  fill = NA,lty=1,lwd=.8) +
  geom_sf(data = land, fill="grey") +
  geom_sf(data = Spring_centroid_sf, aes(color = Year),size =1.5, alpha = .5,shape=16) +  # Adjust size and alpha here
  #labs(title = "Centre of Gravity (Mean) within Core Areas:Spring", x = "Longitude", y = "Latitude",
    #   color = "Year") +
  scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(centroid_sf$Year)) +
 # #annotateCA----
  annotate("text",x=-69.7,y=40.5,label="NanSh", color = "black",size = 2.9) +
  annotate("text",x=-68.5,y=43.1,label="EGoM", vjust = 0, color = "black",size = 2.9) +
  annotate("text",x=-67.1,y=41,label="GB", vjust = 0, color = "black",size = 2.9) +
  annotate("text",x=-69.5,y=42.1,label="CC", vjust = 0, color = "black",size = 2.9) +
  annotate("text",x=-66.75,y=43.81,label="BoF", vjust = 0, color = "black",size = 2.9) +
  annotate("text",x=-66.3,y=42.5,label="BB", vjust = 0, color = "black",size = 2.9) +
  annotate("text",x=-61.75,y=43.7,label="Sable", vjust = 0, color = "black",size = 2.9) +
  annotate("text",x=-59,y=44.5,label="Gully", vjust = 0, color = "black",size = 2.9) +
  annotate("text",x=-59,y=45.8,label="CB", vjust = 0,color = "black",size = 2.9) +
  annotate("text",x=-69,y=45,label="USA", vjust = 0,color = "black",size = 4) +
  annotate("text",x=-66.1,y=46,label="Canada", vjust = 0,color = "black",size = 4) +
     #ENDannotateCA----
  xlim(-70.5, -58) + ylim(39.5, 47.2)+
 labs(title = "", x = "",y = "",color = "Year") +
  geom_segment(
    data = arrow_data,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    arrow = arrow(length = unit(0.35, "cm")),
    color = "black",
    alpha = .7,
    size = .7
)+theme_bw()+
  theme(text = element_text(family = "serif",size=12),  
        legend.position = "none",               # Position of the legend
        legend.direction = "horizontal",
        legend.box.background =element_rect(color = "black",fill = "white",linewidth = .5),
        legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.title = element_blank(),             # Hide legend title
        legend.text = element_text(angle=45,size = 12, family = "serif",h=1), # Customize legend text
        axis.title.y = element_blank(),             # Remove y-axis label
        #axis.title.x = element_blank(),      # Customize x-axis label
        plot.margin=margin(.1,.1,.5,.1))
COG_CA_map
#END #Plot the COG CA map ----
library(ggplot2)
library(cowplot)
library(gridExtra)

# Ensure both plots have identical margins
COG_Reg_map <- COG_Reg_map + theme(plot.margin = margin(0, 0,0,0),axis.title.y = element_blank())
COG_CA_map <- COG_CA_map + theme(plot.margin = margin(0, 0,0,0),axis.title.y = element_blank())

# Combine plots with better alignment
COGCombo <- plot_grid(
  COG_Reg_map, 
  COG_CA_map, 
  nrow = 2, 
  #rel_heights = c(1,2),rel_widths =c(1,1), 
  labels = c("(a)", "(b)"), 
 align = "vh",  # Align both horizontally and vertically
  axis = "tblr"  # Align all axes
)
COGCombo
ggsave(here::here("R/DataforFinalFigs/COGCombo.jpeg"), plot = COGCombo, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg") 
# COG SEGMENTS AND DISTANCES----
#create a dataframe with the coordinates for the arrows
# Assuming your Spring_centroid_sf has coordinates in order by Year
#From Claude
# MAKING ARROWS Create arrow data by group----
#  Find the earliest year in the first Period and latest year in the second Period for each Core Area
#Create a single arrow connecting these points
#Result in one directional arrow per Core Area showing the overall movement----
arrow_dataRegion <- centroid_reg_sf %>%
  group_by(survey) %>%
  summarize(
    # Start point - earliest year in first period
    #start_point = first(geometry[Year == min(Year[Period == first(Period)])]),
    start_point = first(geometry[Year == min(Year)]),
    # End point - latest year in second period
    end_point = first(geometry[Year == max(Year[Period == last(Period)])])
  ) %>%
  mutate(
    start_coords = st_coordinates(start_point),
    end_coords = st_coordinates(end_point),
    x = start_coords[,1],
    y = start_coords[,2],
    xend = end_coords[,1],
    yend = end_coords[,2]
  ) %>%
  st_drop_geometry()

# Add text to your existing plot (before theme_bw())
arrow_data <- Spring_centroid_sf %>%
  group_by(ordCore_Area) %>%
  summarize(
    # Start point - earliest year in first period
    #start_point = first(geometry[Year == min(Year[Period == first(Period)])]),
    start_point = first(geometry[Year == min(Year)]),
    # End point - latest year in second period
    end_point = first(geometry[Year == max(Year[Period == last(Period)])])
  ) %>%
  mutate(
    start_coords = st_coordinates(start_point),
    end_coords = st_coordinates(end_point),
    x = start_coords[,1],
    y = start_coords[,2],
    xend = end_coords[,1],
    yend = end_coords[,2]
  ) %>%
  st_drop_geometry()
#END Result in one directional arrow per Core Area showing the overall movement----
#to compute the distance in periods----
library(dplyr)
library(sf)
library(geosphere)
# Group by both survey and period----
centroid_reg_sf <- st_transform(centroid_reg_sf, crs = 4326)
arrow_dataRegionPeriod <- centroid_reg_sf %>%
  group_by(survey, Period) %>%  
  summarize(
    start_point = first(geometry[Year == min(Year)]),  
    end_point = first(geometry[Year == max(Year)]),  
    start_year = min(Year),  # Get the start year
    end_year = max(Year)     # Get the end year
  ) %>%
  mutate(
    start_coords = st_coordinates(start_point),
    end_coords = st_coordinates(end_point),
    x = start_coords[,1],   
    y = start_coords[,2],   
    xend = end_coords[,1],  
    yend = end_coords[,2],  
    dx = xend - x,          
    dy = yend - y,          
    d_total = distHaversine(cbind(x, y), cbind(xend, yend))/1000,  # Great-circle distance in meters
    years_in_period = end_year - start_year,  # Compute the number of years in the period
    dist_per_year = d_total / years_in_period   # Distance per year
  ) %>%
  select(survey, Period, x, y, xend, yend, d_total,  dist_per_year) %>%
  st_drop_geometry()
names(arrow_dataRegionPeriod)
# Rename the columns
library(rlang)
COGRegMovement<- arrow_dataRegionPeriod %>%
  set_names(
    c("Region_CoreArea", "Period", "Long.Start", "Lat.Start", 
      "Long.End", "Lat.End", "Distance(km)", "Dist_per_year")
  )
names(COGRegMovement)
Spring_centroid_sf <- st_transform(Spring_centroid_sf, crs = 4326)
library(dplyr)
library(sf)
library(geosphere)

arrow_dataCoreAreabyPeriod <- Spring_centroid_sf %>%
  group_by(ordCore_Area, Period) %>%  
  summarize(
    start_point = first(geometry[Year == min(Year)]),  
    end_point = first(geometry[Year == max(Year)]),  
    start_year = min(Year),  # Get the start year
    end_year = max(Year)     # Get the end year
  ) %>%
  mutate(
    start_coords = st_coordinates(start_point),
    end_coords = st_coordinates(end_point),
    x = start_coords[,1],   
    y = start_coords[,2],   
    xend = end_coords[,1],  
    yend = end_coords[,2],  
    dx = xend - x,          
    dy = yend - y,          
    d_total = distHaversine(cbind(x, y), cbind(xend, yend))/1000,  # Great-circle distance in meters
    years_in_period = end_year - start_year,  # Compute the number of years in the period
    dist_per_year = d_total / years_in_period   # Distance per year
  ) %>%
  select(ordCore_Area, Period, x, y, xend, yend,d_total, dist_per_year) %>%
  st_drop_geometry()

names(arrow_dataCoreAreabyPeriod) 
names(COGRegMovement)
COGCoreMovement<- arrow_dataCoreAreabyPeriod %>%
  set_names(
    c("Region_CoreArea", "Period", "Long.Start", "Lat.Start", 
      "Long.End", "Lat.End", "Distance(km)","Dist_per_year")
  )

#JOIN TABLES.
library(flextable);library(officer)
COGSupptable<-merge(COGRegMovement,COGCoreMovement,all=T)
COGSupptable <- COGSupptable %>%
  mutate(Region_CoreArea= factor(Region_CoreArea, 
levels = c("Canada", "USA", "CapeBreton","Gully","Sable","Browns","Georges","Nantucket","BOF","EGOM","CapeCod"), ordered = TRUE))  %>%  # Custom order for Period
arrange(Region_CoreArea, Period)
    
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
print(COGSupptabledoc2, target = here::here("R/DataforFinalFigs/COGSupptabledoc2.docx"))


#END Create arrow data by group----
ggsave(here::here("R/DataforFinalFigs/Figure3COG_CAMap.jpeg"), plot = COG_CA_map, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg") 
# FOR SUPPLEMENTAL USING CA DATA
#Plot script from 05_Centre of gravity
#plot for Supplemental COG Lat vs Long by Core Area----
suppcogca<-ggplot(Spring_centroid_dataCA, aes(x = centroid_longitude, y = centroid_latitude, color = Year)) +
  geom_point(na.rm=TRUE,alpha=1,size=1.5,shape=19) +
  labs(title = "Center of Gravity by Core Area: Spring",
       x = "Longitude",
       y = "Latitude",
       color = "Year") +
  scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(Spring_centroid_dataCA$Year)) +
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  facet_wrap(.~Core_Area,scales="free",nrow=3)
#END plot for Supplemental COG Lat vs Long by Core Area----
ggsave(here::here("R/DataforFinalFigs/FigureSUPPCOG_CAMap.jpeg"), plot =suppcogca, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg") 

# PLOT COG Slopes and CI CORE AREAS and combine----
COGSlopeCI$ord2Core_Area <- factor(COGSlopeCI$ordCore_Area,levels = rev(unique(COGSlopeCI$ordCore_Area)))                               
unique(COGSlopeCI$ord2Core_Area)
unique(COGSlopeCI$ordCore_Area)
p1<-ggplot(COGSlopeCI[COGSlopeCI$AxesNE=="Latitude",] , aes(x = (ord2Core_Area), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 2,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  geom_vline(xintercept = seq(1.5, length(unique(COGSlopeCI$ord2Core_Area)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  #scale_x_discrete(limits = rev(levels(factor(COGSlopeCI$ordCore_Area))))+ 
    ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change (DD/yr)")+
  annotate("text", x = 1.6, y = .015, label = "North", hjust = 0,size=4,family="serif") +
  annotate("text", x = 1.6, y = -.012, label = "South", hjust = 0,size=4,family="serif") +
  labs(tag = "(a) Latitude")+
    theme(plot.tag.position = c(.3, 1.1),plot.tag = element_text(family = "serif"),
        axis.text.x =element_text(angle=90), 
        axis.title.y = element_text(margin = margin(r = 5)),  #  space for y-axis label
        plot.margin = margin(5.5, 5.5, 5.5, 5.5))  # Increase left margin
        

p2<-ggplot(COGSlopeCI[COGSlopeCI$AxesNE=="Longitude",] , aes(x = ordCore_Area, y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 2,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
    ylim(-0.018,0.018)+
  coord_flip() +
  geom_vline(xintercept = seq(1.5, length(unique(filtered_df$ordCore_Area)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  xlab("")+  ylab("Rate of change (DD/yr)") +
  annotate("text", x = 3, y = -.017, label = "West", hjust = 0,size=4,family="serif") +
  annotate("text", x = 3, y = .009, label = "East", hjust = 0,size=4,family="serif") +
  labs(tag = "(b) Longitude",title=NULL)+
  theme(plot.tag.position = c(.3, 1.1),plot.tag = element_text(family = "serif"),
        axis.text.x=element_text(angle=90),
        axis.title.x= element_text(margin = margin(b = 0)),  # Bring x-axis label closer
        plot.margin = margin(5.5, 5.5, 5.5, 5.5))
        

combined <- (p1 | p2) +
  plot_layout(guides = "collect") &
  theme(
    legend.title=element_blank(),legend.text=element_text(family="serif"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(0.2, 'cm'),
    plot.margin = margin(30, 0, 0, 5)
  )
combined
# Save with optimal dimensions for publication
#ggsave("combined_figure.pdf", combined, width = 10, height = 5, 
 #      dpi = 300, device = cairo_pdf)
##END Combine----
ggsave(here::here("R/DataforFinalFigs/FigureS2COGCA.jpeg"), plot = combined, dpi = 600, width =10, height = 5, units = "in", device = "jpeg")


#Figure DIST REG AND CA Trends ----
# From 6_1_Plot DIst NOTE the regioanl scale is from 06_DistancefromHague
df_REGtransformedSpringAgg<-read.csv(here::here("2024-10-04/Output/Shift_Indicators/df_REGtransformedSpringAgg.csv"))
DistHagTrans_CA<-read.csv(here::here("R/DataforFinalFigs/DistHagCASpringTransformedforFig.csv"))
DistHagTrans_CA$ordCore_Area<-factor(DistHagTrans_CA$ordCore_Area,levels=c("CapeCod","EGOM","BOF","Nantucket","Georges","Browns","Sable","Gully","CapeBreton"))

pd <- position_dodge(.75)
##Plot Distance for REG----
#have to set order for region because plot is flipped
df_REGtransformedSpringAgg$Ord2Region<-factor(df_REGtransformedSpringAgg$Region, levels=c("USA","Canada"))
DistReg<-ggplot(df_REGtransformedSpringAgg , aes(x = Ord2Region, y = Avg_Dist_Mean.OPP,fill=Period)) +
  geom_linerange(aes(ymin = Avg_Dist_Q5.OPP, ymax =Avg_Dist_Q95.OPP),position=pd,color="darkgrey",size=1.5)+
  geom_point(shape=21, size = 3,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  coord_flip()+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  xlab("")+  ylab("")+
  #ylab("Distance from Hague line (km)")+
  theme_bw() + # A clean theme
  theme(
    text = element_text(family = "serif",size=14),  
    axis.text = element_text(family = "serif",size=14),      
    legend.position = "none",
    #legend.position = c(0.75, 0.2),               # Position of the legend
    #legend.box.background = element_blank(), # Transparent legend box
    legend.title = element_blank(),             # Hide legend title
    legend.text = element_text(size = 12, family = "serif"), # Customize legend text
    axis.title.y = element_blank(),             # Remove y-axis label
    axis.title.x = element_text(size = 14),      # Customize x-axis label
    panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
    plot.margin=margin(10,40,20,30)
  )
##Plot Distance for CA----
DistHag<-ggplot(DistHagTrans_CA , aes(x = factor(ordCore_Area), y = Avg_Dist_Mean.OPP,fill=Period)) +
    geom_linerange(aes(ymin = Avg_Dist_Q5.OPP, ymax =Avg_Dist_Q95.OPP),position=pd,color="darkgrey",size=1.5)+
    geom_point(shape=21, size = 3,position=pd) +
   scale_fill_manual(values=c("steelblue", "orangered"))+
   coord_flip()+
   #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
   geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
   xlab("")+  ylab("Distance from Hague line (km)")+
  theme_bw() + # A clean theme
  theme(
    text = element_text(family = "serif",size=14),  
    axis.text = element_text(family = "serif",size=14),           
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

  
ggsave(here::here("R/DataforFinalFigs/DistHag.jpeg"), plot = DistHagCombo, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")

##FIg Range Edge----
rangeedge <- read.csv(here::here("R/DataforFinalFigs/Edge_df_NSreshp.csv"))
names(rangeedge)
rangeedge$Period<-NULL
rangeedge$Period[rangeedge$Year<2006]<-"Before Warming"
rangeedge$Period[rangeedge$Year>2005]<-"During Warming"
names(rangeedge)

# First plot - with Year color gradient----
ggplot(rangeedge, aes(x = Estimate_km_E_quantile_0.5, y = Estimate_km_N_quantile_0.5, color = Year)) +
  geom_point(na.rm = TRUE, alpha = 1, size = 1.5, shape = 19) +
  scale_color_gradientn(colors = c("darkblue", "lightblue", "orange", "red"), 
                    limits = range(rangeedge$Year)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Period, nrow = 2,scales="fixed")
#end first plot----
# Second plot - with Period colors and error bars
# Create a position dodge object for consistent spacing
# Create a jitter object for more controlled random displacement
# Create a jitter object with both horizontal and vertical displacement
set.seed(123)  # For reproducibility
pos_jitter <- position_jitter(width = .31, height = .31)  # Jitter both horizontally and vertically
##together----
ggplot(rangeedge, aes(x = Estimate_km_E_quantile_0.5, y = Estimate_km_N_quantile_0.5, 
                      color = Period)) +
  geom_errorbarh(aes(
    xmin = Estimate_km_E_quantile_0.05 - Std_Dev_km_E_quantile_0.05, 
    xmax = Estimate_km_E_quantile_0.95 + Std_Dev_km_E_quantile_0.95
  ), height = 0.1, position = pos_jitter, na.rm = TRUE) +
  
  geom_errorbar(aes(
    ymin = Estimate_km_N_quantile_0.5 - Std_Dev_km_N_quantile_0.5, 
    ymax = Estimate_km_N_quantile_0.5 + Std_Dev_km_N_quantile_0.5
  ), width = 0.1, position = pos_jitter, na.rm = TRUE) +
  
  geom_point(position = pos_jitter, na.rm = TRUE, alpha = 1, size = 1.5, shape = 19) +
  scale_color_manual(values = c("steelblue","orangered")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        strip.background = element_rect(colour = "black", fill = "white"))
#END together----
## Two facets
RangeEdge<-ggplot(rangeedge, aes(x = Estimate_km_E_quantile_0.5, y = Estimate_km_N_quantile_0.5, 
                      color = Period)) +
  geom_errorbarh(aes(
    xmin = Estimate_km_E_quantile_0.05 - Std_Dev_km_E_quantile_0.05, 
    xmax = Estimate_km_E_quantile_0.95 + Std_Dev_km_E_quantile_0.95
  ), height = 0.1, position = pos_jitter, na.rm = TRUE) +
  
  geom_errorbar(aes(
    ymin = Estimate_km_N_quantile_0.5 - Std_Dev_km_N_quantile_0.5, 
    ymax = Estimate_km_N_quantile_0.5 + Std_Dev_km_N_quantile_0.5
  ), width = 0.1, position = pos_jitter, na.rm = TRUE) +
  
  geom_point(position = pos_jitter, na.rm = TRUE, alpha = 1, size = 1.5, shape = 19) +
  scale_color_manual(values = c("steelblue","orangered")) +
  ylab("Range Edge N (km)")+
  xlab("Range Edge E (km)")+
  theme_bw() +
  theme(legend.position = "none",
        plot.margin=margin(5,10,10,10),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust=.5,size=14,family="serif"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0,size=14,family="serif"),
        axis.title.x=element_text(size=16,hjust=0.5,vjust=-2,family="serif"),
        axis.title.y=element_text(size=16,hjust=0.5,vjust=4,angle=90,family="serif"),
        strip.text=element_text(size=14,family="serif",angle=0),
            strip.background = element_rect(colour = "black", fill = "white"))+
  facet_wrap(~Period,nrow=1,scales="fixed")
RangeEdge
##END Plot Range Edge----
ggsave(here::here("R/DataforFinalFigs/RangeEdge.jpeg"), plot = RangeEdge, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
