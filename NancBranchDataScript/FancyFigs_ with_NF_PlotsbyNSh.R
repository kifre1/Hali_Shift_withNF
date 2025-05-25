library(tidyverse)
library(broom)
library(dplyr)

library(ggplot2)
library(cowplot)
library(patchwork)
library(RColorBrewer)

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
# Plot abuncance indexed regions ----

FigAbd.Region.Spring <- read.csv(here::here("2025-04-23/Output/IndexAbundance/abundance_ind_Region.Spring.csv"),row.names=NULL)
names(FigAbd.Region.Spring)

Reg_SlopSpring <- read.csv(here::here("2025-04-23/Output/IndexAbundance/RegionAbdSlope.Spring.csv"),row.names=NULL)
names(Reg_SlopSpring)
#To estimate the proportion ot toal abundance by Season, I have to do it here as teh script in 3.3Regional_Proportions can only easily be run if you have VAST
Reg_Prop<-read.csv(here::here("2025-04-23/Output/Proportions/proportions_and_density_Regional.csv"))
#isolate spring 
Reg_Prop_Spring <- subset(Reg_Prop, Reg_Prop$Season == "Spring")
names(Reg_Prop_Spring)
#plot check and adding vars to figure out proportion before and during warming.----
Reg_Prop_Spring$Period<-NULL
Reg_Prop_Spring$Period[Reg_Prop_Spring$Year<2006]<-"Before Warming"
Reg_Prop_Spring$Period[Reg_Prop_Spring$Year>2005]<-"During Warming"
Reg_Prop_Spring$Region<-factor(Reg_Prop_Spring$Index_Region)
names(Reg_Prop_Spring)
library(dplyr)

popr <- Reg_Prop_Spring %>%
  group_by(Period) %>%
  mutate(Total_EstimateForPeriod = sum(Index_Estimate)) %>%
  ungroup() %>%
  group_by(Period, Region) %>%
  summarise(Total_EstimateForRegion = sum(Index_Estimate), 
            Total_EstimateForPeriod = first(Total_EstimateForPeriod)) %>%
  mutate(Prop = Total_EstimateForRegion / Total_EstimateForPeriod * 100) %>%
  ungroup()

Regiona_Proportion_Plot_Spring<-ggplot(Reg_Prop_Spring, aes(x = Year, y = Proportion, fill = Index_Region)) +
  geom_area() +
  scale_fill_manual(values = c("orange", "darkblue")) +
  labs(
    title = "Mean Proportion of Total Est. Abun. Spring",
    x = "Year",
    y = "Proportion of Total (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)
##END adding vars to figure out proportion before and during warming.----
popr
##Figure ABD Panel A----
regpal<- c("orange", "darkblue")
colbline<-brewer.pal(3, "Dark2")
custom_labels <- c(
  "Canada: Before (93.9%), During (94%)",
  "USA:    Before ( 6.1%), During ( 6%)"
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
  annotate("text", x = 1996, y = 4.3, label = "Before Warming", color = "black", size = 5,family = "serif") +
  annotate("text", x = 2014, y = 4.3, label = "During Warming", color = "black", size = 5,family = "serif") +
  #Before
 # annotate("text", x = 1995, y = 3.1, label = "94%", color = "black", size = 4) +
#  annotate("segment", x = 1995, xend = 1995, y = 2.7, yend = 2, arrow = arrow(length = unit(.2, "cm"),type="closed")) +
 # annotate("text", x = 1992, y = .85, label = "6%", color = "black", size = 4) +
  #annotate("segment", x = 1992, xend = 1992, y = .7, yend = .3, arrow = arrow(length = unit(.2, "cm"),type="closed")) +
  #After
  #annotate("text", x = 2015, y = 6.2, label = "94%", color = "black", size = 4) +
  #annotate("segment", x = 2015, xend = 2015, y = 5.7, yend = 5.2, arrow = arrow(length = unit(.2, "cm"),type="closed")) +
  #annotate("text", x = 2015, y = .9, label = "6%", color = "black", size = 4) +
  #annotate("segment", x = 2015, xend = 2015, y = .7, yend = .3, arrow = arrow(length = unit(.2, "cm"),type="closed")) +
  #annotate("rect", xmin = 2004, xmax = 2014, ymin = 3, ymax = 5, alpha = 0.2, fill = "red") +
  theme(text = element_text(family = "serif"),  
        legend.box.background = element_blank(), # Transparent legend box
        legend.position.inside = c(.15,.7),
        legend.text = element_text(size = 14,family="serif"),
        plot.margin=margin(10, 5, 10, 15))
ARegionalPlot
#END Figure ABD A----
#Figure ABD RATE----
Reg_SlopSpring$Ord2Region<-factor(Reg_SlopSpring$Index_Region, levels=c("USA","Canada"))

Reg_SlopSpring$RevPeriod <- factor(Reg_SlopSpring$Period, levels = c("During Warming", "Before Warming")) 
RegionRatesPlot<-
  ggplot(Reg_SlopSpring , aes(x = factor(Ord2Region), y = estimate,fill=RevPeriod)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = pd) +
  geom_point(shape = 21, size = 3, position = pd) +
  guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(values = c("orangered","steelblue" ))+ #Reverse the colors
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") + # Dashed line for y=0
  ylim(-0.06, 0.06) +
  theme_minimal() + # A cleaner minimal theme
  theme(
    text = element_text(family = "serif"),  
    legend.position.inside = c(0.25, 0.6),               # Position of the legend
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
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/Figure2AbdAbdRates.jpeg"), plot = Figure2AbdAbdRates, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
#EOA plots and table for supplemental----

# PLOT EAO,----
eaothresh <- read.csv(here::here("R/DataforFinalFigs/Area_ThresholdsforEAO.csv"))
names(EAOcc_DF_Region_Spring)

##PlotEOA----
#IN Appendix S1 Fig S EAO 
EAOplot<-ggplot(area_thresholds %>% filter(Threshold == 90), 
                  aes(x = Year, y = Area_Threshold, color = Region, group = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_fill_manual(values = regpal) +                    # Custom fill colors
  scale_color_manual(values = regpal) +
  labs(title = "Trend in Area Containing 90% of Abundance", 
       x = "Year", 
       y = "Area (km²)",
       color = "Region") +
  guides(color = guide_legend(title = ""))+
  geom_vline(xintercept=2005,lty=2,lwd=1.2)+
  theme(text = element_text(family = "serif"),  
        legend.box.background = element_blank(), # Transparent legend box
        legend.position.inside = c(.15,.7),
        legend.text = element_text(size = 14,family="serif"),
        plot.margin=margin(10, 5, 10, 15))
EAOplot
##END PlotEOA----
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/EAOplot.jpeg"), plot = EAOplot, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")

## PlotEAOvsAbd----
PlotEAOAbd<- ggplot(area_thresholds %>% filter(Threshold == 90),
                 aes(y = log10(Area_Threshold), x = log10(Total_Abundance))) +
  geom_point(aes(color = Period), size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = interaction(Region, Period), color = Period)) +
  stat_poly_eq(
    aes(group = interaction(Region, Period), color = Period,
        label = paste(after_stat(eq.label),
                      after_stat(rr.label),
                      after_stat(p.value.label), sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = 0.9,
    size = 3
  ) +
  facet_wrap(Region~., scales = "free") +
  labs(title = "Area vs Annual Abundance by Region",
       x = "Total Annual Abundance",
       y = "Area (sqkm)",
       color = "Period") +
  theme_minimal()
PlotEAOAbd
## END PlotEAOvsAbd----
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/PlotEAOAbd.jpeg"), plot = PlotEAOAbd, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")

#LOAD Sup2DataPlots.R and alter MakingMapArrowsandTables.r
cogreg<- read.csv(here::here("R/DataforFinalFigs/centroid_dataRegionalforFig.csv"))
cogregslope<- read.csv(here::here("R/DataforFinalFigs/COGSlopeCI_Regional.csv"))
names(cogregslope)
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

#Plot COGMap----
COG_Reg_map<-ggplot() +
  geom_sf(data = contours, color="lightblue") +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black",lty=1,lwd=.8) +
  geom_sf(data = Hague,  fill = NA,lty=1,lwd=.8) +
  geom_sf(data = land, fill = "grey") +  
  geom_sf(data = centroid_reg_sf_spr, aes(color = Year),size =1.5, alpha = .5,shape=16) +  
  xlim(-70.5, -48) + ylim(39.5, 50)+
  labs(title = "", x = "",y = "",
       color = "Year") +
  scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(centroid_reg_sf_spr$Year)) +
  geom_segment(data = arrow_dataRegion,aes(x = x,y = y,xend = xend,yend = yend),
               arrow = arrow(length = unit(0.35, "cm")),
               color = "black",
               alpha = .7,
               size = 1.5) +
    annotate("text",x=-69,y=45,label="USA", vjust = 0,color = "black",size = 3.5) +
  annotate("text",x=-66.,y=46,label="Canada", vjust = 0,color = "black",size = 3.5) +
  theme_bw()+
  theme(text = element_text(family = "serif",size =12),  
        legend.key.size = unit(0.2, "cm"),  # Reduce legend key size
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
COG_Reg_map
#END READ IN FILE AND PLOT COG Region ----
#Plot COG CA----
#Don't forget to run arrows in MakingMapArrowsandTables.r

COG_CA_map<-ggplot() +
  geom_sf(data = contours, color="lightblue") +
  geom_sf(data = CoreAreas_df, fill = "transparent",lwd=1,color="steelblue",lty=1) +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black",lty=1,lwd=.8) +
  geom_sf(data = Hague,  fill = NA,lty=1,lwd=.8) +
  geom_sf(data = land, fill="grey") +
  geom_sf(data = centroid_CA_sf_spr, aes(color = Year),size =1.5, alpha = .5,shape=16) +  # Adjust size and alpha here
  #labs(title = "Centre of Gravity (Mean) within Core Areas:Spring", x = "Longitude", y = "Latitude",
  #   color = "Year") +
  scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(centroid_CA_sf_spr$Year)) +
  annotate("text",x=-69,y=45,label="USA", vjust = 0,color = "black",size = 3.5) +
  annotate("text",x=-66,y=46,label="Canada", vjust = 0,color = "black",size = 3.5) +
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
xlim(-70.5, -48) + ylim(39.5, 50)+
  labs(title = "", x = "",y = "",color = "Year") +
  geom_segment(
    data = arrow_data_CA,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    arrow = arrow(length = unit(0.35, "cm")),
    color = "black",
    alpha = .7,
    size = 1.5
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
#END PLOT COG CA ----
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
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/COGCombo.jpeg"), plot = COGCombo, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg") 

#Making supplemental tables for distances and for slopes----
#Distance Table
COGSupptable<-merge(COGRegMovement,COGCoreMovement,all=T)
COGSupptable <- COGSupptable %>%
  mutate(Region_CoreArea= factor(Region_CoreArea, 
                                 levels = c("Canada", "USA", "Nantucket","Georges","CapeCod","EGOM","BOF","Browns","Sable","Gully","CapeBreton","GrandBanks","GBTail","HaliChan"), ordered = F))  %>%  # Custom order for Period
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
  scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(cogCA_spr$Year)) +
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  facet_wrap(.~ordCoreArea,scales="free",nrow=3)
suppcogca
#END plot for Supplemental COG Lat vs Long by Core Area----
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/FigureSUPPCOG_CAMap.jpeg"), plot =suppcogca, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg") 

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
#END FOR SUPPLEMENTAL USING CA DATA----