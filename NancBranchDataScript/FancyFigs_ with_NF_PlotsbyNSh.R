library(dplyr)

library(broom)

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
#Plot COGMap----
#LOAD Sup2DataPlots.R and MakingMapArrowsandTables.r
cogreg<- read.csv(here::here("R/DataforFinalFigs/centroid_dataRegionalforFig.csv"))
cogregslope<- read.csv(here::here("R/DataforFinalFigs/COGSlopeCI_Regional.csv"))
names(cogregslope)
cogCA<- read.csv(here::here("R/DataforFinalFigs/centroid_dataCAforFig.csv"))
cogCAslope<- read.csv(here::here("R/DataforFinalFigs/COGSlopeCI_CoreAreas.csv"))
names(cogregslope)

unique(cogreg$Season)
cogreg_spring<-cogreg[cogreg$Season=="Spring",]
centroid_reg_sf <- st_as_sf(cogreg_spring, coords = c("centroid_longitude", "centroid_latitude"))
st_crs(centroid_reg_sf) <- crs
centroid_reg_sf <- na.omit(centroid_reg_sf)

COG_Reg_map<-ggplot() +
  geom_sf(data = contours, color="lightblue") +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = EEZ, color="black",lty=1,lwd=.8) +
  geom_sf(data = Hague,  fill = NA,lty=1,lwd=.8) +
  geom_sf(data = land, fill = "grey") +  
  geom_sf(data = centroid_reg_sf, aes(color = Year),size =1.5, alpha = .5,shape=16) +  
  xlim(-70.5, -47) + ylim(39.5, 50)+
  labs(title = "", x = "",y = "",
       color = "Year") +
  scale_color_gradientn(colors = c("darkblue",  "lightblue","orange", "red"), limits = range(centroid_reg_sf$Year)) +
  geom_segment(data = arrow_dataRegion,aes(x = x,y = y,xend = xend,yend = yend),
              arrow = arrow(length = unit(.3, "cm")),color = "black",
           alpha = 0.8,
            size = 1.7)+
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
COG_Reg_map
#END READ IN FILE AND PLOT COG Region ---- 
