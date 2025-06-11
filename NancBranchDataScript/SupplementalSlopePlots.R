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


#ABUNDANCE SLOPES Calculate and plot the change in slope for each time period
#Regional
Reg_SlopSpring <- read.csv(here::here("2025-04-23/Output/IndexAbundance/RegionAbdSlope.Spring.csv"),row.names=NULL)
names(Reg_SlopSpring)
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
  # theme_minimal() + # A cleaner minimal theme
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
  ylab("Rate of change in Logged Abundance/year")
RegionRatesPlot
RegionRatesPlot


FigureAAbdRates<-plot_grid(ARegionalPlot, RegionRatesPlot, nrow = 2,rel_heights = c(2, 1),labels = c("(a)", "(b)"))#,align = "v", axis = "lr") # Add labels
Figure2AbdAbdRates
#ggsave(here::here("R/DataforFinalFigs/Figure2AbdAbdRates.tiff"), plot = Figure2AbdAbdRates, dpi = 600, width = 8, height = 6, units = "in", device = "tiff")
# END Plot abundance indexed regions  ----
#ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/Figure2AbdAbdRates.jpeg"), plot = Figure2AbdAbdRates, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")



abundance_ind_CA$Period<-NULL
abundance_ind_CA$Period[abundance_ind_CA$Year<2006]<-"Before Warming"
abundance_ind_CA$Period[abundance_ind_CA$Year>2005]<-"During Warming"

CA_Abundance_coefficients_df <- abundance_ind_CA %>%
  group_by(Index_Region,Period) %>%
  do({
    model <- lm((Index_Estimate) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

CA_Abundance_coefficients_df <- CA_Abundance_coefficients_df%>%
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept
CA_Abundance_coefficients_df$ordRegion<-factor(CA_Abundance_coefficients_df$Index_Region,levels=c("EGOM","BOF","CapeBreton","HaliChan",
                                                                                                  "CapeCod","Browns","Gully","GrandBanks",
                                                                                                  "Nantucket","Georges","Sable","GBTail"))

pd <- position_dodge(.5)

ggplot(CA_Abundance_coefficients_df  , aes(x =  fct_rev(factor(ordRegion)), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  coord_flip()+
  scale_fill_manual(values=c("steelblue", "orangered"))+
  #geom_vline(xintercept = seq(1.5, length(unique(filtered_df$ordCore_Area)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  # ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change in Abundance count/yr")+
  ggtitle("Rate of change in Abundance Before \n and during accelerated warming , Spring ")