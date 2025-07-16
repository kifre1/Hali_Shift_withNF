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
  ylim(-0.1,0.1)+
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
  ylab("")
RegionRatesPlot

#Core Areas----


#2.2 Calculate and plot the change in slope for each time period for each CA----
abundance_ind_CA <- read.csv(here::here("2025-04-23/Output/IndexAbundance/abundance_ind_CA.csv"),row.names=NULL)
abundance_ind_CA$Period<-NULL
abundance_ind_CA$Period[abundance_ind_CA$Year<2006]<-"Before Warming"
abundance_ind_CA$Period[abundance_ind_CA$Year>2005]<-"During Warming"

names(abundance_ind_CA)
abundance_ind_CA.spr<- subset(abundance_ind_CA, abundance_ind_CA$Season == "Spring")

abundance_ind_CA.spr$ordCoreArea<-factor(abundance_ind_CA.spr$Index_Region, levels=c("Nantucket","CapeCod","EGOM","Georges","BOF","Browns","Sable","CapeBreton","Gully","HaliChan","GrandBanks","GBTail"))

#Assign colors to regions----
region_colours2 <- c(
  "Nantucket" = "#56B4E9",
  "CapeCod" = "#2171B5",
  "EGOM" = "#004995",
  "Georges" = "#EDA752",
  "BOF" = "#8C510A",
  "Browns" = "#D8781D",
  "Sable" = "#C0C0C0",
  "Gully" = "#7F7F7F",
  "CapeBreton" = "#4D4D4D",
  "HaliChan" = "#00441B",
  "GrandBanks" = "#238B45",
  "GBTail" = "#81C784"
)


abundance_ind_CA.spr$ordCoreArea<-factor(abundance_ind_CA.spr$ordCoreArea, levels=names(region_colours2))
#END assign colors estimate slopes----
CA_Abundance_coefficients_df <- abundance_ind_CA.spr %>%
  group_by(ordCoreArea,Period) %>%
  do({
    model <- lm(log10(Index_Estimate+1) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

CA_Abundance_coefficients_df <- CA_Abundance_coefficients_df%>%
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept

#END assign colors and order and estimate slopes----
pd <- position_dodge(.5)
# Plot the coefficients with error bars----
#ggplot(CA_Abundance_coefficients_df  , aes(x =  fct_rev(factor(ordCoreArea)), y = estimate,fill=Period)) +
CARatesPlot<-  ggplot(CA_Abundance_coefficients_df  , aes(x =  factor(ordCoreArea), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  coord_flip()+
  scale_fill_manual(values=c("steelblue", "orangered"))+
  #geom_vline(xintercept = seq(1.5, length(unique(filtered_df$ordCore_Area)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
   ylim(-0.1,0.1)+
  xlab("")  +ylab("Rate of change in Logged Abundance/yr")
#+ggtitle("Rate of change in Abundance Before \n and during accelerated warming , Spring ")

  CARatesPlot
  

#END manual labels----
  # Ensure both plots have identical margins
  RegionRatesPlot<- RegionRatesPlot + theme(plot.margin = margin(t=0,b=0,l=10,r=5),axis.title.y = element_blank())
  CARatesPlot <- CARatesPlot + theme(plot.margin = margin(t=0,b=10,l=10,r=5),axis.title.y = element_blank(),legend.position = "none")
  
  # Combine plots with better alignment
  RatesAbdCombo <- plot_grid(
    RegionRatesPlot, 
    CARatesPlot, 
    nrow = 2, 
    rel_heights = c(1,2),rel_widths =c(1,1), 
    labels = c("(a)", "(b)"), 
    align = "v",  # Align both horizontally and vertically
    axis = "tblr"  # Align all axes
  )
  RatesAbdCombo
  ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/SUPPRatesAbdCombo.jpeg"), plot =  RatesAbdCombo, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg") 
  

FigureAbdSlopesRates<-
  plot_grid(RegionRatesPlot, CARatesPlot, nrow = 2,rel_heights = c(1, 2),labels = c("(a)", "(b)"),align = "v", axis = "lr") # Add labels
  plot_layout(guides = "collect") &
    theme(
      legend.title=element_blank(),legend.text=element_text(family="serif"),
      legend.position = "top",
      legend.box = "horizontal",
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0.2, 'cm'),
      plot.margin = margin(30, 0, 0, 5)
    )
  FigureAbdSlopesRates
#to be more accurate aobut labels, we can use the following code to add a legend
final_plot2 <- draw_plot_label(
  final_plot2,
  label = c("a", "b"),
  x = c(0.01, 0.01, 0.52),  # x-positions for labels (in [0, 1] units)
  y = c(0.99, 0.48, 0.48),  # y-positions (top of each panel)
  size = 14,
  fontface = "bold"
)
# Save the final plot with legend
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/FigureAbdSlopesRates.jpeg"), plot = final_plot, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")



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
#COGrates of change----
cogCAslope<- read.csv(here::here("R/DataforFinalFigs/COGSlopeCI_CoreAreas.csv"),row.names=NULL)
names(COGSlopeCI_CoreAreas)
# PLOT COG Slopes and CI CORE AREAS and combine----
cogCAslope$ordCoreArea<-factor(cogCAslope$Stratum, levels=c("Nantucket","CapeCod","EGOM","Georges","BOF","Browns","Sable","CapeBreton","Gully","HaliChan","GrandBanks","GBTail"))


unique(cogCAslope$ordCoreArea)
cogCAslope_spr<-cogCAslope[cogCAslope$Season=="Spring",]

cogCAslope_spr$ord2CoreArea <- factor(cogCAslope_spr$ordCoreArea,levels = rev(unique(cogCAslope_spr$ordCoreArea)))                               
unique(cogCAslope_spr$ord2CoreArea)
unique(cogCAslope_spr$ordCoreArea)
p1<-ggplot(cogCAslope_spr[cogCAslope_spr$AxesNE=="Latitude",] , aes(x = (ordCoreArea), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 2,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  geom_vline(xintercept = seq(1.5, length(unique(cogCAslope_spr$ordCoreArea)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  #scale_x_discrete(limits = rev(levels(factor(COGSlopeCI$ordCore_Area))))+ 
  #ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change (DD/yr)")+
  annotate("text", x = 1.6, y = .015, label = "North", hjust = 0,size=4,family="serif") +
  annotate("text", x = 1.6, y = -.012, label = "South", hjust = 0,size=4,family="serif") +
  labs(tag = "(a) Latitude")+
  theme(plot.tag.position = c(.3, 1.1),plot.tag = element_text(family = "serif"),
        axis.text.x =element_text(angle=90), 
        axis.title.y = element_text(margin = margin(r = 5)),  #  space for y-axis label
        plot.margin = margin(5.5, 5.5, 5.5, 5.5))  # Increase left margin

p1
p2<-ggplot(cogCAslope_spr[cogCAslope_spr$AxesNE=="Longitude",] , aes(x = ordCoreArea, y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 2,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  #ylim(-0.018,0.018)+
  coord_flip() +
  geom_vline(xintercept = seq(1.5, length(unique(cogCAslope_spr$ordCoreArea)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  xlab("")+  ylab("Rate of change (DD/yr)") +
  annotate("text", x = 3, y = -.017, label = "West", hjust = 0,size=4,family="serif") +
  annotate("text", x = 3, y = .009, label = "East", hjust = 0,size=4,family="serif") +
  labs(tag = "(b) Longitude",title=NULL)+
  theme(plot.tag.position = c(.3, 1.1),plot.tag = element_text(family = "serif"),
        axis.text.x=element_text(angle=90),
        axis.title.x= element_text(margin = margin(b = 0)),  # Bring x-axis label closer
        plot.margin = margin(5.5, 5.5, 5.5, 5.5))

p2
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
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/FigureSCOGCASlopes.jpeg"), plot = combined, dpi = 600, width =10, height = 5, units = "in", device = "jpeg")
#END FOR SUPPLEMENTAL USING CA DATA----

