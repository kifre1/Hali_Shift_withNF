#MakeSlopeFile of all indicators----
Sys.setenv(OMP_NUM_THREADS = 4)#increase the amount of cores delegated to this script
library(tidyverse)
library(broom)
library(dplyr)

library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)
library(RColorBrewer)
library(scales)


library(ggpubr)
library(grid)

# First, create a standardized theme for all plots to ensure consistency
standard_theme <- function(show_legend = FALSE, bottom_margin = 3, top_margin = 3) {
  theme_bw(base_family = "serif") +
    theme(
      legend.position = if(show_legend) "bottom" else "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.key = element_rect(fill = "white", color = NA),
      axis.text.y = element_text(size = 12,angle = 0),
      axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
      axis.title = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 8)),
      strip.text = element_text(size = 12),
      strip.background = element_rect(colour = "black", fill = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(top_margin, 15, bottom_margin, 15),
      panel.spacing = unit(0.3, "cm")
    )
}
#end trail----
#Read in FIle
# Read in the coefficients dataframes
Combined_CentredSLopeBefAfterCoefficients <- read.csv("~/My_Program_Files/R/Hali_Shift_withNF/2025-04-23/Output/Shift_Indicators/Combined_CentredSLopeBefAfterCoefficients.csv")
combocoef<-Combined_CentredSLopeBefAfterCoefficients 
# Plotting the Scaled Slope Indicators
pd<-position_dodge(.3)
indicator_dividers <- data.frame(xintercept = c(1.5,2.5,3.5, 5.5,7.5,8.5,10.5))  # Replace with your actual breaks
summary(combocoef)
combocoef$Region[combocoef$Theme=="Range Edge"]<-"Range Edge"
combocoef$Region<-factor(combocoef$Region, levels = c("Range Edge","Canada", "USA"))
combocoef$Period<-factor(combocoef$Period, levels = c("1990-2005", "2006-2023"))
combocoef$Theme<-factor(combocoef$Theme)
#Does not work as facet SO we will have to filter for the range edge plot

#Rangeplot----
rangcoef<-combocoef%>%filter(Region=="Range Edge")
library(ggplot2)
library(forcats)

# Ensure Indicator has correct order
rangcoef$Indicator <- factor(rangcoef$Indicator, levels = c(
  "Leading Edge N", "Leading Edge E", 
  "Trailing Edge N", "Trailing Edge E"
))

# Position dodge setting
pd <- position_dodge(width = 0.2)

rangpl<-ggplot(rangcoef, aes(y = estimate, x = fct_rev(Indicator), fill = Period)) +
  # Error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = pd, width = 0, linewidth = 0.9) +
    # Points
  geom_point(shape = 21, size = 3, position = pd, stroke = 0.3, color = "black") +
    # Optional indicator divider lines
  geom_vline(data = indicator_dividers, aes(xintercept = xintercept), 
             linetype = "solid", color = "grey60", size = 0.4) +
    # Horizontal reference line
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
  # add more ticks to y-axis
  scale_y_continuous(breaks = seq(-30, 30, by = 5), 
                     limits = c(-30, 30), 
                     labels = scales::number_format(accuracy = 1)) +
  # Flip coordinates
  coord_flip() +
  annotate("text", x = 4.3, y = 15, label = "Northward", 
           family = "serif", size = 4, hjust = 0)+
  annotate("text", x = 4.3, y = -25, label = "Southward", 
           family = "serif", size = 4, hjust = 0)+
  annotate("text", x = 3.3, y = -25, label = "Eastward", 
           family = "serif", size = 4, hjust = 0)+
  annotate("text", x = 3.3, y = 15, label = "Westward", 
           family = "serif", size = 4, hjust = 0)+
  # Manual fill colors
  scale_fill_manual(values = c("steelblue", "orangered")) +
    # Facet by Region 
  facet_wrap(~Region, scales = "free", labeller = label_wrap_gen(width = 15)) +
  # Labels
  labs(
    #y = "Centered Slope of Indicator",
    y="",
    x = NULL,
    fill = "Period"
  ) +
  
  # Theme
  standard_theme()
rangpl
#ENDRangeplot----
#ALL THE REST
regpal= c("steelblue", "orangered")
rocoef<-combocoef%>%filter(Region!="Range Edge")
#ABD----
abdcoef<-rocoef%>%filter(Indicator=="Abundance")
abdpl <- ggplot(abdcoef, aes(y = estimate, x = Indicator, fill = Period)) +
  # Error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = pd, width = 0, linewidth = 0.9, color = "black") +
  # Points
  geom_point(size = 3, position = pd, stroke = 0.3, shape = 21, color = "black") +
  # Optional indicator divider lines
  geom_vline(data = indicator_dividers, aes(xintercept = xintercept), 
             linetype = "solid", color = "grey60", size = 0.4) +
  # Horizontal reference line
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
  # y-axis ticks
  scale_y_continuous(breaks = seq(-0.10, 0.10, by = 0.025), 
                     limits = c(-0.05, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  # Flip coordinates
  coord_flip() +
  # Manual fill colors for Period
  #guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(values = c("steelblue", "orangered")) +
  # Facet by Region 
  facet_wrap(Region ~ ., nrow=1,scales = "fixed", labeller = label_wrap_gen(width = 15)) +
  # Labels
  labs(
    #y = "Centered Slope of Indicator",
    y="",
    x = NULL,
    fill = "Period"
  ) +
  # Theme
  standard_theme()

abdpl
#ENDABD----
#COG----
cogcoef<-rocoef%>%filter(Indicator=="COG North" | Indicator=="COG East")
cogpl <- ggplot(cogcoef, aes(y = estimate, x = Indicator, fill = Period)) +
  # Error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = pd, width = 0, linewidth = .8, color = "black") +
  # Points
  geom_point(size = 2.5, position = pd, stroke = 0.3, shape = 21, color = "black") +
  # Optional indicator divider lines
  geom_vline(data = indicator_dividers, aes(xintercept = xintercept), 
             linetype = "solid", color = "grey60", size = 0.4) +
  # Horizontal reference line
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
  # y-axis ticks
  scale_y_continuous(breaks = seq(-0.20, 0.20, by = 0.1), 
                     limits = c(-0.20, 0.20), 
                     labels = scales::number_format(accuracy = 0.1)) +
  # Flip coordinates
  coord_flip() +
    # Manual fill colors for Period
  #guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(values = c("steelblue", "orangered")) +
  # Facet by Region 
  facet_wrap(Region ~ ., nrow=1,scales = "fixed", labeller = label_wrap_gen(width = 15)) +
  #direction
  annotate("text", x = 2.3, y = .05, label = "Northward", 
           family = "serif", size = 4, hjust = 0)+
  annotate("text", x = 2.3, y = -.2, label = "Southward", 
           family = "serif", size = 4, hjust = 0)+
  annotate("text", x = 1.3, y = .05, label = "Eastward", 
           family = "serif", size = 4, hjust = 0)+
  annotate("text", x = 1.3, y = -.20, label = "Westward", 
           family = "serif", size = 4, hjust = 0)+
  # Labels
  labs(
    #y = "Centered Slope of Indicator",
    y="",
    x = NULL,
    fill = "Period"
  ) +standard_theme()
  

cogpl
#END COGpl---


#AO----
AOcoef<-rocoef%>%filter(Indicator=="Area Occupied")
AOcoefpl <- ggplot(AOcoef, aes(y = estimate, x = Indicator, fill = Period)) +
  # Error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = pd, width = 0, linewidth = 0.9, color = "black") +
  # Points
  geom_point(size = 3, position = pd, stroke = 0.3, shape = 21, color = "black") +
  # Optional indicator divider lines
  geom_vline(data = indicator_dividers, aes(xintercept = xintercept), 
             linetype = "solid", color = "grey60", size = 0.4) +
  # Horizontal reference line
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
  # y-axis ticks
  scale_y_continuous(breaks = seq(-0.020, 0.02, by = 0.01), 
                     limits = c(-0.02, 0.02), 
                     labels = scales::number_format(accuracy = 0.001)) +
  # Flip coordinates
  coord_flip() +
  # Manual fill colors for Period
  #guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(values = c("steelblue", "orangered")) +
  # Facet by Region 
  facet_wrap(Region ~ ., nrow=1,scales = "fixed", labeller = label_wrap_gen(width = 15)) +
  # Labels
  labs(
    #y = "Centered Slope of Indicator",
    y="",
    x = NULL,
    fill = "Period"
  ) +
  # Theme
  standard_theme()

AOcoefpl


#AOBD----
AOabdcoef<-rocoef%>%filter(Indicator=="AO vs Abd")
AOabdpl <- ggplot(AOabdcoef, aes(y = estimate, x = Indicator, fill = Period)) +
  # Error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = pd, width = 0, linewidth = 0.9, color = "black") +
  # Points
  geom_point(size = 3, position = pd, stroke = 0.3, shape = 21, color = "black") +
  # Optional indicator divider lines
  geom_vline(data = indicator_dividers, aes(xintercept = xintercept), 
             linetype = "solid", color = "grey60", size = 0.4) +
  # Horizontal reference line
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
  # y-axis ticks
  scale_y_continuous(breaks = seq(-1, 1, by = 0.5), 
                     limits = c(-1, 1), 
                     labels = scales::number_format(accuracy = 0.1)) +
  # Flip coordinates
  coord_flip() +
  # Manual fill colors for Period
  #guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(values = c("steelblue", "orangered")) +
  # Facet by Region 
  facet_wrap(Region ~ ., nrow=1,scales = "fixed", labeller = label_wrap_gen(width = 15)) +
  # Labels
  labs(
    #y = "Centered Slope of Indicator",
    y="",
    x = NULL,
    fill = "Period"
  ) +
  # Theme
  standard_theme()

AOabdpl
#END AOAB----
#DWA----
dwacoef<-rocoef%>%filter(Indicator=="Depth-wtd Abd")
dwapl <- ggplot(dwacoef, aes(y = estimate, x = Indicator, fill = Period)) +
  # Error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = pd, width = 0, linewidth = 0.9, color = "black") +
  # Points
  geom_point(size = 3, position = pd, stroke = 0.3, shape = 21, color = "black") +
  # Optional indicator divider lines
  geom_vline(data = indicator_dividers, aes(xintercept = xintercept), 
             linetype = "solid", color = "grey60", size = 0.4) +
  # Horizontal reference line
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
  # y-axis ticks
  scale_y_continuous(breaks = seq(-12, 12, by = 4), 
                     limits = c(-12,12), 
                     labels = scales::number_format(accuracy = 1)) +
  # Flip coordinates
  coord_flip() +
  # Manual fill colors for Period
  #guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(values = c("steelblue", "orangered")) +
  # Facet by Region 
  facet_wrap(Region ~ ., nrow=1,scales = "fixed", labeller = label_wrap_gen(width = 15)) +
  # Labels
  labs(
   # y = "Centered Slope of Indicator",
    y="",
    x = NULL,
    fill = "Period"
  ) +
  # Theme
  standard_theme() 
dwapl
#END DWA----
#DTB----
dtbcoef<-rocoef%>%filter(Indicator=="Distance to Border")
dtbpl <- ggplot(dtbcoef, aes(y = estimate, x = Indicator, fill = Period)) +
  # Error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = pd, width = 0, linewidth = 0.9, color = "black") +
  # Points
  geom_point(size = 3, position = pd, stroke = 0.3, shape = 21, color = "black") +
  # Optional indicator divider lines
  geom_vline(data = indicator_dividers, aes(xintercept = xintercept), 
             linetype = "solid", color = "grey60", size = 0.4) +
  # Horizontal reference line
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
  # y-axis ticks
  scale_y_continuous(breaks = seq(-25, 25, by = 10), 
                     limits = c(-25, 25), 
                     labels = scales::number_format(accuracy = 1)) +
  # Flip coordinates
  coord_flip() +
  # Manual fill colors for Period
  #guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(values = c("steelblue", "orangered")) +
  # Facet by Region 
  facet_wrap(Region ~ ., nrow=1,scales = "fixed", labeller = label_wrap_gen(width = 15)) +
  # Labels
  labs(
    y = "Centered Slope of Indicator",
    #y="",
    x = NULL,
    fill = "Period"
  ) +
  # Theme
  standard_theme()
dtbpl
#END DTB----

abdpl
AOcoefpl
dtbpl
dwapl
cogpl
rangpl

# Combine all plots into one figure---

rangpl<-rangpl + 
    standard_theme(show_legend = FALSE, bottom_margin = 0, top_margin = 0)
abdpl<-abdpl +
    standard_theme(show_legend = FALSE, bottom_margin = 1, top_margin = 1)
cogpl<-cogpl +
    standard_theme(show_legend = FALSE, bottom_margin = 1, top_margin = 1)
AOcoefpl<-AOcoefpl+
    standard_theme(show_legend = FALSE, bottom_margin = 1, top_margin = 1)
AOabdpl<-AOabdpl +
    standard_theme(show_legend = FALSE, bottom_margin = 1, top_margin = 1)
dwapl<-dwapl +
    standard_theme(show_legend = FALSE, bottom_margin = 1, top_margin = 1)
dtbpl<-dtbpl +
    standard_theme(show_legend = TRUE, bottom_margin = 1, top_margin = 1)


library(gridExtra)
library(egg)

simple_combo <- ggarrange(
  abdpl + theme(legend.position = "none"),
  cogpl + theme(legend.position = "none",strip.text = element_blank()),
  AOcoefpl + theme(legend.position = "none",strip.text = element_blank()),
  AOabdpl + theme(legend.position = "none",strip.text = element_blank()),
  rangpl + theme(legend.position = "none",strip.text = element_blank()),
  dwapl + theme(legend.position = "none",strip.text = element_blank()),
  dtbpl + theme(legend.position = "bottom",strip.text = element_blank()),
  nrow =7,
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)","(g)"))
# Save the combined plot
 
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/CentredSlopeCombo.jpeg"), plot = simple_combo, dpi = 600, width = 8, height = 11, units = "in", device = "jpeg") 
