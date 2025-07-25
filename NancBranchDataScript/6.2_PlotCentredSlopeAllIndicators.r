#MakeSlopeFile of all indicators----

library(tidyverse)
library(broom)
library(dplyr)

library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)
library(RColorBrewer)
library(scales)
Load required libraries
library(ggplot2)
library(ggpubr)
library(grid)

# First, create a standardized theme for all plots to ensure consistency
standard_theme <- function(show_legend = FALSE, bottom_margin = 5, top_margin = 5) {
  theme_bw(base_family = "serif") +
    theme(
      legend.position = if(show_legend) "bottom" else "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.key = element_rect(fill = "white", color = NA),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
      axis.title = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 8)),
      strip.text = element_text(size = 12),
      strip.background = element_rect(colour = "black", fill = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(top_margin, 10, bottom_margin, 10),
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
  
  # Manual fill colors
  scale_fill_manual(values = c("steelblue", "orangered")) +
  annotate("text", x = 4.2, y = 19, label = "Northward", 
           family = "serif", size = 4, hjust = 0)+
  annotate("text", x = 4.2, y = -30, label = "Southward", 
           family = "serif", size = 4, hjust = 0)+
  annotate("text", x = 2.8, y = 19, label = "Eastward", 
           family = "serif", size = 4, hjust = 0)+
  annotate("text", x = 2.8, y = -30, label = "Westward", 
           family = "serif", size = 4, hjust = 0)+
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
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.key = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12,angle=0, hjust = .5, vjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )
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
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",  # No legend for COG plot
    #legend.title = element_blank(),
    #legend.text = element_text(size = 14),
    #legend.text = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 0, 0, 10)
  )

abdpl
#ENDABD----
#COG----
cogcoef<-rocoef%>%filter(Indicator=="COG North" | Indicator=="COG East")
cogpl <- ggplot(cogcoef, aes(y = estimate, x = Indicator, fill = Period)) +
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
  scale_y_continuous(breaks = seq(-0.20, 0.20, by = 0.05), 
                     limits = c(-0.10, 0.20), 
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
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",  # No legend for COG plot
    #legend.title = element_blank(),
    #legend.text = element_text(size = 14),
    #legend.text = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 14),
    #strip.text = element_text(size = 14),
    strip.text = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 0, 0, 10)
  )

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
  scale_y_continuous(breaks = seq(-0.10, 0.10, by = 0.005), 
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
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",  # No legend for COG plot
    #legend.title = element_blank(),
    #legend.text = element_text(size = 14),
    #legend.text = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 0, 0, 10)
  )

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
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25), 
                     limits = c(-1, 1), 
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
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",  # No legend for COG plot
    #legend.title = element_blank(),
    #legend.text = element_text(size = 14),
    #legend.text = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 0, 0, 10)
  )

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
  scale_y_continuous(breaks = seq(-15, 5, by = 5), 
                     limits = c(-15, 5), 
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
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",  # No legend for COG plot
    #legend.title = element_blank(),
    #legend.text = element_text(size = 14),
    #legend.text = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 0, 0, 10)
  )
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
  scale_y_continuous(breaks = seq(-15, 25, by = 5), 
                     limits = c(-15, 25), 
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
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",  # No legend for COG plot
    #legend.title = element_blank(),
    #legend.text = element_text(size = 14),
    #legend.text = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 0, 0, 10)
  )
dtbpl
#END DTB----

abdpl
AOcoefpl
dtbpl
dwapl
cogpl
rangpl
simple_combo <- ggarrange(
  abdpl + theme(legend.position = "none",plot.margin = margin(5, 10, 5, 10),),
  cogpl + theme(legend.position = "none",strip.text = element_blank()),
  AOcoefpl + theme(legend.position = "none",strip.text = element_blank()),
  AOabdpl + theme(legend.position = "none",strip.text = element_blank()),
  rangpl + theme(legend.position = "none",strip.text = element_blank()),
  dwapl + theme(legend.position = "none",strip.text = element_blank()),
  dtbpl + theme(legend.position = "none",strip.text = element_blank()),
  nrow =8,
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)","(g)"))

# Combine all plots into one figure---

rangpl<-rangpl + 
    standard_theme(show_legend = FALSE, bottom_margin = 2, top_margin = 2)
abdpl<-abdpl +
    standard_theme(show_legend = FALSE, bottom_margin = 2, top_margin = 2)
cogpl<-cogpl +
    standard_theme(show_legend = FALSE, bottom_margin = 2, top_margin = 2)
AOcoefpl<-AOcoefpl+
    standard_theme(show_legend = FALSE, bottom_margin = 2, top_margin = 2)
AOabdpl<-AOabdpl +
    standard_theme(show_legend = FALSE, bottom_margin = 2, top_margin = 2)
dwapl<-dwapl +
    standard_theme(show_legend = FALSE, bottom_margin = 2, top_margin = 2)
dtbpl<-dtbpl +
    standard_theme(show_legend = FALSE, bottom_margin = 2, top_margin = 2)


library(gridExtra)
library(egg)

plots_list <- list(
  abdpl + standard_theme(show_legend = FALSE),
  cogpl + standard_theme(show_legend = FALSE),
  AOcoefpl + standard_theme(show_legend = FALSE),
  AOabdpl + standard_theme(show_legend = FALSE),
  rangpl +    standard_theme(show_legend = FALSE),
  dwapl + standard_theme(show_legend = FALSE),
  dtbpl + standard_theme(show_legend = FALSE)
)

# Combine plots without legends
slopCombo_clean <- ggarrange(
  plotlist = plots_list,
  nrow = 6,
  ncol = 1,
  heights = c(1, 1, 1, 1, 1.5, 1),
  align = "v",
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
  label.x = 0.02,
  label.y = 0.98,
  font.label = list(size = 12, color = "black", face = "bold")
)

  