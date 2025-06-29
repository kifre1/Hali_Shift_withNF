library(dplyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(here)
library(readr)

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
# Calculate the angle in radians between two points (x1, y1) and (x2, y2)

#Schematic #7/


schem <- read_csv(here::here("2025-04-23/Output/Shift_Indicators/SchematicFigIndicatorScaledCoefficients.csv"),show_col_types = FALSE)
names(schem)
View(schem)
#----
# Assuming your data is in a data frame called `schem`

library(ggplot2)
library(dplyr)
library(tidyr)

# Clean and prepare your data
schem_clean <- schem %>%
    mutate(
    TrendIndicator = trimws(TrendIndicator),
    SCSignif = trimws(SCSignif),
    #SignifMarker = ifelse(Signif != "NS", "*", ""),  # This line creates the column
    Period = factor(Period, levels = c("Before", "During"))
  )

# Plot
flowpot<-ggplot(schem_clean, aes(x = TrendIndicator, y = ScaledSlopeCoefficient, fill = Period)) +
  #geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = .5) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  geom_text(aes(label = ScSignifLabel),
            position = position_dodge(width = 0.4),
            vjust = 0, size = 3.5,color="black") +
 geom_segment(data = data.frame(y = seq(-0.5, 1, 0.5)),
              aes(x = 0, xend = Inf, y = y, yend = y),
             inherit.aes = FALSE, color = "grey80", linetype = "dotted")+
  geom_hline(yintercept = 0, color = "black", linewidth = 1) + # Emphasize zero line
  coord_polar(start = 0,clip="off") +
  facet_wrap(~Region) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 0, vjust = .5),) +
  labs(title = "Trend in Shift Indicators by Region and Period",
       x = "", y = "Scaled Slope Coefficient")


flowpot<- flowpot+geom_text(data = radial_labels, aes(x = x, y = y, label = label),
              inherit.aes = FALSE, angle = 0, hjust = -0.2, size = 4)
flowpot<-flowpot+theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 12, angle = 20, hjust = 1.5),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0),
    panel.spacing = unit(2.5, "lines"),
    legend.position = "top",
    text = element_text(family = "serif"),  
    legend.box.background = element_blank(), # Transparent legend box
    legend.text = element_text(size = 10,family="serif"),
    legend.title = element_blank(),
    strip.text = element_text(size = 12, family = "serif", angle = 0)
      )
flowpot
#flowpot<-plot_grid(DistReg, DistHag, nrow = 2,rel_heights = c(1, 2),labels = c("(a)", "(b)"),align = "v", axis = "lr") # Add labels


ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/FLowPot.jpeg"), plot = flowpot, dpi = 600, width = 12, height = 10, units = "in", device = "jpeg")
#barplot
summarypot<-ggplot(schem_clean, aes(x = fct_rev(TrendIndicator), y = ScaledSlopeCoefficient, fill = Period))+
  #geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = .8) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  geom_text(aes(label = ScSignifLabel),
            position = position_dodge(width = 0.4),hjust = 0.5,
            vjust = .5, size = 3.5,color="black") +
  geom_segment(data = data.frame(y = seq(-0.5, 1, 0.5)),
               aes(x = 0, xend = Inf, y = y, yend = y),
               inherit.aes = FALSE, color = "grey80", linetype = "dotted")+
    geom_hline(yintercept = 0, color = "black", linewidth = 1) + # Emphasize zero line
 coord_flip(clip="off") +
  facet_wrap(~Region) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = .5),) +
  labs(title = "Trend in Shift Indicators by Region and Period",
       x = "", y = "Scaled Slope Coefficient")


summarypot<-summarypot+theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1.5),
    axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0),
    panel.spacing = unit(2.5, "lines"),
    legend.position = "top",
    text = element_text(family = "serif"),  
    legend.box.background = element_blank(), # Transparent legend box
    legend.text = element_text(size = 10,family="serif"),
    legend.title = element_blank(),
    strip.text = element_text(size = 12, family = "serif", angle = 0)
  )
summarypot
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/SchematicSummaryPot.jpeg"), plot = 
         summarypot, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
#barplot

flowpot<- flowpot+geom_text(data = radial_labels, aes(x = x, y = y, label = label),
                            inherit.aes = FALSE, angle = 0, hjust = -0.2, size = 4)

flowpot
#flowpot<-plot_grid(DistReg, DistHag, nrow = 2,rel_heights = c(1, 2),labels = c("(a)", "(b)"),align = "v", axis = "lr") # Add labels


ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/FLowPot.jpeg"), plot = flowpot, dpi = 600, width = 12, height = 10, units = "in", device = "jpeg")


library(ggplot2)
library(dplyr)
library(tidyr)

library(ggplot2)
library(dplyr)
library(tidyr)

library(ggplot2)
library(dplyr)

# Prepare data
schem_clean <- schem %>%
  filter(Region %in% c("Canada", "USA")) %>%
  mutate(
    TrendIndicator = trimws(TrendIndicator),
    Signif = trimws(Signif),
    #SignifMarker = ifelse(Signif != "NS", "*", ""),
    Period = factor(Period, levels = c("Before", "During"))
  )

# Create a base plot
p <- ggplot(schem_clean, aes(x = TrendIndicator, y = ScaledSlopeCoefficient, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = SignifLabel),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  coord_polar(start = 0, clip = "off") +
  facet_wrap(~Region) +
  scale_y_continuous(breaks = NULL, limits = c(-1.2, 1.2)) +  # Hide default y-axis
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),  # Hide default radial labels
    axis.ticks.y = element_blank(),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  labs(
    title = "Flower Plot of Trend Indicators by Region and Period",
    x = "", y = ""
  )

# Add manual radial labels
radial_labels <- data.frame(
  x = 1,  # dummy x
  y = seq(-1, 1, 0.5),
  label = seq(-1, 1, 0.5)
)

p + geom_text(data = radial_labels, aes(x = x, y = y, label = label),
              inherit.aes = FALSE, angle = 0, hjust = -0.2, size = 3)
