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
#EAO ----
#NOTE a lot of this is derived from EAOrmarkdown which is in the NancBranchDataScript folder
#NancBranchDataScript/EAOrmarkdown.Rmd
#where spring was isolated

regpal<- c("orange", "darkblue")

# PLOT EAO,----
Area_ThresholdsforEAO<- read.csv(here::here("R/DataforFinalFigs/Area_ThresholdsforEAO.csv"))
names(Area_ThresholdsforEAO)
Area_ThresholdsforEAO$Period[Area_ThresholdsforEAO$Period == "Before Warming"] <- "1990-2005"
Area_ThresholdsforEAO$Period[Area_ThresholdsforEAO$Period == "During Warming"] <- "2006-2023"
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
  filter(term == "scale(Year)")
  filter(term == "Year")
#END slopes for EAO over time----
##PlotEOA----
#IN 
EAOplot<-ggplot(Area_ThresholdsforEAO %>% filter(Threshold == 90), 
                aes(x = Year, y =Area_Threshold/1000000, color = Region, group = Region)) +
  geom_line(size = 2) +
  scale_y_log10(
   # breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50),
    #labels = c("0.1", "0.5", "1", "2", "5", "10", "20", "50")
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  scale_fill_manual(values = regpal) +                    # Custom fill colors
  scale_color_manual(values = regpal) +
  labs(title = "",
       x = expression(""),
       y = expression(atop("Area Occupied", "(millions km"^2*")")),
       color = "Region") +
  guides(color = guide_legend(title = ""))+
  geom_vline(xintercept=2005,lty=2,lwd=1.2)+
  theme(text = element_text(family = "serif"),  
        legend.box.background = element_blank(), # Transparent legend box
        legend.position = c(.7,.5),
        legend.text = element_text(size = 12,family="serif"),
        plot.margin=margin(0, 0, 0, 0))
EAOplot
##END PlotEOA----
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/EAOplot.jpeg"), plot = EAOplot, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
library(ggpmisc)
names(Area_ThresholdsforEAO$Period)[names(Area_ThresholdsforEAO$Period) == "Before Warming"] <- "1990-2005"
names(Area_ThresholdsforEAO$Period)[names(Area_ThresholdsforEAO$Period) == "During Warming"] <- "2006-2023"
## PlotEAOvsAbd----
# Tidied Area vs Abundance Plot----
PlotEAOAbd <- ggplot(Area_ThresholdsforEAO %>% filter(Threshold == 90),
                     aes(y = Area_Threshold/1000000,  # Remove log10() transformation
                         x = Total_Abundance/1000000)) +  # Remove log10() transformation
  # Points with better styling
  geom_point(aes(color = Period), 
             size = 2.5, 
             alpha = 0.7) +
  scale_color_manual(values = c("1990-2005" = "steelblue", "2006-2023" = "orangered"))+
  # Smoothed regression lines
  geom_smooth(method = "lm", 
              se = TRUE,
              aes(group = interaction(Region, Period), 
                  color = Period),
              alpha = .1) +
  # Add log10 scales for both axes
  scale_x_log10() +  # This replaces the log10() transformation in aes()
  scale_y_log10() +  # This replaces the log10() transformation in aes()
  # Faceting by region
  facet_wrap(Region ~ ., 
             scales = "free",
             labeller = label_wrap_gen(width = 15)) +
  # Improved labels - you can keep the log notation in labels
  labs(title = "",
       x =  expression("Total Annual Abundance(Millions)"),
       y = "",#expression(atop("Area Occupied (millions km"^2*")")),
       color = "Period") +
  guides(fill = "none") +
  # Clean theme
  theme_bw() +
  theme(
    text = element_text(family = "serif"),  
    legend.box.background = element_blank(),
    legend.position = "top",
    axis.title = element_text(size = 14, family = "serif"),
    axis.text = element_text(size = 14, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, family = "serif"),
    legend.key = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 12, family = "serif"),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )
# Display the plot
print(PlotEAOAbd)

#END scaled slopes fro EAO vs Abundance----
# Alternative: If you want different colors for periods----
# PlotEAOAbd + scale_color_manual(values = c("Period1" = "steelblue", "Period2" = "orangered"))

PlotEAOAbd
## END PlotEAOvsAbd----
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/PlotEAOAbd.jpeg"), plot = PlotEAOAbd, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")

#Range Edge  ----
rangeedge<-read.csv(here::here("R/DataforFinalFigs/Edge_df_NSreshp.csv"))
range.spr<-rangeedge[rangeedge$Season=="Spring",]
range.spr$Period<-NULL
range.spr$Period[range.spr$Year<2006]<-"1990-2005"
range.spr$Period[range.spr$Year>2005]<-"2006-2023"
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
# Estimate distance shifted.----
library(geosphere)
names(range.spr)
##CHECK THE FLOOLOOWING.
Leadrange.sprAgg<-range.spr%>% group_by(Period)
  mutate(
    # YearGroup = cut(Year, breaks = seq(1990, 2023, by = 5), right = FALSE)
    YearGroup = cut(Year, breaks = seq(1990, 2025, by = 5), right = FALSE)
  ) %>%
  group_by(YearGroup) %>%
  summarise(AveE = mean(Estimate_km_E_quantile_0.95, na.rm = TRUE),
            AveN = mean(Estimate_km_N_quantile_0.95, na.rm = TRUE))
# Reference point (first group)
ref_point <- Leadrange.sprAgg%>%
  filter(YearGroup == unique(YearGroup)[1]) %>%
  slice(1)  # just in case there are multiple rows

# Reference point (last group)
final_point <- Leadrange.sprAgg %>%
  filter(YearGroup == unique(YearGroup)[7]) %>%
  slice(1) 

# Vector of distances from the reference point to each group
# Calculate Euclidean distance to all other points
Leadrange.sprAgg <- Leadrange.sprAgg %>%
  arrange(YearGroup) %>%  # ensure chronological order
  mutate(
    PrevE = lag(AveE),
    PrevN = lag(AveN),
    StepDist = sqrt((AveE - PrevE)^2 + (AveN - PrevN)^2),
    CumulativeDist = cumsum(replace_na(StepDist, 0))
  )


library(dplyr)
centroid_1 <- Leadrange.sprAgg %>% filter(YearGroup == unique(YearGroup)[1]) %>% slice(1)
centroid_7 <- Leadrange.sprAgg %>% filter(YearGroup == unique(YearGroup)[7]) %>% slice(1)

# Directional shifts
shift_Easting <- centroid_7$AveE - centroid_1$AveE
shift_Northing <- centroid_7$AveN - centroid_1$AveN

# Total Euclidean shift
total_shift <- sqrt(shift_Easting^2 + shift_Northing^2)
shift_summary <- tibble(
  Direction = c("Easting", "Northing", "Total"),
  Shift = c(shift_Easting, shift_Northing, total_shift)
)

print(shift_summary)

set.seed(123)  # For reproducibility
pos_jitter <- position_jitter(width = .9, height = .9)  # Jitter both horizontally and vertically
names(range.spr)


#END range edge distance----
names(range.spr)

# Larger terminal points with "final" annotation
RangeEdgeV3_large_terminal <- ggplot(range.spr,
                                     aes(x = Estimate_km_E_quantile_0.05, 
                                         y = Estimate_km_N_quantile_0.05, 
                                         color = Year)) +
  scale_x_continuous(breaks = seq(-500, 1300, by = 100)) +
  scale_y_continuous(breaks = seq(4700, 6000, by = 100)) +
  # Path for 0.05 quantile points with color gradient (no arrow)
  geom_path(aes(group = 1), 
            lwd = 1) +
  
  # Path for 0.95 quantile points with color gradient (no arrow)
  geom_path(aes(x = Estimate_km_E_quantile_0.95, 
                y = Estimate_km_N_quantile_0.95, 
                group = 2), 
            lwd = 1) +
  
  # Regular points for 0.05 quantile
  geom_point(position = pos_jitter, 
             na.rm = TRUE, 
             alpha = .75, 
             size = 2, 
             shape = 19) +
  
  # Regular points for 0.95 quantile
  geom_point(aes(x = Estimate_km_E_quantile_0.95, 
                 y = Estimate_km_N_quantile_0.95, 
                 color = Year),
             position = pos_jitter, 
             na.rm = TRUE, 
             alpha = .75, 
             size = 2, 
             shape = 19) +
  
  # Larger terminal point for 0.05 quantile (most recent year)
  geom_point(data = range.spr[range.spr$Year == max(range.spr$Year, na.rm = TRUE), ],
             aes(x = Estimate_km_E_quantile_0.05, 
                 y = Estimate_km_N_quantile_0.05, 
                 color = Year),
             position = pos_jitter, 
             na.rm = TRUE, 
             alpha = 1, 
             size = 5, 
             shape = 19) +
  
  # Larger terminal point for 0.95 quantile (most recent year)
  geom_point(data = range.spr[range.spr$Year == max(range.spr$Year, na.rm = TRUE), ],
             aes(x = Estimate_km_E_quantile_0.95, 
                 y = Estimate_km_N_quantile_0.95, 
                 color = Year),
             position = pos_jitter, 
             na.rm = TRUE, 
             alpha = 1, 
             size = 5, 
             shape = 19) +
  
  # Annotation for 0.05 quantile terminal point
  annotate("text",
          # x = range.spr$Estimate_km_E_quantile_0.05[range.spr$Year == max(range.spr$Year, na.rm = TRUE)]+100,
           #y = range.spr$Estimate_km_N_quantile_0.05[range.spr$Year == max(range.spr$Year, na.rm = TRUE)] + 
            # (max(range.spr$Estimate_km_N_quantile_0.05, na.rm = TRUE) - 
             #   min(range.spr$Estimate_km_N_quantile_0.05, na.rm = TRUE)) * 0.03,
          x=-100,y=4750,
           label = "Trailing Edge",
           color = "black",
           size = 5,
           family = "serif") +
  
  # Annotation for 0.95 quantile terminal point  
  annotate("text",
          # x = range.spr$Estimate_km_E_quantile_0.95[range.spr$Year == max(range.spr$Year, na.rm = TRUE)],
           #y = range.spr$Estimate_km_N_quantile_0.95[range.spr$Year == max(range.spr$Year, na.rm = TRUE)] + 
            # (max(range.spr$Estimate_km_N_quantile_0.95, na.rm = TRUE) - 
             #   min(range.spr$Estimate_km_N_quantile_0.95, na.rm = TRUE)) * 0.03,
          x=500,y=5300,
           label = "Leading Edge",
           color = "black",
           size = 5,
           family = "serif") +
  
  # Blue to orange gradient for Year
  scale_color_gradient2(low = "steelblue3", 
                        mid = "yellow", 
                        high = "orangered", 
                        midpoint = 2005, 
                        name = "Year") +
  
  # Labels
  xlab("Range Edge W to E (km)") +
  ylab("Range Edge S to N (km)") +
  
  # Theme
  theme_bw() +
  theme(
    legend.position = "right",
    plot.margin = margin(5, 10, 10, 10),
    axis.text.x = element_text(angle = 90, 
                               vjust = 0, 
                               hjust = 0.5, 
                               size = 14, 
                               family = "serif"),
    axis.text.y = element_text(angle = 0, 
                               vjust = 0.5, 
                               hjust = 0, 
                               size = 14, 
                               family = "serif"),
    axis.title.x = element_text(size = 14, 
                                hjust = 0.5, 
                                vjust = -2, 
                                family = "serif"),
    axis.title.y = element_text(size = 14, 
                                hjust = 0.5, 
                                vjust = 4, 
                                angle = 90, 
                                family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, family = "serif"),
    strip.text = element_text(size = 14, 
                              family = "serif", 
                              angle = 0),
    strip.background = element_rect(colour = "black", 
                                    fill = "white")
  )

RangeEdgeV3_large_terminal

#Combineplots
# Ensure both plots have identical margins
RangeEdge2 <- RangeEdgeV3_large_terminal + theme(plot.margin = margin(10, 0,15,0))
PlotEAOAbd <- PlotEAOAbd + theme(plot.margin = margin(t=0, r=15,b=10,l=10))#,axis.title.y = element_blank())
EAOplot <- EAOplot + theme(plot.margin = margin(15, 0,20,0))#,axis.title.y = element_blank())


# Combo plot: Proper nested approach----
# Create top row WITHOUT labels (labels will be added at the end)
bottom_row <- plot_grid(
  EAOplot, 
  PlotEAOAbd, 
  ncol = 2, 
  rel_heights = c(1,1),rel_widths =c(.5,.5), 
  align = "h",
  axis = "b"
)

# Create bottom row WITHOUT labels
top_centered <- plot_grid(
  NULL, 
  RangeEdge2, 
  NULL,
  ncol = 3,
  rel_widths = c(0.1, .8, 0.1)
)

# Combine rows and add ALL labels at this final step
# Combine rows WITHOUT automatic labels
EAORangeCombo_alt <- plot_grid(
  top_centered,
  bottom_row,
  nrow = 2,
  rel_heights = c(.55,.45)
)
EAORangeCombo_final <- ggdraw(EAORangeCombo_alt) +
  draw_plot_label(label = "(a)", x = 0.01, y = 0.99, size = 14) +   # Top left
  draw_plot_label(label = "(b)", x = 0.01, y = 0.45, size = 14) +   # Top right  
  draw_plot_label(label = "(c)", x = 0.52, y = 0.45, size = 14)     # Bottom (centered plot)


# Display the result
print(EAORangeCombo_final)
##END Plot Range Edge----
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/RangeEdge.jpeg"), plot = RangeEdge, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
#END Combo plot: Proper nested approach----

ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/EAORangeCombo_final.jpeg"), plot = EAORangeCombo_final, dpi = 600, width = 10, height = 8, units = "in", device = "jpeg") 

CombEAOABDrANGE
