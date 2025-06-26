# Calculate the angle in radians between two points (x1, y1) and (x2, y2)

#Schematic #7/
library(ggplot2)
library(dplyr)
library(stringr)
library(here)
library(readr)

schem <- read_csv(here("2025-04-23/Output/Shift_Indicators/SchematicFig.csv"))
library(ggplot2)
library(dplyr)
library(stringr)
library(here)
library(readr)

schem <- read_csv(here::here("2025-04-23/Output/Shift_Indicators/SchematicFig.csv"),show_col_types = FALSE)

plot_df <- schem %>%
  group_by(Indicator, Region) %>%
  summarise(
    sum_length = sum(LevValue),
    mean_score = mean(as.numeric(LevValue)),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(mean_score = round(mean_score, digits = 0))
plot_df <- plot_df %>%
  mutate(
    Indicator_wrapped = str_wrap(Indicator, 12),
    Indicator_ordered = fct_reorder(Indicator_wrapped, sum_length)
  )

# Find positions of indicators in the circular axis
indicator_levels <- levels(plot_df$Indicator_ordered)

divider_positions <- seq(1.5, length(indicator_levels) - 0.5, by = 1)


plt <- ggplot(plot_df) +
  # Add category dividers
  geom_vline(xintercept = divider_positions, color = "grey80", linewidth = 0.4) +
  
  # Grid lines
  geom_hline(aes(yintercept = y), data.frame(y = -1:3), color = "lightgrey") +
  
  # Bars per region
  geom_col(
    aes(
      x = Indicator_ordered,
      y = sum_length,
      fill = Region
    ),
    position = position_dodge2(preserve = "single"),
    alpha = 0.9
  ) +
  
  # Mean score dots
  geom_point(
    aes(
      x = Indicator_ordered,
      y = mean_score,
      group = Region
    ),
    position = position_dodge2(width = 0.9, preserve = "single"),
    size = 3,
    color = "gray12"
  ) +
  
  # Lollipop lines
  geom_segment(
    aes(
      x = Indicator_ordered,
      y = -1,
      xend = Indicator_ordered,
      yend = 3,
      group = Region
    ),
    linetype = "dashed",
    color = "gray12",
    position = position_dodge2(width = 0.9)
  ) +
  
  coord_polar() +
  
  scale_y_continuous(
    limits = c(-1, 2),
    expand = c(0, 0),
    breaks = c(-1, 1, 2, 3)
  ) +
  scale_fill_manual(values = c("red", "violet", "blue")) +
  guides(
    fill = guide_legend(title = "Region", title.position = "top", title.hjust = 0.5)
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 10),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "\nShift Indicators",
    subtitle = "\n Relative indicator magnitude"
  )
plt
plt+theme(margin=c(0,0,0,0)) # Remove margins around the plot
ggsave("schematic_plot.png", plt, width = 12, height = 12, dpi = 300)

