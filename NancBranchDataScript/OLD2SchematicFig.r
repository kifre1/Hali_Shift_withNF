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
library(DiagrammeR)
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

#' Plot a flowerplot
#'
#' @param df data.frame with indicator scores, grouping, labels, and relative weight
#' @param grouping character string for the name of the grouping column in `df`
#' @param labels character string for the name of the labels column in `df`
#' @param score character string for the name of the score column in `df`
#' @param max_score numeric value for the maximum possible value of the scale (i.e. petal length). Default is 100
#' @param min_score numeric value for the minimum possible value of the scale (i.e. petal length). Default is 0
#' @param weight character string for the name of the weight column in `df`
#' @param title Defaults to the unique value of the `area_name` column of the `df`, but can take any character value. Alternatively, use `FALSE` to avoid having a title.
#' @param bintextsize numeric value for the size of the text in the bins. Default is 3
#' @param zeroline logical value to add a zero line to the plot. Default is `FALSE`
#'
#' @return plot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom dplyr if_else n
#' @importFrom ggplot2 geom_crossbar geom_text geom_errorbar geom_hline coord_polar
#' @export
#'
#' @examples
#'
#' indicatorbins <- data.frame(grouping=rep(c("Biodiversity",
#'                                            "Habitat",
#'                                            "Productivity"),
#'                                          times=c(3,5,3)),
#'                             labels=c("Genetic Diversity",
#'                                      "Species Diversity",
#'                                      "Functional Diversity",
#'
#'                                      "Environmental Representativity",
#'                                      "Key Fish Habitat",
#'                                      "Connectivity",
#'                                      "Uniqueness",
#'                                      "Threats to Habitat",
#'
#'                                      "Biomass Metrics",
#'                                      "Structure and Function",
#'                                      "Threats to Productivity"),
#'                             score=runif(11,55,100),
#'                             weight=1,
#'                             area_name = "Random Example MPA")
#'
#' plot_flowerplot(indicatorbins)
#'
#'
plot_flowerplot <- function(df,grouping="grouping",labels="labels",score="score",weight="weight",title=unique(df[["area_name"]]),max_score = 100,min_score = 0,bintextsize=3,zeroline=FALSE){
  scalerange <- max_score-min_score
  
  #SEE ISSUE 47 in MarConsNetApp. Do not delete, we may revisit this.
  #  calc_letter_grade <- function(percent){
  #    cutoffs=c(min_score, seq(max_score-scalerange*.4, max_score, by = 10/3/100*scalerange))
  #    grades=c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
  #    cut(percent,cutoffs,grades)
  #  }
  #
  #
  #
  #  grades <- c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
  #  flowerPalette <- colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu"))(length(grades))
  #
  #
  # # # Define stoplight color scheme
  #  flowerPalette <- c(
  #    "F" = "#FF0000",    # Bright Red
  #    "D-" = "#FF3300",   # Slightly lighter red
  #    "D" = "#FF6600",    # Red-Orange
  #    "D+" = "#FF9900",   # Orange
  #    "C-" = "#FFCC00",   # Yellow-Orange
  #    "C" = "#FFFF00",    # Yellow
  #    "C+" = "#CCFF33",   # Yellow-Green
  #    "B-" = "#99FF66",   # Light Green
  #    "B" = "#66FF66",    # Medium Green
  #    "B+" = "#33CC33",   # Bright Green
  #    "A-" = "#009900",   # Dark Green
  #    "A" = "#006600",    # Very Dark Green
  #    "A+" = "#003300"    # Almost Black-Green
  #  )
  
  
  # Note, I had to change calc_letter_grade because previously in the analysis
  # function we assign A=5,B=4,C=3,D=2,F=1 (we then multiple by 20 adjust the
  # scale to be 1-100.). Because the previous calc_letter_grade was fluid, it meant
  # that a value of say, 20, may not always be a F (it could be F-, or something),
  # I therefore adjusted this to hard code in the values we previously set so the
  # grades didn't change.
  
  
  grades <- c("A", "B", "C", "D", "F")
  flowerPalette <- rev(colorRampPalette(brewer.pal(5,"RdYlBu"))(length(grades)))
  
  names(flowerPalette) <- grades
  
  ngroups <- length(unique(df[[grouping]]))
  
  df <- arrange(df,!!sym(grouping),!!sym(labels))
  rawdata <- data.frame(grouping=factor(df[[grouping]],levels = unique(df[[grouping]])),
                        labels=factor(df[[labels]],levels = unique(df[[labels]])),
                        score=df[[score]],
                        weight=df[[weight]]) |>
    mutate(weight=weight/sum(weight))
  data <- calc_group_score(df = rawdata,
                           grouping_var = "labels",
                           score_var = "score",
                           weight_var = "weight") |>
    mutate(pos=cumsum(weight)-weight/2,
           bg=dplyr::if_else(is.nan(score),"#EDEDED","white"))
  data$score[which(is.nan(data$score))] <- NA
  
  grouped_df <- calc_group_score(df = rawdata,
                                 grouping_var = "grouping",
                                 score_var = "score",
                                 weight_var = "weight") |>
    mutate(y=max_score+scalerange*.5,
           x=cumsum(weight)-weight/2,
           angle=360-x*360,
           angle=dplyr::if_else(angle>90&angle<270,
                                angle-180,
                                angle)
    )
  
  p <- ggplot(data=data,aes(width = weight))+
    geom_crossbar(aes(x = pos, y = max_score-scalerange*1.5, ymax = max_score-scalerange*1.5, ymin = min_score,fill=calc_letter_grade(weighted.mean(score,weight,na.rm = TRUE))),color="transparent")+
    ggplot2::geom_crossbar(stat="identity",linewidth=0.2,color='lightgrey',aes(x=pos,y=max_score,ymax=max_score,ymin=min_score),fill=data$bg)+
    ggplot2::geom_crossbar(stat="identity",linewidth=0.2,color='black',aes(x=pos,y=score,ymax=score,ymin=min_score,fill=calc_letter_grade(score)))+
    ggplot2::coord_polar()+
    theme(panel.grid.major = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.line  = ggplot2::element_blank(),
          axis.text  = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          legend.position="none")+
    scale_x_continuous(labels = data$labels,
                       breaks = data$pos)+
    scale_y_continuous(limits = c(max_score-scalerange*1.5,max_score+scalerange*.55))+ #PROBLEM HERE
    scale_fill_manual(values=flowerPalette)+
    scale_color_manual(values=flowerPalette)+
    ggplot2::geom_errorbar(data=grouped_df,
                           inherit.aes=FALSE,
                           aes(x=x,
                               ymin=y,
                               ymax=y,
                               width=weight-0.01,
                               col = calc_letter_grade(score)),
                           linewidth = 7)+
    ggplot2::geom_text(aes(label=calc_letter_grade(weighted.mean(score,weight,na.rm = TRUE))),
                       x=min_score,
                       y=max_score-scalerange*1.5,
                       size=8)+
    ggplot2::geom_text(aes(label=gsub(" ","\n",labels),
                           x=pos,
                           y=max_score+scalerange*.2),
                       size=2)+
    
    ggplot2::geom_text(data=grouped_df,
                       inherit.aes = FALSE,
                       aes(label=grouping,
                           x=x,
                           y=y,
                           angle=angle),
                       size=bintextsize)
  if(zeroline){
    p <- p + ggplot2::geom_hline(yintercept = 0,linetype="dotted")
    
  }
  
  
  
  if(title==FALSE){
    return(p)
  } else {
    return(p+
             labs(title=title) +
             theme(plot.title = element_text(hjust = 0.5)))
  }
  
}
