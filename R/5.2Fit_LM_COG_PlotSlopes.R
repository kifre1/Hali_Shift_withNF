
#this originally comes from 05_CentreofGravity.R followed by 05.2
library(tidyverse)
#by Core Area
centroid_data <- read.csv(here::here("","2025-04-23/Output/Shift_Indicators/seasonal_centroid_data_CA.csv"),sep = ",")
centroid_data$Period<-NULL
centroid_data$Period[centroid_data$Year<2006]<-"Before Warming"
centroid_data$Period[centroid_data$Year>2005]<-"During Warming"
str(centroid_data$Stratum)
centroid_data$Stratum<-factor(centroid_data$Stratum,levels=c("EGOM","BOF","CapeBreton","HaliChan",
                                                             "CapeCod","Browns","Gully","GrandBanks",
                                                             "Nantucket","Georges","Sable","GBTail"))
str(centroid_data)
#By Region
centroid_data_Reg <- read.csv(here::here("","2025-04-23/Output/Shift_Indicators/seasonal_centroid_data_region.csv"),sep = ",")
centroid_data_Reg$Period<-NULL
centroid_data_Reg$Period[centroid_data_Reg$Year<2006]<-"Before Warming"
centroid_data_Reg$Period[centroid_data_Reg$Year>2005]<-"During Warming"
str(centroid_data_Reg)

#write.csv(centroid_data_Reg,here::here("R/DataforFinalFigs/centroid_dataRegionalforFig.csv"),row.names = F)

summary(centroid_data)
names(centroid_data)
#set the data up for order

#centroid_data$ordCore_Area<-factor(centroid_data$Core_Area,levels=c("CapeCod","EGOM","BOF","Nantucket","Georges","Browns","Sable","Gully","CapeBreton"))
#centroid_data$Period<-NULL
#centroid_data$Period[centroid_data$Year<2006]<-"Before Warming"
#centroid_data$Period[centroid_data$Year>2005]<-"During Warming"

#write.csv(centroid_data,here::here("R/DataforFinalFigs/centroid_dataCAforFig.csv"),row.names = F)

library(dplyr)
library(broom)
library(ggplot2)
library(patchwork)
library(grid)  # For unit() function

# CORE AREAS Perform lm on each group and extract coefficients----
COGcoefficientsLat_df <- centroid_data %>%
  group_by(Period,Stratum, Season) %>%
  do({
      model <- lm(centroid_latitude ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
COGcoefficientsLat_df$AxesNE<-"Latitude"

COGcoefficientsLon_df <- centroid_data %>%
  group_by(Period,Stratum, Season) %>%
  do({
    modlong<-lm(centroid_longitude~Year, data=.)
data.frame(t(coef(modlong)))
tidy(modlong, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

COGcoefficientsLon_df$AxesNE<-"Longitude"

COGcoefficientsJt_df<-merge (COGcoefficientsLat_df,COGcoefficientsLon_df,all=T)

summary(COGcoefficientsJt_df)

filtered_df <- COGcoefficientsJt_df %>%
  filter(term == "Year")  #  to plot slopes and CIs

write.csv(filtered_df,here::here("R/DataforFinalFigs/COGSlopeCI_CoreAreas.csv"),row.names = F)
##end CORE AREA SLOPES AND LM----
# REGIONAL Perform lm on each group and extract coefficients----
Reg_COGcoefficientsLat_df <- centroid_data_Reg %>%
  group_by(Period,Season,Stratum) %>%
  do({
    model <- lm(centroid_latitude ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
Reg_COGcoefficientsLat_df$AxesNE<-"Latitude"

Reg_COGcoefficientsLon_df <- centroid_data_Reg %>%
  group_by(Period,Season,Stratum) %>%
  do({
    modlong<-lm(centroid_longitude~Year, data=.)
    data.frame(t(coef(modlong)))
    tidy(modlong, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

Reg_COGcoefficientsLon_df$AxesNE<-"Longitude"

Reg_COGcoefficientsJt_df<-merge (Reg_COGcoefficientsLat_df,Reg_COGcoefficientsLon_df,all=T)

summary(Reg_COGcoefficientsJt_df)

Reg_filtered_df <- Reg_COGcoefficientsJt_df %>%
  filter(term == "Year" )  # Replace "x" with "Intercept" to plot intercept

write.csv(Reg_filtered_df,here::here("R/DataforFinalFigs/COGSlopeCI_Regional.csv"),row.names = F)
#END REGIONAL COG SLOPES AND LM----


#Take only Spring data
Reg_filtered_df <- read.csv(here::here("","R/DataforFinalFigs/COGSlopeCI_Regional.csv"),sep = ",")
filtered_df <- read.csv(here::here("","R/DataforFinalFigs/COGSlopeCI_CoreAreas.csv"),sep = ",")

filtered_df_spring <- filtered_df %>%
  filter(Season == "Spring" )  
Reg_filtered_df_spring <- Reg_filtered_df %>%
  filter(Season == "Spring" ) 

# Plot the slope estimates with confidence intervals for each group
theme_replace(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour="black"),
              strip.background=element_rect(colour="black",fill="white"))
theme_set(theme_bw())
theme_replace(legend.key =element_rect(colour="black",fill="white"),
              plot.margin = unit(c(1.5,3,1.5,1), "cm"),
              #plot.margin=margin(3,4,4,0),
              #plot.margin=margin(3,4,4,0),
              plot.title=element_text(size=16,vjust=1,family="serif"),
              legend.background=element_rect(size=.9,colour="white",fill="white"),
              strip.text=element_text(size=14,family="serif",angle=0),
              panel.border = element_rect(colour = "black",fill=NA),
              panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              strip.background=element_rect(colour="black",fill="white"),
              axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1,size=12,family="serif"),
              axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=12,family="serif"),
              axis.title.x=element_text(size=12,hjust=0.5,vjust=-2,family="serif"))
pd <- position_dodge(.5)
# PLOT CORE AREAS trials ----

ggplot(filtered_df_spring , aes(x = factor(Stratum), y = estimate,fill=Period,colour=AxesNE)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  scale_fill_manual(values=c("steelblue", "wheat"))+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  xlab("")+  ylab("Rate of change (Dec.Deg/yr")+
  ggtitle ("Latitude")

ggplot(filtered_df_spring , aes(x = factor(Stratum), y = estimate,fill=AxesNE)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  scale_fill_manual(values=c("steelblue", "wheat"))+
  annotate("text", x = 1.5, y = .015, label = "North or West", hjust = 0) +
  annotate("text", x = 1.5, y = -.012, label = "South or East", hjust = 0) +
  #scale_x_discrete(expand = expansion(add = 1)) # Add space on x-axis+
  #expand_limits(x = 5)+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  guides(color = guide_legend(title = NULL))+
  xlab("")+  ylab("Rate of change (Dec.Deg/yr)")+
  facet_wrap(Period~.)
# PLOT CORE AREAS and combine----
p1<-ggplot(filtered_df_spring[filtered_df_spring$AxesNE=="Latitude",] , aes(x = factor(Stratum), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  geom_vline(xintercept = seq(1.5, length(unique(filtered_df_spring$Stratum)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change (Dec.Deg/yr)")+
  annotate("text", x = 1.6, y = .015, label = "North", hjust = 0,size=6) +
  annotate("text", x = 1.6, y = -.012, label = "South", hjust = 0,size=6) +
  labs(tag = "A. Latitude")+
  theme(plot.tag.position = c(.3, 1.05))+
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm"))
  
p2<-ggplot(filtered_df_spring[filtered_df_spring$AxesNE=="Longitude",] , aes(x = factor(Stratum), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  ylim(-0.024,0.024)+
  coord_flip() +
  geom_vline(xintercept = seq(1.5, length(unique(filtered_df_spring$Stratum)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  xlab("")+  ylab("Rate of change (Dec.Deg/yr)") +
  annotate("text", x = 1, y = -.022, label = "West", hjust = 0,size=6) +
  annotate("text", x = 1, y = .007, label = "East", hjust = -1,size=6) +
  labs(tag = "B. Longitude",title=NULL)+
  theme(plot.tag.position = c(.3, 1.05))+
  #ggtitle("Longitude")+
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


combined <- (p1 | p2 ) +
  plot_layout(guides = "collect") + # Collects shared legends
  plot_annotation(title="Rate of change in Centre of Gravity per year, by Time Period and Core Area")+
  theme(plot.margin = unit(c(2, 0, 0,0), "cm")) # Removes outer margins
combined

#END PLOT CORE AREAS and combine----
summary(Reg_filtered_df[Reg_filtered_df$AxesNE=="Latitude",])
summary(Reg_filtered_df[Reg_filtered_df$AxesNE=="Longitude",])
#END PLOT REGIONS and combine----
#Reg_filtered_df_spring$ordRegion<-factor(Reg_filtered_df$survey,levels=c("Canada","USA"))

pr1<-ggplot(Reg_filtered_df_spring [Reg_filtered_df_spring$AxesNE=="Latitude",] , aes(x = factor(Stratum), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  geom_vline(xintercept = seq(1.5, length(unique(Reg_filtered_df_spring$Stratum)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  ylim(-0.040,0.060)+
  xlab("")+  ylab("Rate of change (Dec.Deg/yr)")+
  annotate("text", x = 0.5, y = .05, label = "North", hjust = 0,size=6) +
  annotate("text", x = 0.5, y = -.035, label = "South", hjust = 0,size=6) +
  ggtitle("Latitude")+
  theme(plot.tag.position = c(.3, 1.05))+
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm"))
#Reg_filtered_df$ordRegion<-factor(Reg_filtered_df$survey,levels=c("USA","Canada"))

pr2<-ggplot(Reg_filtered_df_spring[Reg_filtered_df_spring$AxesNE=="Longitude",] , aes(x = rev(factor(Stratum)), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  scale_fill_manual(values=c("steelblue", "orangered"))+
  ylim(-0.1,0.21)+
  coord_flip() +
  geom_vline(xintercept = seq(1.5, length(unique(Reg_filtered_df_spring$Stratum)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  xlab("")+  ylab("Rate of change (Dec.Deg/yr)") +
  annotate("text", x = .5, y = -.095, label = "West", hjust = 0,size=6) +
  annotate("text", x = .5, y = .12, label = "East", hjust = 0,size=6) +
  #labs(tag = "B. Longitude",title=NULL)+
  theme(plot.tag.position = c(.3, 1.05))+
 ggtitle("Longitude")+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


combinedreg <- (pr1 | pr2 ) +
  plot_layout(guides = "collect") + # Collects shared legends
  plot_annotation(title="Rate of change in Centre of Gravity per year, by Time Period and Region")+
  theme(plot.margin = unit(c(1, 0, 0,0), "cm")) # Removes outer margins
combinedreg

# Save the coefficients to a new file
#write.csv(centroid_latitude, "COGgrouped_lm_coefficients.csv", row.names = FALSE)

