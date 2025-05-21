#Step 1: Plot Abundance Trends and ave file for plotting.

#Abundance trend Plots
library(dplyr)

library(broom)

library(ggplot2)

library(patchwork)

library(grid)  # For unit() function
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


# Plot abuncance indexed regions 
#SpSt_DF<-read.csv(here::here("2024-10-04/Output/PredictionData_for_ShiftAnalysis.csv"))

# Figure 2, 

abundance_ind_Region<-read.csv(here::here("2025-04-23/Output/IndexAbundance/abundance_ind_Region.csv"))
abundance_ind_CA<-read.csv(here::here("2025-04-23/Output/IndexAbundance/abundance_ind_CA.csv"))
#plot the regional abundance seasonally 
#remove "all", isolate spring, rename file and regions
abundance_ind_Region<-subset(abundance_ind_Region, abundance_ind_Region$Index_Region=="Canada"|abundance_ind_Region$Index_Region=="USA")
abundance_ind_Region<-subset(abundance_ind_Region, abundance_ind_Region$Season=="Spring")
#Here Nancy is creating another file named SPring so she can plot it elsewhere----
abundance_ind_Region.Spring<-subset(abundance_ind_Region, abundance_ind_Region$Season=="Spring")
abundance_ind_Region.Spring$Period<-NULL
abundance_ind_Region.Spring$Period[abundance_ind_Region.Spring$Year<2006]<-"Before Warming"
abundance_ind_Region.Spring$Period[abundance_ind_Region.Spring$Year>2005]<-"During Warming"
abundance_ind_Region.Spring$Region<-factor(abundance_ind_Region.Spring$Index_Region)
write.csv(abundance_ind_Region.Spring, here::here("2025-04-23/Output/IndexAbundance/abundance_ind_Region.Spring.csv"))

regpal<- c("orange", "darkblue")
RegionalPlot<- ggplot(data = abundance_ind_Region, aes(x = Year, y = Index_Estimate, color = Index_Region))+
  geom_point()+
  scale_y_continuous(labels = scales::scientific) +
  geom_line()+
  geom_errorbar(data = abundance_ind_Region, aes(x = Year, ymin = (Index_Estimate - Index_SD), ymax = (Index_Estimate + Index_SD), color = Index_Region, group = Index_Region), alpha = 0.65) +
  labs(title="Estimated Abundance \nAggregated by Region: Spring", y="Count", x="Year")+
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)+
  scale_color_manual(values = regpal) +
guides(color = guide_legend(title = NULL))
RegionalPlot

#Calculate the change in slope
abundance_ind_Region$Period<-NULL
abundance_ind_Region$Period[abundance_ind_Region$Year<2006]<-"Before Warming"
abundance_ind_Region$Period[abundance_ind_Region$Year>2005]<-"During Warming"

Reg_Abundance_coefficients_df <- abundance_ind_Region %>%
  group_by(Index_Region,Period) %>%
  do({
    model <- lm((Index_Estimate) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

Reg_Abundance_coefficients_df <- Reg_Abundance_coefficients_df%>%
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept
Reg_Abundance_coefficients_df$ordRegion<-factor(Reg_Abundance_coefficients_df$Index_Region,levels=c("Canada", "USA"))
pd <- position_dodge(.5)
write.csv(Reg_Abundance_coefficients_df, here::here("2025-04-23/Output/IndexAbundance/Reg_Abundance_coefficients_df.csv"))
ggplot(Reg_Abundance_coefficients_df  , aes(x =  fct_rev(factor(ordRegion)), y = estimate,fill=Period)) +
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


#Core Areas
abundance_ind_CA<-subset(abundance_ind_CA, abundance_ind_CA$Season=="Spring")
region_colours <- c(
  "EGOM" ="#004995",
  "BOF"  = "#8C510A",
  "CapeBreton" ="#4D4D4D",
  "HaliChan" ="#00441B",
  "CapeCod" ="#2171B5",
  "Browns" ="#D8781D",
  "Gully"="#7F7F7F",
  "GrandBanks" ="#238B45",
  "Nantucket" = "#56B4E9",
  "Georges" ="#EDA752",
  "Sable"  ="#C0C0C0",
  "GBTail" = 	"#81C784"
)
#factor the order 
abundance_ind_CA$Index_Region <- factor(abundance_ind_CA$Index_Region, levels = names(region_colours))

CAPlot<- ggplot(data = abundance_ind_CA, aes(x = Year, y = Index_Estimate, color = Index_Region))+
  geom_point(size=2)+
  scale_y_continuous(labels = scales::scientific) +
  geom_line(size=.8)+
  geom_errorbar(data = abundance_ind_CA, aes(x = Year, ymin = (Index_Estimate - Index_SD), ymax = (Index_Estimate + Index_SD), color = Index_Region, group = Index_Region), alpha = 0.65) +
  #facet_grid(.~Season)+
  scale_color_manual(values = region_colours)+
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)+
labs(title="Estimated Abundance \nAggregated by CoreArea: Spring", y="Count", x="Year")+
  guides(color = guide_legend(title = NULL))
#theme(legend.position = "none")  # Remove legend
CAPlot

#Figure4: plot abundance trends with a map of the core areas 
#go to Sup2DataPlots.R to make CAMAP
(CAMAP / CAPlot)+ plot_layout(heights = c(1,1))

#Calculate the change in slope
abundance_ind_CA$Period<-NULL
abundance_ind_CA$Period[abundance_ind_CA$Year<2006]<-"Before"
abundance_ind_CA$Period[abundance_ind_CA$Year>2005]<-"After"

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
CA_Abundance_coefficients_df$ordRegion<-factor(CA_Abundance_coefficients_df$Index_Region,levels=c("BOF", "Browns","CapeBreton", "CapeCod", "EGOM", "GBTail", "Georges", "GrandBanks", "Gully",     
                                                                                                  "HaliChan", "Nantucket",  "Sable"))
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
