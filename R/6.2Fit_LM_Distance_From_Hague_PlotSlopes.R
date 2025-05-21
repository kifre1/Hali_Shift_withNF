
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

pd <- position_dodge(.5)


#Plot 1: Core Areas, DFH time series plots----
#Read in centroid Distance from Hague. created in 06_Distance_From_Hague.R
dist_hague_CA<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_CA_seasonal.csv"))

summary(dist_hague_CA)

names(dist_hague_CA)

#set the data up for order
dist_hague_CA$Stratum<-factor(dist_hague_CA$Stratum,,levels=c("EGOM","BOF","CapeBreton","HaliChan",
                                                                  "CapeCod","Browns","Gully","GrandBanks",
                                                                  "Nantucket","Georges","Sable","GBTail"))
#dist_hague_CA$ordCore_Area<-factor(dist_hague_CA$Core_Area,levels=c("CapeCod","EGOM","BOF","Nantucket","Georges","Browns","Sable","Gully","CapeBreton"))

#add the time period info 
dist_hague_CA$Period<-NULL
dist_hague_CA$Period[dist_hague_CA$Year<2006]<-"Before Warming"
dist_hague_CA$Period[dist_hague_CA$Year>2005]<-"During Warming"


#Checking Georges q5 q95

gb<-dist_hague_CA[dist_hague_CA$Stratum=="Georges",]
pd <- position_dodge(.5)

ggplot(gb,aes(x = factor(Stratum), y = Dist_Mean,fill=Period)) +
  
  geom_errorbar(aes(ymin = Dist_Q5, ymax =Dist_Q95),position=pd)+
  
  geom_point(shape=21, size = 4,position=pd) +
  
  scale_fill_manual(values=c("steelblue", "wheat"))+

  coord_flip()

#Have to multiply all US ones so they are on LHS of Hague line


selected_categories <- c("CapeCod","EGOM","Nantucket","Georges")

df_transformed <- dist_hague_CA %>%
  
  mutate(
    
    Dist_Q5.OPP = if_else(Stratum %in% selected_categories, Dist_Q5 * -1, Dist_Q5))

#Have to account georges as straddling stock

selected_categories2 <- c("CapeCod","EGOM","Nantucket")

df_transformed2 <- df_transformed %>%
  
  mutate(
    
    Dist_Mean.OPP = if_else(Stratum %in% selected_categories2, Dist_Mean * -1, Dist_Mean),
    
    Dist_Med.OPP = if_else(Stratum %in% selected_categories2, Dist_Med * -1, Dist_Med),
    
    Dist_Q95.OPP = if_else(Stratum %in% selected_categories2, Dist_Q95 * -1, Dist_Q95)
    
  )
print(df_transformed2)
print(df_transformed)


# aggregate

df_transformedSpringaggagg <- df_transformed2 %>%
  filter(Season == "Spring") %>%
  
  group_by(Stratum, Period) %>%
  
  summarize(
    
    Avg_Dist_Mean.OPP = mean(Dist_Mean.OPP, na.rm = TRUE),
    
    Avg_Dist_Med.OPP = mean(Dist_Med.OPP, na.rm = TRUE),
  
    Avg_Dist_Q5.OPP = mean(Dist_Q5.OPP, na.rm = TRUE),
    
    Avg_Dist_Q95.OPP = mean(Dist_Q95.OPP, na.rm = TRUE)
    
  )
write.csv(df_transformedSpringaggagg,here::here("R/DataforFinalFigs/DistHagCASpringTransformedforFig.csv"),row.names = F)


#FIGURE 6
pd
pd <- position_dodge(.75)

ggplot(df_transformedSpringaggagg , aes(x = factor(Stratum), y = Avg_Dist_Mean.OPP,fill=Period)) +
  
  geom_linerange(aes(ymin = Avg_Dist_Q5.OPP, ymax =Avg_Dist_Q95.OPP),position=pd,color="darkgrey",size=1.5)+
  
  geom_point(shape=21, size = 4,position=pd) +
  
  scale_fill_manual(values=c("steelblue", "wheat"))+
  
  coord_flip()+
  
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  
  xlab("")+  ylab("Plot ")+
  
  ggtitle ("Q5, Average, Q95 Distance from Hague line (km) for each Core Area ")
#----

#Plot2: Core areas DFH rate of change (lm)----
# Perform lm on each group and extract coefficients

DFHcoefficients_df <- dist_hague_CA %>%
  
  group_by(Season,Period,Stratum) %>%
  
  do({
    
    model <- lm(Dist_Mean ~ Year, data = .)
    
    data.frame(t(coef(model)))
    
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
    
  }) %>%
  
  ungroup()

DFHfiltered_df <- DFHcoefficients_df[DFHcoefficients_df$Season=="Spring",]%>%
  
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept

ggplot(DFHfiltered_df  , aes(x = factor(Stratum), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high),position=pd)+
  geom_point(shape=21, size = 3,position=pd) +
  coord_flip()+
  scale_fill_manual(values=c("steelblue", "orangered"))+
  geom_vline(xintercept = seq(1.5, length(unique(DFHfiltered_df$Stratum)) - 0.5, by = 1),color = "gray", linetype = "solid", size = 0.5)+
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
 # ylim(-0.018,0.018)+
  xlab("")+  ylab("Rate of change (Dec.Deg/yr)")+
  #annotate("text", x = 1.6, y = .015, label = "North", hjust = 0,size=6) +
  #annotate("text", x = 1.6, y = -.012, label = "South", hjust = 0,size=6) +
  #labs(tag = "A. Latitude")+
  theme(plot.tag.position = c(.3, 1.05))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

#----
#PLOT3: regional DFH rate of change ----

dist_hague_Reg<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_Reg_seasonal.csv"))

dist_hague_Reg$Period<-NULL
pd <- position_dodge(.75)
dist_hague_Reg$Period[dist_hague_Reg$Year<2006]<-"Earlier 1985-2005"

dist_hague_Reg$Period[dist_hague_Reg$Year>2005]<-"Later 2006-2019"

Overall.DFHcoefficients_df <- dist_hague_Reg %>%
  
  group_by(Season,Stratum,Period) %>%
  
  do({
    
    model <- lm(Dist_Mean ~ Year, data = .)
    
    data.frame(t(coef(model)))
    
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
    
  }) %>%
    ungroup()

Overall.DFHfiltered_df <- Overall.DFHcoefficients_df[Overall.DFHcoefficients_df$Season=="Spring",]%>%
  
  filter(term == "Year")  # Replace "x" with "Intercept" to plot intercept
Overall.DFHfiltered_df$Stratum<-factor(Overall.DFHfiltered_df$Stratum,levels=c("USA","Canada"))


ggplot(Overall.DFHfiltered_df  , aes(x = factor(Stratum), y = estimate,fill=Period)) +
  geom_errorbar(aes(ymin = conf.low, ymax =conf.high, , color = factor(Stratum)),position=pd, , show.legend = FALSE)+
  geom_point(shape=21, size = 3,position=pd) +
  #scale_y_reverse()+
  coord_flip()+
      scale_fill_manual(values=c("steelblue", "orangered"))+
  scale_color_manual(values = c("Canada" = "darkorange", "USA" = "darkblue")) + # Define colors for each region
   geom_hline(yintercept=0,lty=2)+
  labs(title = " ", x = " ", y = "Rate of change (km/yr)") +
    theme(plot.tag.position = c(.3, 1.05))+
  theme(plot.margin = unit(c(0, 1, 1, 1), "cm"))
#----
#PLOT4: regional DFH time series plots----
dist_hague_Reg<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_Reg_seasonal.csv"))


dist_hague_RegFromZero<-dist_hague_Reg %>%
  mutate(
    Dist_Mean = case_when(
      Stratum %in% c("USA") ~ Dist_Mean * -1,
      TRUE ~ Dist_Mean
    ),
    Dist_Med = case_when(
      Stratum %in% c("USA") ~ Dist_Med * -1,
      TRUE ~ Dist_Med
    ),
    Dist_Q5 = case_when(
      Stratum %in% c("USA") ~ Dist_Q5 * -1,
      TRUE ~ Dist_Q5
    ),
    Dist_Q95 = case_when(
      Stratum %in% c("USA") ~ Dist_Q95 * -1,
      TRUE ~ Dist_Q95
    )
  )



#isolate spring
dist_hague_RegFromZero_Spring<-subset(dist_hague_RegFromZero, dist_hague_RegFromZero$Season=="Spring")

DFHtimeseries <- ggplot(dist_hague_RegFromZero_Spring, aes(x = Year, y = Dist_Mean, color = Stratum)) +
  geom_errorbar(aes(ymin = Dist_Q5, ymax = Dist_Q95)) +
  geom_point() +
  # Add the median points
  geom_point(aes(x = Year, y = Dist_Med), shape = 1, size = 2, color = "black", stroke = 1, show.legend = FALSE) +
  # Add a dummy layer for the legend entry for "Median"
  geom_point(data = data.frame(Year = min(dist_hague_RegFromZero_Spring$Year), 
                               Dist_Mean = min(dist_hague_RegFromZero_Spring$Dist_Mean), 
                               Stratum = NA), 
             aes(shape = "Median"), color = "black", size = 2, stroke = 1) +
  labs(title = "Mean Distance from Hague Line", x = "Year", y = "km", color = NULL, shape = NULL) + # Remove legend titles
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("darkorange", "darkblue")) +
  scale_shape_manual(values = c("Median" = 1))  # Map "Median" to the open circle shape


DFHtimeseries

#plot on top of eachother
spring_dist_hague_Reg<-subset(dist_hague_Reg, dist_hague_Reg$Season=="Spring")

Canada <- subset(spring_dist_hague_Reg, Stratum == "Canada")
USA <- subset(spring_dist_hague_Reg, Stratum == "USA")
# Calculate the range for each dataset
USA_range <- range(USA$Dist_Mean)
Canada_range <- range(Canada$Dist_Mean)


par(mar = c(5, 4, 2, 5) + 0.1,
    family = "serif") 
plot(USA$Year,USA$Dist_Mean, type = "l", col = "darkblue", ylim = USA_range, 
     xlab = "Year", ylab = "USA Distance (km)")
# Add scale numbers for DFO data
axis(2, col ="darkblue")
# Overlay NMFS data
par(new = TRUE)
plot(Canada$Year, Canada$Dist_Mean, type = "l", col = "orange", axes = FALSE, bty = "n", xlab = "", ylab = "")
# Add scale numbers for NMFS data
axis(4, ylim = Canada_range, col = "orange")
# Change color of left y-axis label
mtext("Canada Distance (km)", side = 4, line = 2)
#mtext('Distance from COG(mean) to Hague Line, Spring', side = 3)  # Add title here
abline(v = 2005, lty = 2, lwd = 1)
#----
#PLOT5: aggregated Regional DFH----
#Have to multiply US ones so they are on LHS of Hague line
selected_categories <- c("USA")
df_transformed <- dist_hague_Reg %>%
  mutate(
    Dist_Mean.OPP = if_else(Stratum %in% selected_categories, Dist_Mean * -1, Dist_Mean),
    Dist_Med.OPP = if_else(Stratum %in% selected_categories, Dist_Med * -1, Dist_Med),
    Dist_Q5.OPP = if_else(Stratum %in% selected_categories, Dist_Q5 * -1, Dist_Q5),
    Dist_Q95.OPP = if_else(Stratum %in% selected_categories, Dist_Q95 * -1, Dist_Q95)
  )
print(df_transformed)

# aggregate
#add the time period info 
df_transformed$Period<-NULL
df_transformed$Period[df_transformed$Year<2006]<-"Before Warming"
df_transformed$Period[df_transformed$Year>2005]<-"During Warming"

df_transformedSpringaggagg <- df_transformed %>%
  filter(Season == "Spring") %>%
  
  group_by(Stratum, Period) %>%
  
  summarize(
    
    Avg_Dist_Mean.OPP = mean(Dist_Mean.OPP, na.rm = TRUE),
    
    Avg_Dist_Med.OPP = mean(Dist_Med.OPP, na.rm = TRUE),
    
    Avg_Dist_Q5.OPP = mean(Dist_Q5.OPP, na.rm = TRUE),
    
    Avg_Dist_Q95.OPP = mean(Dist_Q95.OPP, na.rm = TRUE)
    
  )
write.csv(df_transformedSpringaggagg,here::here("R/DataforFinalFigs/DistHagRegSpringTransformedforFig.csv"),row.names = F)

#FIGURE
pd <- position_dodge(.75)

ggplot(df_transformedSpringaggagg , aes(x = factor(Stratum), y = Avg_Dist_Mean.OPP,fill=Period)) +
  
  geom_linerange(aes(ymin = Avg_Dist_Q5.OPP, ymax =Avg_Dist_Q95.OPP),position=pd,color="darkgrey",size=1.5)+
  
  geom_point(shape=21, size = 4,position=pd) +
  
  scale_fill_manual(values=c("steelblue", "wheat"))+
  
  coord_flip()+
  
  #geom_vline(xintercept=c(1.5,2.5),lty=2,col="gray50")+
  
  geom_hline(yintercept=0,lty=2)+#geom_text(aes(label=paste("R2=",signif(R2,digits=2)),x=1.2,y=min(slope)),cex=2)+
  
  xlab("")+  ylab("Plot ")+
  
  ggtitle ("Q5, Average, Q95 Distance from Hague line (km) ")
#----

#Other archived plots----

library(ggplot2)
dist_hague_all<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_all_seasonal.csv"))
dist_hague_CA<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_CA_seasonal.csv"))
dist_hague_Reg<-read.csv(here::here("2025-04-23/Output/Shift_Indicators/dist_hague_Reg_seasonal.csv"))
head(dist_hague_Reg)

dist_hague_all$Dist_Q5<- dist_hague_all$Dist_Q5*-1 # becasue Dist_Q5 for the area as a whole is on the US side of the Hague 

dist_hague_all <- dist_hague_all %>%
  filter(Season == "Spring" ) 

#Overall "COG Distance from Hague Line (Spring)"
ggplot(dist_hague_all, aes(x = Year, y = Dist_Mean, color=Season)) +
  geom_point() +
  geom_errorbar(aes(ymin = Dist_Q5, ymax = Dist_Q95), width = 0.2,col="darkgrey") +  # Add error bars
  geom_point(aes(x = Year, y = Dist_Med),shape=1,size=2,stroke=1)+
  #facet_wrap(.~Season, scales="free") +
  labs(title = "COG Distance from Hague Line (Spring)", x = "Year", y = "km") +
  theme_bw()

#core area - start with spring
#Put Q5 in the US for Georges
dist_hague_CA <- dist_hague_CA %>%
  mutate(Dist_Q5 = if_else(Stratum == "Georges", Dist_Q5 * -1, Dist_Q5))

#TODO- mutate nantucket, EGOM, CapeCOd
#subset spring
spring_dist_hague_CA <- dist_hague_CA %>%
  filter(Season == "Spring" ) 
#factor levels for plotting
spring_dist_hague_CA$Stratum<-factor(spring_dist_hague_CA$Stratum,levels=c("EGOM","BOF","CapeBreton","HaliChan",
                                                                           "CapeCod","Browns","Gully","GrandBanks",
                                                                           "Nantucket","Georges","Sable","GBTail"))
#"COG Distance from Hague Line: Spring" per core area
ggplot(spring_dist_hague_CA, aes(x = Year, y = Dist_Mean)) +
  geom_errorbar(aes(ymin = Dist_Q5, ymax = Dist_Q95), width = 0.2, col="darkgrey") +  # Add error bars
  geom_point(colour="blue") +
  geom_point(aes(x = Year, y = Dist_Med),shape=1,size=2,col="black",stroke=1)+
  facet_wrap(.~Stratum, scales="free") +
  labs(title = "COG Distance from Hague Line: Spring", x = "Year", y = "km") +
  theme_bw()


#Look at EGOM and BOF as a pair
subCA_distHagueEGBOF <- dist_hague_ca_DisplayFromZero %>%
  filter(Stratum == "EGOM" | Stratum == "BOF")%>%
  filter(Season == "Spring" )


ggplot(subCA_distHagueEGBOF, aes(y= Year*-1, x = Dist_Mean, color=Stratum)) +
  geom_point(aes(x=Dist_Med),shape=1,size=2,col="black",stroke=1)+
  geom_errorbar(aes(xmin=Dist_Q5,xmax=Dist_Q95)) +
  #geom_point(pch=1,size=5,col="black") +
  geom_point() +
  scale_x_continuous(breaks=seq(-150,150,25)) +
  scale_color_manual(name = " ", values = c("darkorange", "blue")) +
  #labs(title = "COG Distance from Hague Line: Spring", x = "Year", y = "km") +
  labs(title = "COG Distance from Hague Line \n EGOM vs. BOF", y = "", x = "km") +  
  #geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  #annotate("text",x=1990,y=-20,label="USA", color = "black",size = 4) +
  #annotate("text",x=1990,y=20,label="Canada", vjust = 0, color = "black",size = 4) +
  theme_bw()



