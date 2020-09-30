#############################################################
#import library
#############################################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggrepel)






#############################################################
#import data and preprocess color scale
#############################################################
data_path <- "score_case_combined.csv"
data <- read_csv(data_path)

#preprocess data with color scale
bins <- c(-Inf,0,0.25,0.75,1.20,1.55,1.85,2.10,2.35,2.70,3.05,3.40,3.75,
          4.00)
data$color_scale <- cut(data$SCORE, breaks=bins)
color_v <- c("#ffffff","#fdfec0","#ffd799","#ffb078","#fc8861","#f2615d",
             "#d8466c","#87377a","#942c81","#721f82","#51127c", "#2d1160",
             "#100c2e")
names(color_v) = levels(data$color_scale)



#############################################################
#version 1 plot with colored points
#############################################################
version1_plot <- function(state){
  state_data <- data[data$STATE==state,]
  state_event <- state_data[!is.na(state_data$EVENT_CATG_S),]
  state_event_date <- state_event$DATE
  plot_1 <- ggplot(state_data, 
                   mapping=aes(x=DATE, y=NEWCD_NORM_500, fill=color_scale))+
    geom_point(shape=21, col="gray",size=5)+
    scale_fill_manual(values=color_v)+
    geom_vline(xintercept=state_event_date, linetype="dotted")+
    geom_text(data=state_event, mapping=aes(label=EVENT_CATG_S), 
    size=4, angle=90, vjust=-1, hjust=-2) +
    labs(x='Date', y='New_Cases_Norm', title=state, fill='score')+
    theme_bw()
  plot_1
}




#############################################################
#version 2 plot with separate color line
#############################################################
data$dummy <- -10
version2_plot <- function(state){
  state_data <- data[data$STATE==state,]
  state_event <- state_data[!is.na(state_data$EVENT_CATG_S),]
  state_event_date <- state_event$DATE
  plot_2 <- ggplot(state_data)+
    geom_point(mapping=aes(x=DATE, y=NEWCD_NORM_500), shape=21,
               fill="black", size=4)+
    geom_point(mapping=aes(x=DATE, y=dummy, fill=color_scale),
               shape=22, size=4, color="transparent")+
    scale_fill_manual(values=color_v)+
    geom_vline(xintercept=state_event_date, linetype="dotted")+
    geom_text(data=state_event, mapping=aes(x=DATE, y=NEWCD_NORM_500,
                                            label=EVENT_CATG_S), 
              size=4, angle=90, vjust=-1, hjust=-2) +
    labs(x='Date', y='New_Cases_Norm', title=state, fill='score')
  plot_2
}



