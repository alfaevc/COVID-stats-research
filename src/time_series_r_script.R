library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(ggseas)

data <- read_csv("covid.com.csv") %>% 
  select(c(-1)) 
data <- data[!is.na(data$NEWCD_NORM_500),]


time_series_plot <- function(state, plot_lst){
  cases_data <- data[data$STATE==state,]
  cases_data <- select(cases_data, c(3, 9))
  
  plot1 <- ggplot(data.frame(cases_data), aes(DATE, NEWCD_NORM_500)) + 
    geom_line() + 
    labs(x = "Time index", y = "New Daily Cases Normalized",
         title=state)

  
  plot2 <- ggplot(data.frame(cases_data), aes(DATE, NEWCD_NORM_500)) + 
    geom_line(col="red") + 
    labs(x = "Time index", y = "New Daily Cases Normalized")+
    stat_rollapplyr(width = 7, align = "right", size = 2, alpha=.4) + 
    ggtitle(paste("Width=7, ", state))
  #print(moving_avg_weekly)
  
  
  plot3 <- ggplot(data.frame(cases_data), aes(DATE, NEWCD_NORM_500)) + 
    geom_line(col="red") + 
    labs(x = "Time index", y = "New Daily Cases Normalized")+
    stat_rollapplyr(width = 14, align = "right", size = 2, alpha=.4) + 
    ggtitle(paste("Width=14, ", state))
  #print(moving_avg_biweekly)
  
  pdf(paste(state, "-part1.pdf"))
  print(plot1)
  print(plot2)
  print(plot3)
  acf(cases_data$NEWCD_NORM_500, plot=TRUE, main=paste("lag=30", state))
  acf(cases_data$NEWCD_NORM_500, plot=TRUE, lag.max=60,
               main=paste("lag=60",state))
  acf(cases_data$NEWCD_NORM_500, plot=TRUE, lag.max=90,
               main=paste("lag=90", state))
  dev.off()
  
}

n.colmeans = function(df, n = 7){
  aggregate(x = df,
            by = list(gl(ceiling(nrow(df)/n), n)[1:nrow(df)]),
            FUN = sum)}


weekly_lag_plot <- function(state, plot_lst){
  cases_data <- data[data$STATE==state,]
  cases_data <- select(cases_data, c(3, 9))
  cases_week <- n.colmeans(data.frame(cases_data$NEWCD_NORM_500))
  cases_week$week <- 1:nrow(cases_week)
  cases_week$week <- seq(from=as.Date("2020-01-23"), by="week",
                         length.out = nrow(cases_week))
  cases_week <- cases_week %>% 
    select(c(-1)) %>% 
    rename(new_norm_cases = cases_data.NEWCD_NORM_500)
  
  pdf(paste(state, "-part2.pdf"))

  acf(cases_week$new_norm_cases, plot=TRUE, lag.max=12,
               main=paste("lag=12", state))
  acf(cases_week$new_norm_cases, plot=TRUE, lag.max=16, 
               main=paste("lag=16", state))
  acf(cases_week$new_norm_cases, plot=TRUE, 
               main=paste("lag=30", state))
  dev.off()
}

state_vec = c("FLORIDA", "WASHINGTON", "PENNSYLVANIA", "OKLAHOMA", "NEW YORK",
              "GEORGIA")


for (state in state_vec){
  time_series_plot(state, plot_lst)
  weekly_lag_plot(state, plot_lst)
}

# 
# plot_lst = list()
# plot_lst <- time_series_plot("FLORIDA", plot_lst)
# #print(plot_lst)


