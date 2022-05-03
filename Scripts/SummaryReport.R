## ---------------------------
##
## Script name: SummaryReport.R
## Purpose of script:
## Author: Meyrick Chapman
## Date Created: 2022-04-09
## Copyright (c) Hedge Analytics Ltd, 2022
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##   
## ---------------------------

## set working directory

## project directory = default 

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readr)
library(ggpubr)
library(gridExtra)
library("grid")
library("ggplotify")
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
m_rescale<-function(x,lbound,ubound){
  fact<-(ubound-lbound)/( max(x)-min(x))
  result<-fact*(x-max(x))+ubound
  return(result)
}

ggstd<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  
  ggplot(data = Long_data,
         aes(
           x = as.Date(Date),
           y = value,
           colour = key
         )) +
    geom_line() +
    labs(title = charttitle,
         caption = chartcaption) +
    scale_x_date(date_breaks = xaxis_date_breaks,
                 date_labels =  "%b %Y",
                 name = "Date") +
    scale_y_continuous(name = yaxis_title, labels = scales::comma)+
    theme(legend.position="bottom")
}

## ---------------------------


SummaryNames <- read.csv("Data/SummaryReportNames.csv")
COTR<-read.csv("Data/COTRFinsince2006.csv")

COTR$Pct_of_OI_Dealer_Net<-COTR$Pct_of_OI_Dealer_Long_All - COTR$Pct_of_OI_Dealer_Short_All
COTR$Pct_of_OI_Asset_Mgr_Net<-COTR$Pct_of_OI_Asset_Mgr_Long_All - COTR$Pct_of_OI_Asset_Mgr_Short_All
COTR$Pct_of_OI_Lev_Money_Net<-COTR$Pct_of_OI_Lev_Money_Long_All - COTR$Pct_of_OI_Lev_Money_Short_All
COTR$Pct_of_OI_Other_Rept_Net<-COTR$Pct_of_OI_Other_Rept_Long_All - COTR$Pct_of_OI_Other_Rept_Short_All
COTR$Pct_of_OI_Tot_Rept_Net<-COTR$Pct_of_OI_Tot_Rept_Long_All - COTR$Pct_of_OI_Tot_Rept_Short_All
COTR$Pct_of_OI_NonRept_Net <-COTR$Pct_of_OI_NonRept_Long_All - COTR$Pct_of_OI_NonRept_Short_All

COTR <- 
  COTR %>%
  filter(Report_Date_as_MM_DD_YYYY >=(Sys.Date()- (365*15)))

plot_list = list()
plot_list2 = list()

for(series in SummaryNames[1:nrow(SummaryNames),1]){
  
  mkt <-
    COTR %>%
    filter(Market_and_Exchange_Names == series)
  
  mkt <- mkt[,c('Report_Date_as_MM_DD_YYYY',
      'Pct_of_OI_Dealer_Net',
      'Pct_of_OI_Asset_Mgr_Net',
      'Pct_of_OI_Lev_Money_Net',
      'Pct_of_OI_Other_Rept_Net',
      'Pct_of_OI_NonRept_Net')]
  
  colnames(mkt)[1] <- 'Date'
  colnames(mkt)[2] <- 'Dealer'
  colnames(mkt)[3] <- 'AM'
  colnames(mkt)[4] <- 'Lev'
  colnames(mkt)[5] <- 'Other'
  colnames(mkt)[6] <- 'Non-rep'
  
  nondealers <- mkt[,c('Date','Dealer')]
  nondealers$Dealer <- (-nondealers$Dealer)
  colnames(nondealers)[2] <- 'Non-dealers'
  
  mktlng <-
    mkt %>%
    pivot_longer(c(-Date), names_to = 'key', values_to = 'value')
  
  p <- ggstd(mktlng,series,"source: CFTC", "2 years", "Net long/short: % of OI")

  plot_list[[series]] <- as.grob(p)
  ggsave(paste0("ALL_",series,".png"), path = "Output/")
  
  nondealerslng <-
    nondealers %>%
    pivot_longer(c(-Date),names_to = 'key', values_to = 'value')
  
  p1 <- ggstd(nondealerslng,series,"source: CFTC", "2 years", "Net long/short: % of OI")
  plot_list2[[series]] <- as.grob(p1)
  ggsave(paste0("ND_",series,".png"), path = "Output/")
  
}

library(cowplot)

#All positions
#Equities

p<-plot_grid(
    plot_list[[1]],
    plot_list[[2]],
    ncol = 2,
    labels = LETTERS[1:2]
  )
plot_grid(plot_list[[3]], plot_list[[4]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list[[5]], plot_list[[6]],ncol=2, labels=LETTERS[1:2])
#STIR
plot_grid(plot_list[[9]], plot_list[[10]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list[[7]], plot_list[[8]],ncol=2, labels=LETTERS[1:2])
#Bonds
plot_grid(plot_list[[11]], plot_list[[12]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list[[13]], plot_list[[14]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list[[15]], plot_list[[16]],ncol=2, labels=LETTERS[1:2])
#Currencies
plot_grid(plot_list[[17]], plot_list[[18]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list[[19]], plot_list[[20]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list[[21]], plot_list[[22]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list[[23]], plot_list[[24]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list[[25]], plot_list[[26]],ncol=2, labels=LETTERS[1:2])
#Other
plot_grid(plot_list[[27]], plot_list[[28]],ncol=2, labels=LETTERS[1:2])

#Non-dealer positioning
#Equities
plot_grid(plot_list2[[1]], plot_list2[[2]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list2[[3]], plot_list2[[4]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list2[[5]], plot_list2[[6]],ncol=2, labels=LETTERS[1:2])
#STIR
plot_grid(plot_list2[[9]], plot_list2[[10]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list2[[7]], plot_list2[[8]],ncol=2, labels=LETTERS[1:2])
#Bonds
plot_grid(plot_list2[[11]], plot_list2[[12]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list2[[13]], plot_list2[[14]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list2[[15]], plot_list2[[16]],ncol=2, labels=LETTERS[1:2])
#Currencies
plot_grid(plot_list2[[17]], plot_list2[[18]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list2[[19]], plot_list2[[20]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list2[[21]], plot_list2[[22]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list2[[23]], plot_list2[[24]],ncol=2, labels=LETTERS[1:2])
plot_grid(plot_list2[[25]], plot_list2[[26]],ncol=2, labels=LETTERS[1:2])
#Other
plot_grid(plot_list2[[27]], plot_list2[[28]],ncol=2, labels=LETTERS[1:2])

plot <- 7
plot_grid(plot_list2[[plot]], plot_list[[plot]],ncol=2, labels=LETTERS[1:2])

#NASDAQ - 5 (shift to non-dealers)
#S&P500 - 4  (dealers still hold risk)
#VIX - 6 (shift to non-dealers)
# E$ - 7 (dealers still hold risk)
#UST Bond - 16 (shift to non-dealers)
#AUD - 17 (dealers still hold risk) also most FX markets
