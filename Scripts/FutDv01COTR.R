## ---------------------------
##
## Script name: FutDv01COTR
## Purpose of script: calculate Dv01 for net open positions in Trys futures reported in COTR 
## Author: Meyrick Chapman
## Date Created: 2021-10-05
## Copyright (c) Hedge Analytics Ltd, 2021
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
library(rdbnomics)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(rvest)
library(stringr)
library(ggvis)
library(knitr)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
ggstandard<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  
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
    scale_y_continuous(name = yaxis_title, labels = scales::comma)
}


## ---------------------------
FutDv01spec <- read.csv("Data/FutDv01specs.csv")

COTR<-readr::read_csv("Data/COTRFinsince2006.csv")
COTR$Report_Date_as_MM_DD_YYYY<-as.Date(ymd(COTR$Report_Date_as_MM_DD_YYYY))

COTR<-arrange(COTR, Report_Date_as_MM_DD_YYYY)

COTR$Dealer_Positions_Net<-COTR$Dealer_Positions_Long_All - COTR$Dealer_Positions_Short_All
COTR$Asset_Mgr_Positions_Net<-COTR$Asset_Mgr_Positions_Long_All - COTR$Asset_Mgr_Positions_Short_All
COTR$Lev_Money_Positions_Net<-COTR$Lev_Money_Positions_Long_All - COTR$Lev_Money_Positions_Short_All
COTR$Other_Rept_Positions_Net<-COTR$Other_Rept_Positions_Long_All - COTR$Other_Rept_Positions_Short_All
COTR$NonRept_Positions_Net <-COTR$NonRept_Positions_Long_All - COTR$NonRept_Positions_Short_All

COTR$Pct_of_OI_Dealer_Net<-COTR$Pct_of_OI_Dealer_Long_All - COTR$Pct_of_OI_Dealer_Short_All
COTR$Pct_of_OI_Asset_Mgr_Net<-COTR$Pct_of_OI_Asset_Mgr_Long_All - COTR$Pct_of_OI_Asset_Mgr_Short_All
COTR$Pct_of_OI_Lev_Money_Net<-COTR$Pct_of_OI_Lev_Money_Long_All - COTR$Pct_of_OI_Lev_Money_Short_All
COTR$Pct_of_OI_Other_Rept_Net<-COTR$Pct_of_OI_Other_Rept_Long_All - COTR$Pct_of_OI_Other_Rept_Short_All
COTR$Pct_of_OI_NonRept_Net <-COTR$Pct_of_OI_NonRept_Long_All - COTR$Pct_of_OI_NonRept_Short_All

sincedate<-unique(COTR$Report_Date_as_MM_DD_YYYY)
defaultdate<-as.Date(sincedate[length(sincedate)-52])

column_names<-readr::read_csv(here::here("Data/columns.csv"),col_names = FALSE)
OIraw<-as.data.frame(column_names[1:20,])

data2 <-
  data.frame('period' = as.Date(COTR$Report_Date_as_MM_DD_YYYY), COTR$Market_and_Exchange_Names, COTR[, names(select(COTR, contains("Positions_Net")))])


FutDv01<-readr::read_csv("Data/FutDv01.csv")
#names(FutDv01)[names(FutDv01) == "Date"] <- "period"
FutDv01$period<-as.Date(ymd(FutDv01$period))

FutDv01<- FutDv01 |>
  dplyr::filter(period>=defaultdate)

plot_list = list()

allDur <- data.frame(matrix(NA,nrow=0,ncol=7))
names(allDur) <-
  c(
    "Dealer_Positions_Net",
    "Asset_Mgr_Positions_Net",
    "Lev_Money_Positions_Net",
    "Other_Rept_Positions_Net",
    "NonRept_Positions_Net",
    "period",
    "markets"
  )


for(markets in names(FutDv01[2:ncol(FutDv01)])){
  tickvalue <- FutDv01spec[match(markets, FutDv01spec$Name),'Price.per.tick']
  df <- data2 |>
    dplyr::filter(period>=defaultdate, COTR.Market_and_Exchange_Names == markets)
Dur<-as.data.frame(sapply(df[,c(3:7)]*tickvalue, '*', FutDv01[,markets]))

names(Dur)<-names(df[3:7])
Dur$Date<-FutDv01$period
Durlng<-
  Dur %>% pivot_longer(c(-Date), names_to = "key", values_to = "value")

p<-ggstandard(Durlng,paste0("Dv01: ",markets),"source: CFTC,Hedge Analytics","2 month","US$")

plot_list[[markets]] = p

df1 <- cbind(Dur,markets)
allDur <- rbind(allDur,df1)

}

library(gridExtra)
library(grid)
library("ggplotify")

n <- length(plot_list[1:2])
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plot_list[1:2], ncol=nCol))

n <- length(plot_list[3:4])
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plot_list[3:4], ncol=nCol))

n <- length(plot_list[5:6])
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plot_list[5:6], ncol=nCol))

target <-
  c(
    "UST 2Y NOTE - CHICAGO BOARD OF TRADE",
    "UST 5Y NOTE - CHICAGO BOARD OF TRADE",
    "UST 10Y NOTE - CHICAGO BOARD OF TRADE",
    "ULTRA UST 10Y - CHICAGO BOARD OF TRADE",
    "UST BOND - CHICAGO BOARD OF TRADE",
    "ULTRA UST BOND - CHICAGO BOARD OF TRADE"
  )

USTLevAMDur <-
  allDur %>%
  filter(markets %in% target) 

LevInvestors <- match('Lev_Money_Positions_Net', names(USTLevAMDur))
AMInvestors <- match('Asset_Mgr_Positions_Net', names(USTLevAMDur))

USTLevDur <- USTLevAMDur[,c(6,7,LevInvestors)]
  
  lngUSTLevDur <- USTLevDur
names(lngUSTLevDur) <- c('Date','key','value')    
  
p<-ggstandard(lngUSTLevDur,"CBoT bond futures held by Leveraged Accounts, net Dv01","source: CFTC,Hedge Analytics","2 month","$Dv01")
p

USTAMDur <- USTLevAMDur[,c(6,7,AMInvestors)]

lngUSTAMDur <- USTAMDur
names(lngUSTAMDur) <- c('Date','key','value')    

p<-ggstandard(lngUSTAMDur,"CBoT bond futures held by asset managers, net Dv01","source: CFTC,Hedge Analytics","2 month","$Dv01")
p
