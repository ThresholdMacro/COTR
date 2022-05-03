## ---------------------------
##
## Script name: COTR
## Purpose of script:
## Author: Meyrick Chapman
## Date Created: 2021-07-06
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
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------

COTR<-readr::read_csv(here::here("Data/COTRFinsince2006.csv"))
#COTR<-COTR[1:29845,]
lastdate<-
  max(as.Date(COTR$Report_Date_as_MM_DD_YYYY))
COTR$Report_Date_as_MM_DD_YYYY<-as.Date(ymd(COTR$Report_Date_as_MM_DD_YYYY))
url<-"https://www.cftc.gov/dea/newcot/FinComWk.txt"
destfile<-"Data/latestFinCOTR.txt"
download.file(url,destfile)
latest<-readr::read_csv(here::here("Data/latestFinCOTR.txt"), col_names = FALSE)

names(latest)<-names(COTR)
latest$Report_Date_as_MM_DD_YYYY<-as.Date(ymd(latest$Report_Date_as_MM_DD_YYYY))
if(latest$Report_Date_as_MM_DD_YYYY[nrow(latest)]>COTR$Report_Date_as_MM_DD_YYYY[nrow(COTR)]) {
  COTR <- rbind (COTR, latest)
  
}

arrange(COTR, Report_Date_as_MM_DD_YYYY)
write.csv(COTR,"Data/COTRsince2006.csv", row.names = FALSE)

#set up a list of fixed income markets

markets <- c(
  "30-DAY FEDERAL FUNDS - CHICAGO BOARD OF TRADE",
  "3-MONTH EURODOLLARS - CHICAGO MERCANTILE EXCHANGE",
  "2-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE",
  "5-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE",
  "10-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE",
  "U.S. TREASURY BONDS - CHICAGO BOARD OF TRADE",
  "LONG-TERM U.S. TREASURY BONDS - CHICAGO BOARD OF TRADE"
)

#choise market to display
market<-NULL
for (market in markets) {
  print(market)
  
  cutoff <- as.numeric(gregexpr(" - ", market))
  mktname <- substr(market, start = 1, stop = (cutoff - 1))
  
  df <- COTR |>
    dplyr::filter(`Market_and_Exchange_Names` == market)
  
  LevNetPct <-
    data.frame(
      'period' = df$Report_Date_as_MM_DD_YYYY,
      'Leveraged_Net_Position' = df$Pct_of_OI_Lev_Money_Long_All - df$Pct_of_OI_Lev_Money_Short_All
    )
  
  AMNetPct <-
    data.frame(
      'period' = df$Report_Date_as_MM_DD_YYYY,
      'AssetManager_Net_Position' = df$Pct_of_OI_Asset_Mgr_Long_All - df$Pct_of_OI_Asset_Mgr_Short_All
    )
  
  DealNetPct <-
    data.frame(
      'period' = df$Report_Date_as_MM_DD_YYYY,
      'Dealer_Net_Position' = df$Pct_of_OI_Dealer_Long_All - df$Pct_of_OI_Dealer_Short_All
    )
  
  OtherNetPct <-
    data.frame(
      'period' = df$Report_Date_as_MM_DD_YYYY,
      'Other_Net_Position' = df$Pct_of_OI_Other_Rept_Long_All - df$Pct_of_OI_Other_Rept_Short_All
    )
  
  NonRepNetPct <-
    data.frame(
      'period' = df$Report_Date_as_MM_DD_YYYY,
      'NonReportable_Net_Position' = df$Pct_of_OI_NonRept_Long_All - df$Pct_of_OI_NonRept_Short_All
    )
  
  BuySideNet <- merge(LevNetPct, AMNetPct, by = 'period')
  Professional<-merge(BuySideNet,DealNetPct,by='period')
  Other<-merge(OtherNetPct,NonRepNetPct,by = 'period')
  All<-merge(Professional,Other, by='period')
  
  data1 <-
    All %>% pivot_longer(-period, names_to = "key", values_to = "value")
  data1 <- arrange(data1, key)
  
  p <- ggplot(data = data1,
              aes(x = period, y = value, color = key)) +
    geom_line() +
    labs(
      title = paste0("COTR: ", mktname, " % of OI (futures & options)"),
      caption = "source: CFTC, Hedge Analytics Ltd"
    )  +
    # scale_x_date(date_breaks = "24 month", date_labels =  "%Y", name = "Date") +
    scale_y_continuous(name = "Percent of total OI", labels = scales::comma)
 print(p)
  
}
